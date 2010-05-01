;;; PHP for GNU Guile.

;; Copyright (C) 2010 Jon Herron

;; This library is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.
;; 
;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.
;; 
;; You should have received a copy of the GNU Lesser General Public
;; License along with this library; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301 USA

;;; Code:

;;; defined in tokenize-base.scm

(define-module (language php tokenize)
  #:use-module (system base lalr)
  #:export (make-tokenizer))

(define parse-mode 'txt)

(define (make-tokenizer port)
  (lexer-init 'port port)
  lexer)

(define (make-token tok txt)
  (if (eq? parse-mode 'txt)
    (make-lexical-token 'T_INLINE_HTML #f txt)
    (make-lexical-token tok #f txt)))

(define (lexer-error tok)
  (syntax-error "Invalid token: " tok))

(define (syntax-error message . args)
  (apply throw 'SyntaxError message args))

(define (read-string str-char yygetc yyungetc)
  (define tok 'T_CONSTANT_ENCAPSULATED_STRING)
  (define (get-char)
    (let ((c (yygetc)))
      (if (char=? c #\\)
        (let ((nc (yygetc)))
          (cond 
            ((char=? nc #\n) #\newline)
            ((char=? nc #\t) #\tab)
            (else (begin (yyungetc) c))))
        c)))
  (let loop ((s '()))
    (let ((c (get-char)))
      (if (eq? c 'eof)
        (syntax-error "Unexpected eof inside string"))
      (if (char=? c str-char)
        (make-token tok (list->string (reverse s)))
        (loop (cons c s))))))

;;; below merged in from tokenize-silex.scm


; *** This file starts with a copy of the file multilex.scm ***
; SILex - Scheme Implementation of Lex
; Copyright (C) 2001  Danny Dube'
; 
; This program is free software; you can redistribute it and/or
; modify it under the terms of the GNU General Public License
; as published by the Free Software Foundation; either version 2
; of the License, or (at your option) any later version.
; 
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
; 
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

;
; Gestion des Input Systems
; Fonctions a utiliser par l'usager:
;   lexer-make-IS, lexer-get-func-getc, lexer-get-func-ungetc,
;   lexer-get-func-line, lexer-get-func-column et lexer-get-func-offset
;

; Taille initiale par defaut du buffer d'entree
(define lexer-init-buffer-len 1024)

; Numero du caractere newline
(define lexer-integer-newline (char->integer #\newline))

; Constructeur d'IS brut
(define lexer-raw-IS-maker
  (lambda (buffer read-ptr input-f counters)
    (let ((input-f          input-f)                ; Entree reelle
	  (buffer           buffer)                 ; Buffer
	  (buflen           (string-length buffer))
	  (read-ptr         read-ptr)
	  (start-ptr        1)                      ; Marque de debut de lexeme
	  (start-line       1)
	  (start-column     1)
	  (start-offset     0)
	  (end-ptr          1)                      ; Marque de fin de lexeme
	  (point-ptr        1)                      ; Le point
	  (user-ptr         1)                      ; Marque de l'usager
	  (user-line        1)
	  (user-column      1)
	  (user-offset      0)
	  (user-up-to-date? #t))                    ; Concerne la colonne seul.
      (letrec
	  ((start-go-to-end-none         ; Fonctions de depl. des marques
	    (lambda ()
	      (set! start-ptr end-ptr)))
	   (start-go-to-end-line
	    (lambda ()
	      (let loop ((ptr start-ptr) (line start-line))
		(if (= ptr end-ptr)
		    (begin
		      (set! start-ptr ptr)
		      (set! start-line line))
		    (if (char=? (string-ref buffer ptr) #\newline)
			(loop (+ ptr 1) (+ line 1))
			(loop (+ ptr 1) line))))))
	   (start-go-to-end-all
	    (lambda ()
	      (set! start-offset (+ start-offset (- end-ptr start-ptr)))
	      (let loop ((ptr start-ptr)
			 (line start-line)
			 (column start-column))
		(if (= ptr end-ptr)
		    (begin
		      (set! start-ptr ptr)
		      (set! start-line line)
		      (set! start-column column))
		    (if (char=? (string-ref buffer ptr) #\newline)
			(loop (+ ptr 1) (+ line 1) 1)
			(loop (+ ptr 1) line (+ column 1)))))))
	   (start-go-to-user-none
	    (lambda ()
	      (set! start-ptr user-ptr)))
	   (start-go-to-user-line
	    (lambda ()
	      (set! start-ptr user-ptr)
	      (set! start-line user-line)))
	   (start-go-to-user-all
	    (lambda ()
	      (set! start-line user-line)
	      (set! start-offset user-offset)
	      (if user-up-to-date?
		  (begin
		    (set! start-ptr user-ptr)
		    (set! start-column user-column))
		  (let loop ((ptr start-ptr) (column start-column))
		    (if (= ptr user-ptr)
			(begin
			  (set! start-ptr ptr)
			  (set! start-column column))
			(if (char=? (string-ref buffer ptr) #\newline)
			    (loop (+ ptr 1) 1)
			    (loop (+ ptr 1) (+ column 1))))))))
	   (end-go-to-point
	    (lambda ()
	      (set! end-ptr point-ptr)))
	   (point-go-to-start
	    (lambda ()
	      (set! point-ptr start-ptr)))
	   (user-go-to-start-none
	    (lambda ()
	      (set! user-ptr start-ptr)))
	   (user-go-to-start-line
	    (lambda ()
	      (set! user-ptr start-ptr)
	      (set! user-line start-line)))
	   (user-go-to-start-all
	    (lambda ()
	      (set! user-ptr start-ptr)
	      (set! user-line start-line)
	      (set! user-column start-column)
	      (set! user-offset start-offset)
	      (set! user-up-to-date? #t)))
	   (init-lexeme-none             ; Debute un nouveau lexeme
	    (lambda ()
	      (if (< start-ptr user-ptr)
		  (start-go-to-user-none))
	      (point-go-to-start)))
	   (init-lexeme-line
	    (lambda ()
	      (if (< start-ptr user-ptr)
		  (start-go-to-user-line))
	      (point-go-to-start)))
	   (init-lexeme-all
	    (lambda ()
	      (if (< start-ptr user-ptr)
		  (start-go-to-user-all))
	      (point-go-to-start)))
	   (get-start-line               ; Obtention des stats du debut du lxm
	    (lambda ()
	      start-line))
	   (get-start-column
	    (lambda ()
	      start-column))
	   (get-start-offset
	    (lambda ()
	      start-offset))
	   (peek-left-context            ; Obtention de caracteres (#f si EOF)
	    (lambda ()
	      (char->integer (string-ref buffer (- start-ptr 1)))))
	   (peek-char
	    (lambda ()
	      (if (< point-ptr read-ptr)
		  (char->integer (string-ref buffer point-ptr))
		  (let ((c (input-f)))
		    (if (char? c)
			(begin
			  (if (= read-ptr buflen)
			      (reorganize-buffer))
			  (string-set! buffer point-ptr c)
			  (set! read-ptr (+ point-ptr 1))
			  (char->integer c))
			(begin
			  (set! input-f (lambda () 'eof))
			  #f))))))
	   (read-char
	    (lambda ()
	      (if (< point-ptr read-ptr)
		  (let ((c (string-ref buffer point-ptr)))
		    (set! point-ptr (+ point-ptr 1))
		    (char->integer c))
		  (let ((c (input-f)))
		    (if (char? c)
			(begin
			  (if (= read-ptr buflen)
			      (reorganize-buffer))
			  (string-set! buffer point-ptr c)
			  (set! read-ptr (+ point-ptr 1))
			  (set! point-ptr read-ptr)
			  (char->integer c))
			(begin
			  (set! input-f (lambda () 'eof))
			  #f))))))
	   (get-start-end-text           ; Obtention du lexeme
	    (lambda ()
	      (substring buffer start-ptr end-ptr)))
	   (get-user-line-line           ; Fonctions pour l'usager
	    (lambda ()
	      (if (< user-ptr start-ptr)
		  (user-go-to-start-line))
	      user-line))
	   (get-user-line-all
	    (lambda ()
	      (if (< user-ptr start-ptr)
		  (user-go-to-start-all))
	      user-line))
	   (get-user-column-all
	    (lambda ()
	      (cond ((< user-ptr start-ptr)
		     (user-go-to-start-all)
		     user-column)
		    (user-up-to-date?
		     user-column)
		    (else
		     (let loop ((ptr start-ptr) (column start-column))
		       (if (= ptr user-ptr)
			   (begin
			     (set! user-column column)
			     (set! user-up-to-date? #t)
			     column)
			   (if (char=? (string-ref buffer ptr) #\newline)
			       (loop (+ ptr 1) 1)
			       (loop (+ ptr 1) (+ column 1)))))))))
	   (get-user-offset-all
	    (lambda ()
	      (if (< user-ptr start-ptr)
		  (user-go-to-start-all))
	      user-offset))
	   (user-getc-none
	    (lambda ()
	      (if (< user-ptr start-ptr)
		  (user-go-to-start-none))
	      (if (< user-ptr read-ptr)
		  (let ((c (string-ref buffer user-ptr)))
		    (set! user-ptr (+ user-ptr 1))
		    c)
		  (let ((c (input-f)))
		    (if (char? c)
			(begin
			  (if (= read-ptr buflen)
			      (reorganize-buffer))
			  (string-set! buffer user-ptr c)
			  (set! read-ptr (+ read-ptr 1))
			  (set! user-ptr read-ptr)
			  c)
			(begin
			  (set! input-f (lambda () 'eof))
			  'eof))))))
	   (user-getc-line
	    (lambda ()
	      (if (< user-ptr start-ptr)
		  (user-go-to-start-line))
	      (if (< user-ptr read-ptr)
		  (let ((c (string-ref buffer user-ptr)))
		    (set! user-ptr (+ user-ptr 1))
		    (if (char=? c #\newline)
			(set! user-line (+ user-line 1)))
		    c)
		  (let ((c (input-f)))
		    (if (char? c)
			(begin
			  (if (= read-ptr buflen)
			      (reorganize-buffer))
			  (string-set! buffer user-ptr c)
			  (set! read-ptr (+ read-ptr 1))
			  (set! user-ptr read-ptr)
			  (if (char=? c #\newline)
			      (set! user-line (+ user-line 1)))
			  c)
			(begin
			  (set! input-f (lambda () 'eof))
			  'eof))))))
	   (user-getc-all
	    (lambda ()
	      (if (< user-ptr start-ptr)
		  (user-go-to-start-all))
	      (if (< user-ptr read-ptr)
		  (let ((c (string-ref buffer user-ptr)))
		    (set! user-ptr (+ user-ptr 1))
		    (if (char=? c #\newline)
			(begin
			  (set! user-line (+ user-line 1))
			  (set! user-column 1))
			(set! user-column (+ user-column 1)))
		    (set! user-offset (+ user-offset 1))
		    c)
		  (let ((c (input-f)))
		    (if (char? c)
			(begin
			  (if (= read-ptr buflen)
			      (reorganize-buffer))
			  (string-set! buffer user-ptr c)
			  (set! read-ptr (+ read-ptr 1))
			  (set! user-ptr read-ptr)
			  (if (char=? c #\newline)
			      (begin
				(set! user-line (+ user-line 1))
				(set! user-column 1))
			      (set! user-column (+ user-column 1)))
			  (set! user-offset (+ user-offset 1))
			  c)
			(begin
			  (set! input-f (lambda () 'eof))
			  'eof))))))
	   (user-ungetc-none
	    (lambda ()
	      (if (> user-ptr start-ptr)
		  (set! user-ptr (- user-ptr 1)))))
	   (user-ungetc-line
	    (lambda ()
	      (if (> user-ptr start-ptr)
		  (begin
		    (set! user-ptr (- user-ptr 1))
		    (let ((c (string-ref buffer user-ptr)))
		      (if (char=? c #\newline)
			  (set! user-line (- user-line 1))))))))
	   (user-ungetc-all
	    (lambda ()
	      (if (> user-ptr start-ptr)
		  (begin
		    (set! user-ptr (- user-ptr 1))
		    (let ((c (string-ref buffer user-ptr)))
		      (if (char=? c #\newline)
			  (begin
			    (set! user-line (- user-line 1))
			    (set! user-up-to-date? #f))
			  (set! user-column (- user-column 1)))
		      (set! user-offset (- user-offset 1)))))))
	   (reorganize-buffer            ; Decaler ou agrandir le buffer
	    (lambda ()
	      (if (< (* 2 start-ptr) buflen)
		  (let* ((newlen (* 2 buflen))
			 (newbuf (make-string newlen))
			 (delta (- start-ptr 1)))
		    (let loop ((from (- start-ptr 1)))
		      (if (< from buflen)
			  (begin
			    (string-set! newbuf
					 (- from delta)
					 (string-ref buffer from))
			    (loop (+ from 1)))))
		    (set! buffer    newbuf)
		    (set! buflen    newlen)
		    (set! read-ptr  (- read-ptr delta))
		    (set! start-ptr (- start-ptr delta))
		    (set! end-ptr   (- end-ptr delta))
		    (set! point-ptr (- point-ptr delta))
		    (set! user-ptr  (- user-ptr delta)))
		  (let ((delta (- start-ptr 1)))
		    (let loop ((from (- start-ptr 1)))
		      (if (< from buflen)
			  (begin
			    (string-set! buffer
					 (- from delta)
					 (string-ref buffer from))
			    (loop (+ from 1)))))
		    (set! read-ptr  (- read-ptr delta))
		    (set! start-ptr (- start-ptr delta))
		    (set! end-ptr   (- end-ptr delta))
		    (set! point-ptr (- point-ptr delta))
		    (set! user-ptr  (- user-ptr delta)))))))
	(list (cons 'start-go-to-end
		    (cond ((eq? counters 'none) start-go-to-end-none)
			  ((eq? counters 'line) start-go-to-end-line)
			  ((eq? counters 'all ) start-go-to-end-all)))
	      (cons 'end-go-to-point
		    end-go-to-point)
	      (cons 'init-lexeme
		    (cond ((eq? counters 'none) init-lexeme-none)
			  ((eq? counters 'line) init-lexeme-line)
			  ((eq? counters 'all ) init-lexeme-all)))
	      (cons 'get-start-line
		    get-start-line)
	      (cons 'get-start-column
		    get-start-column)
	      (cons 'get-start-offset
		    get-start-offset)
	      (cons 'peek-left-context
		    peek-left-context)
	      (cons 'peek-char
		    peek-char)
	      (cons 'read-char
		    read-char)
	      (cons 'get-start-end-text
		    get-start-end-text)
	      (cons 'get-user-line
		    (cond ((eq? counters 'none) #f)
			  ((eq? counters 'line) get-user-line-line)
			  ((eq? counters 'all ) get-user-line-all)))
	      (cons 'get-user-column
		    (cond ((eq? counters 'none) #f)
			  ((eq? counters 'line) #f)
			  ((eq? counters 'all ) get-user-column-all)))
	      (cons 'get-user-offset
		    (cond ((eq? counters 'none) #f)
			  ((eq? counters 'line) #f)
			  ((eq? counters 'all ) get-user-offset-all)))
	      (cons 'user-getc
		    (cond ((eq? counters 'none) user-getc-none)
			  ((eq? counters 'line) user-getc-line)
			  ((eq? counters 'all ) user-getc-all)))
	      (cons 'user-ungetc
		    (cond ((eq? counters 'none) user-ungetc-none)
			  ((eq? counters 'line) user-ungetc-line)
			  ((eq? counters 'all ) user-ungetc-all))))))))

; Construit un Input System
; Le premier parametre doit etre parmi "port", "procedure" ou "string"
; Prend un parametre facultatif qui doit etre parmi
; "none", "line" ou "all"
(define lexer-make-IS
  (lambda (input-type input . largs)
    (let ((counters-type (cond ((null? largs)
				'line)
			       ((memq (car largs) '(none line all))
				(car largs))
			       (else
				'line))))
      (cond ((and (eq? input-type 'port) (input-port? input))
	     (let* ((buffer   (make-string lexer-init-buffer-len #\newline))
		    (read-ptr 1)
		    (input-f  (lambda () (read-char input))))
	       (lexer-raw-IS-maker buffer read-ptr input-f counters-type)))
	    ((and (eq? input-type 'procedure) (procedure? input))
	     (let* ((buffer   (make-string lexer-init-buffer-len #\newline))
		    (read-ptr 1)
		    (input-f  input))
	       (lexer-raw-IS-maker buffer read-ptr input-f counters-type)))
	    ((and (eq? input-type 'string) (string? input))
	     (let* ((buffer   (string-append (string #\newline) input))
		    (read-ptr (string-length buffer))
		    (input-f  (lambda () 'eof)))
	       (lexer-raw-IS-maker buffer read-ptr input-f counters-type)))
	    (else
	     (let* ((buffer   (string #\newline))
		    (read-ptr 1)
		    (input-f  (lambda () 'eof)))
	       (lexer-raw-IS-maker buffer read-ptr input-f counters-type)))))))

; Les fonctions:
;   lexer-get-func-getc, lexer-get-func-ungetc,
;   lexer-get-func-line, lexer-get-func-column et lexer-get-func-offset
(define lexer-get-func-getc
  (lambda (IS) (cdr (assq 'user-getc IS))))
(define lexer-get-func-ungetc
  (lambda (IS) (cdr (assq 'user-ungetc IS))))
(define lexer-get-func-line
  (lambda (IS) (cdr (assq 'get-user-line IS))))
(define lexer-get-func-column
  (lambda (IS) (cdr (assq 'get-user-column IS))))
(define lexer-get-func-offset
  (lambda (IS) (cdr (assq 'get-user-offset IS))))

;
; Gestion des lexers
;

; Fabrication de lexer a partir d'arbres de decision
(define lexer-make-tree-lexer
  (lambda (tables IS)
    (letrec
	(; Contenu de la table
	 (counters-type        (vector-ref tables 0))
	 (<<EOF>>-pre-action   (vector-ref tables 1))
	 (<<ERROR>>-pre-action (vector-ref tables 2))
	 (rules-pre-actions    (vector-ref tables 3))
	 (table-nl-start       (vector-ref tables 5))
	 (table-no-nl-start    (vector-ref tables 6))
	 (trees-v              (vector-ref tables 7))
	 (acc-v                (vector-ref tables 8))

	 ; Contenu du IS
	 (IS-start-go-to-end    (cdr (assq 'start-go-to-end IS)))
	 (IS-end-go-to-point    (cdr (assq 'end-go-to-point IS)))
	 (IS-init-lexeme        (cdr (assq 'init-lexeme IS)))
	 (IS-get-start-line     (cdr (assq 'get-start-line IS)))
	 (IS-get-start-column   (cdr (assq 'get-start-column IS)))
	 (IS-get-start-offset   (cdr (assq 'get-start-offset IS)))
	 (IS-peek-left-context  (cdr (assq 'peek-left-context IS)))
	 (IS-peek-char          (cdr (assq 'peek-char IS)))
	 (IS-read-char          (cdr (assq 'read-char IS)))
	 (IS-get-start-end-text (cdr (assq 'get-start-end-text IS)))
	 (IS-get-user-line      (cdr (assq 'get-user-line IS)))
	 (IS-get-user-column    (cdr (assq 'get-user-column IS)))
	 (IS-get-user-offset    (cdr (assq 'get-user-offset IS)))
	 (IS-user-getc          (cdr (assq 'user-getc IS)))
	 (IS-user-ungetc        (cdr (assq 'user-ungetc IS)))

	 ; Resultats
	 (<<EOF>>-action   #f)
	 (<<ERROR>>-action #f)
	 (rules-actions    #f)
	 (states           #f)
	 (final-lexer      #f)

	 ; Gestion des hooks
	 (hook-list '())
	 (add-hook
	  (lambda (thunk)
	    (set! hook-list (cons thunk hook-list))))
	 (apply-hooks
	  (lambda ()
	    (let loop ((l hook-list))
	      (if (pair? l)
		  (begin
		    ((car l))
		    (loop (cdr l)))))))

	 ; Preparation des actions
	 (set-action-statics
	  (lambda (pre-action)
	    (pre-action final-lexer IS-user-getc IS-user-ungetc)))
	 (prepare-special-action-none
	  (lambda (pre-action)
	    (let ((action #f))
	      (let ((result
		     (lambda ()
		       (action "")))
		    (hook
		     (lambda ()
		       (set! action (set-action-statics pre-action)))))
		(add-hook hook)
		result))))
	 (prepare-special-action-line
	  (lambda (pre-action)
	    (let ((action #f))
	      (let ((result
		     (lambda (yyline)
		       (action "" yyline)))
		    (hook
		     (lambda ()
		       (set! action (set-action-statics pre-action)))))
		(add-hook hook)
		result))))
	 (prepare-special-action-all
	  (lambda (pre-action)
	    (let ((action #f))
	      (let ((result
		     (lambda (yyline yycolumn yyoffset)
		       (action "" yyline yycolumn yyoffset)))
		    (hook
		     (lambda ()
		       (set! action (set-action-statics pre-action)))))
		(add-hook hook)
		result))))
	 (prepare-special-action
	  (lambda (pre-action)
	    (cond ((eq? counters-type 'none)
		   (prepare-special-action-none pre-action))
		  ((eq? counters-type 'line)
		   (prepare-special-action-line pre-action))
		  ((eq? counters-type 'all)
		   (prepare-special-action-all  pre-action)))))
	 (prepare-action-yytext-none
	  (lambda (pre-action)
	    (let ((get-start-end-text IS-get-start-end-text)
		  (start-go-to-end    IS-start-go-to-end)
		  (action             #f))
	      (let ((result
		     (lambda ()
		       (let ((yytext (get-start-end-text)))
			 (start-go-to-end)
			 (action yytext))))
		    (hook
		     (lambda ()
		       (set! action (set-action-statics pre-action)))))
		(add-hook hook)
		result))))
	 (prepare-action-yytext-line
	  (lambda (pre-action)
	    (let ((get-start-end-text IS-get-start-end-text)
		  (start-go-to-end    IS-start-go-to-end)
		  (action             #f))
	      (let ((result
		     (lambda (yyline)
		       (let ((yytext (get-start-end-text)))
			 (start-go-to-end)
			 (action yytext yyline))))
		    (hook
		     (lambda ()
		       (set! action (set-action-statics pre-action)))))
		(add-hook hook)
		result))))
	 (prepare-action-yytext-all
	  (lambda (pre-action)
	    (let ((get-start-end-text IS-get-start-end-text)
		  (start-go-to-end    IS-start-go-to-end)
		  (action             #f))
	      (let ((result
		     (lambda (yyline yycolumn yyoffset)
		       (let ((yytext (get-start-end-text)))
			 (start-go-to-end)
			 (action yytext yyline yycolumn yyoffset))))
		    (hook
		     (lambda ()
		       (set! action (set-action-statics pre-action)))))
		(add-hook hook)
		result))))
	 (prepare-action-yytext
	  (lambda (pre-action)
	    (cond ((eq? counters-type 'none)
		   (prepare-action-yytext-none pre-action))
		  ((eq? counters-type 'line)
		   (prepare-action-yytext-line pre-action))
		  ((eq? counters-type 'all)
		   (prepare-action-yytext-all  pre-action)))))
	 (prepare-action-no-yytext-none
	  (lambda (pre-action)
	    (let ((start-go-to-end    IS-start-go-to-end)
		  (action             #f))
	      (let ((result
		     (lambda ()
		       (start-go-to-end)
		       (action)))
		    (hook
		     (lambda ()
		       (set! action (set-action-statics pre-action)))))
		(add-hook hook)
		result))))
	 (prepare-action-no-yytext-line
	  (lambda (pre-action)
	    (let ((start-go-to-end    IS-start-go-to-end)
		  (action             #f))
	      (let ((result
		     (lambda (yyline)
		       (start-go-to-end)
		       (action yyline)))
		    (hook
		     (lambda ()
		       (set! action (set-action-statics pre-action)))))
		(add-hook hook)
		result))))
	 (prepare-action-no-yytext-all
	  (lambda (pre-action)
	    (let ((start-go-to-end    IS-start-go-to-end)
		  (action             #f))
	      (let ((result
		     (lambda (yyline yycolumn yyoffset)
		       (start-go-to-end)
		       (action yyline yycolumn yyoffset)))
		    (hook
		     (lambda ()
		       (set! action (set-action-statics pre-action)))))
		(add-hook hook)
		result))))
	 (prepare-action-no-yytext
	  (lambda (pre-action)
	    (cond ((eq? counters-type 'none)
		   (prepare-action-no-yytext-none pre-action))
		  ((eq? counters-type 'line)
		   (prepare-action-no-yytext-line pre-action))
		  ((eq? counters-type 'all)
		   (prepare-action-no-yytext-all  pre-action)))))

	 ; Fabrique les fonctions de dispatch
	 (prepare-dispatch-err
	  (lambda (leaf)
	    (lambda (c)
	      #f)))
	 (prepare-dispatch-number
	  (lambda (leaf)
	    (let ((state-function #f))
	      (let ((result
		     (lambda (c)
		       state-function))
		    (hook
		     (lambda ()
		       (set! state-function (vector-ref states leaf)))))
		(add-hook hook)
		result))))
	 (prepare-dispatch-leaf
	  (lambda (leaf)
	    (if (eq? leaf 'err)
		(prepare-dispatch-err leaf)
		(prepare-dispatch-number leaf))))
	 (prepare-dispatch-<
	  (lambda (tree)
	    (let ((left-tree  (list-ref tree 1))
		  (right-tree (list-ref tree 2)))
	      (let ((bound      (list-ref tree 0))
		    (left-func  (prepare-dispatch-tree left-tree))
		    (right-func (prepare-dispatch-tree right-tree)))
		(lambda (c)
		  (if (< c bound)
		      (left-func c)
		      (right-func c)))))))
	 (prepare-dispatch-=
	  (lambda (tree)
	    (let ((left-tree  (list-ref tree 2))
		  (right-tree (list-ref tree 3)))
	      (let ((bound      (list-ref tree 1))
		    (left-func  (prepare-dispatch-tree left-tree))
		    (right-func (prepare-dispatch-tree right-tree)))
		(lambda (c)
		  (if (= c bound)
		      (left-func c)
		      (right-func c)))))))
	 (prepare-dispatch-tree
	  (lambda (tree)
	    (cond ((not (pair? tree))
		   (prepare-dispatch-leaf tree))
		  ((eq? (car tree) '=)
		   (prepare-dispatch-= tree))
		  (else
		   (prepare-dispatch-< tree)))))
	 (prepare-dispatch
	  (lambda (tree)
	    (let ((dicho-func (prepare-dispatch-tree tree)))
	      (lambda (c)
		(and c (dicho-func c))))))

	 ; Fabrique les fonctions de transition (read & go) et (abort)
	 (prepare-read-n-go
	  (lambda (tree)
	    (let ((dispatch-func (prepare-dispatch tree))
		  (read-char     IS-read-char))
	      (lambda ()
		(dispatch-func (read-char))))))
	 (prepare-abort
	  (lambda (tree)
	    (lambda ()
	      #f)))
	 (prepare-transition
	  (lambda (tree)
	    (if (eq? tree 'err)
		(prepare-abort     tree)
		(prepare-read-n-go tree))))

	 ; Fabrique les fonctions d'etats ([set-end] & trans)
	 (prepare-state-no-acc
	   (lambda (s r1 r2)
	     (let ((trans-func (prepare-transition (vector-ref trees-v s))))
	       (lambda (action)
		 (let ((next-state (trans-func)))
		   (if next-state
		       (next-state action)
		       action))))))
	 (prepare-state-yes-no
	  (lambda (s r1 r2)
	    (let ((peek-char       IS-peek-char)
		  (end-go-to-point IS-end-go-to-point)
		  (new-action1     #f)
		  (trans-func (prepare-transition (vector-ref trees-v s))))
	      (let ((result
		     (lambda (action)
		       (let* ((c (peek-char))
			      (new-action
			       (if (or (not c) (= c lexer-integer-newline))
				   (begin
				     (end-go-to-point)
				     new-action1)
				   action))
			      (next-state (trans-func)))
			 (if next-state
			     (next-state new-action)
			     new-action))))
		    (hook
		     (lambda ()
		       (set! new-action1 (vector-ref rules-actions r1)))))
		(add-hook hook)
		result))))
	 (prepare-state-diff-acc
	  (lambda (s r1 r2)
	    (let ((end-go-to-point IS-end-go-to-point)
		  (peek-char       IS-peek-char)
		  (new-action1     #f)
		  (new-action2     #f)
		  (trans-func (prepare-transition (vector-ref trees-v s))))
	      (let ((result
		     (lambda (action)
		       (end-go-to-point)
		       (let* ((c (peek-char))
			      (new-action
			       (if (or (not c) (= c lexer-integer-newline))
				   new-action1
				   new-action2))
			      (next-state (trans-func)))
			 (if next-state
			     (next-state new-action)
			     new-action))))
		    (hook
		     (lambda ()
		       (set! new-action1 (vector-ref rules-actions r1))
		       (set! new-action2 (vector-ref rules-actions r2)))))
		(add-hook hook)
		result))))
	 (prepare-state-same-acc
	  (lambda (s r1 r2)
	    (let ((end-go-to-point IS-end-go-to-point)
		  (trans-func (prepare-transition (vector-ref trees-v s)))
		  (new-action #f))
	      (let ((result
		     (lambda (action)
		       (end-go-to-point)
		       (let ((next-state (trans-func)))
			 (if next-state
			     (next-state new-action)
			     new-action))))
		    (hook
		     (lambda ()
		       (set! new-action (vector-ref rules-actions r1)))))
		(add-hook hook)
		result))))
	 (prepare-state
	  (lambda (s)
	    (let* ((acc (vector-ref acc-v s))
		   (r1 (car acc))
		   (r2 (cdr acc)))
	      (cond ((not r1)  (prepare-state-no-acc   s r1 r2))
		    ((not r2)  (prepare-state-yes-no   s r1 r2))
		    ((< r1 r2) (prepare-state-diff-acc s r1 r2))
		    (else      (prepare-state-same-acc s r1 r2))))))

	 ; Fabrique la fonction de lancement du lexage a l'etat de depart
	 (prepare-start-same
	  (lambda (s1 s2)
	    (let ((peek-char    IS-peek-char)
		  (eof-action   #f)
		  (start-state  #f)
		  (error-action #f))
	      (let ((result
		     (lambda ()
		       (if (not (peek-char))
			   eof-action
			   (start-state error-action))))
		    (hook
		     (lambda ()
		       (set! eof-action   <<EOF>>-action)
		       (set! start-state  (vector-ref states s1))
		       (set! error-action <<ERROR>>-action))))
		(add-hook hook)
		result))))
	 (prepare-start-diff
	  (lambda (s1 s2)
	    (let ((peek-char         IS-peek-char)
		  (eof-action        #f)
		  (peek-left-context IS-peek-left-context)
		  (start-state1      #f)
		  (start-state2      #f)
		  (error-action      #f))
	      (let ((result
		     (lambda ()
		       (cond ((not (peek-char))
			      eof-action)
			     ((= (peek-left-context) lexer-integer-newline)
			      (start-state1 error-action))
			     (else
			      (start-state2 error-action)))))
		    (hook
		     (lambda ()
		       (set! eof-action <<EOF>>-action)
		       (set! start-state1 (vector-ref states s1))
		       (set! start-state2 (vector-ref states s2))
		       (set! error-action <<ERROR>>-action))))
		(add-hook hook)
		result))))
	 (prepare-start
	  (lambda ()
	    (let ((s1 table-nl-start)
		  (s2 table-no-nl-start))
	      (if (= s1 s2)
		  (prepare-start-same s1 s2)
		  (prepare-start-diff s1 s2)))))

	 ; Fabrique la fonction principale
	 (prepare-lexer-none
	  (lambda ()
	    (let ((init-lexeme IS-init-lexeme)
		  (start-func  (prepare-start)))
	      (lambda ()
		(init-lexeme)
		((start-func))))))
	 (prepare-lexer-line
	  (lambda ()
	    (let ((init-lexeme    IS-init-lexeme)
		  (get-start-line IS-get-start-line)
		  (start-func     (prepare-start)))
	      (lambda ()
		(init-lexeme)
		(let ((yyline (get-start-line)))
		  ((start-func) yyline))))))
	 (prepare-lexer-all
	  (lambda ()
	    (let ((init-lexeme      IS-init-lexeme)
		  (get-start-line   IS-get-start-line)
		  (get-start-column IS-get-start-column)
		  (get-start-offset IS-get-start-offset)
		  (start-func       (prepare-start)))
	      (lambda ()
		(init-lexeme)
		(let ((yyline   (get-start-line))
		      (yycolumn (get-start-column))
		      (yyoffset (get-start-offset)))
		  ((start-func) yyline yycolumn yyoffset))))))
	 (prepare-lexer
	  (lambda ()
	    (cond ((eq? counters-type 'none) (prepare-lexer-none))
		  ((eq? counters-type 'line) (prepare-lexer-line))
		  ((eq? counters-type 'all)  (prepare-lexer-all))))))

      ; Calculer la valeur de <<EOF>>-action et de <<ERROR>>-action
      (set! <<EOF>>-action   (prepare-special-action <<EOF>>-pre-action))
      (set! <<ERROR>>-action (prepare-special-action <<ERROR>>-pre-action))

      ; Calculer la valeur de rules-actions
      (let* ((len (quotient (vector-length rules-pre-actions) 2))
	     (v (make-vector len)))
	(let loop ((r (- len 1)))
	  (if (< r 0)
	      (set! rules-actions v)
	      (let* ((yytext? (vector-ref rules-pre-actions (* 2 r)))
		     (pre-action (vector-ref rules-pre-actions (+ (* 2 r) 1)))
		     (action (if yytext?
				 (prepare-action-yytext    pre-action)
				 (prepare-action-no-yytext pre-action))))
		(vector-set! v r action)
		(loop (- r 1))))))

      ; Calculer la valeur de states
      (let* ((len (vector-length trees-v))
	     (v (make-vector len)))
	(let loop ((s (- len 1)))
	  (if (< s 0)
	      (set! states v)
	      (begin
		(vector-set! v s (prepare-state s))
		(loop (- s 1))))))

      ; Calculer la valeur de final-lexer
      (set! final-lexer (prepare-lexer))

      ; Executer les hooks
      (apply-hooks)

      ; Resultat
      final-lexer)))

; Fabrication de lexer a partir de listes de caracteres taggees
(define lexer-make-char-lexer
  (let* ((char->class
	  (lambda (c)
	    (let ((n (char->integer c)))
	      (list (cons n n)))))
	 (merge-sort
	  (lambda (l combine zero-elt)
	    (if (null? l)
		zero-elt
		(let loop1 ((l l))
		  (if (null? (cdr l))
		      (car l)
		      (loop1
		       (let loop2 ((l l))
			 (cond ((null? l)
				l)
			       ((null? (cdr l))
				l)
			       (else
				(cons (combine (car l) (cadr l))
				      (loop2 (cddr l))))))))))))
	 (finite-class-union
	  (lambda (c1 c2)
	    (let loop ((c1 c1) (c2 c2) (u '()))
	      (if (null? c1)
		  (if (null? c2)
		      (reverse u)
		      (loop c1 (cdr c2) (cons (car c2) u)))
		  (if (null? c2)
		      (loop (cdr c1) c2 (cons (car c1) u))
		      (let* ((r1 (car c1))
			     (r2 (car c2))
			     (r1start (car r1))
			     (r1end (cdr r1))
			     (r2start (car r2))
			     (r2end (cdr r2)))
			(if (<= r1start r2start)
			    (cond ((< (+ r1end 1) r2start)
				   (loop (cdr c1) c2 (cons r1 u)))
				  ((<= r1end r2end)
				   (loop (cdr c1)
					 (cons (cons r1start r2end) (cdr c2))
					 u))
				  (else
				   (loop c1 (cdr c2) u)))
			    (cond ((> r1start (+ r2end 1))
				   (loop c1 (cdr c2) (cons r2 u)))
				  ((>= r1end r2end)
				   (loop (cons (cons r2start r1end) (cdr c1))
					 (cdr c2)
					 u))
				  (else
				   (loop (cdr c1) c2 u))))))))))
	 (char-list->class
	  (lambda (cl)
	    (let ((classes (map char->class cl)))
	      (merge-sort classes finite-class-union '()))))
	 (class-<
	  (lambda (b1 b2)
	    (cond ((eq? b1 'inf+) #f)
		  ((eq? b2 'inf-) #f)
		  ((eq? b1 'inf-) #t)
		  ((eq? b2 'inf+) #t)
		  (else (< b1 b2)))))
	 (finite-class-compl
	  (lambda (c)
	    (let loop ((c c) (start 'inf-))
	      (if (null? c)
		  (list (cons start 'inf+))
		  (let* ((r (car c))
			 (rstart (car r))
			 (rend (cdr r)))
		    (if (class-< start rstart)
			(cons (cons start (- rstart 1))
			      (loop c rstart))
			(loop (cdr c) (+ rend 1))))))))
	 (tagged-chars->class
	  (lambda (tcl)
	    (let* ((inverse? (car tcl))
		   (cl (cdr tcl))
		   (class-tmp (char-list->class cl)))
	      (if inverse? (finite-class-compl class-tmp) class-tmp))))
	 (charc->arc
	  (lambda (charc)
	    (let* ((tcl (car charc))
		   (dest (cdr charc))
		   (class (tagged-chars->class tcl)))
	      (cons class dest))))
	 (arc->sharcs
	  (lambda (arc)
	    (let* ((range-l (car arc))
		   (dest (cdr arc))
		   (op (lambda (range) (cons range dest))))
	      (map op range-l))))
	 (class-<=
	  (lambda (b1 b2)
	    (cond ((eq? b1 'inf-) #t)
		  ((eq? b2 'inf+) #t)
		  ((eq? b1 'inf+) #f)
		  ((eq? b2 'inf-) #f)
		  (else (<= b1 b2)))))
	 (sharc-<=
	  (lambda (sharc1 sharc2)
	    (class-<= (caar sharc1) (caar sharc2))))
	 (merge-sharcs
	  (lambda (l1 l2)
	    (let loop ((l1 l1) (l2 l2))
	      (cond ((null? l1)
		     l2)
		    ((null? l2)
		     l1)
		    (else
		     (let ((sharc1 (car l1))
			   (sharc2 (car l2)))
		       (if (sharc-<= sharc1 sharc2)
			   (cons sharc1 (loop (cdr l1) l2))
			   (cons sharc2 (loop l1 (cdr l2))))))))))
	 (class-= eqv?)
	 (fill-error
	  (lambda (sharcs)
	    (let loop ((sharcs sharcs) (start 'inf-))
	      (cond ((class-= start 'inf+)
		     '())
		    ((null? sharcs)
		     (cons (cons (cons start 'inf+) 'err)
			   (loop sharcs 'inf+)))
		    (else
		     (let* ((sharc (car sharcs))
			    (h (caar sharc))
			    (t (cdar sharc)))
		       (if (class-< start h)
			   (cons (cons (cons start (- h 1)) 'err)
				 (loop sharcs h))
			   (cons sharc (loop (cdr sharcs)
					     (if (class-= t 'inf+)
						 'inf+
						 (+ t 1)))))))))))
	 (charcs->tree
	  (lambda (charcs)
	    (let* ((op (lambda (charc) (arc->sharcs (charc->arc charc))))
		   (sharcs-l (map op charcs))
		   (sorted-sharcs (merge-sort sharcs-l merge-sharcs '()))
		   (full-sharcs (fill-error sorted-sharcs))
		   (op (lambda (sharc) (cons (caar sharc) (cdr sharc))))
		   (table (list->vector (map op full-sharcs))))
	      (let loop ((left 0) (right (- (vector-length table) 1)))
		(if (= left right)
		    (cdr (vector-ref table left))
		    (let ((mid (quotient (+ left right 1) 2)))
		      (if (and (= (+ left 2) right)
			       (= (+ (car (vector-ref table mid)) 1)
				  (car (vector-ref table right)))
			       (eqv? (cdr (vector-ref table left))
				     (cdr (vector-ref table right))))
			  (list '=
				(car (vector-ref table mid))
				(cdr (vector-ref table mid))
				(cdr (vector-ref table left)))
			  (list (car (vector-ref table mid))
				(loop left (- mid 1))
				(loop mid right))))))))))
    (lambda (tables IS)
      (let ((counters         (vector-ref tables 0))
	    (<<EOF>>-action   (vector-ref tables 1))
	    (<<ERROR>>-action (vector-ref tables 2))
	    (rules-actions    (vector-ref tables 3))
	    (nl-start         (vector-ref tables 5))
	    (no-nl-start      (vector-ref tables 6))
	    (charcs-v         (vector-ref tables 7))
	    (acc-v            (vector-ref tables 8)))
	(let* ((len (vector-length charcs-v))
	       (v (make-vector len)))
	  (let loop ((i (- len 1)))
	    (if (>= i 0)
		(begin
		  (vector-set! v i (charcs->tree (vector-ref charcs-v i)))
		  (loop (- i 1)))
		(lexer-make-tree-lexer
		 (vector counters
			 <<EOF>>-action
			 <<ERROR>>-action
			 rules-actions
			 'decision-trees
			 nl-start
			 no-nl-start
			 v
			 acc-v)
		 IS))))))))

; Fabrication d'un lexer a partir de code pre-genere
(define lexer-make-code-lexer
  (lambda (tables IS)
    (let ((<<EOF>>-pre-action   (vector-ref tables 1))
	  (<<ERROR>>-pre-action (vector-ref tables 2))
	  (rules-pre-action     (vector-ref tables 3))
	  (code                 (vector-ref tables 5)))
      (code <<EOF>>-pre-action <<ERROR>>-pre-action rules-pre-action IS))))

(define lexer-make-lexer
  (lambda (tables IS)
    (let ((automaton-type (vector-ref tables 4)))
      (cond ((eq? automaton-type 'decision-trees)
	     (lexer-make-tree-lexer tables IS))
	    ((eq? automaton-type 'tagged-chars-lists)
	     (lexer-make-char-lexer tables IS))
	    ((eq? automaton-type 'code)
	     (lexer-make-code-lexer tables IS))))))

;
; Table generated from the file php.l by SILex 1.0
;

(define lexer-default-table
  (vector
   'line
   (lambda (yycontinue yygetc yyungetc)
     (lambda (yytext yyline)
                 '*eoi*
       ))
   (lambda (yycontinue yygetc yyungetc)
     (lambda (yytext yyline)
                 (lexer-error (yygetc))
       ))
   (vector
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
            (make-token 'T_ABSTRACT yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
      (make-token 'T_AND_EQUAL yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
         (make-token 'T_ARRAY yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
           (make-token 'T_ARRAY_CAST yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
        (make-token 'T_AS yytext)

;todo:          (make-token 'T_BAD_CHARACTER yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
      (make-token 'T_BOOLEAN_AND yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
      (make-token 'T_BOOLEAN_OR yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                      (make-token 'T_BOOL_CAST yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
         (make-token 'T_BREAK yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
        (make-token 'T_CASE yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
         (make-token 'T_CATCH yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
         (make-token 'T_CLASS yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
             (make-token 'T_CLASS_C yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
         (make-token 'T_CLONE yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                       (begin (let ((token (make-token 'T_CLOSE_TAG yytext))) (set! parse-mode 'txt) token))
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
            (make-token 'T_COMMENT yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
              (make-token 'T_COMMENT yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                 (make-token 'T_COMMENT yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
      (make-token 'T_CONCAT_EQUAL yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                 (make-token 'T_CONST yytext)
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline)
                (read-string #\" yygetc yyungetc) 
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline)
                (read-string #\' yygetc yyungetc)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                (make-token 'T_CONTINUE yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                (make-token 'T_CURLY_OPEN yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                (make-token 'T_DEC yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                 (make-token 'T_DECLARE yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                 (make-token 'T_DIR yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                (make-token 'T_DIV_EQUAL yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                (make-token 'T_DNUMBER yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                (make-token 'T_DOC_COMMENT yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                (make-token 'T_DO yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                (make-token 'T_DOLLAR_OPEN_CURLY_BRACES yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                (make-token 'T_DOUBLE_ARROW yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                (make-token 'T_DOUBLE_CAST yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                (make-token 'T_DOUBLE_CAST yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                 (make-token 'T_DOUBLE_CAST yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                (make-token 'T_DOUBLE_COLON yytext) ; also T_PAAMAYIM_NEKUDOTAYIM
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                (make-token 'T_ECHO yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                (make-token 'T_ELSE yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                (make-token 'T_ELSEIF yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                 (make-token 'T_EMPTY yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                (make-token 'T_ENCAPSED_AND_WHITESPACE yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                (make-token 'T_ENDDECLARE yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                (make-token 'T_ENDFOR yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                (make-token 'T_ENDFOREACH yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                 (make-token 'T_ENDIF yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                 (make-token 'T_ENDSWITCH yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                (make-token 'T_ENDWHILE yytext)

;todo:          (make-token 'T_END_HEREDOC yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                (make-token 'T_EVAL yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                 (make-token 'T_EXTENDS yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                (make-token 'T_FILE yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                 (make-token 'T_FINAL yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                 (make-token 'T_FOR yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                 (make-token 'T_FOREACH yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                (make-token 'T_FUNCTION yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                (make-token 'T_FUNC_C yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                (make-token 'T_GLOBAL yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                (make-token 'T_GOTO yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                  (make-token 'T_HALT_COMPILER yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                (make-token 'T_IF yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                (make-token 'T_IMPLEMENTS yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                (make-token 'T_INC yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                 (make-token 'T_INCLUDE yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                (make-token 'T_INCLUDE_ONCE yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                (make-token 'T_INSTANCEOF yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                 (make-token 'T_INT_CAST yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                 (make-token 'T_INT_CAST yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                 (make-token 'T_INTERFACE yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                 (make-token 'T_ISSET yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                (make-token 'T_IS_EQUAL yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                (make-token 'T_IS_GREATER_OR_EQUAL yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                 (make-token 'T_IS_IDENTICAL yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                 (make-token 'T_IS_NOT_EQUAL yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                 (make-token 'T_IS_NOT_IDENTICAL yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                (make-token 'T_IS_SMALLER_OR_EQUAL yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                (make-token 'T_LINE yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                (make-token 'T_LIST yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                 (make-token 'T_LNUMBER yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                 (make-token 'T_LOGICAL_AND yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                (make-token 'T_LOGICAL_OR yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                 (make-token 'T_LOGICAL_XOR yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                (make-token 'T_MOD_EQUAL yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                (make-token 'T_MUL_EQUAL yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                 (make-token 'T_NS_C yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                 (make-token 'T_NEW yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                         (make-token 'T_NUM_STRING yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                (make-token 'T_OBJECT_CAST yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                (make-token 'T_OBJECT_OPERATOR yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                                                         (begin (set! parse-mode 'php) (make-token 'T_OPEN_TAG yytext))
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                 (begin (set! parse-mode 'php) (make-token 'T_OPEN_TAG_WITH_ECHO yytext))
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                (make-token 'T_OR_EQUAL yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                (make-token 'T_PLUS_EQUAL yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                 (make-token 'T_PRINT yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                 (make-token 'T_PRIVATE yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                (make-token 'T_PUBLIC yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                 (make-token 'T_PROTECTED yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                 (make-token 'T_REQUIRE yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                (make-token 'T_REQUIRE_ONCE yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                (make-token 'T_RETURN yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                (make-token 'T_SL yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                 (make-token 'T_SL_EQUAL yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                (make-token 'T_SR yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                 (make-token 'T_SR_EQUAL yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                 (make-token 'T_START_HEREDOC yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                (make-token 'T_STATIC yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                         (make-token 'T_STRING yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                (make-token 'T_STRING_CAST yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                   (make-token 'T_STRING_VARNAME yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                (make-token 'T_SWITCH yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                 (make-token 'T_THROW yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                 (make-token 'T_TRY yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                 (make-token 'T_UNSET yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                 (make-token 'T_UNSET_CAST yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                 (make-token 'T_USE yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                 (make-token 'T_VAR yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                 (make-token 'T_VARIABLE yytext)  ;todo: handle all legal var name
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                 (make-token 'T_WHILE yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                 (if (eq? parse-mode 'txt) (make-token 'T_WHITESPACE yytext) (yycontinue))
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                (if (eq? parse-mode 'txt) (make-token 'T_WHITESPACE yytext) (yycontinue))
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                (if (eq? parse-mode 'txt) (make-token 'T_WHITESPACE yytext) (yycontinue))
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                 (if (eq? parse-mode 'txt) (make-token 'T_WHITESPACE yytext) (yycontinue))
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                (make-token 'T_XOR_EQUAL yytext)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Other tokens
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                 (make-token 'open-paren yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                 (make-token 'close-paren yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                 (make-token 'open-brace yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                 (make-token 'close-brace yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                 (make-token 'open-bracket yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                 (make-token 'close-bracket yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                 (make-token 'comma yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                 (make-token 'semi yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                 (make-token 'asteriks yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                 (make-token 'plus yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                 (make-token 'minus yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                 (make-token 'divide yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                 (make-token 'equals yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                 (make-token 'dot yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                 (make-token 'qmark yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                 (make-token 'greater-than yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                 (make-token 'less-than yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                 (make-token 'null yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                                (make-token 'word yytext)
        )))
   'decision-trees
   0
   0
   '#((91 (43 (34 (13 (10 (9 err 12) (11 11 err)) (32 (14 10 err) (33 52
    27))) (38 (36 (35 41 44) (37 36 46)) (40 (39 54 40) (41 53 (42 8
    21))))) (59 (47 (45 (44 29 4) (46 38 42)) (49 (48 43 25) (58 24 34)))
    (63 (61 (60 3 45) (62 35 28)) (65 (64 47 err) (= 78 2 1))))) (109 (99
    (95 (93 (92 6 err) (94 5 9)) (97 (96 48 err) (98 55 50))) (103 (101
    (100 49 37) (102 33 32)) (105 (104 31 1) (106 30 (108 1 26))))) (117
    (113 (111 (110 1 20) (112 23 19)) (115 (114 1 18) (116 17 16))) (121
    (119 (118 15 14) (120 13 22)) (124 (123 1 39) (125 51 (126 7 err)))))))
    (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97 err (123 1 err))))
    (86 (58 (48 err 1) (65 err (85 1 56))) (96 (91 1 (95 err 1)) (97 err
    (123 1 err)))) err err err err err err (= 61 57 err) (= 13 10 err) (=
    10 11 err) (= 9 12 err) (95 (58 (48 err 1) (65 err (91 1 err))) (104 (=
    96 err 1) (105 58 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1
    err))) (97 (96 1 err) (98 59 (123 1 err)))) (96 (65 (48 err (58 1 err))
    (91 1 (95 err 1))) (111 (97 err (110 1 61)) (116 (115 1 60) (123 1
    err)))) (96 (65 (48 err (58 1 err)) (91 1 (95 err 1))) (105 (97 err
    (104 1 63)) (115 (114 1 62) (123 1 err)))) (96 (65 (48 err (58 1 err))
    (91 1 (95 err 1))) (117 (97 err (116 1 65)) (120 (119 1 64) (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (101 (= 96 err 1) (102
    66 (123 1 err)))) (96 (65 (48 err (58 1 err)) (91 1 (95 err 1))) (115
    (97 err (114 1 68)) (118 (117 1 67) (123 1 err)))) (96 (65 (48 err (58
    1 err)) (91 1 (95 err 1))) (102 (97 err (101 1 70)) (118 (117 1 69)
    (123 1 err)))) (= 61 71 err) (95 (58 (48 err 1) (65 err (91 1 err)))
    (111 (= 96 err 1) (112 72 (123 1 err)))) (95 (58 (48 err 1) (65 err (91
    1 err))) (114 (= 96 err 1) (115 73 (123 1 err)))) (48 err (58 24 err))
    err (95 (58 (48 err 1) (65 err (91 1 err))) (105 (= 96 err 1) (106 74
    (123 1 err)))) (= 61 75 err) (62 (61 err 77) (63 76 err)) (44 (43 err
    79) (= 61 78 err)) (102 (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1)
    (97 err 1))) (111 (109 (103 83 1) (110 82 81)) (116 (115 1 80) (123 1
    err)))) (96 (65 (48 err (58 1 err)) (91 1 (95 err 1))) (109 (97 err
    (108 1 85)) (112 (111 1 84) (123 1 err)))) (97 (65 (48 err (58 1 err))
    (95 (91 1 err) (96 1 err))) (112 (106 (105 1 88) (111 1 87)) (118 (117
    1 86) (123 1 err)))) (100 (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1)
    (97 err (99 1 94)))) (118 (109 (108 1 93) (110 92 (111 91 1))) (120
    (119 90 1) (121 89 (123 1 err))))) (= 58 95 err) (62 (61 err 96) (63 97
    err)) (97 (65 err (91 98 err)) (123 98 (124 99 err))) (95 (48 (= 46 101
    err) (65 (58 1 err) (91 1 err))) (102 (97 (96 1 err) (101 1 102)) (112
    (111 1 100) (123 1 err)))) (46 (45 err 104) (= 62 103 err)) (= 36 105
    err) err (11 (10 106 err) (= 36 107 106)) (= 61 108 err) (47 (= 42 110
    err) (61 (48 111 err) (62 109 err))) (= 10 112 44) (61 (47 (= 37 115
    err) (48 119 (60 err 113))) (64 (62 117 (63 118 116)) (= 115 114 err)))
    (62 (61 err 120) (63 121 err)) (= 62 121 err) (91 (58 (48 err 1) (65
    err 1)) (96 (95 err 122) (97 err (123 1 err)))) (97 (65 (48 err (58 1
    err)) (95 (91 1 err) (96 1 err))) (109 (98 125 (108 1 124)) (112 (111 1
    123) (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (114 (= 96
    err 1) (115 126 (123 1 err)))) (62 (61 err 127) (= 124 128 err)) (= 97
    129 err) (105 (100 (98 (97 err 138) (99 137 err)) (102 (101 135 err)
    (103 134 err))) (114 (111 (106 133 err) (112 132 err)) (116 (115 136
    131) (= 117 130 err)))) (39 (38 err 139) (= 61 140 err)) (97 (65 (48
    err (58 1 err)) (95 (91 1 err) (96 1 err))) (111 (99 (98 1 143) (110 1
    141)) (115 (114 1 142) (123 1 err)))) (77 (58 (48 err 1) (65 err (76 1
    144))) (96 (91 1 (95 err 1)) (97 err (123 1 err)))) err (95 (58 (48 err
    1) (65 err (91 1 err))) (105 (= 96 err 1) (106 145 (123 1 err)))) (95
    (58 (48 err 1) (65 err (91 1 err))) (114 (= 96 err 1) (115 146 (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (101 (= 96 err 1) (102
    147 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (115 (= 96
    err 1) (116 148 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err)))
    (121 (= 96 err 1) (122 149 (123 1 err)))) (95 (58 (48 err 1) (65 err
    (91 1 err))) (114 (= 96 err 1) (115 150 (123 1 err)))) (95 (58 (48 err
    1) (65 err (91 1 err))) (105 (= 96 err 1) (106 151 (123 1 err)))) (95
    (58 (48 err 1) (65 err (91 1 err))) (97 (96 1 err) (98 152 (123 1
    err)))) (96 (65 (48 err (58 1 err)) (91 1 (95 err 1))) (114 (97 err
    (113 1 154)) (117 (116 1 153) (123 1 err)))) (95 (58 (48 err 1) (65 err
    (91 1 err))) (98 (= 96 err 1) (99 155 (123 1 err)))) (96 (65 (48 err
    (58 1 err)) (91 1 (95 err 1))) (106 (97 err (105 1 157)) (112 (111 1
    156) (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (108 (= 96
    err 1) (109 158 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err)))
    (119 (= 96 err 1) (120 159 (123 1 err)))) err (95 (58 (48 err 1) (65
    err (91 1 err))) (114 (= 96 err 1) (115 160 (123 1 err)))) (91 (58 (48
    err 1) (65 err 1)) (96 (95 err 1) (97 err (123 1 err)))) (95 (58 (48
    err 1) (65 err (91 1 err))) (115 (= 96 err 1) (116 161 (123 1 err))))
    (= 61 162 err) (= 61 163 err) err err err (95 (58 (48 err 1) (65 err
    (91 1 err))) (115 (= 96 err 1) (116 164 (123 1 err)))) (97 (65 (48 err
    (58 1 err)) (95 (91 1 err) (96 1 err))) (115 (= 99 167 1) (117 (116 166
    165) (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (112 (= 96
    err 1) (113 168 (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95
    err 1) (97 err (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err)))
    (116 (= 96 err 1) (117 169 (123 1 err)))) (95 (58 (48 err 1) (65 err
    (91 1 err))) (111 (= 96 err 1) (112 170 (123 1 err)))) (95 (58 (48 err
    1) (65 err (91 1 err))) (110 (= 96 err 1) (111 171 (123 1 err)))) (95
    (58 (48 err 1) (65 err (91 1 err))) (114 (= 96 err 1) (115 172 (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (110 (= 96 err 1) (111
    173 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (116 (= 96
    err 1) (117 174 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err)))
    (97 (96 1 err) (98 175 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1
    err))) (100 (= 96 err 1) (101 176 (123 1 err)))) (95 (58 (48 err 1) (65
    err (91 1 err))) (112 (= 96 err 1) (113 177 (123 1 err)))) (95 (58 (48
    err 1) (65 err (91 1 err))) (115 (= 96 err 1) (116 178 (123 1 err))))
    (95 (58 (48 err 1) (65 err (91 1 err))) (104 (= 96 err 1) (105 179 (123
    1 err)))) err (= 61 180 err) err (91 (65 err 98) (97 err (123 98 err)))
    err (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97 err (123 1
    err)))) (= 100 181 err) (95 (58 (48 err 1) (65 err (91 1 err))) (99 (=
    96 err 1) (100 182 (123 1 err)))) err err err (= 36 183 err) (35 (11
    (10 183 err) (34 183 186)) (92 (91 183 185) (= 123 184 183))) err err
    (11 (10 187 err) (= 42 188 187)) (= 10 189 111) err (61 (60 err 190)
    (62 191 err)) (= 99 192 err) (= 61 193 err) (62 (61 err 193) (= 112 194
    err)) err err (= 115 195 err) err err (77 (68 (58 (48 err 1) (65 err
    (67 1 201))) (70 (69 200 1) (71 199 (76 1 197)))) (96 (79 (78 1 196)
    (91 1 (95 err 1))) (104 (97 err 1) (105 198 (123 1 err))))) (95 (58 (48
    err 1) (65 err (91 1 err))) (110 (= 96 err 1) (111 202 (123 1 err))))
    (96 (65 (48 err (58 1 err)) (91 1 (95 err 1))) (111 (97 err (98 204 1))
    (112 203 (123 1 err)))) (96 (65 (48 err (58 1 err)) (91 1 (95 err 1)))
    (116 (97 err (115 1 206)) (117 205 (123 1 err)))) (95 (58 (48 err 1)
    (65 err (91 1 err))) (101 (= 96 err 1) (102 207 (123 1 err)))) err err
    (= 115 208 err) (= 110 209 err) (= 116 210 err) (= 98 211 err) (= 110
    212 err) (= 108 213 err) (= 111 214 err) (= 101 215 err) (= 111 216
    err) (= 114 217 err) err err (95 (58 (48 err 1) (65 err (91 1 err)))
    (100 (= 96 err 1) (101 218 (123 1 err)))) (95 (58 (48 err 1) (65 err
    (91 1 err))) (114 (= 96 err 1) (115 219 (123 1 err)))) (95 (58 (48 err
    1) (65 err (91 1 err))) (115 (= 96 err 1) (116 220 (123 1 err)))) (77
    (58 (48 err 1) (65 err (76 1 221))) (96 (91 1 (95 err 1)) (97 err (123
    1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (108 (= 96 err 1)
    (109 222 (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1)
    (97 err (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1)
    (97 err (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (101 (=
    96 err 1) (102 223 (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96
    (95 err 1) (97 err (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1
    err))) (111 (= 96 err 1) (112 224 (123 1 err)))) (95 (58 (48 err 1) (65
    err (91 1 err))) (116 (= 96 err 1) (117 225 (123 1 err)))) (95 (58 (48
    err 1) (65 err (91 1 err))) (116 (= 96 err 1) (117 226 (123 1 err))))
    (95 (58 (48 err 1) (65 err (91 1 err))) (117 (= 96 err 1) (118 227 (123
    1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (117 (= 96 err 1)
    (118 228 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (108 (=
    96 err 1) (109 229 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1
    err))) (116 (= 96 err 1) (117 230 (123 1 err)))) (96 (65 (48 err (58 1
    err)) (91 1 (95 err 1))) (111 (97 err (110 1 232)) (119 (118 1 231)
    (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (108 (= 96 err
    1) (109 221 (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err
    1) (97 err (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1)
    (97 err (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (116 (=
    96 err 1) (117 233 (123 1 err)))) err err (95 (58 (48 err 1) (65 err
    (91 1 err))) (101 (= 96 err 1) (102 234 (123 1 err)))) (95 (58 (48 err
    1) (65 err (91 1 err))) (101 (= 96 err 1) (102 235 (123 1 err)))) (95
    (58 (48 err 1) (65 err (91 1 err))) (116 (= 96 err 1) (117 236 (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (108 (= 96 err 1) (109
    237 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (108 (= 96
    err 1) (109 238 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err)))
    (111 (= 96 err 1) (112 239 (123 1 err)))) (95 (58 (48 err 1) (65 err
    (91 1 err))) (98 (= 96 err 1) (99 240 (123 1 err)))) (95 (58 (48 err 1)
    (65 err (91 1 err))) (99 (= 96 err 1) (100 241 (123 1 err)))) (95 (58
    (48 err 1) (65 err (91 1 err))) (101 (= 96 err 1) (102 242 (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (97 (96 1 err) (98 243
    (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (101 (= 96 err
    1) (102 244 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (108
    (= 96 err 1) (109 245 (123 1 err)))) (101 (91 (58 (48 err 1) (65 err
    1)) (96 (95 err 1) (97 err (100 1 250)))) (115 (103 (102 1 249) (= 105
    248 1)) (119 (116 247 1) (120 246 (123 1 err))))) (95 (58 (48 err 1)
    (65 err (91 1 err))) (116 (= 96 err 1) (117 251 (123 1 err)))) (95 (58
    (48 err 1) (65 err (91 1 err))) (101 (= 96 err 1) (102 252 (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (111 (= 96 err 1) (112
    253 (123 1 err)))) err err (95 (58 (48 err 1) (65 err (91 1 err))) (108
    (= 96 err 1) (109 254 (123 1 err)))) (34 (= 10 err 183) (91 (35 186
    183) (92 185 183))) (35 (11 (10 184 err) (34 184 257)) (92 (91 184 256)
    (= 125 255 184))) (91 (11 (10 183 err) (= 34 186 183)) (101 (92 185
    (100 183 259)) (= 119 258 183))) (34 (= 10 err 183) (91 (35 186 183)
    (92 185 183))) (11 (10 187 err) (= 42 260 187)) (42 (= 10 err 261) (47
    (43 263 261) (48 262 261))) err err err (= 114 264 err) err (= 104 265
    err) (= 99 266 err) (91 (58 (48 err 1) (65 err (66 267 1))) (96 (95 err
    1) (97 err (123 1 err)))) (74 (58 (48 err 1) (65 err (73 1 268))) (96
    (91 1 (95 err 1)) (97 err (123 1 err)))) (95 (58 (48 err 1) (65 err (91
    1 err))) (97 (96 1 err) (98 269 (123 1 err)))) (85 (65 (48 err (58 1
    err)) (= 73 271 1)) (95 (86 270 (91 1 err)) (97 (96 1 err) (123 1
    err)))) (74 (58 (48 err 1) (65 err (73 1 272))) (96 (91 1 (95 err 1))
    (97 err (123 1 err)))) (77 (58 (48 err 1) (65 err (76 1 273))) (96 (91
    1 (95 err 1)) (97 err (123 1 err)))) (96 (65 (48 err (58 1 err)) (91 1
    (95 err 1))) (116 (97 err (115 1 275)) (117 274 (123 1 err)))) (95 (58
    (48 err 1) (65 err (91 1 err))) (110 (= 96 err 1) (111 276 (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (115 (= 96 err 1) (116
    277 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (99 (= 96
    err 1) (100 278 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err)))
    (101 (= 96 err 1) (102 279 (123 1 err)))) (95 (58 (48 err 1) (65 err
    (91 1 err))) (97 (96 1 err) (98 280 (123 1 err)))) (= 32 281 err) (=
    115 282 err) (= 114 283 err) (= 106 284 err) (= 116 285 err) (= 111 286
    err) (= 117 287 err) (= 97 288 err) (= 111 289 err) (= 114 290 err) (91
    (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97 err (123 1 err)))) (95
    (58 (48 err 1) (65 err (91 1 err))) (97 (96 1 err) (98 291 (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (116 (= 96 err 1) (117
    292 (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97
    err (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (101 (= 96
    err 1) (102 293 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err)))
    (116 (= 96 err 1) (117 294 (123 1 err)))) (95 (58 (48 err 1) (65 err
    (91 1 err))) (119 (= 96 err 1) (120 295 (123 1 err)))) (95 (58 (48 err
    1) (65 err (91 1 err))) (99 (= 96 err 1) (100 296 (123 1 err)))) (95
    (58 (48 err 1) (65 err (91 1 err))) (105 (= 96 err 1) (106 297 (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (114 (= 96 err 1) (115
    298 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (105 (= 96
    err 1) (106 299 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err)))
    (105 (= 96 err 1) (106 300 (123 1 err)))) (95 (58 (48 err 1) (65 err
    (91 1 err))) (101 (= 96 err 1) (102 301 (123 1 err)))) (95 (58 (48 err
    1) (65 err (91 1 err))) (97 (96 1 err) (98 302 (123 1 err)))) (95 (58
    (48 err 1) (65 err (91 1 err))) (116 (= 96 err 1) (117 303 (123 1
    err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97 err (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (116 (= 96 err 1) (117
    304 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (114 (= 96
    err 1) (115 305 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err)))
    (97 (96 1 err) (98 306 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1
    err))) (117 (= 96 err 1) (118 307 (123 1 err)))) (95 (58 (48 err 1) (65
    err (91 1 err))) (101 (= 96 err 1) (102 308 (123 1 err)))) (91 (58 (48
    err 1) (65 err 1)) (96 (95 err 1) (97 err (123 1 err)))) (95 (58 (48
    err 1) (65 err (91 1 err))) (97 (96 1 err) (98 309 (123 1 err)))) (95
    (58 (48 err 1) (65 err (91 1 err))) (116 (= 96 err 1) (117 310 (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (97 (96 1 err) (98 311
    (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (108 (= 96 err
    1) (109 312 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (110
    (= 96 err 1) (111 313 (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96
    (95 err 1) (97 err (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1
    err))) (104 (= 96 err 1) (105 314 (123 1 err)))) (95 (58 (48 err 1) (65
    err (91 1 err))) (119 (= 96 err 1) (120 315 (123 1 err)))) (95 (58 (48
    err 1) (65 err (91 1 err))) (102 (= 96 err 1) (103 316 (123 1 err))))
    (95 (58 (48 err 1) (65 err (91 1 err))) (111 (= 96 err 1) (112 317 (123
    1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (101 (= 96 err 1)
    (102 318 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (121 (=
    96 err 1) (122 319 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1
    err))) (105 (= 96 err 1) (106 320 (123 1 err)))) (91 (58 (48 err 1) (65
    err 1)) (96 (95 err 1) (97 err (123 1 err)))) (95 (58 (48 err 1) (65
    err (91 1 err))) (97 (96 1 err) (98 321 (123 1 err)))) (34 (= 10 err
    255) (91 (35 323 255) (92 322 255))) (92 (34 (= 10 err 184) (35 257 (91
    184 256))) (119 (= 100 325 184) (125 (120 324 184) (126 255 184)))) (35
    (11 (10 184 err) (34 184 257)) (92 (91 184 256) (= 125 255 184))) (35
    (11 (10 183 err) (34 183 186)) (92 (91 183 185) (= 93 326 183))) (35
    (11 (10 183 err) (34 183 186)) (92 (91 183 185) (= 93 327 183))) (42 (=
    10 err 187) (47 (43 260 187) (48 328 187))) (11 (10 261 err) (= 42 263
    261)) (11 (10 261 err) (= 42 263 261)) (42 (= 10 err 261) (47 (43 263
    261) (48 329 261))) (= 105 330 err) (= 112 331 err) (= 114 332 err) (78
    (58 (48 err 1) (65 err (77 1 333))) (96 (91 1 (95 err 1)) (97 err (123
    1 err)))) (79 (58 (48 err 1) (65 err (78 1 334))) (96 (91 1 (95 err 1))
    (97 err (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (108 (=
    96 err 1) (109 335 (123 1 err)))) (79 (58 (48 err 1) (65 err (78 1
    336))) (96 (91 1 (95 err 1)) (97 err (123 1 err)))) (77 (58 (48 err 1)
    (65 err (76 1 337))) (96 (91 1 (95 err 1)) (97 err (123 1 err)))) (83
    (58 (48 err 1) (65 err (82 1 338))) (96 (91 1 (95 err 1)) (97 err (123
    1 err)))) (91 (58 (48 err 1) (65 err (66 339 1))) (96 (95 err 1) (97
    err (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (105 (= 96
    err 1) (106 340 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err)))
    (116 (= 96 err 1) (117 341 (123 1 err)))) (95 (58 (48 err 1) (65 err
    (91 1 err))) (101 (= 96 err 1) (102 342 (123 1 err)))) (95 (58 (48 err
    1) (65 err (91 1 err))) (115 (= 96 err 1) (116 343 (123 1 err)))) (95
    (58 (48 err 1) (65 err (91 1 err))) (104 (= 96 err 1) (105 344 (123 1
    err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97 err (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (107 (= 96 err 1) (108
    345 (123 1 err)))) err (= 101 346 err) (= 105 347 err) (= 101 348 err)
    (42 (41 err 350) (= 101 349 err)) (= 97 351 err) (= 98 352 err) (= 108
    353 err) (= 108 354 err) (= 97 355 err) (95 (58 (48 err 1) (65 err (91
    1 err))) (121 (= 96 err 1) (122 356 (123 1 err)))) (95 (58 (48 err 1)
    (65 err (91 1 err))) (114 (= 96 err 1) (115 357 (123 1 err)))) (91 (58
    (48 err 1) (65 err 1)) (96 (95 err 1) (97 err (123 1 err)))) (91 (58
    (48 err 1) (65 err 1)) (96 (95 err 1) (97 err (123 1 err)))) (91 (58
    (48 err 1) (65 err 1)) (96 (95 err 1) (97 err (123 1 err)))) (95 (58
    (48 err 1) (65 err (91 1 err))) (104 (= 96 err 1) (105 358 (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (99 (= 96 err 1) (100
    359 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (110 (= 96
    err 1) (111 360 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err)))
    (114 (= 96 err 1) (115 361 (123 1 err)))) (95 (58 (48 err 1) (65 err
    (91 1 err))) (99 (= 96 err 1) (100 362 (123 1 err)))) (95 (58 (48 err
    1) (65 err (91 1 err))) (99 (= 96 err 1) (100 363 (123 1 err)))) (95
    (58 (48 err 1) (65 err (91 1 err))) (116 (= 96 err 1) (117 364 (123 1
    err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97 err (123 1
    err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97 err (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (102 (= 96 err 1) (103
    365 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (110 (= 96
    err 1) (111 366 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err)))
    (100 (= 96 err 1) (101 367 (123 1 err)))) (95 (58 (48 err 1) (65 err
    (91 1 err))) (109 (= 96 err 1) (110 368 (123 1 err)))) (95 (58 (48 err
    1) (65 err (91 1 err))) (108 (= 96 err 1) (109 369 (123 1 err)))) (95
    (58 (48 err 1) (65 err (91 1 err))) (105 (= 96 err 1) (106 370 (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (99 (= 96 err 1) (100
    371 (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97
    err (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (100 (= 96
    err 1) (101 372 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err)))
    (105 (= 96 err 1) (106 373 (123 1 err)))) (95 (58 (48 err 1) (65 err
    (91 1 err))) (105 (= 96 err 1) (106 374 (123 1 err)))) (91 (58 (48 err
    1) (65 err 1)) (96 (95 err 1) (97 err (123 1 err)))) (95 (58 (48 err 1)
    (65 err (91 1 err))) (114 (= 96 err 1) (115 375 (123 1 err)))) (95 (58
    (48 err 1) (65 err (91 1 err))) (99 (= 96 err 1) (100 376 (123 1
    err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97 err (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (102 (= 96 err 1) (103
    377 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (114 (= 96
    err 1) (115 378 (123 1 err)))) (91 (11 (10 255 err) (= 34 323 255))
    (101 (92 322 (100 255 380)) (= 119 379 255))) (34 (= 10 err 255) (91
    (35 323 255) (92 322 255))) (91 (11 (10 184 err) (= 34 257 184)) (94
    (92 256 (93 184 381)) (= 125 255 184))) (91 (11 (10 184 err) (= 34 257
    184)) (94 (92 256 (93 184 382)) (= 125 255 184))) (34 (= 10 err 326)
    (91 (35 384 326) (92 383 326))) (35 (11 (10 327 err) (34 327 387)) (40
    (39 327 385) (= 91 386 327))) (11 (10 187 err) (= 42 260 187)) (11 (10
    261 err) (= 42 263 261)) (= 112 388 err) err (= 105 389 err) (70 (58
    (48 err 1) (65 err (69 1 390))) (96 (91 1 (95 err 1)) (97 err (123 1
    err)))) (70 (58 (48 err 1) (65 err (69 1 391))) (96 (91 1 (95 err 1))
    (97 err (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (116 (=
    96 err 1) (117 392 (123 1 err)))) (68 (58 (48 err 1) (65 err (67 1
    393))) (96 (91 1 (95 err 1)) (97 err (123 1 err)))) (70 (58 (48 err 1)
    (65 err (69 1 394))) (96 (91 1 (95 err 1)) (97 err (123 1 err)))) (91
    (58 (48 err 1) (65 err 1)) (96 (95 err 395) (97 err (123 1 err)))) (84
    (58 (48 err 1) (65 err (83 1 396))) (96 (91 1 (95 err 1)) (97 err (123
    1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (110 (= 96 err 1)
    (111 397 (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1)
    (97 err (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1)
    (97 err (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1)
    (97 err (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1)
    (97 err (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1)
    (97 err (123 1 err)))) (= 116 398 err) (= 110 399 err) (= 99 400 err)
    (= 103 401 err) err (= 116 402 err) (= 108 403 err) (= 41 404 err) (42
    (41 err 406) (= 101 405 err)) (= 121 407 err) (91 (58 (48 err 1) (65
    err 1)) (96 (95 err 1) (97 err (123 1 err)))) (95 (58 (48 err 1) (65
    err (91 1 err))) (97 (96 1 err) (98 408 (123 1 err)))) (91 (58 (48 err
    1) (65 err 1)) (96 (95 err 1) (97 err (123 1 err)))) (91 (58 (48 err 1)
    (65 err 1)) (96 (95 err 1) (97 err (123 1 err)))) (91 (58 (48 err 1)
    (65 err 1)) (96 (95 err 1) (97 err (123 1 err)))) (95 (58 (48 err 1)
    (65 err (91 1 err))) (101 (= 96 err 1) (102 409 (123 1 err)))) (91 (58
    (48 err 1) (65 err 1)) (96 (95 err 1) (97 err (123 1 err)))) (95 (58
    (48 err 1) (65 err (91 1 err))) (116 (= 96 err 1) (117 410 (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (101 (= 96 err 1) (102
    411 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (97 (96 1
    err) (98 412 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (99
    (= 96 err 1) (100 413 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1
    err))) (101 (= 96 err 1) (102 414 (123 1 err)))) (95 (58 (48 err 1) (65
    err (91 1 err))) (101 (= 96 err 1) (102 415 (123 1 err)))) (91 (58 (48
    err 1) (65 err 1)) (96 (95 err 1) (97 err (123 1 err)))) (95 (58 (48
    err 1) (65 err (91 1 err))) (111 (= 96 err 1) (112 416 (123 1 err))))
    (95 (58 (48 err 1) (65 err (91 1 err))) (104 (= 96 err 1) (105 417 (123
    1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (115 (= 96 err 1)
    (116 418 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (108 (=
    96 err 1) (109 419 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1
    err))) (116 (= 96 err 1) (117 420 (123 1 err)))) (95 (58 (48 err 1) (65
    err (91 1 err))) (101 (= 96 err 1) (102 421 (123 1 err)))) (95 (58 (48
    err 1) (65 err (91 1 err))) (108 (= 96 err 1) (109 422 (123 1 err))))
    (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97 err (123 1 err))))
    (95 (58 (48 err 1) (65 err (91 1 err))) (101 (= 96 err 1) (102 423 (123
    1 err)))) (35 (11 (10 255 err) (34 255 323)) (92 (91 255 322) (= 93 424
    255))) (35 (11 (10 255 err) (34 255 323)) (92 (91 255 322) (= 93 425
    255))) (35 (11 (10 381 err) (34 381 427)) (92 (91 381 426) (= 125 424
    381))) (39 (11 (10 382 err) (= 34 430 382)) (92 (40 428 (91 382 429))
    (= 125 425 382))) (91 (11 (10 326 err) (= 34 384 326)) (101 (92 383
    (100 326 432)) (= 119 431 326))) (34 (= 10 err 326) (91 (35 384 326)
    (92 383 326))) (35 (11 (10 327 err) (34 327 387)) (40 (39 327 385) (=
    91 386 327))) (40 (34 (= 10 err 327) (35 387 (39 327 385))) (100 (= 91
    386 327) (119 (101 434 327) (120 433 327)))) (35 (11 (10 327 err) (34
    327 387)) (40 (39 327 385) (= 91 386 327))) (= 116 435 err) (= 112 436
    err) (84 (58 (48 err 1) (65 err (83 1 437))) (96 (91 1 (95 err 1)) (97
    err (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 438) (97
    err (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 439) (97
    err (123 1 err)))) (85 (58 (48 err 1) (65 err (84 1 440))) (96 (91 1
    (95 err 1)) (97 err (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96
    (95 err 441) (97 err (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96
    (95 err 442) (97 err (123 1 err)))) (84 (58 (48 err 1) (65 err (83 1
    443))) (96 (91 1 (95 err 1)) (97 err (123 1 err)))) (95 (58 (48 err 1)
    (65 err (91 1 err))) (117 (= 96 err 1) (118 444 (123 1 err)))) (= 41
    445 err) (= 103 446 err) (= 116 447 err) (= 101 448 err) (= 41 449 err)
    (= 101 450 err) err (= 97 451 err) err (= 41 452 err) (95 (58 (48 err
    1) (65 err (91 1 err))) (99 (= 96 err 1) (100 453 (123 1 err)))) (91
    (58 (48 err 1) (65 err 1)) (96 (95 err 454) (97 err (123 1 err)))) (95
    (58 (48 err 1) (65 err (91 1 err))) (101 (= 96 err 1) (102 455 (123 1
    err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97 err (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (99 (= 96 err 1) (100
    456 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (101 (= 96
    err 1) (102 457 (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95
    err 458) (97 err (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err)))
    (110 (= 96 err 1) (111 459 (123 1 err)))) (95 (58 (48 err 1) (65 err
    (91 1 err))) (110 (= 96 err 1) (111 460 (123 1 err)))) (91 (58 (48 err
    1) (65 err 1)) (96 (95 err 1) (97 err (123 1 err)))) (91 (58 (48 err 1)
    (65 err 1)) (96 (95 err 1) (97 err (123 1 err)))) (95 (58 (48 err 1)
    (65 err (91 1 err))) (101 (= 96 err 1) (102 461 (123 1 err)))) (95 (58
    (48 err 1) (65 err (91 1 err))) (99 (= 96 err 1) (100 462 (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (97 (96 1 err) (98 463
    (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (97 (96 1 err)
    (98 464 (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1)
    (97 err (123 1 err)))) (34 (= 10 err 424) (91 (35 466 424) (92 465
    424))) (35 (11 (10 425 err) (34 425 469)) (40 (39 425 467) (= 91 468
    425))) (92 (34 (= 10 err 381) (35 427 (91 381 426))) (119 (= 100 471
    381) (125 (120 470 381) (126 424 381)))) (35 (11 (10 381 err) (34 381
    427)) (92 (91 381 426) (= 125 424 381))) (39 (11 (10 382 err) (= 34 430
    382)) (92 (40 428 (91 382 429)) (= 125 425 382))) (91 (34 (= 10 err
    382) (39 (35 430 382) (40 428 382))) (119 (100 (92 429 382) (101 473
    382)) (125 (120 472 382) (126 425 382)))) (39 (11 (10 382 err) (= 34
    430 382)) (92 (40 428 (91 382 429)) (= 125 425 382))) (34 (= 10 err
    326) (91 (35 384 326) (92 383 326))) (35 (11 (10 326 err) (34 326 384))
    (92 (91 326 383) (= 93 474 326))) (39 (11 (10 327 err) (= 34 387 327))
    (92 (40 385 (91 327 386)) (= 93 474 327))) (35 (11 (10 327 err) (34 327
    387)) (40 (39 327 385) (= 91 386 327))) (= 32 475 err) (= 116 476 err)
    (81 (58 (48 err 1) (65 err (80 1 477))) (96 (91 1 (95 err 1)) (97 err
    (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 478) (97 err
    (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (99 (= 96 err 1)
    (100 479 (123 1 err)))) (74 (58 (48 err 1) (65 err (73 1 480))) (96 (91
    1 (95 err 1)) (97 err (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96
    (95 err 481) (97 err (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96
    (95 err 1) (97 err (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96
    (95 err 482) (97 err (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1
    err))) (101 (= 96 err 1) (102 483 (123 1 err)))) err (= 41 484 err) (=
    41 485 err) (= 114 486 err) err (= 41 487 err) (= 110 488 err) err (95
    (58 (48 err 1) (65 err (91 1 err))) (116 (= 96 err 1) (117 489 (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (111 (= 96 err 1) (112
    490 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (100 (= 96
    err 1) (101 491 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err)))
    (101 (= 96 err 1) (102 492 (123 1 err)))) (95 (58 (48 err 1) (65 err
    (91 1 err))) (111 (= 96 err 1) (112 493 (123 1 err)))) (95 (58 (48 err
    1) (65 err (91 1 err))) (111 (= 96 err 1) (112 494 (123 1 err)))) (95
    (58 (48 err 1) (65 err (91 1 err))) (116 (= 96 err 1) (117 495 (123 1
    err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97 err (123 1
    err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97 err (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (104 (= 96 err 1) (105
    496 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (99 (= 96
    err 1) (100 497 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err)))
    (114 (= 96 err 1) (115 498 (123 1 err)))) (91 (11 (10 424 err) (= 34
    466 424)) (101 (92 465 (100 424 500)) (= 119 499 424))) (34 (= 10 err
    424) (91 (35 466 424) (92 465 424))) (35 (11 (10 425 err) (34 425 469))
    (40 (39 425 467) (= 91 468 425))) (40 (34 (= 10 err 425) (35 469 (39
    425 467))) (100 (= 91 468 425) (119 (101 502 425) (120 501 425)))) (35
    (11 (10 425 err) (34 425 469)) (40 (39 425 467) (= 91 468 425))) (35
    (11 (10 381 err) (34 381 427)) (92 (91 381 426) (= 125 424 381))) (91
    (11 (10 381 err) (= 34 427 381)) (94 (92 426 (93 381 503)) (= 125 424
    381))) (40 (34 (= 10 err 382) (35 430 (39 382 428))) (93 (= 91 429 382)
    (125 (94 503 382) (126 425 382)))) (39 (11 (10 382 err) (= 34 430 382))
    (92 (40 428 (91 382 429)) (= 125 425 382))) (35 (11 (10 474 err) (34
    474 506)) (40 (39 474 504) (= 91 505 474))) (= 108 507 err) (= 62 121
    err) (91 (58 (48 err 1) (65 err (66 508 1))) (96 (95 err 1) (97 err
    (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97 err
    (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (111 (= 96 err
    1) (112 509 (123 1 err)))) (80 (58 (48 err 1) (65 err (79 1 510))) (96
    (91 1 (95 err 1)) (97 err (123 1 err)))) (91 (58 (48 err 1) (65 err 1))
    (96 (95 err 1) (97 err (123 1 err)))) (91 (58 (48 err 1) (65 err 1))
    (96 (95 err 511) (97 err (123 1 err)))) (91 (58 (48 err 1) (65 err 1))
    (96 (95 err 1) (97 err (123 1 err)))) err err (= 41 512 err) err (= 41
    406 err) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97 err (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (110 (= 96 err 1) (111
    513 (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97
    err (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97
    err (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (102 (= 96
    err 1) (103 514 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err)))
    (110 (= 96 err 1) (111 515 (123 1 err)))) (95 (58 (48 err 1) (65 err
    (91 1 err))) (115 (= 96 err 1) (116 516 (123 1 err)))) (91 (58 (48 err
    1) (65 err 1)) (96 (95 err 1) (97 err (123 1 err)))) (95 (58 (48 err 1)
    (65 err (91 1 err))) (104 (= 96 err 1) (105 517 (123 1 err)))) (95 (58
    (48 err 1) (65 err (91 1 err))) (101 (= 96 err 1) (102 518 (123 1
    err)))) (34 (= 10 err 424) (91 (35 466 424) (92 465 424))) (35 (11 (10
    424 err) (34 424 466)) (92 (91 424 465) (= 93 519 424))) (39 (11 (10
    425 err) (= 34 469 425)) (92 (40 467 (91 425 468)) (= 93 519 425))) (35
    (11 (10 425 err) (34 425 469)) (40 (39 425 467) (= 91 468 425))) (39
    (11 (10 503 err) (= 34 522 503)) (92 (40 520 (91 503 521)) (= 125 519
    503))) (35 (11 (10 474 err) (34 474 506)) (40 (39 474 504) (= 91 505
    474))) (40 (34 (= 10 err 474) (35 506 (39 474 504))) (100 (= 91 505
    474) (119 (101 524 474) (120 523 474)))) (35 (11 (10 474 err) (34 474
    506)) (40 (39 474 504) (= 91 505 474))) (= 97 525 err) (68 (58 (48 err
    1) (65 err (67 1 526))) (96 (91 1 (95 err 1)) (97 err (123 1 err))))
    (95 (58 (48 err 1) (65 err (91 1 err))) (109 (= 96 err 1) (110 527 (123
    1 err)))) (79 (58 (48 err 1) (65 err (78 1 528))) (96 (91 1 (95 err 1))
    (97 err (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1)
    (97 err (123 1 err)))) err (95 (58 (48 err 1) (65 err (91 1 err))) (99
    (= 96 err 1) (100 529 (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96
    (95 err 1) (97 err (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1
    err))) (99 (= 96 err 1) (100 530 (123 1 err)))) (91 (58 (48 err 1) (65
    err 1)) (96 (95 err 1) (97 err (123 1 err)))) (91 (58 (48 err 1) (65
    err 1)) (96 (95 err 1) (97 err (123 1 err)))) (91 (58 (48 err 1) (65
    err 1)) (96 (95 err 1) (97 err (123 1 err)))) (35 (11 (10 519 err) (34
    519 533)) (40 (39 519 531) (= 91 532 519))) (39 (11 (10 503 err) (= 34
    522 503)) (92 (40 520 (91 503 521)) (= 125 519 503))) (91 (34 (= 10 err
    503) (39 (35 522 503) (40 520 503))) (119 (100 (92 521 503) (101 535
    503)) (125 (120 534 503) (126 519 503)))) (39 (11 (10 503 err) (= 34
    522 503)) (92 (40 520 (91 503 521)) (= 125 519 503))) (35 (11 (10 474
    err) (34 474 506)) (40 (39 474 504) (= 91 505 474))) (35 (11 (10 474
    err) (34 474 506)) (40 (39 474 504) (= 91 505 474))) (= 110 536 err)
    (70 (58 (48 err 1) (65 err (69 1 537))) (96 (91 1 (95 err 1)) (97 err
    (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (112 (= 96 err
    1) (113 538 (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err
    539) (97 err (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err)))
    (101 (= 96 err 1) (102 540 (123 1 err)))) (95 (58 (48 err 1) (65 err
    (91 1 err))) (101 (= 96 err 1) (102 541 (123 1 err)))) (35 (11 (10 519
    err) (34 519 533)) (40 (39 519 531) (= 91 532 519))) (40 (34 (= 10 err
    519) (35 533 (39 519 531))) (100 (= 91 532 519) (119 (101 543 519) (120
    542 519)))) (35 (11 (10 519 err) (34 519 533)) (40 (39 519 531) (= 91
    532 519))) (39 (11 (10 503 err) (= 34 522 503)) (92 (40 520 (91 503
    521)) (= 125 519 503))) (39 (11 (10 503 err) (= 34 522 503)) (92 (40
    520 (91 503 521)) (= 125 519 503))) (= 103 544 err) (91 (58 (48 err 1)
    (65 err 1)) (96 (95 err 545) (97 err (123 1 err)))) (95 (58 (48 err 1)
    (65 err (91 1 err))) (105 (= 96 err 1) (106 546 (123 1 err)))) (91 (58
    (48 err 1) (65 err 1)) (96 (95 err 547) (97 err (123 1 err)))) (91 (58
    (48 err 1) (65 err 1)) (96 (95 err 1) (97 err (123 1 err)))) (91 (58
    (48 err 1) (65 err 1)) (96 (95 err 1) (97 err (123 1 err)))) (35 (11
    (10 519 err) (34 519 533)) (40 (39 519 531) (= 91 532 519))) (35 (11
    (10 519 err) (34 519 533)) (40 (39 519 531) (= 91 532 519))) (= 117 548
    err) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 549) (97 err (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (108 (= 96 err 1) (109
    550 (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97
    err (123 1 err)))) (= 97 551 err) (91 (58 (48 err 1) (65 err 1)) (96
    (95 err 1) (97 err (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1
    err))) (101 (= 96 err 1) (102 552 (123 1 err)))) (= 103 553 err) (95
    (58 (48 err 1) (65 err (91 1 err))) (114 (= 96 err 1) (115 554 (123 1
    err)))) (= 101 555 err) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1)
    (97 err (123 1 err)))) (= 61 556 err) (= 34 557 err) (= 112 558 err) (=
    104 559 err) (= 112 560 err) (= 34 561 err) (= 62 331 err))
   '#((#f . #f) (140 . 140) (140 . 140) (129 . 129) (128 . 128) (127 . 127)
    (126 . 126) (125 . 125) (123 . 123) (#f . #f) (119 . 119) (118 . 118)
    (117 . 117) (140 . 140) (140 . 140) (140 . 140) (140 . 140) (140 . 140)
    (140 . 140) (140 . 140) (140 . 140) (130 . 130) (140 . 140) (140 . 140)
    (77 . 77) (77 . 77) (140 . 140) (#f . #f) (137 . 137) (131 . 131) (140
    . 140) (140 . 140) (140 . 140) (140 . 140) (#f . #f) (134 . 134) (#f .
    #f) (140 . 140) (132 . 132) (124 . 124) (21 . 21) (20 . 20) (135 . 135)
    (133 . 133) (#f . #f) (138 . 138) (#f . #f) (136 . 136) (140 . 140)
    (140 . 140) (140 . 140) (#f . #f) (120 . 120) (122 . 122) (#f . #f)
    (140 . 140) (140 . 140) (121 . 121) (140 . 140) (140 . 140) (140 . 140)
    (140 . 140) (140 . 140) (140 . 140) (140 . 140) (140 . 140) (140 . 140)
    (140 . 140) (140 . 140) (140 . 140) (140 . 140) (82 . 82) (140 . 140)
    (79 . 79) (140 . 140) (72 . 72) (101 . 101) (70 . 70) (91 . 91) (61 .
    61) (140 . 140) (140 . 140) (140 . 140) (59 . 59) (140 . 140) (140 .
    140) (140 . 140) (140 . 140) (140 . 140) (140 . 140) (140 . 140) (140 .
    140) (140 . 140) (140 . 140) (140 . 140) (36 . 36) (69 . 69) (32 . 32)
    (115 . 115) (31 . 31) (30 . 30) (#f . #f) (140 . 140) (87 . 87) (24 .
    24) (23 . 23) (#f . #f) (#f . #f) (18 . 18) (27 . 27) (#f . #f) (#f .
    #f) (15 . 15) (99 . 99) (#f . #f) (88 . 88) (88 . 88) (74 . 74) (72 .
    72) (#f . #f) (81 . 81) (14 . 14) (140 . 140) (140 . 140) (140 . 140)
    (140 . 140) (140 . 140) (90 . 90) (6 . 6) (#f . #f) (#f . #f) (#f . #f)
    (#f . #f) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (#f . #f)
    (5 . 5) (1 . 1) (140 . 140) (140 . 140) (140 . 140) (140 . 140) (140 .
    140) (114 . 114) (113 . 113) (140 . 140) (110 . 110) (140 . 140) (140 .
    140) (140 . 140) (140 . 140) (140 . 140) (140 . 140) (140 . 140) (140 .
    140) (140 . 140) (84 . 84) (80 . 80) (140 . 140) (73 . 73) (102 . 102)
    (140 . 140) (140 . 140) (140 . 140) (140 . 140) (140 . 140) (140 . 140)
    (140 . 140) (140 . 140) (52 . 52) (140 . 140) (140 . 140) (140 . 140)
    (140 . 140) (140 . 140) (140 . 140) (140 . 140) (71 . 71) (28 . 28)
    (140 . 140) (#f . #f) (#f . #f) (#f . #f) (41 . 41) (#f . #f) (#f . #f)
    (16 . 16) (103 . 103) (100 . 100) (#f . #f) (89 . 89) (#f . #f) (#f .
    #f) (140 . 140) (140 . 140) (140 . 140) (140 . 140) (140 . 140) (140 .
    140) (140 . 140) (140 . 140) (140 . 140) (140 . 140) (140 . 140) (140 .
    140) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (#f .
    #f) (#f . #f) (#f . #f) (#f . #f) (78 . 78) (140 . 140) (140 . 140)
    (139 . 139) (140 . 140) (140 . 140) (140 . 140) (140 . 140) (140 . 140)
    (140 . 140) (140 . 140) (140 . 140) (140 . 140) (140 . 140) (140 . 140)
    (76 . 76) (140 . 140) (140 . 140) (140 . 140) (140 . 140) (140 . 140)
    (57 . 57) (140 . 140) (140 . 140) (140 . 140) (140 . 140) (140 . 140)
    (48 . 48) (140 . 140) (140 . 140) (140 . 140) (140 . 140) (140 . 140)
    (140 . 140) (38 . 38) (37 . 37) (140 . 140) (#f . #f) (#f . #f) (41 .
    41) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (17 . 17) (#f . #f) (#f .
    #f) (#f . #f) (#f . #f) (140 . 140) (140 . 140) (140 . 140) (140 . 140)
    (140 . 140) (140 . 140) (140 . 140) (140 . 140) (140 . 140) (140 . 140)
    (140 . 140) (140 . 140) (9 . 9) (140 . 140) (4 . 4) (#f . #f) (#f . #f)
    (#f . #f) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (#f . #f)
    (140 . 140) (140 . 140) (116 . 116) (111 . 111) (109 . 109) (140 . 140)
    (140 . 140) (140 . 140) (140 . 140) (140 . 140) (140 . 140) (140 . 140)
    (92 . 92) (68 . 68) (140 . 140) (140 . 140) (140 . 140) (140 . 140)
    (140 . 140) (140 . 140) (140 . 140) (51 . 51) (140 . 140) (140 . 140)
    (140 . 140) (45 . 45) (140 . 140) (140 . 140) (40 . 40) (140 . 140)
    (140 . 140) (#f . #f) (41 . 41) (#f . #f) (#f . #f) (#f . #f) (#f . #f)
    (17 . 17) (17 . 17) (#f . #f) (88 . 88) (#f . #f) (140 . 140) (140 .
    140) (140 . 140) (140 . 140) (140 . 140) (140 . 140) (140 . 140) (140 .
    140) (19 . 19) (13 . 13) (11 . 11) (10 . 10) (8 . 8) (#f . #f) (#f .
    #f) (#f . #f) (#f . #f) (65 . 65) (#f . #f) (#f . #f) (#f . #f) (#f .
    #f) (#f . #f) (2 . 2) (140 . 140) (108 . 108) (104 . 104) (98 . 98)
    (140 . 140) (94 . 94) (140 . 140) (140 . 140) (140 . 140) (140 . 140)
    (140 . 140) (140 . 140) (56 . 56) (140 . 140) (140 . 140) (140 . 140)
    (140 . 140) (140 . 140) (43 . 43) (140 . 140) (39 . 39) (140 . 140) (#f
    . #f) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (41 . 41) (85 . 85) (#f .
    #f) (41 . 41) (#f . #f) (#f . #f) (140 . 140) (140 . 140) (140 . 140)
    (140 . 140) (140 . 140) (140 . 140) (140 . 140) (140 . 140) (#f . #f)
    (#f . #f) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (33 . 33) (#f . #f)
    (7 . 7) (#f . #f) (140 . 140) (96 . 96) (140 . 140) (93 . 93) (140 .
    140) (140 . 140) (62 . 62) (140 . 140) (140 . 140) (53 . 53) (49 . 49)
    (140 . 140) (140 . 140) (140 . 140) (140 . 140) (25 . 25) (#f . #f) (#f
    . #f) (#f . #f) (41 . 41) (85 . 85) (#f . #f) (41 . 41) (#f . #f) (#f .
    #f) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (140 . 140) (140 . 140)
    (140 . 140) (140 . 140) (140 . 140) (26 . 26) (140 . 140) (140 . 140)
    (112 . 112) (#f . #f) (#f . #f) (#f . #f) (35 . 35) (#f . #f) (#f . #f)
    (3 . 3) (140 . 140) (140 . 140) (140 . 140) (140 . 140) (140 . 140)
    (140 . 140) (140 . 140) (54 . 54) (47 . 47) (140 . 140) (140 . 140)
    (140 . 140) (#f . #f) (41 . 41) (85 . 85) (#f . #f) (41 . 41) (#f . #f)
    (#f . #f) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (140 . 140)
    (75 . 75) (140 . 140) (140 . 140) (50 . 50) (140 . 140) (22 . 22) (106
    . 106) (86 . 86) (#f . #f) (34 . 34) (#f . #f) (0 . 0) (140 . 140) (95
    . 95) (67 . 67) (140 . 140) (140 . 140) (140 . 140) (46 . 46) (140 .
    140) (140 . 140) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (85
    . 85) (#f . #f) (41 . 41) (#f . #f) (140 . 140) (140 . 140) (140 . 140)
    (12 . 12) (66 . 66) (140 . 140) (64 . 64) (140 . 140) (60 . 60) (44 .
    44) (42 . 42) (#f . #f) (85 . 85) (#f . #f) (41 . 41) (#f . #f) (#f .
    #f) (#f . #f) (140 . 140) (140 . 140) (140 . 140) (140 . 140) (140 .
    140) (85 . 85) (#f . #f) (41 . 41) (#f . #f) (#f . #f) (#f . #f) (140 .
    140) (140 . 140) (140 . 140) (97 . 97) (63 . 63) (#f . #f) (#f . #f)
    (#f . #f) (140 . 140) (140 . 140) (55 . 55) (#f . #f) (83 . 83) (140 .
    140) (#f . #f) (140 . 140) (#f . #f) (58 . 58) (#f . #f) (#f . #f) (#f
    . #f) (#f . #f) (#f . #f) (#f . #f) (#f . #f))))

;
; User functions
;

(define lexer #f)

(define lexer-get-line   #f)
(define lexer-getc       #f)
(define lexer-ungetc     #f)

(define lexer-init
  (lambda (input-type input)
    (let ((IS (lexer-make-IS input-type input 'line)))
      (set! lexer (lexer-make-lexer lexer-default-table IS))
      (set! lexer-get-line   (lexer-get-func-line IS))
      (set! lexer-getc       (lexer-get-func-getc IS))
      (set! lexer-ungetc     (lexer-get-func-ungetc IS)))))
