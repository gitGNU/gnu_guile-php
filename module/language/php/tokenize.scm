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

(define (get-char yygetc yyungetc)
  (let ((c (yygetc)))
    (if (and (char? c) (char=? c #\\))
	(let ((nc (yygetc)))
	  (cond
	   ((char=? nc #\n) #\newline)
	   ((char=? nc #\t) #\tab)
	   (else
	    (begin (yyungetc) c))))
	c)))

(define (read-til stop-reading yygetc yyungetc)
  (let loop ((s '()))
    (let ((c (get-char yygetc yyungetc)))
      (if (stop-reading c)
	  (list->string (reverse s))
	  (loop (cons c s))))))

(define (read-single-line-comment yygetc yyungetc)
  (define (stop-reading c)
    (if (or (eq? c 'eof) (not (char? c)))
	#t
	(char=? c #\newline)))
  (make-token 'T_COMMENT (read-til stop-reading yygetc yyungetc)))

(define (read-multi-line-comment yygetc yyungetc)
  (define (stop-reading c)
    (if (or (eq? c 'eof) (not (char? c)))
	#t
	(if (eq? c #\*)
	    (let ((nc (yygetc)))
	      (if (and (char? nc) (char=? nc #\/))
		  #t
		  (begin (yyungetc) #f)))
	    #f)))
  (make-token 'T_COMMENT (read-til stop-reading yygetc yyungetc)))
	
(define (read-string str-char yygetc yyungetc)
  (define tok 'T_CONSTANT_ENCAPSULATED_STRING)
  (define (stop-reading c)
    (if (eq? c 'eof)
	(syntax-error "Unexpected eof inside string")
	(char=? c str-char)))
  (make-token tok (read-til stop-reading yygetc yyungetc)))

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
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline)
          (read-single-line-comment yygetc yyungetc)
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline)
          (read-multi-line-comment yygetc yyungetc)
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
    err) (61 (48 44 err) (62 109 err))) err (61 (47 (= 37 113 err) (48 117
    (60 err 111))) (64 (62 115 (63 116 114)) (= 115 112 err))) (62 (61 err
    118) (63 119 err)) (= 62 119 err) (91 (58 (48 err 1) (65 err 1)) (96
    (95 err 120) (97 err (123 1 err)))) (97 (65 (48 err (58 1 err)) (95 (91
    1 err) (96 1 err))) (109 (98 123 (108 1 122)) (112 (111 1 121) (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (114 (= 96 err 1) (115
    124 (123 1 err)))) (62 (61 err 125) (= 124 126 err)) (= 97 127 err)
    (105 (100 (98 (97 err 136) (99 135 err)) (102 (101 133 err) (103 132
    err))) (114 (111 (106 131 err) (112 130 err)) (116 (115 134 129) (= 117
    128 err)))) (39 (38 err 137) (= 61 138 err)) (97 (65 (48 err (58 1
    err)) (95 (91 1 err) (96 1 err))) (111 (99 (98 1 141) (110 1 139)) (115
    (114 1 140) (123 1 err)))) (77 (58 (48 err 1) (65 err (76 1 142))) (96
    (91 1 (95 err 1)) (97 err (123 1 err)))) err (95 (58 (48 err 1) (65 err
    (91 1 err))) (105 (= 96 err 1) (106 143 (123 1 err)))) (95 (58 (48 err
    1) (65 err (91 1 err))) (114 (= 96 err 1) (115 144 (123 1 err)))) (95
    (58 (48 err 1) (65 err (91 1 err))) (101 (= 96 err 1) (102 145 (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (115 (= 96 err 1) (116
    146 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (121 (= 96
    err 1) (122 147 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err)))
    (114 (= 96 err 1) (115 148 (123 1 err)))) (95 (58 (48 err 1) (65 err
    (91 1 err))) (105 (= 96 err 1) (106 149 (123 1 err)))) (95 (58 (48 err
    1) (65 err (91 1 err))) (97 (96 1 err) (98 150 (123 1 err)))) (96 (65
    (48 err (58 1 err)) (91 1 (95 err 1))) (114 (97 err (113 1 152)) (117
    (116 1 151) (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (98
    (= 96 err 1) (99 153 (123 1 err)))) (96 (65 (48 err (58 1 err)) (91 1
    (95 err 1))) (106 (97 err (105 1 155)) (112 (111 1 154) (123 1 err))))
    (95 (58 (48 err 1) (65 err (91 1 err))) (108 (= 96 err 1) (109 156 (123
    1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (119 (= 96 err 1)
    (120 157 (123 1 err)))) err (95 (58 (48 err 1) (65 err (91 1 err)))
    (114 (= 96 err 1) (115 158 (123 1 err)))) (91 (58 (48 err 1) (65 err
    1)) (96 (95 err 1) (97 err (123 1 err)))) (95 (58 (48 err 1) (65 err
    (91 1 err))) (115 (= 96 err 1) (116 159 (123 1 err)))) (= 61 160 err)
    (= 61 161 err) err err err (95 (58 (48 err 1) (65 err (91 1 err))) (115
    (= 96 err 1) (116 162 (123 1 err)))) (97 (65 (48 err (58 1 err)) (95
    (91 1 err) (96 1 err))) (115 (= 99 165 1) (117 (116 164 163) (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (112 (= 96 err 1) (113
    166 (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97
    err (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (116 (= 96
    err 1) (117 167 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err)))
    (111 (= 96 err 1) (112 168 (123 1 err)))) (95 (58 (48 err 1) (65 err
    (91 1 err))) (110 (= 96 err 1) (111 169 (123 1 err)))) (95 (58 (48 err
    1) (65 err (91 1 err))) (114 (= 96 err 1) (115 170 (123 1 err)))) (95
    (58 (48 err 1) (65 err (91 1 err))) (110 (= 96 err 1) (111 171 (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (116 (= 96 err 1) (117
    172 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (97 (96 1
    err) (98 173 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err)))
    (100 (= 96 err 1) (101 174 (123 1 err)))) (95 (58 (48 err 1) (65 err
    (91 1 err))) (112 (= 96 err 1) (113 175 (123 1 err)))) (95 (58 (48 err
    1) (65 err (91 1 err))) (115 (= 96 err 1) (116 176 (123 1 err)))) (95
    (58 (48 err 1) (65 err (91 1 err))) (104 (= 96 err 1) (105 177 (123 1
    err)))) err (= 61 178 err) err (91 (65 err 98) (97 err (123 98 err)))
    err (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97 err (123 1
    err)))) (= 100 179 err) (95 (58 (48 err 1) (65 err (91 1 err))) (99 (=
    96 err 1) (100 180 (123 1 err)))) err err err (= 36 181 err) (35 (11
    (10 181 err) (34 181 184)) (92 (91 181 183) (= 123 182 181))) err err
    (= 42 185 err) (61 (60 err 186) (62 187 err)) (= 99 188 err) (= 61 189
    err) (62 (61 err 189) (= 112 190 err)) err err (= 115 191 err) err err
    (77 (68 (58 (48 err 1) (65 err (67 1 197))) (70 (69 196 1) (71 195 (76
    1 193)))) (96 (79 (78 1 192) (91 1 (95 err 1))) (104 (97 err 1) (105
    194 (123 1 err))))) (95 (58 (48 err 1) (65 err (91 1 err))) (110 (= 96
    err 1) (111 198 (123 1 err)))) (96 (65 (48 err (58 1 err)) (91 1 (95
    err 1))) (111 (97 err (98 200 1)) (112 199 (123 1 err)))) (96 (65 (48
    err (58 1 err)) (91 1 (95 err 1))) (116 (97 err (115 1 202)) (117 201
    (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (101 (= 96 err
    1) (102 203 (123 1 err)))) err err (= 115 204 err) (= 110 205 err) (=
    116 206 err) (= 98 207 err) (= 110 208 err) (= 108 209 err) (= 111 210
    err) (= 101 211 err) (= 111 212 err) (= 114 213 err) err err (95 (58
    (48 err 1) (65 err (91 1 err))) (100 (= 96 err 1) (101 214 (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (114 (= 96 err 1) (115
    215 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (115 (= 96
    err 1) (116 216 (123 1 err)))) (77 (58 (48 err 1) (65 err (76 1 217)))
    (96 (91 1 (95 err 1)) (97 err (123 1 err)))) (95 (58 (48 err 1) (65 err
    (91 1 err))) (108 (= 96 err 1) (109 218 (123 1 err)))) (91 (58 (48 err
    1) (65 err 1)) (96 (95 err 1) (97 err (123 1 err)))) (91 (58 (48 err 1)
    (65 err 1)) (96 (95 err 1) (97 err (123 1 err)))) (95 (58 (48 err 1)
    (65 err (91 1 err))) (101 (= 96 err 1) (102 219 (123 1 err)))) (91 (58
    (48 err 1) (65 err 1)) (96 (95 err 1) (97 err (123 1 err)))) (95 (58
    (48 err 1) (65 err (91 1 err))) (111 (= 96 err 1) (112 220 (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (116 (= 96 err 1) (117
    221 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (116 (= 96
    err 1) (117 222 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err)))
    (117 (= 96 err 1) (118 223 (123 1 err)))) (95 (58 (48 err 1) (65 err
    (91 1 err))) (117 (= 96 err 1) (118 224 (123 1 err)))) (95 (58 (48 err
    1) (65 err (91 1 err))) (108 (= 96 err 1) (109 225 (123 1 err)))) (95
    (58 (48 err 1) (65 err (91 1 err))) (116 (= 96 err 1) (117 226 (123 1
    err)))) (96 (65 (48 err (58 1 err)) (91 1 (95 err 1))) (111 (97 err
    (110 1 228)) (119 (118 1 227) (123 1 err)))) (95 (58 (48 err 1) (65 err
    (91 1 err))) (108 (= 96 err 1) (109 217 (123 1 err)))) (91 (58 (48 err
    1) (65 err 1)) (96 (95 err 1) (97 err (123 1 err)))) (91 (58 (48 err 1)
    (65 err 1)) (96 (95 err 1) (97 err (123 1 err)))) (95 (58 (48 err 1)
    (65 err (91 1 err))) (116 (= 96 err 1) (117 229 (123 1 err)))) err err
    (95 (58 (48 err 1) (65 err (91 1 err))) (101 (= 96 err 1) (102 230 (123
    1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (101 (= 96 err 1)
    (102 231 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (116 (=
    96 err 1) (117 232 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1
    err))) (108 (= 96 err 1) (109 233 (123 1 err)))) (95 (58 (48 err 1) (65
    err (91 1 err))) (108 (= 96 err 1) (109 234 (123 1 err)))) (95 (58 (48
    err 1) (65 err (91 1 err))) (111 (= 96 err 1) (112 235 (123 1 err))))
    (95 (58 (48 err 1) (65 err (91 1 err))) (98 (= 96 err 1) (99 236 (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (99 (= 96 err 1) (100
    237 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (101 (= 96
    err 1) (102 238 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err)))
    (97 (96 1 err) (98 239 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1
    err))) (101 (= 96 err 1) (102 240 (123 1 err)))) (95 (58 (48 err 1) (65
    err (91 1 err))) (108 (= 96 err 1) (109 241 (123 1 err)))) (101 (91 (58
    (48 err 1) (65 err 1)) (96 (95 err 1) (97 err (100 1 246)))) (115 (103
    (102 1 245) (= 105 244 1)) (119 (116 243 1) (120 242 (123 1 err)))))
    (95 (58 (48 err 1) (65 err (91 1 err))) (116 (= 96 err 1) (117 247 (123
    1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (101 (= 96 err 1)
    (102 248 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (111 (=
    96 err 1) (112 249 (123 1 err)))) err err (95 (58 (48 err 1) (65 err
    (91 1 err))) (108 (= 96 err 1) (109 250 (123 1 err)))) (34 (= 10 err
    181) (91 (35 184 181) (92 183 181))) (35 (11 (10 182 err) (34 182 253))
    (92 (91 182 252) (= 125 251 182))) (91 (11 (10 181 err) (= 34 184 181))
    (101 (92 183 (100 181 255)) (= 119 254 181))) (34 (= 10 err 181) (91
    (35 184 181) (92 183 181))) (11 (10 185 err) (= 42 256 185)) err err (=
    114 257 err) err (= 104 258 err) (= 99 259 err) (91 (58 (48 err 1) (65
    err (66 260 1))) (96 (95 err 1) (97 err (123 1 err)))) (74 (58 (48 err
    1) (65 err (73 1 261))) (96 (91 1 (95 err 1)) (97 err (123 1 err))))
    (95 (58 (48 err 1) (65 err (91 1 err))) (97 (96 1 err) (98 262 (123 1
    err)))) (85 (65 (48 err (58 1 err)) (= 73 264 1)) (95 (86 263 (91 1
    err)) (97 (96 1 err) (123 1 err)))) (74 (58 (48 err 1) (65 err (73 1
    265))) (96 (91 1 (95 err 1)) (97 err (123 1 err)))) (77 (58 (48 err 1)
    (65 err (76 1 266))) (96 (91 1 (95 err 1)) (97 err (123 1 err)))) (96
    (65 (48 err (58 1 err)) (91 1 (95 err 1))) (116 (97 err (115 1 268))
    (117 267 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (110 (=
    96 err 1) (111 269 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1
    err))) (115 (= 96 err 1) (116 270 (123 1 err)))) (95 (58 (48 err 1) (65
    err (91 1 err))) (99 (= 96 err 1) (100 271 (123 1 err)))) (95 (58 (48
    err 1) (65 err (91 1 err))) (101 (= 96 err 1) (102 272 (123 1 err))))
    (95 (58 (48 err 1) (65 err (91 1 err))) (97 (96 1 err) (98 273 (123 1
    err)))) (= 32 274 err) (= 115 275 err) (= 114 276 err) (= 106 277 err)
    (= 116 278 err) (= 111 279 err) (= 117 280 err) (= 97 281 err) (= 111
    282 err) (= 114 283 err) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1)
    (97 err (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (97 (96
    1 err) (98 284 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err)))
    (116 (= 96 err 1) (117 285 (123 1 err)))) (91 (58 (48 err 1) (65 err
    1)) (96 (95 err 1) (97 err (123 1 err)))) (95 (58 (48 err 1) (65 err
    (91 1 err))) (101 (= 96 err 1) (102 286 (123 1 err)))) (95 (58 (48 err
    1) (65 err (91 1 err))) (116 (= 96 err 1) (117 287 (123 1 err)))) (95
    (58 (48 err 1) (65 err (91 1 err))) (119 (= 96 err 1) (120 288 (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (99 (= 96 err 1) (100
    289 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (105 (= 96
    err 1) (106 290 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err)))
    (114 (= 96 err 1) (115 291 (123 1 err)))) (95 (58 (48 err 1) (65 err
    (91 1 err))) (105 (= 96 err 1) (106 292 (123 1 err)))) (95 (58 (48 err
    1) (65 err (91 1 err))) (105 (= 96 err 1) (106 293 (123 1 err)))) (95
    (58 (48 err 1) (65 err (91 1 err))) (101 (= 96 err 1) (102 294 (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (97 (96 1 err) (98 295
    (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (116 (= 96 err
    1) (117 296 (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err
    1) (97 err (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (116
    (= 96 err 1) (117 297 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1
    err))) (114 (= 96 err 1) (115 298 (123 1 err)))) (95 (58 (48 err 1) (65
    err (91 1 err))) (97 (96 1 err) (98 299 (123 1 err)))) (95 (58 (48 err
    1) (65 err (91 1 err))) (117 (= 96 err 1) (118 300 (123 1 err)))) (95
    (58 (48 err 1) (65 err (91 1 err))) (101 (= 96 err 1) (102 301 (123 1
    err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97 err (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (97 (96 1 err) (98 302
    (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (116 (= 96 err
    1) (117 303 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (97
    (96 1 err) (98 304 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1
    err))) (108 (= 96 err 1) (109 305 (123 1 err)))) (95 (58 (48 err 1) (65
    err (91 1 err))) (110 (= 96 err 1) (111 306 (123 1 err)))) (91 (58 (48
    err 1) (65 err 1)) (96 (95 err 1) (97 err (123 1 err)))) (95 (58 (48
    err 1) (65 err (91 1 err))) (104 (= 96 err 1) (105 307 (123 1 err))))
    (95 (58 (48 err 1) (65 err (91 1 err))) (119 (= 96 err 1) (120 308 (123
    1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (102 (= 96 err 1)
    (103 309 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (111 (=
    96 err 1) (112 310 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1
    err))) (101 (= 96 err 1) (102 311 (123 1 err)))) (95 (58 (48 err 1) (65
    err (91 1 err))) (121 (= 96 err 1) (122 312 (123 1 err)))) (95 (58 (48
    err 1) (65 err (91 1 err))) (105 (= 96 err 1) (106 313 (123 1 err))))
    (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97 err (123 1 err))))
    (95 (58 (48 err 1) (65 err (91 1 err))) (97 (96 1 err) (98 314 (123 1
    err)))) (34 (= 10 err 251) (91 (35 316 251) (92 315 251))) (92 (34 (=
    10 err 182) (35 253 (91 182 252))) (119 (= 100 318 182) (125 (120 317
    182) (126 251 182)))) (35 (11 (10 182 err) (34 182 253)) (92 (91 182
    252) (= 125 251 182))) (35 (11 (10 181 err) (34 181 184)) (92 (91 181
    183) (= 93 319 181))) (35 (11 (10 181 err) (34 181 184)) (92 (91 181
    183) (= 93 320 181))) (42 (= 10 err 185) (47 (43 256 185) (48 321
    185))) (= 105 322 err) (= 112 323 err) (= 114 324 err) (78 (58 (48 err
    1) (65 err (77 1 325))) (96 (91 1 (95 err 1)) (97 err (123 1 err))))
    (79 (58 (48 err 1) (65 err (78 1 326))) (96 (91 1 (95 err 1)) (97 err
    (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (108 (= 96 err
    1) (109 327 (123 1 err)))) (79 (58 (48 err 1) (65 err (78 1 328))) (96
    (91 1 (95 err 1)) (97 err (123 1 err)))) (77 (58 (48 err 1) (65 err (76
    1 329))) (96 (91 1 (95 err 1)) (97 err (123 1 err)))) (83 (58 (48 err
    1) (65 err (82 1 330))) (96 (91 1 (95 err 1)) (97 err (123 1 err))))
    (91 (58 (48 err 1) (65 err (66 331 1))) (96 (95 err 1) (97 err (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (105 (= 96 err 1) (106
    332 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (116 (= 96
    err 1) (117 333 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err)))
    (101 (= 96 err 1) (102 334 (123 1 err)))) (95 (58 (48 err 1) (65 err
    (91 1 err))) (115 (= 96 err 1) (116 335 (123 1 err)))) (95 (58 (48 err
    1) (65 err (91 1 err))) (104 (= 96 err 1) (105 336 (123 1 err)))) (91
    (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97 err (123 1 err)))) (95
    (58 (48 err 1) (65 err (91 1 err))) (107 (= 96 err 1) (108 337 (123 1
    err)))) err (= 101 338 err) (= 105 339 err) (= 101 340 err) (42 (41 err
    342) (= 101 341 err)) (= 97 343 err) (= 98 344 err) (= 108 345 err) (=
    108 346 err) (= 97 347 err) (95 (58 (48 err 1) (65 err (91 1 err)))
    (121 (= 96 err 1) (122 348 (123 1 err)))) (95 (58 (48 err 1) (65 err
    (91 1 err))) (114 (= 96 err 1) (115 349 (123 1 err)))) (91 (58 (48 err
    1) (65 err 1)) (96 (95 err 1) (97 err (123 1 err)))) (91 (58 (48 err 1)
    (65 err 1)) (96 (95 err 1) (97 err (123 1 err)))) (91 (58 (48 err 1)
    (65 err 1)) (96 (95 err 1) (97 err (123 1 err)))) (95 (58 (48 err 1)
    (65 err (91 1 err))) (104 (= 96 err 1) (105 350 (123 1 err)))) (95 (58
    (48 err 1) (65 err (91 1 err))) (99 (= 96 err 1) (100 351 (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (110 (= 96 err 1) (111
    352 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (114 (= 96
    err 1) (115 353 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err)))
    (99 (= 96 err 1) (100 354 (123 1 err)))) (95 (58 (48 err 1) (65 err (91
    1 err))) (99 (= 96 err 1) (100 355 (123 1 err)))) (95 (58 (48 err 1)
    (65 err (91 1 err))) (116 (= 96 err 1) (117 356 (123 1 err)))) (91 (58
    (48 err 1) (65 err 1)) (96 (95 err 1) (97 err (123 1 err)))) (91 (58
    (48 err 1) (65 err 1)) (96 (95 err 1) (97 err (123 1 err)))) (95 (58
    (48 err 1) (65 err (91 1 err))) (102 (= 96 err 1) (103 357 (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (110 (= 96 err 1) (111
    358 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (100 (= 96
    err 1) (101 359 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err)))
    (109 (= 96 err 1) (110 360 (123 1 err)))) (95 (58 (48 err 1) (65 err
    (91 1 err))) (108 (= 96 err 1) (109 361 (123 1 err)))) (95 (58 (48 err
    1) (65 err (91 1 err))) (105 (= 96 err 1) (106 362 (123 1 err)))) (95
    (58 (48 err 1) (65 err (91 1 err))) (99 (= 96 err 1) (100 363 (123 1
    err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97 err (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (100 (= 96 err 1) (101
    364 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (105 (= 96
    err 1) (106 365 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err)))
    (105 (= 96 err 1) (106 366 (123 1 err)))) (91 (58 (48 err 1) (65 err
    1)) (96 (95 err 1) (97 err (123 1 err)))) (95 (58 (48 err 1) (65 err
    (91 1 err))) (114 (= 96 err 1) (115 367 (123 1 err)))) (95 (58 (48 err
    1) (65 err (91 1 err))) (99 (= 96 err 1) (100 368 (123 1 err)))) (91
    (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97 err (123 1 err)))) (95
    (58 (48 err 1) (65 err (91 1 err))) (102 (= 96 err 1) (103 369 (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (114 (= 96 err 1) (115
    370 (123 1 err)))) (91 (11 (10 251 err) (= 34 316 251)) (101 (92 315
    (100 251 372)) (= 119 371 251))) (34 (= 10 err 251) (91 (35 316 251)
    (92 315 251))) (91 (11 (10 182 err) (= 34 253 182)) (94 (92 252 (93 182
    373)) (= 125 251 182))) (91 (11 (10 182 err) (= 34 253 182)) (94 (92
    252 (93 182 374)) (= 125 251 182))) (34 (= 10 err 319) (91 (35 376 319)
    (92 375 319))) (35 (11 (10 320 err) (34 320 379)) (40 (39 320 377) (=
    91 378 320))) (11 (10 185 err) (= 42 256 185)) (= 112 380 err) err (=
    105 381 err) (70 (58 (48 err 1) (65 err (69 1 382))) (96 (91 1 (95 err
    1)) (97 err (123 1 err)))) (70 (58 (48 err 1) (65 err (69 1 383))) (96
    (91 1 (95 err 1)) (97 err (123 1 err)))) (95 (58 (48 err 1) (65 err (91
    1 err))) (116 (= 96 err 1) (117 384 (123 1 err)))) (68 (58 (48 err 1)
    (65 err (67 1 385))) (96 (91 1 (95 err 1)) (97 err (123 1 err)))) (70
    (58 (48 err 1) (65 err (69 1 386))) (96 (91 1 (95 err 1)) (97 err (123
    1 err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 387) (97 err (123
    1 err)))) (84 (58 (48 err 1) (65 err (83 1 388))) (96 (91 1 (95 err 1))
    (97 err (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (110 (=
    96 err 1) (111 389 (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96
    (95 err 1) (97 err (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96
    (95 err 1) (97 err (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96
    (95 err 1) (97 err (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96
    (95 err 1) (97 err (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96
    (95 err 1) (97 err (123 1 err)))) (= 116 390 err) (= 110 391 err) (= 99
    392 err) (= 103 393 err) err (= 116 394 err) (= 108 395 err) (= 41 396
    err) (42 (41 err 398) (= 101 397 err)) (= 121 399 err) (91 (58 (48 err
    1) (65 err 1)) (96 (95 err 1) (97 err (123 1 err)))) (95 (58 (48 err 1)
    (65 err (91 1 err))) (97 (96 1 err) (98 400 (123 1 err)))) (91 (58 (48
    err 1) (65 err 1)) (96 (95 err 1) (97 err (123 1 err)))) (91 (58 (48
    err 1) (65 err 1)) (96 (95 err 1) (97 err (123 1 err)))) (91 (58 (48
    err 1) (65 err 1)) (96 (95 err 1) (97 err (123 1 err)))) (95 (58 (48
    err 1) (65 err (91 1 err))) (101 (= 96 err 1) (102 401 (123 1 err))))
    (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97 err (123 1 err))))
    (95 (58 (48 err 1) (65 err (91 1 err))) (116 (= 96 err 1) (117 402 (123
    1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (101 (= 96 err 1)
    (102 403 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (97 (96
    1 err) (98 404 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err)))
    (99 (= 96 err 1) (100 405 (123 1 err)))) (95 (58 (48 err 1) (65 err (91
    1 err))) (101 (= 96 err 1) (102 406 (123 1 err)))) (95 (58 (48 err 1)
    (65 err (91 1 err))) (101 (= 96 err 1) (102 407 (123 1 err)))) (91 (58
    (48 err 1) (65 err 1)) (96 (95 err 1) (97 err (123 1 err)))) (95 (58
    (48 err 1) (65 err (91 1 err))) (111 (= 96 err 1) (112 408 (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (104 (= 96 err 1) (105
    409 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (115 (= 96
    err 1) (116 410 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err)))
    (108 (= 96 err 1) (109 411 (123 1 err)))) (95 (58 (48 err 1) (65 err
    (91 1 err))) (116 (= 96 err 1) (117 412 (123 1 err)))) (95 (58 (48 err
    1) (65 err (91 1 err))) (101 (= 96 err 1) (102 413 (123 1 err)))) (95
    (58 (48 err 1) (65 err (91 1 err))) (108 (= 96 err 1) (109 414 (123 1
    err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97 err (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (101 (= 96 err 1) (102
    415 (123 1 err)))) (35 (11 (10 251 err) (34 251 316)) (92 (91 251 315)
    (= 93 416 251))) (35 (11 (10 251 err) (34 251 316)) (92 (91 251 315) (=
    93 417 251))) (35 (11 (10 373 err) (34 373 419)) (92 (91 373 418) (=
    125 416 373))) (39 (11 (10 374 err) (= 34 422 374)) (92 (40 420 (91 374
    421)) (= 125 417 374))) (91 (11 (10 319 err) (= 34 376 319)) (101 (92
    375 (100 319 424)) (= 119 423 319))) (34 (= 10 err 319) (91 (35 376
    319) (92 375 319))) (35 (11 (10 320 err) (34 320 379)) (40 (39 320 377)
    (= 91 378 320))) (40 (34 (= 10 err 320) (35 379 (39 320 377))) (100 (=
    91 378 320) (119 (101 426 320) (120 425 320)))) (35 (11 (10 320 err)
    (34 320 379)) (40 (39 320 377) (= 91 378 320))) (= 116 427 err) (= 112
    428 err) (84 (58 (48 err 1) (65 err (83 1 429))) (96 (91 1 (95 err 1))
    (97 err (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 430)
    (97 err (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 431)
    (97 err (123 1 err)))) (85 (58 (48 err 1) (65 err (84 1 432))) (96 (91
    1 (95 err 1)) (97 err (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96
    (95 err 433) (97 err (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96
    (95 err 434) (97 err (123 1 err)))) (84 (58 (48 err 1) (65 err (83 1
    435))) (96 (91 1 (95 err 1)) (97 err (123 1 err)))) (95 (58 (48 err 1)
    (65 err (91 1 err))) (117 (= 96 err 1) (118 436 (123 1 err)))) (= 41
    437 err) (= 103 438 err) (= 116 439 err) (= 101 440 err) (= 41 441 err)
    (= 101 442 err) err (= 97 443 err) err (= 41 444 err) (95 (58 (48 err
    1) (65 err (91 1 err))) (99 (= 96 err 1) (100 445 (123 1 err)))) (91
    (58 (48 err 1) (65 err 1)) (96 (95 err 446) (97 err (123 1 err)))) (95
    (58 (48 err 1) (65 err (91 1 err))) (101 (= 96 err 1) (102 447 (123 1
    err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97 err (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (99 (= 96 err 1) (100
    448 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (101 (= 96
    err 1) (102 449 (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95
    err 450) (97 err (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err)))
    (110 (= 96 err 1) (111 451 (123 1 err)))) (95 (58 (48 err 1) (65 err
    (91 1 err))) (110 (= 96 err 1) (111 452 (123 1 err)))) (91 (58 (48 err
    1) (65 err 1)) (96 (95 err 1) (97 err (123 1 err)))) (91 (58 (48 err 1)
    (65 err 1)) (96 (95 err 1) (97 err (123 1 err)))) (95 (58 (48 err 1)
    (65 err (91 1 err))) (101 (= 96 err 1) (102 453 (123 1 err)))) (95 (58
    (48 err 1) (65 err (91 1 err))) (99 (= 96 err 1) (100 454 (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (97 (96 1 err) (98 455
    (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (97 (96 1 err)
    (98 456 (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1)
    (97 err (123 1 err)))) (34 (= 10 err 416) (91 (35 458 416) (92 457
    416))) (35 (11 (10 417 err) (34 417 461)) (40 (39 417 459) (= 91 460
    417))) (92 (34 (= 10 err 373) (35 419 (91 373 418))) (119 (= 100 463
    373) (125 (120 462 373) (126 416 373)))) (35 (11 (10 373 err) (34 373
    419)) (92 (91 373 418) (= 125 416 373))) (39 (11 (10 374 err) (= 34 422
    374)) (92 (40 420 (91 374 421)) (= 125 417 374))) (91 (34 (= 10 err
    374) (39 (35 422 374) (40 420 374))) (119 (100 (92 421 374) (101 465
    374)) (125 (120 464 374) (126 417 374)))) (39 (11 (10 374 err) (= 34
    422 374)) (92 (40 420 (91 374 421)) (= 125 417 374))) (34 (= 10 err
    319) (91 (35 376 319) (92 375 319))) (35 (11 (10 319 err) (34 319 376))
    (92 (91 319 375) (= 93 466 319))) (39 (11 (10 320 err) (= 34 379 320))
    (92 (40 377 (91 320 378)) (= 93 466 320))) (35 (11 (10 320 err) (34 320
    379)) (40 (39 320 377) (= 91 378 320))) (= 32 467 err) (= 116 468 err)
    (81 (58 (48 err 1) (65 err (80 1 469))) (96 (91 1 (95 err 1)) (97 err
    (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 470) (97 err
    (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (99 (= 96 err 1)
    (100 471 (123 1 err)))) (74 (58 (48 err 1) (65 err (73 1 472))) (96 (91
    1 (95 err 1)) (97 err (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96
    (95 err 473) (97 err (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96
    (95 err 1) (97 err (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96
    (95 err 474) (97 err (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1
    err))) (101 (= 96 err 1) (102 475 (123 1 err)))) err (= 41 476 err) (=
    41 477 err) (= 114 478 err) err (= 41 479 err) (= 110 480 err) err (95
    (58 (48 err 1) (65 err (91 1 err))) (116 (= 96 err 1) (117 481 (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (111 (= 96 err 1) (112
    482 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (100 (= 96
    err 1) (101 483 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err)))
    (101 (= 96 err 1) (102 484 (123 1 err)))) (95 (58 (48 err 1) (65 err
    (91 1 err))) (111 (= 96 err 1) (112 485 (123 1 err)))) (95 (58 (48 err
    1) (65 err (91 1 err))) (111 (= 96 err 1) (112 486 (123 1 err)))) (95
    (58 (48 err 1) (65 err (91 1 err))) (116 (= 96 err 1) (117 487 (123 1
    err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97 err (123 1
    err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97 err (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (104 (= 96 err 1) (105
    488 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (99 (= 96
    err 1) (100 489 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err)))
    (114 (= 96 err 1) (115 490 (123 1 err)))) (91 (11 (10 416 err) (= 34
    458 416)) (101 (92 457 (100 416 492)) (= 119 491 416))) (34 (= 10 err
    416) (91 (35 458 416) (92 457 416))) (35 (11 (10 417 err) (34 417 461))
    (40 (39 417 459) (= 91 460 417))) (40 (34 (= 10 err 417) (35 461 (39
    417 459))) (100 (= 91 460 417) (119 (101 494 417) (120 493 417)))) (35
    (11 (10 417 err) (34 417 461)) (40 (39 417 459) (= 91 460 417))) (35
    (11 (10 373 err) (34 373 419)) (92 (91 373 418) (= 125 416 373))) (91
    (11 (10 373 err) (= 34 419 373)) (94 (92 418 (93 373 495)) (= 125 416
    373))) (40 (34 (= 10 err 374) (35 422 (39 374 420))) (93 (= 91 421 374)
    (125 (94 495 374) (126 417 374)))) (39 (11 (10 374 err) (= 34 422 374))
    (92 (40 420 (91 374 421)) (= 125 417 374))) (35 (11 (10 466 err) (34
    466 498)) (40 (39 466 496) (= 91 497 466))) (= 108 499 err) (= 62 119
    err) (91 (58 (48 err 1) (65 err (66 500 1))) (96 (95 err 1) (97 err
    (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97 err
    (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (111 (= 96 err
    1) (112 501 (123 1 err)))) (80 (58 (48 err 1) (65 err (79 1 502))) (96
    (91 1 (95 err 1)) (97 err (123 1 err)))) (91 (58 (48 err 1) (65 err 1))
    (96 (95 err 1) (97 err (123 1 err)))) (91 (58 (48 err 1) (65 err 1))
    (96 (95 err 503) (97 err (123 1 err)))) (91 (58 (48 err 1) (65 err 1))
    (96 (95 err 1) (97 err (123 1 err)))) err err (= 41 504 err) err (= 41
    398 err) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97 err (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (110 (= 96 err 1) (111
    505 (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97
    err (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97
    err (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (102 (= 96
    err 1) (103 506 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err)))
    (110 (= 96 err 1) (111 507 (123 1 err)))) (95 (58 (48 err 1) (65 err
    (91 1 err))) (115 (= 96 err 1) (116 508 (123 1 err)))) (91 (58 (48 err
    1) (65 err 1)) (96 (95 err 1) (97 err (123 1 err)))) (95 (58 (48 err 1)
    (65 err (91 1 err))) (104 (= 96 err 1) (105 509 (123 1 err)))) (95 (58
    (48 err 1) (65 err (91 1 err))) (101 (= 96 err 1) (102 510 (123 1
    err)))) (34 (= 10 err 416) (91 (35 458 416) (92 457 416))) (35 (11 (10
    416 err) (34 416 458)) (92 (91 416 457) (= 93 511 416))) (39 (11 (10
    417 err) (= 34 461 417)) (92 (40 459 (91 417 460)) (= 93 511 417))) (35
    (11 (10 417 err) (34 417 461)) (40 (39 417 459) (= 91 460 417))) (39
    (11 (10 495 err) (= 34 514 495)) (92 (40 512 (91 495 513)) (= 125 511
    495))) (35 (11 (10 466 err) (34 466 498)) (40 (39 466 496) (= 91 497
    466))) (40 (34 (= 10 err 466) (35 498 (39 466 496))) (100 (= 91 497
    466) (119 (101 516 466) (120 515 466)))) (35 (11 (10 466 err) (34 466
    498)) (40 (39 466 496) (= 91 497 466))) (= 97 517 err) (68 (58 (48 err
    1) (65 err (67 1 518))) (96 (91 1 (95 err 1)) (97 err (123 1 err))))
    (95 (58 (48 err 1) (65 err (91 1 err))) (109 (= 96 err 1) (110 519 (123
    1 err)))) (79 (58 (48 err 1) (65 err (78 1 520))) (96 (91 1 (95 err 1))
    (97 err (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1)
    (97 err (123 1 err)))) err (95 (58 (48 err 1) (65 err (91 1 err))) (99
    (= 96 err 1) (100 521 (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96
    (95 err 1) (97 err (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1
    err))) (99 (= 96 err 1) (100 522 (123 1 err)))) (91 (58 (48 err 1) (65
    err 1)) (96 (95 err 1) (97 err (123 1 err)))) (91 (58 (48 err 1) (65
    err 1)) (96 (95 err 1) (97 err (123 1 err)))) (91 (58 (48 err 1) (65
    err 1)) (96 (95 err 1) (97 err (123 1 err)))) (35 (11 (10 511 err) (34
    511 525)) (40 (39 511 523) (= 91 524 511))) (39 (11 (10 495 err) (= 34
    514 495)) (92 (40 512 (91 495 513)) (= 125 511 495))) (91 (34 (= 10 err
    495) (39 (35 514 495) (40 512 495))) (119 (100 (92 513 495) (101 527
    495)) (125 (120 526 495) (126 511 495)))) (39 (11 (10 495 err) (= 34
    514 495)) (92 (40 512 (91 495 513)) (= 125 511 495))) (35 (11 (10 466
    err) (34 466 498)) (40 (39 466 496) (= 91 497 466))) (35 (11 (10 466
    err) (34 466 498)) (40 (39 466 496) (= 91 497 466))) (= 110 528 err)
    (70 (58 (48 err 1) (65 err (69 1 529))) (96 (91 1 (95 err 1)) (97 err
    (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (112 (= 96 err
    1) (113 530 (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err
    531) (97 err (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err)))
    (101 (= 96 err 1) (102 532 (123 1 err)))) (95 (58 (48 err 1) (65 err
    (91 1 err))) (101 (= 96 err 1) (102 533 (123 1 err)))) (35 (11 (10 511
    err) (34 511 525)) (40 (39 511 523) (= 91 524 511))) (40 (34 (= 10 err
    511) (35 525 (39 511 523))) (100 (= 91 524 511) (119 (101 535 511) (120
    534 511)))) (35 (11 (10 511 err) (34 511 525)) (40 (39 511 523) (= 91
    524 511))) (39 (11 (10 495 err) (= 34 514 495)) (92 (40 512 (91 495
    513)) (= 125 511 495))) (39 (11 (10 495 err) (= 34 514 495)) (92 (40
    512 (91 495 513)) (= 125 511 495))) (= 103 536 err) (91 (58 (48 err 1)
    (65 err 1)) (96 (95 err 537) (97 err (123 1 err)))) (95 (58 (48 err 1)
    (65 err (91 1 err))) (105 (= 96 err 1) (106 538 (123 1 err)))) (91 (58
    (48 err 1) (65 err 1)) (96 (95 err 539) (97 err (123 1 err)))) (91 (58
    (48 err 1) (65 err 1)) (96 (95 err 1) (97 err (123 1 err)))) (91 (58
    (48 err 1) (65 err 1)) (96 (95 err 1) (97 err (123 1 err)))) (35 (11
    (10 511 err) (34 511 525)) (40 (39 511 523) (= 91 524 511))) (35 (11
    (10 511 err) (34 511 525)) (40 (39 511 523) (= 91 524 511))) (= 117 540
    err) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 541) (97 err (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (108 (= 96 err 1) (109
    542 (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97
    err (123 1 err)))) (= 97 543 err) (91 (58 (48 err 1) (65 err 1)) (96
    (95 err 1) (97 err (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1
    err))) (101 (= 96 err 1) (102 544 (123 1 err)))) (= 103 545 err) (95
    (58 (48 err 1) (65 err (91 1 err))) (114 (= 96 err 1) (115 546 (123 1
    err)))) (= 101 547 err) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1)
    (97 err (123 1 err)))) (= 61 548 err) (= 34 549 err) (= 112 550 err) (=
    104 551 err) (= 112 552 err) (= 34 553 err) (= 62 323 err))
   '#((#f . #f) (139 . 139) (139 . 139) (128 . 128) (127 . 127) (126 . 126)
    (125 . 125) (124 . 124) (122 . 122) (#f . #f) (118 . 118) (117 . 117)
    (116 . 116) (139 . 139) (139 . 139) (139 . 139) (139 . 139) (139 . 139)
    (139 . 139) (139 . 139) (139 . 139) (129 . 129) (139 . 139) (139 . 139)
    (76 . 76) (76 . 76) (139 . 139) (#f . #f) (136 . 136) (130 . 130) (139
    . 139) (139 . 139) (139 . 139) (139 . 139) (#f . #f) (133 . 133) (#f .
    #f) (139 . 139) (131 . 131) (123 . 123) (20 . 20) (19 . 19) (134 . 134)
    (132 . 132) (15 . 15) (137 . 137) (#f . #f) (135 . 135) (139 . 139)
    (139 . 139) (139 . 139) (#f . #f) (119 . 119) (121 . 121) (#f . #f)
    (139 . 139) (139 . 139) (120 . 120) (139 . 139) (139 . 139) (139 . 139)
    (139 . 139) (139 . 139) (139 . 139) (139 . 139) (139 . 139) (139 . 139)
    (139 . 139) (139 . 139) (139 . 139) (139 . 139) (81 . 81) (139 . 139)
    (78 . 78) (139 . 139) (71 . 71) (100 . 100) (69 . 69) (90 . 90) (60 .
    60) (139 . 139) (139 . 139) (139 . 139) (58 . 58) (139 . 139) (139 .
    139) (139 . 139) (139 . 139) (139 . 139) (139 . 139) (139 . 139) (139 .
    139) (139 . 139) (139 . 139) (139 . 139) (35 . 35) (68 . 68) (31 . 31)
    (114 . 114) (30 . 30) (29 . 29) (#f . #f) (139 . 139) (86 . 86) (23 .
    23) (22 . 22) (#f . #f) (#f . #f) (17 . 17) (26 . 26) (16 . 16) (98 .
    98) (#f . #f) (87 . 87) (87 . 87) (73 . 73) (71 . 71) (#f . #f) (80 .
    80) (14 . 14) (139 . 139) (139 . 139) (139 . 139) (139 . 139) (139 .
    139) (89 . 89) (6 . 6) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (#f .
    #f) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (5 . 5) (1 . 1)
    (139 . 139) (139 . 139) (139 . 139) (139 . 139) (139 . 139) (113 . 113)
    (112 . 112) (139 . 139) (109 . 109) (139 . 139) (139 . 139) (139 . 139)
    (139 . 139) (139 . 139) (139 . 139) (139 . 139) (139 . 139) (139 . 139)
    (83 . 83) (79 . 79) (139 . 139) (72 . 72) (101 . 101) (139 . 139) (139
    . 139) (139 . 139) (139 . 139) (139 . 139) (139 . 139) (139 . 139) (139
    . 139) (51 . 51) (139 . 139) (139 . 139) (139 . 139) (139 . 139) (139 .
    139) (139 . 139) (139 . 139) (70 . 70) (27 . 27) (139 . 139) (#f . #f)
    (#f . #f) (#f . #f) (40 . 40) (#f . #f) (102 . 102) (99 . 99) (#f . #f)
    (88 . 88) (#f . #f) (#f . #f) (139 . 139) (139 . 139) (139 . 139) (139
    . 139) (139 . 139) (139 . 139) (139 . 139) (139 . 139) (139 . 139) (139
    . 139) (139 . 139) (139 . 139) (#f . #f) (#f . #f) (#f . #f) (#f . #f)
    (#f . #f) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (77 . 77)
    (139 . 139) (139 . 139) (138 . 138) (139 . 139) (139 . 139) (139 . 139)
    (139 . 139) (139 . 139) (139 . 139) (139 . 139) (139 . 139) (139 . 139)
    (139 . 139) (139 . 139) (75 . 75) (139 . 139) (139 . 139) (139 . 139)
    (139 . 139) (139 . 139) (56 . 56) (139 . 139) (139 . 139) (139 . 139)
    (139 . 139) (139 . 139) (47 . 47) (139 . 139) (139 . 139) (139 . 139)
    (139 . 139) (139 . 139) (139 . 139) (37 . 37) (36 . 36) (139 . 139) (#f
    . #f) (#f . #f) (40 . 40) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (#f .
    #f) (#f . #f) (139 . 139) (139 . 139) (139 . 139) (139 . 139) (139 .
    139) (139 . 139) (139 . 139) (139 . 139) (139 . 139) (139 . 139) (139 .
    139) (139 . 139) (9 . 9) (139 . 139) (4 . 4) (#f . #f) (#f . #f) (#f .
    #f) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (139 .
    139) (139 . 139) (115 . 115) (110 . 110) (108 . 108) (139 . 139) (139 .
    139) (139 . 139) (139 . 139) (139 . 139) (139 . 139) (139 . 139) (91 .
    91) (67 . 67) (139 . 139) (139 . 139) (139 . 139) (139 . 139) (139 .
    139) (139 . 139) (139 . 139) (50 . 50) (139 . 139) (139 . 139) (139 .
    139) (44 . 44) (139 . 139) (139 . 139) (39 . 39) (139 . 139) (139 .
    139) (#f . #f) (40 . 40) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (28 .
    28) (#f . #f) (87 . 87) (#f . #f) (139 . 139) (139 . 139) (139 . 139)
    (139 . 139) (139 . 139) (139 . 139) (139 . 139) (139 . 139) (18 . 18)
    (13 . 13) (11 . 11) (10 . 10) (8 . 8) (#f . #f) (#f . #f) (#f . #f) (#f
    . #f) (64 . 64) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (2 .
    2) (139 . 139) (107 . 107) (103 . 103) (97 . 97) (139 . 139) (93 . 93)
    (139 . 139) (139 . 139) (139 . 139) (139 . 139) (139 . 139) (139 . 139)
    (55 . 55) (139 . 139) (139 . 139) (139 . 139) (139 . 139) (139 . 139)
    (42 . 42) (139 . 139) (38 . 38) (139 . 139) (#f . #f) (#f . #f) (#f .
    #f) (#f . #f) (#f . #f) (40 . 40) (84 . 84) (#f . #f) (40 . 40) (#f .
    #f) (#f . #f) (139 . 139) (139 . 139) (139 . 139) (139 . 139) (139 .
    139) (139 . 139) (139 . 139) (139 . 139) (#f . #f) (#f . #f) (#f . #f)
    (#f . #f) (#f . #f) (#f . #f) (32 . 32) (#f . #f) (7 . 7) (#f . #f)
    (139 . 139) (95 . 95) (139 . 139) (92 . 92) (139 . 139) (139 . 139) (61
    . 61) (139 . 139) (139 . 139) (52 . 52) (48 . 48) (139 . 139) (139 .
    139) (139 . 139) (139 . 139) (24 . 24) (#f . #f) (#f . #f) (#f . #f)
    (40 . 40) (84 . 84) (#f . #f) (40 . 40) (#f . #f) (#f . #f) (#f . #f)
    (#f . #f) (#f . #f) (#f . #f) (139 . 139) (139 . 139) (139 . 139) (139
    . 139) (139 . 139) (25 . 25) (139 . 139) (139 . 139) (111 . 111) (#f .
    #f) (#f . #f) (#f . #f) (34 . 34) (#f . #f) (#f . #f) (3 . 3) (139 .
    139) (139 . 139) (139 . 139) (139 . 139) (139 . 139) (139 . 139) (139 .
    139) (53 . 53) (46 . 46) (139 . 139) (139 . 139) (139 . 139) (#f . #f)
    (40 . 40) (84 . 84) (#f . #f) (40 . 40) (#f . #f) (#f . #f) (#f . #f)
    (#f . #f) (#f . #f) (#f . #f) (#f . #f) (139 . 139) (74 . 74) (139 .
    139) (139 . 139) (49 . 49) (139 . 139) (21 . 21) (105 . 105) (85 . 85)
    (#f . #f) (33 . 33) (#f . #f) (0 . 0) (139 . 139) (94 . 94) (66 . 66)
    (139 . 139) (139 . 139) (139 . 139) (45 . 45) (139 . 139) (139 . 139)
    (#f . #f) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (84 . 84) (#f . #f)
    (40 . 40) (#f . #f) (139 . 139) (139 . 139) (139 . 139) (12 . 12) (65 .
    65) (139 . 139) (63 . 63) (139 . 139) (59 . 59) (43 . 43) (41 . 41) (#f
    . #f) (84 . 84) (#f . #f) (40 . 40) (#f . #f) (#f . #f) (#f . #f) (139
    . 139) (139 . 139) (139 . 139) (139 . 139) (139 . 139) (84 . 84) (#f .
    #f) (40 . 40) (#f . #f) (#f . #f) (#f . #f) (139 . 139) (139 . 139)
    (139 . 139) (96 . 96) (62 . 62) (#f . #f) (#f . #f) (#f . #f) (139 .
    139) (139 . 139) (54 . 54) (#f . #f) (82 . 82) (139 . 139) (#f . #f)
    (139 . 139) (#f . #f) (57 . 57) (#f . #f) (#f . #f) (#f . #f) (#f . #f)
    (#f . #f) (#f . #f) (#f . #f))))

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
