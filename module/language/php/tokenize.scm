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
                 (make-token 'true yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                 (make-token 'false yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                                (make-token 'word yytext)
        )))
   'decision-trees
   0
   0
   '#((84 (44 (35 (13 (10 (9 err 14) (11 13 err)) (32 (14 12 err) (33 54
    (34 29 43)))) (39 (37 (36 46 38) (38 48 56)) (41 (40 42 55) (42 10 (43
    23 31))))) (61 (48 (46 (45 6 40) (47 44 45)) (58 (49 27 26) (59 36 (60
    5 47)))) (65 (63 (62 37 30) (64 49 err)) (71 (70 1 2) (= 78 4 1)))))
    (108 (98 (93 (91 (85 3 1) (92 8 err)) (95 (94 7 11) (96 50 (97 err
    57)))) (102 (100 (99 52 51) (101 39 35)) (104 (103 34 33) (= 105 32
    1)))) (117 (112 (110 (109 28 1) (111 22 25)) (114 (113 21 1) (115 20
    (116 19 18)))) (121 (119 (118 17 16) (120 15 24)) (124 (123 1 41) (125
    53 (126 9 err))))))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97
    err (123 1 err)))) (91 (58 (48 err 1) (65 err (66 58 1))) (96 (95 err
    1) (97 err (123 1 err)))) (83 (58 (48 err 1) (65 err (82 1 59))) (96
    (91 1 (95 err 1)) (97 err (123 1 err)))) (86 (58 (48 err 1) (65 err (85
    1 60))) (96 (91 1 (95 err 1)) (97 err (123 1 err)))) err err err err
    err err (= 61 61 err) (= 13 12 err) (= 10 13 err) (= 9 14 err) (95 (58
    (48 err 1) (65 err (91 1 err))) (104 (= 96 err 1) (105 62 (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (97 (96 1 err) (98 63
    (123 1 err)))) (96 (65 (48 err (58 1 err)) (91 1 (95 err 1))) (111 (97
    err (110 1 65)) (116 (115 1 64) (123 1 err)))) (96 (65 (48 err (58 1
    err)) (91 1 (95 err 1))) (105 (97 err (104 1 67)) (115 (114 1 66) (123
    1 err)))) (96 (65 (48 err (58 1 err)) (91 1 (95 err 1))) (117 (97 err
    (116 1 69)) (120 (119 1 68) (123 1 err)))) (95 (58 (48 err 1) (65 err
    (91 1 err))) (101 (= 96 err 1) (102 70 (123 1 err)))) (96 (65 (48 err
    (58 1 err)) (91 1 (95 err 1))) (115 (97 err (114 1 72)) (118 (117 1 71)
    (123 1 err)))) (96 (65 (48 err (58 1 err)) (91 1 (95 err 1))) (102 (97
    err (101 1 74)) (118 (117 1 73) (123 1 err)))) (= 61 75 err) (95 (58
    (48 err 1) (65 err (91 1 err))) (111 (= 96 err 1) (112 76 (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (114 (= 96 err 1) (115
    77 (123 1 err)))) (48 err (58 26 err)) err (95 (58 (48 err 1) (65 err
    (91 1 err))) (105 (= 96 err 1) (106 78 (123 1 err)))) (= 61 79 err) (62
    (61 err 81) (63 80 err)) (44 (43 err 83) (= 61 82 err)) (102 (91 (58
    (48 err 1) (65 err 1)) (96 (95 err 1) (97 err 1))) (111 (109 (103 87 1)
    (110 86 85)) (116 (115 1 84) (123 1 err)))) (96 (65 (48 err (58 1 err))
    (91 1 (95 err 1))) (109 (97 err (108 1 89)) (112 (111 1 88) (123 1
    err)))) (98 (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97 err 90)))
    (112 (106 (105 1 93) (111 1 92)) (118 (117 1 91) (123 1 err)))) (100
    (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97 err (99 1 99)))) (118
    (109 (108 1 98) (110 97 (111 96 1))) (120 (119 95 1) (121 94 (123 1
    err))))) (= 58 100 err) (62 (61 err 101) (63 102 err)) (97 (65 err (91
    103 err)) (123 103 (124 104 err))) (95 (48 (= 46 106 err) (65 (58 1
    err) (91 1 err))) (102 (97 (96 1 err) (101 1 107)) (112 (111 1 105)
    (123 1 err)))) (46 (45 err 109) (= 62 108 err)) (= 36 110 err) err (11
    (10 111 err) (= 36 112 111)) (= 61 113 err) (47 (= 42 115 err) (61 (48
    46 err) (62 114 err))) err (61 (47 (= 37 118 err) (48 122 (60 err
    116))) (64 (62 120 (63 121 119)) (= 115 117 err))) (62 (61 err 123) (63
    124 err)) (= 62 124 err) (91 (58 (48 err 1) (65 err 1)) (96 (95 err
    125) (97 err (123 1 err)))) (97 (65 (48 err (58 1 err)) (95 (91 1 err)
    (96 1 err))) (109 (98 128 (108 1 127)) (112 (111 1 126) (123 1 err))))
    (95 (58 (48 err 1) (65 err (91 1 err))) (114 (= 96 err 1) (115 129 (123
    1 err)))) (62 (61 err 130) (= 124 131 err)) (= 97 132 err) (105 (100
    (98 (97 err 141) (99 140 err)) (102 (101 138 err) (103 137 err))) (114
    (111 (106 136 err) (112 135 err)) (116 (115 139 134) (= 117 133 err))))
    (39 (38 err 142) (= 61 143 err)) (97 (65 (48 err (58 1 err)) (95 (91 1
    err) (96 1 err))) (111 (99 (98 1 146) (110 1 144)) (115 (114 1 145)
    (123 1 err)))) (77 (58 (48 err 1) (65 err (76 1 147))) (96 (91 1 (95
    err 1)) (97 err (123 1 err)))) (86 (58 (48 err 1) (65 err (85 1 148)))
    (96 (91 1 (95 err 1)) (97 err (123 1 err)))) (77 (58 (48 err 1) (65 err
    (76 1 149))) (96 (91 1 (95 err 1)) (97 err (123 1 err)))) err (95 (58
    (48 err 1) (65 err (91 1 err))) (105 (= 96 err 1) (106 150 (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (114 (= 96 err 1) (115
    151 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (101 (= 96
    err 1) (102 152 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err)))
    (115 (= 96 err 1) (116 153 (123 1 err)))) (96 (65 (48 err (58 1 err))
    (91 1 (95 err 1))) (118 (97 err (117 1 154)) (122 (121 1 155) (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (114 (= 96 err 1) (115
    156 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (105 (= 96
    err 1) (106 157 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err)))
    (97 (96 1 err) (98 158 (123 1 err)))) (96 (65 (48 err (58 1 err)) (91 1
    (95 err 1))) (114 (97 err (113 1 160)) (117 (116 1 159) (123 1 err))))
    (95 (58 (48 err 1) (65 err (91 1 err))) (98 (= 96 err 1) (99 161 (123 1
    err)))) (96 (65 (48 err (58 1 err)) (91 1 (95 err 1))) (106 (97 err
    (105 1 163)) (112 (111 1 162) (123 1 err)))) (95 (58 (48 err 1) (65 err
    (91 1 err))) (108 (= 96 err 1) (109 164 (123 1 err)))) (95 (58 (48 err
    1) (65 err (91 1 err))) (119 (= 96 err 1) (120 165 (123 1 err)))) err
    (95 (58 (48 err 1) (65 err (91 1 err))) (114 (= 96 err 1) (115 166 (123
    1 err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97 err (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (115 (= 96 err 1) (116
    167 (123 1 err)))) (= 61 168 err) (= 61 169 err) err err err (95 (58
    (48 err 1) (65 err (91 1 err))) (115 (= 96 err 1) (116 170 (123 1
    err)))) (97 (65 (48 err (58 1 err)) (95 (91 1 err) (96 1 err))) (115 (=
    99 173 1) (117 (116 172 171) (123 1 err)))) (95 (58 (48 err 1) (65 err
    (91 1 err))) (112 (= 96 err 1) (113 174 (123 1 err)))) (91 (58 (48 err
    1) (65 err 1)) (96 (95 err 1) (97 err (123 1 err)))) (95 (58 (48 err 1)
    (65 err (91 1 err))) (116 (= 96 err 1) (117 175 (123 1 err)))) (95 (58
    (48 err 1) (65 err (91 1 err))) (111 (= 96 err 1) (112 176 (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (108 (= 96 err 1) (109
    177 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (110 (= 96
    err 1) (111 178 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err)))
    (114 (= 96 err 1) (115 179 (123 1 err)))) (95 (58 (48 err 1) (65 err
    (91 1 err))) (110 (= 96 err 1) (111 180 (123 1 err)))) (95 (58 (48 err
    1) (65 err (91 1 err))) (116 (= 96 err 1) (117 181 (123 1 err)))) (95
    (58 (48 err 1) (65 err (91 1 err))) (97 (96 1 err) (98 182 (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (100 (= 96 err 1) (101
    183 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (112 (= 96
    err 1) (113 184 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err)))
    (115 (= 96 err 1) (116 185 (123 1 err)))) (95 (58 (48 err 1) (65 err
    (91 1 err))) (104 (= 96 err 1) (105 186 (123 1 err)))) err (= 61 187
    err) err (91 (65 err 103) (97 err (123 103 err))) err (91 (58 (48 err
    1) (65 err 1)) (96 (95 err 1) (97 err (123 1 err)))) (= 100 188 err)
    (95 (58 (48 err 1) (65 err (91 1 err))) (99 (= 96 err 1) (100 189 (123
    1 err)))) err err err (= 36 190 err) (35 (11 (10 190 err) (34 190 193))
    (92 (91 190 192) (= 123 191 190))) err err (= 42 194 err) (61 (60 err
    195) (62 196 err)) (= 99 197 err) (= 61 198 err) (62 (61 err 198) (=
    112 199 err)) err err (= 115 200 err) err err (77 (68 (58 (48 err 1)
    (65 err (67 1 206))) (70 (69 205 1) (71 204 (76 1 202)))) (96 (79 (78 1
    201) (91 1 (95 err 1))) (104 (97 err 1) (105 203 (123 1 err))))) (95
    (58 (48 err 1) (65 err (91 1 err))) (110 (= 96 err 1) (111 207 (123 1
    err)))) (96 (65 (48 err (58 1 err)) (91 1 (95 err 1))) (111 (97 err (98
    209 1)) (112 208 (123 1 err)))) (96 (65 (48 err (58 1 err)) (91 1 (95
    err 1))) (116 (97 err (115 1 211)) (117 210 (123 1 err)))) (95 (58 (48
    err 1) (65 err (91 1 err))) (101 (= 96 err 1) (102 212 (123 1 err))))
    err err (= 115 213 err) (= 110 214 err) (= 116 215 err) (= 98 216 err)
    (= 110 217 err) (= 108 218 err) (= 111 219 err) (= 101 220 err) (= 111
    221 err) (= 114 222 err) err err (95 (58 (48 err 1) (65 err (91 1
    err))) (100 (= 96 err 1) (101 223 (123 1 err)))) (95 (58 (48 err 1) (65
    err (91 1 err))) (114 (= 96 err 1) (115 224 (123 1 err)))) (95 (58 (48
    err 1) (65 err (91 1 err))) (115 (= 96 err 1) (116 225 (123 1 err))))
    (84 (58 (48 err 1) (65 err (83 1 226))) (96 (91 1 (95 err 1)) (97 err
    (123 1 err)))) (70 (58 (48 err 1) (65 err (69 1 227))) (96 (91 1 (95
    err 1)) (97 err (123 1 err)))) (77 (58 (48 err 1) (65 err (76 1 228)))
    (96 (91 1 (95 err 1)) (97 err (123 1 err)))) (95 (58 (48 err 1) (65 err
    (91 1 err))) (108 (= 96 err 1) (109 229 (123 1 err)))) (91 (58 (48 err
    1) (65 err 1)) (96 (95 err 1) (97 err (123 1 err)))) (91 (58 (48 err 1)
    (65 err 1)) (96 (95 err 1) (97 err (123 1 err)))) (95 (58 (48 err 1)
    (65 err (91 1 err))) (101 (= 96 err 1) (102 230 (123 1 err)))) (95 (58
    (48 err 1) (65 err (91 1 err))) (101 (= 96 err 1) (102 227 (123 1
    err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97 err (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (111 (= 96 err 1) (112
    231 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (116 (= 96
    err 1) (117 232 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err)))
    (116 (= 96 err 1) (117 233 (123 1 err)))) (95 (58 (48 err 1) (65 err
    (91 1 err))) (117 (= 96 err 1) (118 234 (123 1 err)))) (95 (58 (48 err
    1) (65 err (91 1 err))) (117 (= 96 err 1) (118 235 (123 1 err)))) (95
    (58 (48 err 1) (65 err (91 1 err))) (108 (= 96 err 1) (109 236 (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (116 (= 96 err 1) (117
    237 (123 1 err)))) (96 (65 (48 err (58 1 err)) (91 1 (95 err 1))) (111
    (97 err (110 1 239)) (119 (118 1 238) (123 1 err)))) (95 (58 (48 err 1)
    (65 err (91 1 err))) (108 (= 96 err 1) (109 228 (123 1 err)))) (91 (58
    (48 err 1) (65 err 1)) (96 (95 err 1) (97 err (123 1 err)))) (91 (58
    (48 err 1) (65 err 1)) (96 (95 err 1) (97 err (123 1 err)))) (95 (58
    (48 err 1) (65 err (91 1 err))) (116 (= 96 err 1) (117 240 (123 1
    err)))) err err (95 (58 (48 err 1) (65 err (91 1 err))) (101 (= 96 err
    1) (102 241 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (101
    (= 96 err 1) (102 242 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1
    err))) (116 (= 96 err 1) (117 243 (123 1 err)))) (95 (58 (48 err 1) (65
    err (91 1 err))) (108 (= 96 err 1) (109 244 (123 1 err)))) (95 (58 (48
    err 1) (65 err (91 1 err))) (108 (= 96 err 1) (109 245 (123 1 err))))
    (95 (58 (48 err 1) (65 err (91 1 err))) (111 (= 96 err 1) (112 246 (123
    1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (98 (= 96 err 1) (99
    247 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (115 (= 96
    err 1) (116 248 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err)))
    (99 (= 96 err 1) (100 249 (123 1 err)))) (95 (58 (48 err 1) (65 err (91
    1 err))) (101 (= 96 err 1) (102 250 (123 1 err)))) (95 (58 (48 err 1)
    (65 err (91 1 err))) (97 (96 1 err) (98 251 (123 1 err)))) (95 (58 (48
    err 1) (65 err (91 1 err))) (101 (= 96 err 1) (102 252 (123 1 err))))
    (95 (58 (48 err 1) (65 err (91 1 err))) (108 (= 96 err 1) (109 253 (123
    1 err)))) (101 (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97 err
    (100 1 258)))) (115 (103 (102 1 257) (= 105 256 1)) (119 (116 255 1)
    (120 254 (123 1 err))))) (95 (58 (48 err 1) (65 err (91 1 err))) (116
    (= 96 err 1) (117 259 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1
    err))) (101 (= 96 err 1) (102 260 (123 1 err)))) (95 (58 (48 err 1) (65
    err (91 1 err))) (111 (= 96 err 1) (112 261 (123 1 err)))) err err (95
    (58 (48 err 1) (65 err (91 1 err))) (108 (= 96 err 1) (109 262 (123 1
    err)))) (34 (= 10 err 190) (91 (35 193 190) (92 192 190))) (35 (11 (10
    191 err) (34 191 265)) (92 (91 191 264) (= 125 263 191))) (91 (11 (10
    190 err) (= 34 193 190)) (101 (92 192 (100 190 267)) (= 119 266 190)))
    (34 (= 10 err 190) (91 (35 193 190) (92 192 190))) (11 (10 194 err) (=
    42 268 194)) err err (= 114 269 err) err (= 104 270 err) (= 99 271 err)
    (91 (58 (48 err 1) (65 err (66 272 1))) (96 (95 err 1) (97 err (123 1
    err)))) (74 (58 (48 err 1) (65 err (73 1 273))) (96 (91 1 (95 err 1))
    (97 err (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (97 (96
    1 err) (98 274 (123 1 err)))) (85 (65 (48 err (58 1 err)) (= 73 276 1))
    (95 (86 275 (91 1 err)) (97 (96 1 err) (123 1 err)))) (74 (58 (48 err
    1) (65 err (73 1 277))) (96 (91 1 (95 err 1)) (97 err (123 1 err))))
    (77 (58 (48 err 1) (65 err (76 1 278))) (96 (91 1 (95 err 1)) (97 err
    (123 1 err)))) (96 (65 (48 err (58 1 err)) (91 1 (95 err 1))) (116 (97
    err (115 1 280)) (117 279 (123 1 err)))) (95 (58 (48 err 1) (65 err (91
    1 err))) (110 (= 96 err 1) (111 281 (123 1 err)))) (95 (58 (48 err 1)
    (65 err (91 1 err))) (115 (= 96 err 1) (116 282 (123 1 err)))) (95 (58
    (48 err 1) (65 err (91 1 err))) (99 (= 96 err 1) (100 283 (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (101 (= 96 err 1) (102
    284 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (97 (96 1
    err) (98 285 (123 1 err)))) (= 32 286 err) (= 115 287 err) (= 114 288
    err) (= 106 289 err) (= 116 290 err) (= 111 291 err) (= 117 292 err) (=
    97 293 err) (= 111 294 err) (= 114 295 err) (91 (58 (48 err 1) (65 err
    1)) (96 (95 err 1) (97 err (123 1 err)))) (95 (58 (48 err 1) (65 err
    (91 1 err))) (97 (96 1 err) (98 296 (123 1 err)))) (95 (58 (48 err 1)
    (65 err (91 1 err))) (116 (= 96 err 1) (117 297 (123 1 err)))) (70 (58
    (48 err 1) (65 err (69 1 298))) (96 (91 1 (95 err 1)) (97 err (123 1
    err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97 err (123 1
    err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97 err (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (101 (= 96 err 1) (102
    299 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (116 (= 96
    err 1) (117 300 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err)))
    (119 (= 96 err 1) (120 301 (123 1 err)))) (95 (58 (48 err 1) (65 err
    (91 1 err))) (99 (= 96 err 1) (100 302 (123 1 err)))) (95 (58 (48 err
    1) (65 err (91 1 err))) (105 (= 96 err 1) (106 303 (123 1 err)))) (95
    (58 (48 err 1) (65 err (91 1 err))) (114 (= 96 err 1) (115 304 (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (105 (= 96 err 1) (106
    305 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (105 (= 96
    err 1) (106 306 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err)))
    (101 (= 96 err 1) (102 307 (123 1 err)))) (95 (58 (48 err 1) (65 err
    (91 1 err))) (97 (96 1 err) (98 308 (123 1 err)))) (95 (58 (48 err 1)
    (65 err (91 1 err))) (116 (= 96 err 1) (117 309 (123 1 err)))) (91 (58
    (48 err 1) (65 err 1)) (96 (95 err 1) (97 err (123 1 err)))) (95 (58
    (48 err 1) (65 err (91 1 err))) (116 (= 96 err 1) (117 310 (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (114 (= 96 err 1) (115
    311 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (97 (96 1
    err) (98 312 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err)))
    (117 (= 96 err 1) (118 313 (123 1 err)))) (95 (58 (48 err 1) (65 err
    (91 1 err))) (101 (= 96 err 1) (102 314 (123 1 err)))) (91 (58 (48 err
    1) (65 err 1)) (96 (95 err 1) (97 err (123 1 err)))) (95 (58 (48 err 1)
    (65 err (91 1 err))) (97 (96 1 err) (98 315 (123 1 err)))) (95 (58 (48
    err 1) (65 err (91 1 err))) (101 (= 96 err 1) (102 298 (123 1 err))))
    (95 (58 (48 err 1) (65 err (91 1 err))) (116 (= 96 err 1) (117 316 (123
    1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (97 (96 1 err) (98
    317 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (108 (= 96
    err 1) (109 318 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err)))
    (110 (= 96 err 1) (111 319 (123 1 err)))) (91 (58 (48 err 1) (65 err
    1)) (96 (95 err 1) (97 err (123 1 err)))) (95 (58 (48 err 1) (65 err
    (91 1 err))) (104 (= 96 err 1) (105 320 (123 1 err)))) (95 (58 (48 err
    1) (65 err (91 1 err))) (119 (= 96 err 1) (120 321 (123 1 err)))) (95
    (58 (48 err 1) (65 err (91 1 err))) (102 (= 96 err 1) (103 322 (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (111 (= 96 err 1) (112
    323 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (101 (= 96
    err 1) (102 324 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err)))
    (121 (= 96 err 1) (122 325 (123 1 err)))) (95 (58 (48 err 1) (65 err
    (91 1 err))) (105 (= 96 err 1) (106 326 (123 1 err)))) (91 (58 (48 err
    1) (65 err 1)) (96 (95 err 1) (97 err (123 1 err)))) (95 (58 (48 err 1)
    (65 err (91 1 err))) (97 (96 1 err) (98 327 (123 1 err)))) (34 (= 10
    err 263) (91 (35 329 263) (92 328 263))) (92 (34 (= 10 err 191) (35 265
    (91 191 264))) (119 (= 100 331 191) (125 (120 330 191) (126 263 191))))
    (35 (11 (10 191 err) (34 191 265)) (92 (91 191 264) (= 125 263 191)))
    (35 (11 (10 190 err) (34 190 193)) (92 (91 190 192) (= 93 332 190)))
    (35 (11 (10 190 err) (34 190 193)) (92 (91 190 192) (= 93 333 190)))
    (42 (= 10 err 194) (47 (43 268 194) (48 334 194))) (= 105 335 err) (=
    112 336 err) (= 114 337 err) (78 (58 (48 err 1) (65 err (77 1 338)))
    (96 (91 1 (95 err 1)) (97 err (123 1 err)))) (79 (58 (48 err 1) (65 err
    (78 1 339))) (96 (91 1 (95 err 1)) (97 err (123 1 err)))) (95 (58 (48
    err 1) (65 err (91 1 err))) (108 (= 96 err 1) (109 340 (123 1 err))))
    (79 (58 (48 err 1) (65 err (78 1 341))) (96 (91 1 (95 err 1)) (97 err
    (123 1 err)))) (77 (58 (48 err 1) (65 err (76 1 342))) (96 (91 1 (95
    err 1)) (97 err (123 1 err)))) (83 (58 (48 err 1) (65 err (82 1 343)))
    (96 (91 1 (95 err 1)) (97 err (123 1 err)))) (91 (58 (48 err 1) (65 err
    (66 344 1))) (96 (95 err 1) (97 err (123 1 err)))) (95 (58 (48 err 1)
    (65 err (91 1 err))) (105 (= 96 err 1) (106 345 (123 1 err)))) (95 (58
    (48 err 1) (65 err (91 1 err))) (116 (= 96 err 1) (117 346 (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (101 (= 96 err 1) (102
    347 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (115 (= 96
    err 1) (116 348 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err)))
    (104 (= 96 err 1) (105 349 (123 1 err)))) (91 (58 (48 err 1) (65 err
    1)) (96 (95 err 1) (97 err (123 1 err)))) (95 (58 (48 err 1) (65 err
    (91 1 err))) (107 (= 96 err 1) (108 350 (123 1 err)))) err (= 101 351
    err) (= 105 352 err) (= 101 353 err) (42 (41 err 355) (= 101 354 err))
    (= 97 356 err) (= 98 357 err) (= 108 358 err) (= 108 359 err) (= 97 360
    err) (95 (58 (48 err 1) (65 err (91 1 err))) (121 (= 96 err 1) (122 361
    (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (114 (= 96 err
    1) (115 362 (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err
    1) (97 err (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1)
    (97 err (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1)
    (97 err (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1)
    (97 err (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (104 (=
    96 err 1) (105 363 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1
    err))) (99 (= 96 err 1) (100 364 (123 1 err)))) (95 (58 (48 err 1) (65
    err (91 1 err))) (110 (= 96 err 1) (111 365 (123 1 err)))) (95 (58 (48
    err 1) (65 err (91 1 err))) (114 (= 96 err 1) (115 366 (123 1 err))))
    (95 (58 (48 err 1) (65 err (91 1 err))) (99 (= 96 err 1) (100 367 (123
    1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (99 (= 96 err 1) (100
    368 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (116 (= 96
    err 1) (117 369 (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95
    err 1) (97 err (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95
    err 1) (97 err (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err)))
    (102 (= 96 err 1) (103 370 (123 1 err)))) (95 (58 (48 err 1) (65 err
    (91 1 err))) (110 (= 96 err 1) (111 371 (123 1 err)))) (95 (58 (48 err
    1) (65 err (91 1 err))) (100 (= 96 err 1) (101 372 (123 1 err)))) (95
    (58 (48 err 1) (65 err (91 1 err))) (109 (= 96 err 1) (110 373 (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (108 (= 96 err 1) (109
    374 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (105 (= 96
    err 1) (106 375 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err)))
    (99 (= 96 err 1) (100 376 (123 1 err)))) (91 (58 (48 err 1) (65 err 1))
    (96 (95 err 1) (97 err (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1
    err))) (100 (= 96 err 1) (101 377 (123 1 err)))) (95 (58 (48 err 1) (65
    err (91 1 err))) (105 (= 96 err 1) (106 378 (123 1 err)))) (95 (58 (48
    err 1) (65 err (91 1 err))) (105 (= 96 err 1) (106 379 (123 1 err))))
    (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97 err (123 1 err))))
    (95 (58 (48 err 1) (65 err (91 1 err))) (114 (= 96 err 1) (115 380 (123
    1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (99 (= 96 err 1) (100
    381 (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97
    err (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (102 (= 96
    err 1) (103 382 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err)))
    (114 (= 96 err 1) (115 383 (123 1 err)))) (91 (11 (10 263 err) (= 34
    329 263)) (101 (92 328 (100 263 385)) (= 119 384 263))) (34 (= 10 err
    263) (91 (35 329 263) (92 328 263))) (91 (11 (10 191 err) (= 34 265
    191)) (94 (92 264 (93 191 386)) (= 125 263 191))) (91 (11 (10 191 err)
    (= 34 265 191)) (94 (92 264 (93 191 387)) (= 125 263 191))) (34 (= 10
    err 332) (91 (35 389 332) (92 388 332))) (35 (11 (10 333 err) (34 333
    392)) (40 (39 333 390) (= 91 391 333))) (11 (10 194 err) (= 42 268
    194)) (= 112 393 err) err (= 105 394 err) (70 (58 (48 err 1) (65 err
    (69 1 395))) (96 (91 1 (95 err 1)) (97 err (123 1 err)))) (70 (58 (48
    err 1) (65 err (69 1 396))) (96 (91 1 (95 err 1)) (97 err (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (116 (= 96 err 1) (117
    397 (123 1 err)))) (68 (58 (48 err 1) (65 err (67 1 398))) (96 (91 1
    (95 err 1)) (97 err (123 1 err)))) (70 (58 (48 err 1) (65 err (69 1
    399))) (96 (91 1 (95 err 1)) (97 err (123 1 err)))) (91 (58 (48 err 1)
    (65 err 1)) (96 (95 err 400) (97 err (123 1 err)))) (84 (58 (48 err 1)
    (65 err (83 1 401))) (96 (91 1 (95 err 1)) (97 err (123 1 err)))) (95
    (58 (48 err 1) (65 err (91 1 err))) (110 (= 96 err 1) (111 402 (123 1
    err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97 err (123 1
    err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97 err (123 1
    err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97 err (123 1
    err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97 err (123 1
    err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97 err (123 1
    err)))) (= 116 403 err) (= 110 404 err) (= 99 405 err) (= 103 406 err)
    err (= 116 407 err) (= 108 408 err) (= 41 409 err) (42 (41 err 411) (=
    101 410 err)) (= 121 412 err) (91 (58 (48 err 1) (65 err 1)) (96 (95
    err 1) (97 err (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err)))
    (97 (96 1 err) (98 413 (123 1 err)))) (91 (58 (48 err 1) (65 err 1))
    (96 (95 err 1) (97 err (123 1 err)))) (91 (58 (48 err 1) (65 err 1))
    (96 (95 err 1) (97 err (123 1 err)))) (91 (58 (48 err 1) (65 err 1))
    (96 (95 err 1) (97 err (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1
    err))) (101 (= 96 err 1) (102 414 (123 1 err)))) (91 (58 (48 err 1) (65
    err 1)) (96 (95 err 1) (97 err (123 1 err)))) (95 (58 (48 err 1) (65
    err (91 1 err))) (116 (= 96 err 1) (117 415 (123 1 err)))) (95 (58 (48
    err 1) (65 err (91 1 err))) (101 (= 96 err 1) (102 416 (123 1 err))))
    (95 (58 (48 err 1) (65 err (91 1 err))) (97 (96 1 err) (98 417 (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (99 (= 96 err 1) (100
    418 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (101 (= 96
    err 1) (102 419 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err)))
    (101 (= 96 err 1) (102 420 (123 1 err)))) (91 (58 (48 err 1) (65 err
    1)) (96 (95 err 1) (97 err (123 1 err)))) (95 (58 (48 err 1) (65 err
    (91 1 err))) (111 (= 96 err 1) (112 421 (123 1 err)))) (95 (58 (48 err
    1) (65 err (91 1 err))) (104 (= 96 err 1) (105 422 (123 1 err)))) (95
    (58 (48 err 1) (65 err (91 1 err))) (115 (= 96 err 1) (116 423 (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (108 (= 96 err 1) (109
    424 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (116 (= 96
    err 1) (117 425 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err)))
    (101 (= 96 err 1) (102 426 (123 1 err)))) (95 (58 (48 err 1) (65 err
    (91 1 err))) (108 (= 96 err 1) (109 427 (123 1 err)))) (91 (58 (48 err
    1) (65 err 1)) (96 (95 err 1) (97 err (123 1 err)))) (95 (58 (48 err 1)
    (65 err (91 1 err))) (101 (= 96 err 1) (102 428 (123 1 err)))) (35 (11
    (10 263 err) (34 263 329)) (92 (91 263 328) (= 93 429 263))) (35 (11
    (10 263 err) (34 263 329)) (92 (91 263 328) (= 93 430 263))) (35 (11
    (10 386 err) (34 386 432)) (92 (91 386 431) (= 125 429 386))) (39 (11
    (10 387 err) (= 34 435 387)) (92 (40 433 (91 387 434)) (= 125 430
    387))) (91 (11 (10 332 err) (= 34 389 332)) (101 (92 388 (100 332 437))
    (= 119 436 332))) (34 (= 10 err 332) (91 (35 389 332) (92 388 332)))
    (35 (11 (10 333 err) (34 333 392)) (40 (39 333 390) (= 91 391 333)))
    (40 (34 (= 10 err 333) (35 392 (39 333 390))) (100 (= 91 391 333) (119
    (101 439 333) (120 438 333)))) (35 (11 (10 333 err) (34 333 392)) (40
    (39 333 390) (= 91 391 333))) (= 116 440 err) (= 112 441 err) (84 (58
    (48 err 1) (65 err (83 1 442))) (96 (91 1 (95 err 1)) (97 err (123 1
    err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 443) (97 err (123 1
    err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 444) (97 err (123 1
    err)))) (85 (58 (48 err 1) (65 err (84 1 445))) (96 (91 1 (95 err 1))
    (97 err (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 446)
    (97 err (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 447)
    (97 err (123 1 err)))) (84 (58 (48 err 1) (65 err (83 1 448))) (96 (91
    1 (95 err 1)) (97 err (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1
    err))) (117 (= 96 err 1) (118 449 (123 1 err)))) (= 41 450 err) (= 103
    451 err) (= 116 452 err) (= 101 453 err) (= 41 454 err) (= 101 455 err)
    err (= 97 456 err) err (= 41 457 err) (95 (58 (48 err 1) (65 err (91 1
    err))) (99 (= 96 err 1) (100 458 (123 1 err)))) (91 (58 (48 err 1) (65
    err 1)) (96 (95 err 459) (97 err (123 1 err)))) (95 (58 (48 err 1) (65
    err (91 1 err))) (101 (= 96 err 1) (102 460 (123 1 err)))) (91 (58 (48
    err 1) (65 err 1)) (96 (95 err 1) (97 err (123 1 err)))) (95 (58 (48
    err 1) (65 err (91 1 err))) (99 (= 96 err 1) (100 461 (123 1 err))))
    (95 (58 (48 err 1) (65 err (91 1 err))) (101 (= 96 err 1) (102 462 (123
    1 err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 463) (97 err (123
    1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (110 (= 96 err 1)
    (111 464 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (110 (=
    96 err 1) (111 465 (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96
    (95 err 1) (97 err (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96
    (95 err 1) (97 err (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1
    err))) (101 (= 96 err 1) (102 466 (123 1 err)))) (95 (58 (48 err 1) (65
    err (91 1 err))) (99 (= 96 err 1) (100 467 (123 1 err)))) (95 (58 (48
    err 1) (65 err (91 1 err))) (97 (96 1 err) (98 468 (123 1 err)))) (95
    (58 (48 err 1) (65 err (91 1 err))) (97 (96 1 err) (98 469 (123 1
    err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97 err (123 1
    err)))) (34 (= 10 err 429) (91 (35 471 429) (92 470 429))) (35 (11 (10
    430 err) (34 430 474)) (40 (39 430 472) (= 91 473 430))) (92 (34 (= 10
    err 386) (35 432 (91 386 431))) (119 (= 100 476 386) (125 (120 475 386)
    (126 429 386)))) (35 (11 (10 386 err) (34 386 432)) (92 (91 386 431) (=
    125 429 386))) (39 (11 (10 387 err) (= 34 435 387)) (92 (40 433 (91 387
    434)) (= 125 430 387))) (91 (34 (= 10 err 387) (39 (35 435 387) (40 433
    387))) (119 (100 (92 434 387) (101 478 387)) (125 (120 477 387) (126
    430 387)))) (39 (11 (10 387 err) (= 34 435 387)) (92 (40 433 (91 387
    434)) (= 125 430 387))) (34 (= 10 err 332) (91 (35 389 332) (92 388
    332))) (35 (11 (10 332 err) (34 332 389)) (92 (91 332 388) (= 93 479
    332))) (39 (11 (10 333 err) (= 34 392 333)) (92 (40 390 (91 333 391))
    (= 93 479 333))) (35 (11 (10 333 err) (34 333 392)) (40 (39 333 390) (=
    91 391 333))) (= 32 480 err) (= 116 481 err) (81 (58 (48 err 1) (65 err
    (80 1 482))) (96 (91 1 (95 err 1)) (97 err (123 1 err)))) (91 (58 (48
    err 1) (65 err 1)) (96 (95 err 483) (97 err (123 1 err)))) (95 (58 (48
    err 1) (65 err (91 1 err))) (99 (= 96 err 1) (100 484 (123 1 err))))
    (74 (58 (48 err 1) (65 err (73 1 485))) (96 (91 1 (95 err 1)) (97 err
    (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 486) (97 err
    (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97 err
    (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 487) (97 err
    (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (101 (= 96 err
    1) (102 488 (123 1 err)))) err (= 41 489 err) (= 41 490 err) (= 114 491
    err) err (= 41 492 err) (= 110 493 err) err (95 (58 (48 err 1) (65 err
    (91 1 err))) (116 (= 96 err 1) (117 494 (123 1 err)))) (95 (58 (48 err
    1) (65 err (91 1 err))) (111 (= 96 err 1) (112 495 (123 1 err)))) (95
    (58 (48 err 1) (65 err (91 1 err))) (100 (= 96 err 1) (101 496 (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (101 (= 96 err 1) (102
    497 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (111 (= 96
    err 1) (112 498 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err)))
    (111 (= 96 err 1) (112 499 (123 1 err)))) (95 (58 (48 err 1) (65 err
    (91 1 err))) (116 (= 96 err 1) (117 500 (123 1 err)))) (91 (58 (48 err
    1) (65 err 1)) (96 (95 err 1) (97 err (123 1 err)))) (91 (58 (48 err 1)
    (65 err 1)) (96 (95 err 1) (97 err (123 1 err)))) (95 (58 (48 err 1)
    (65 err (91 1 err))) (104 (= 96 err 1) (105 501 (123 1 err)))) (95 (58
    (48 err 1) (65 err (91 1 err))) (99 (= 96 err 1) (100 502 (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (114 (= 96 err 1) (115
    503 (123 1 err)))) (91 (11 (10 429 err) (= 34 471 429)) (101 (92 470
    (100 429 505)) (= 119 504 429))) (34 (= 10 err 429) (91 (35 471 429)
    (92 470 429))) (35 (11 (10 430 err) (34 430 474)) (40 (39 430 472) (=
    91 473 430))) (40 (34 (= 10 err 430) (35 474 (39 430 472))) (100 (= 91
    473 430) (119 (101 507 430) (120 506 430)))) (35 (11 (10 430 err) (34
    430 474)) (40 (39 430 472) (= 91 473 430))) (35 (11 (10 386 err) (34
    386 432)) (92 (91 386 431) (= 125 429 386))) (91 (11 (10 386 err) (= 34
    432 386)) (94 (92 431 (93 386 508)) (= 125 429 386))) (40 (34 (= 10 err
    387) (35 435 (39 387 433))) (93 (= 91 434 387) (125 (94 508 387) (126
    430 387)))) (39 (11 (10 387 err) (= 34 435 387)) (92 (40 433 (91 387
    434)) (= 125 430 387))) (35 (11 (10 479 err) (34 479 511)) (40 (39 479
    509) (= 91 510 479))) (= 108 512 err) (= 62 124 err) (91 (58 (48 err 1)
    (65 err (66 513 1))) (96 (95 err 1) (97 err (123 1 err)))) (91 (58 (48
    err 1) (65 err 1)) (96 (95 err 1) (97 err (123 1 err)))) (95 (58 (48
    err 1) (65 err (91 1 err))) (111 (= 96 err 1) (112 514 (123 1 err))))
    (80 (58 (48 err 1) (65 err (79 1 515))) (96 (91 1 (95 err 1)) (97 err
    (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97 err
    (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 516) (97 err
    (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97 err
    (123 1 err)))) err err (= 41 517 err) err (= 41 411 err) (91 (58 (48
    err 1) (65 err 1)) (96 (95 err 1) (97 err (123 1 err)))) (95 (58 (48
    err 1) (65 err (91 1 err))) (110 (= 96 err 1) (111 518 (123 1 err))))
    (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97 err (123 1 err))))
    (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97 err (123 1 err))))
    (95 (58 (48 err 1) (65 err (91 1 err))) (102 (= 96 err 1) (103 519 (123
    1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (110 (= 96 err 1)
    (111 520 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (115 (=
    96 err 1) (116 521 (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96
    (95 err 1) (97 err (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1
    err))) (104 (= 96 err 1) (105 522 (123 1 err)))) (95 (58 (48 err 1) (65
    err (91 1 err))) (101 (= 96 err 1) (102 523 (123 1 err)))) (34 (= 10
    err 429) (91 (35 471 429) (92 470 429))) (35 (11 (10 429 err) (34 429
    471)) (92 (91 429 470) (= 93 524 429))) (39 (11 (10 430 err) (= 34 474
    430)) (92 (40 472 (91 430 473)) (= 93 524 430))) (35 (11 (10 430 err)
    (34 430 474)) (40 (39 430 472) (= 91 473 430))) (39 (11 (10 508 err) (=
    34 527 508)) (92 (40 525 (91 508 526)) (= 125 524 508))) (35 (11 (10
    479 err) (34 479 511)) (40 (39 479 509) (= 91 510 479))) (40 (34 (= 10
    err 479) (35 511 (39 479 509))) (100 (= 91 510 479) (119 (101 529 479)
    (120 528 479)))) (35 (11 (10 479 err) (34 479 511)) (40 (39 479 509) (=
    91 510 479))) (= 97 530 err) (68 (58 (48 err 1) (65 err (67 1 531)))
    (96 (91 1 (95 err 1)) (97 err (123 1 err)))) (95 (58 (48 err 1) (65 err
    (91 1 err))) (109 (= 96 err 1) (110 532 (123 1 err)))) (79 (58 (48 err
    1) (65 err (78 1 533))) (96 (91 1 (95 err 1)) (97 err (123 1 err))))
    (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97 err (123 1 err))))
    err (95 (58 (48 err 1) (65 err (91 1 err))) (99 (= 96 err 1) (100 534
    (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97 err
    (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (99 (= 96 err 1)
    (100 535 (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1)
    (97 err (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1)
    (97 err (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1)
    (97 err (123 1 err)))) (35 (11 (10 524 err) (34 524 538)) (40 (39 524
    536) (= 91 537 524))) (39 (11 (10 508 err) (= 34 527 508)) (92 (40 525
    (91 508 526)) (= 125 524 508))) (91 (34 (= 10 err 508) (39 (35 527 508)
    (40 525 508))) (119 (100 (92 526 508) (101 540 508)) (125 (120 539 508)
    (126 524 508)))) (39 (11 (10 508 err) (= 34 527 508)) (92 (40 525 (91
    508 526)) (= 125 524 508))) (35 (11 (10 479 err) (34 479 511)) (40 (39
    479 509) (= 91 510 479))) (35 (11 (10 479 err) (34 479 511)) (40 (39
    479 509) (= 91 510 479))) (= 110 541 err) (70 (58 (48 err 1) (65 err
    (69 1 542))) (96 (91 1 (95 err 1)) (97 err (123 1 err)))) (95 (58 (48
    err 1) (65 err (91 1 err))) (112 (= 96 err 1) (113 543 (123 1 err))))
    (91 (58 (48 err 1) (65 err 1)) (96 (95 err 544) (97 err (123 1 err))))
    (95 (58 (48 err 1) (65 err (91 1 err))) (101 (= 96 err 1) (102 545 (123
    1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (101 (= 96 err 1)
    (102 546 (123 1 err)))) (35 (11 (10 524 err) (34 524 538)) (40 (39 524
    536) (= 91 537 524))) (40 (34 (= 10 err 524) (35 538 (39 524 536)))
    (100 (= 91 537 524) (119 (101 548 524) (120 547 524)))) (35 (11 (10 524
    err) (34 524 538)) (40 (39 524 536) (= 91 537 524))) (39 (11 (10 508
    err) (= 34 527 508)) (92 (40 525 (91 508 526)) (= 125 524 508))) (39
    (11 (10 508 err) (= 34 527 508)) (92 (40 525 (91 508 526)) (= 125 524
    508))) (= 103 549 err) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 550)
    (97 err (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (105 (=
    96 err 1) (106 551 (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96
    (95 err 552) (97 err (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96
    (95 err 1) (97 err (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96
    (95 err 1) (97 err (123 1 err)))) (35 (11 (10 524 err) (34 524 538))
    (40 (39 524 536) (= 91 537 524))) (35 (11 (10 524 err) (34 524 538))
    (40 (39 524 536) (= 91 537 524))) (= 117 553 err) (91 (58 (48 err 1)
    (65 err 1)) (96 (95 err 554) (97 err (123 1 err)))) (95 (58 (48 err 1)
    (65 err (91 1 err))) (108 (= 96 err 1) (109 555 (123 1 err)))) (91 (58
    (48 err 1) (65 err 1)) (96 (95 err 1) (97 err (123 1 err)))) (= 97 556
    err) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97 err (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (101 (= 96 err 1) (102
    557 (123 1 err)))) (= 103 558 err) (95 (58 (48 err 1) (65 err (91 1
    err))) (114 (= 96 err 1) (115 559 (123 1 err)))) (= 101 560 err) (91
    (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97 err (123 1 err)))) (= 61
    561 err) (= 34 562 err) (= 112 563 err) (= 104 564 err) (= 112 565 err)
    (= 34 566 err) (= 62 336 err))
   '#((#f . #f) (141 . 141) (141 . 141) (141 . 141) (141 . 141) (128 . 128)
    (127 . 127) (126 . 126) (125 . 125) (124 . 124) (122 . 122) (#f . #f)
    (118 . 118) (117 . 117) (116 . 116) (141 . 141) (141 . 141) (141 . 141)
    (141 . 141) (141 . 141) (141 . 141) (141 . 141) (141 . 141) (129 . 129)
    (141 . 141) (141 . 141) (76 . 76) (76 . 76) (141 . 141) (#f . #f) (136
    . 136) (130 . 130) (141 . 141) (141 . 141) (141 . 141) (141 . 141) (#f
    . #f) (133 . 133) (#f . #f) (141 . 141) (131 . 131) (123 . 123) (20 .
    20) (19 . 19) (134 . 134) (132 . 132) (15 . 15) (137 . 137) (#f . #f)
    (135 . 135) (141 . 141) (141 . 141) (141 . 141) (#f . #f) (119 . 119)
    (121 . 121) (#f . #f) (141 . 141) (141 . 141) (141 . 141) (141 . 141)
    (120 . 120) (141 . 141) (141 . 141) (141 . 141) (141 . 141) (141 . 141)
    (141 . 141) (141 . 141) (141 . 141) (141 . 141) (141 . 141) (141 . 141)
    (141 . 141) (141 . 141) (81 . 81) (141 . 141) (78 . 78) (141 . 141) (71
    . 71) (100 . 100) (69 . 69) (90 . 90) (60 . 60) (141 . 141) (141 . 141)
    (141 . 141) (58 . 58) (141 . 141) (141 . 141) (141 . 141) (141 . 141)
    (141 . 141) (141 . 141) (141 . 141) (141 . 141) (141 . 141) (141 . 141)
    (141 . 141) (141 . 141) (35 . 35) (68 . 68) (31 . 31) (114 . 114) (30 .
    30) (29 . 29) (#f . #f) (141 . 141) (86 . 86) (23 . 23) (22 . 22) (#f .
    #f) (#f . #f) (17 . 17) (26 . 26) (16 . 16) (98 . 98) (#f . #f) (87 .
    87) (87 . 87) (73 . 73) (71 . 71) (#f . #f) (80 . 80) (14 . 14) (141 .
    141) (141 . 141) (141 . 141) (141 . 141) (141 . 141) (89 . 89) (6 . 6)
    (#f . #f) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (#f . #f)
    (#f . #f) (#f . #f) (#f . #f) (5 . 5) (1 . 1) (141 . 141) (141 . 141)
    (141 . 141) (141 . 141) (141 . 141) (141 . 141) (141 . 141) (113 . 113)
    (112 . 112) (141 . 141) (141 . 141) (109 . 109) (141 . 141) (141 . 141)
    (141 . 141) (141 . 141) (141 . 141) (141 . 141) (141 . 141) (141 . 141)
    (141 . 141) (83 . 83) (79 . 79) (141 . 141) (72 . 72) (101 . 101) (141
    . 141) (141 . 141) (141 . 141) (141 . 141) (141 . 141) (141 . 141) (141
    . 141) (141 . 141) (141 . 141) (51 . 51) (141 . 141) (141 . 141) (141 .
    141) (141 . 141) (141 . 141) (141 . 141) (141 . 141) (70 . 70) (27 .
    27) (141 . 141) (#f . #f) (#f . #f) (#f . #f) (40 . 40) (#f . #f) (102
    . 102) (99 . 99) (#f . #f) (88 . 88) (#f . #f) (#f . #f) (141 . 141)
    (141 . 141) (141 . 141) (141 . 141) (141 . 141) (141 . 141) (141 . 141)
    (141 . 141) (141 . 141) (141 . 141) (141 . 141) (141 . 141) (#f . #f)
    (#f . #f) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (#f . #f)
    (#f . #f) (#f . #f) (77 . 77) (141 . 141) (141 . 141) (141 . 141) (139
    . 139) (138 . 138) (141 . 141) (141 . 141) (141 . 141) (141 . 141) (141
    . 141) (141 . 141) (141 . 141) (141 . 141) (141 . 141) (141 . 141) (141
    . 141) (75 . 75) (141 . 141) (141 . 141) (141 . 141) (141 . 141) (141 .
    141) (56 . 56) (141 . 141) (141 . 141) (141 . 141) (141 . 141) (141 .
    141) (141 . 141) (47 . 47) (141 . 141) (141 . 141) (141 . 141) (141 .
    141) (141 . 141) (141 . 141) (37 . 37) (36 . 36) (141 . 141) (#f . #f)
    (#f . #f) (40 . 40) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (#f . #f)
    (#f . #f) (141 . 141) (141 . 141) (141 . 141) (141 . 141) (141 . 141)
    (141 . 141) (141 . 141) (141 . 141) (141 . 141) (141 . 141) (141 . 141)
    (141 . 141) (9 . 9) (141 . 141) (4 . 4) (#f . #f) (#f . #f) (#f . #f)
    (#f . #f) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (141 . 141)
    (141 . 141) (140 . 140) (115 . 115) (110 . 110) (108 . 108) (141 . 141)
    (141 . 141) (141 . 141) (141 . 141) (141 . 141) (141 . 141) (141 . 141)
    (91 . 91) (67 . 67) (141 . 141) (141 . 141) (141 . 141) (141 . 141)
    (141 . 141) (141 . 141) (141 . 141) (50 . 50) (141 . 141) (141 . 141)
    (141 . 141) (44 . 44) (141 . 141) (141 . 141) (39 . 39) (141 . 141)
    (141 . 141) (#f . #f) (40 . 40) (#f . #f) (#f . #f) (#f . #f) (#f . #f)
    (28 . 28) (#f . #f) (87 . 87) (#f . #f) (141 . 141) (141 . 141) (141 .
    141) (141 . 141) (141 . 141) (141 . 141) (141 . 141) (141 . 141) (18 .
    18) (13 . 13) (11 . 11) (10 . 10) (8 . 8) (#f . #f) (#f . #f) (#f . #f)
    (#f . #f) (64 . 64) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (#f . #f)
    (2 . 2) (141 . 141) (107 . 107) (103 . 103) (97 . 97) (141 . 141) (93 .
    93) (141 . 141) (141 . 141) (141 . 141) (141 . 141) (141 . 141) (141 .
    141) (55 . 55) (141 . 141) (141 . 141) (141 . 141) (141 . 141) (141 .
    141) (42 . 42) (141 . 141) (38 . 38) (141 . 141) (#f . #f) (#f . #f)
    (#f . #f) (#f . #f) (#f . #f) (40 . 40) (84 . 84) (#f . #f) (40 . 40)
    (#f . #f) (#f . #f) (141 . 141) (141 . 141) (141 . 141) (141 . 141)
    (141 . 141) (141 . 141) (141 . 141) (141 . 141) (#f . #f) (#f . #f) (#f
    . #f) (#f . #f) (#f . #f) (#f . #f) (32 . 32) (#f . #f) (7 . 7) (#f .
    #f) (141 . 141) (95 . 95) (141 . 141) (92 . 92) (141 . 141) (141 . 141)
    (61 . 61) (141 . 141) (141 . 141) (52 . 52) (48 . 48) (141 . 141) (141
    . 141) (141 . 141) (141 . 141) (24 . 24) (#f . #f) (#f . #f) (#f . #f)
    (40 . 40) (84 . 84) (#f . #f) (40 . 40) (#f . #f) (#f . #f) (#f . #f)
    (#f . #f) (#f . #f) (#f . #f) (141 . 141) (141 . 141) (141 . 141) (141
    . 141) (141 . 141) (25 . 25) (141 . 141) (141 . 141) (111 . 111) (#f .
    #f) (#f . #f) (#f . #f) (34 . 34) (#f . #f) (#f . #f) (3 . 3) (141 .
    141) (141 . 141) (141 . 141) (141 . 141) (141 . 141) (141 . 141) (141 .
    141) (53 . 53) (46 . 46) (141 . 141) (141 . 141) (141 . 141) (#f . #f)
    (40 . 40) (84 . 84) (#f . #f) (40 . 40) (#f . #f) (#f . #f) (#f . #f)
    (#f . #f) (#f . #f) (#f . #f) (#f . #f) (141 . 141) (74 . 74) (141 .
    141) (141 . 141) (49 . 49) (141 . 141) (21 . 21) (105 . 105) (85 . 85)
    (#f . #f) (33 . 33) (#f . #f) (0 . 0) (141 . 141) (94 . 94) (66 . 66)
    (141 . 141) (141 . 141) (141 . 141) (45 . 45) (141 . 141) (141 . 141)
    (#f . #f) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (84 . 84) (#f . #f)
    (40 . 40) (#f . #f) (141 . 141) (141 . 141) (141 . 141) (12 . 12) (65 .
    65) (141 . 141) (63 . 63) (141 . 141) (59 . 59) (43 . 43) (41 . 41) (#f
    . #f) (84 . 84) (#f . #f) (40 . 40) (#f . #f) (#f . #f) (#f . #f) (141
    . 141) (141 . 141) (141 . 141) (141 . 141) (141 . 141) (84 . 84) (#f .
    #f) (40 . 40) (#f . #f) (#f . #f) (#f . #f) (141 . 141) (141 . 141)
    (141 . 141) (96 . 96) (62 . 62) (#f . #f) (#f . #f) (#f . #f) (141 .
    141) (141 . 141) (54 . 54) (#f . #f) (82 . 82) (141 . 141) (#f . #f)
    (141 . 141) (#f . #f) (57 . 57) (#f . #f) (#f . #f) (#f . #f) (#f . #f)
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
