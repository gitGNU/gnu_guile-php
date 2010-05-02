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
                                                (make-token 'T_VARIABLE yytext)
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
                                         (make-token 'label yytext)
        )))
   'decision-trees
   0
   0
   '#((79 (44 (35 (13 (10 (9 err 14) (11 13 err)) (32 (14 12 err) (33 55
    (34 30 44)))) (39 (37 (36 47 39) (38 49 57)) (41 (40 43 56) (42 10 (43
    23 32))))) (59 (48 (46 (45 6 41) (47 45 46)) (55 (49 28 27) (56 26 (58
    27 37)))) (64 (61 (60 5 48) (62 38 (63 31 50))) (70 (65 err 1) (71 2
    (78 1 4)))))) (106 (97 (92 (85 (84 1 3) (91 1 8)) (94 (93 err 7) (95 11
    (96 51 err)))) (101 (99 (98 58 53) (100 52 40)) (103 (102 36 35) (104
    34 (105 1 33))))) (116 (111 (109 (108 1 29) (110 1 22)) (113 (112 25
    21) (114 1 (115 20 19)))) (121 (118 (117 18 17) (119 16 (120 15 24)))
    (124 (123 1 42) (125 54 (126 9 err))))))) (91 (58 (48 err 1) (65 err
    1)) (96 (95 err 1) (97 err (123 1 err)))) (91 (58 (48 err 1) (65 err
    (66 59 1))) (96 (95 err 1) (97 err (123 1 err)))) (83 (58 (48 err 1)
    (65 err (82 1 60))) (96 (91 1 (95 err 1)) (97 err (123 1 err)))) (86
    (58 (48 err 1) (65 err (85 1 61))) (96 (91 1 (95 err 1)) (97 err (123 1
    err)))) err err err err err err (= 61 62 err) (= 13 12 err) (= 10 13
    err) (= 9 14 err) (95 (58 (48 err 1) (65 err (91 1 err))) (104 (= 96
    err 1) (105 63 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err)))
    (97 (96 1 err) (98 64 (123 1 err)))) (96 (65 (48 err (58 1 err)) (91 1
    (95 err 1))) (111 (97 err (110 1 66)) (116 (115 1 65) (123 1 err))))
    (96 (65 (48 err (58 1 err)) (91 1 (95 err 1))) (105 (97 err (104 1 68))
    (115 (114 1 67) (123 1 err)))) (96 (65 (48 err (58 1 err)) (91 1 (95
    err 1))) (117 (97 err (116 1 70)) (120 (119 1 69) (123 1 err)))) (95
    (58 (48 err 1) (65 err (91 1 err))) (101 (= 96 err 1) (102 71 (123 1
    err)))) (96 (65 (48 err (58 1 err)) (91 1 (95 err 1))) (115 (97 err
    (114 1 73)) (118 (117 1 72) (123 1 err)))) (96 (65 (48 err (58 1 err))
    (91 1 (95 err 1))) (102 (97 err (101 1 75)) (118 (117 1 74) (123 1
    err)))) (= 61 76 err) (95 (58 (48 err 1) (65 err (91 1 err))) (111 (=
    96 err 1) (112 77 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1
    err))) (114 (= 96 err 1) (115 78 (123 1 err)))) (91 (58 (48 err 26) (65
    err 1)) (96 (95 err 1) (97 err (123 1 err)))) (48 err (58 27 err)) err
    (95 (58 (48 err 1) (65 err (91 1 err))) (105 (= 96 err 1) (106 79 (123
    1 err)))) (= 61 80 err) (62 (61 err 82) (63 81 err)) (44 (43 err 84) (=
    61 83 err)) (102 (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97 err
    1))) (111 (109 (103 88 1) (110 87 86)) (116 (115 1 85) (123 1 err))))
    (96 (65 (48 err (58 1 err)) (91 1 (95 err 1))) (109 (97 err (108 1 90))
    (112 (111 1 89) (123 1 err)))) (98 (91 (58 (48 err 1) (65 err 1)) (96
    (95 err 1) (97 err 91))) (112 (106 (105 1 94) (111 1 93)) (118 (117 1
    92) (123 1 err)))) (100 (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1)
    (97 err (99 1 100)))) (118 (109 (108 1 99) (110 98 (111 97 1))) (120
    (119 96 1) (121 95 (123 1 err))))) (= 58 101 err) (62 (61 err 102) (63
    103 err)) (95 (56 (55 err 104) (65 err (91 104 err))) (97 (96 104 err)
    (123 104 (124 105 err)))) (95 (48 (= 46 107 err) (65 (58 1 err) (91 1
    err))) (102 (97 (96 1 err) (101 1 108)) (112 (111 1 106) (123 1 err))))
    (46 (45 err 110) (= 62 109 err)) (= 36 111 err) err (11 (10 112 err) (=
    36 113 112)) (= 61 114 err) (47 (= 42 116 err) (61 (48 47 err) (62 115
    err))) err (61 (47 (= 37 119 err) (48 123 (60 err 117))) (64 (62 121
    (63 122 120)) (= 115 118 err))) (62 (61 err 124) (63 125 err)) (= 62
    125 err) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 126) (97 err (123 1
    err)))) (97 (65 (48 err (58 1 err)) (95 (91 1 err) (96 1 err))) (109
    (98 129 (108 1 128)) (112 (111 1 127) (123 1 err)))) (95 (58 (48 err 1)
    (65 err (91 1 err))) (114 (= 96 err 1) (115 130 (123 1 err)))) (62 (61
    err 131) (= 124 132 err)) (= 97 133 err) (105 (100 (98 (97 err 142) (99
    141 err)) (102 (101 139 err) (103 138 err))) (114 (111 (106 137 err)
    (112 136 err)) (116 (115 140 135) (= 117 134 err)))) (39 (38 err 143)
    (= 61 144 err)) (97 (65 (48 err (58 1 err)) (95 (91 1 err) (96 1 err)))
    (111 (99 (98 1 147) (110 1 145)) (115 (114 1 146) (123 1 err)))) (77
    (58 (48 err 1) (65 err (76 1 148))) (96 (91 1 (95 err 1)) (97 err (123
    1 err)))) (86 (58 (48 err 1) (65 err (85 1 149))) (96 (91 1 (95 err 1))
    (97 err (123 1 err)))) (77 (58 (48 err 1) (65 err (76 1 150))) (96 (91
    1 (95 err 1)) (97 err (123 1 err)))) err (95 (58 (48 err 1) (65 err (91
    1 err))) (105 (= 96 err 1) (106 151 (123 1 err)))) (95 (58 (48 err 1)
    (65 err (91 1 err))) (114 (= 96 err 1) (115 152 (123 1 err)))) (95 (58
    (48 err 1) (65 err (91 1 err))) (101 (= 96 err 1) (102 153 (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (115 (= 96 err 1) (116
    154 (123 1 err)))) (96 (65 (48 err (58 1 err)) (91 1 (95 err 1))) (118
    (97 err (117 1 155)) (122 (121 1 156) (123 1 err)))) (95 (58 (48 err 1)
    (65 err (91 1 err))) (114 (= 96 err 1) (115 157 (123 1 err)))) (95 (58
    (48 err 1) (65 err (91 1 err))) (105 (= 96 err 1) (106 158 (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (97 (96 1 err) (98 159
    (123 1 err)))) (96 (65 (48 err (58 1 err)) (91 1 (95 err 1))) (114 (97
    err (113 1 161)) (117 (116 1 160) (123 1 err)))) (95 (58 (48 err 1) (65
    err (91 1 err))) (98 (= 96 err 1) (99 162 (123 1 err)))) (96 (65 (48
    err (58 1 err)) (91 1 (95 err 1))) (106 (97 err (105 1 164)) (112 (111
    1 163) (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (108 (=
    96 err 1) (109 165 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1
    err))) (119 (= 96 err 1) (120 166 (123 1 err)))) err (95 (58 (48 err 1)
    (65 err (91 1 err))) (114 (= 96 err 1) (115 167 (123 1 err)))) (91 (58
    (48 err 1) (65 err 1)) (96 (95 err 1) (97 err (123 1 err)))) (95 (58
    (48 err 1) (65 err (91 1 err))) (115 (= 96 err 1) (116 168 (123 1
    err)))) (= 61 169 err) (= 61 170 err) err err err (95 (58 (48 err 1)
    (65 err (91 1 err))) (115 (= 96 err 1) (116 171 (123 1 err)))) (97 (65
    (48 err (58 1 err)) (95 (91 1 err) (96 1 err))) (115 (= 99 174 1) (117
    (116 173 172) (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err)))
    (112 (= 96 err 1) (113 175 (123 1 err)))) (91 (58 (48 err 1) (65 err
    1)) (96 (95 err 1) (97 err (123 1 err)))) (95 (58 (48 err 1) (65 err
    (91 1 err))) (116 (= 96 err 1) (117 176 (123 1 err)))) (95 (58 (48 err
    1) (65 err (91 1 err))) (111 (= 96 err 1) (112 177 (123 1 err)))) (95
    (58 (48 err 1) (65 err (91 1 err))) (108 (= 96 err 1) (109 178 (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (110 (= 96 err 1) (111
    179 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (114 (= 96
    err 1) (115 180 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err)))
    (110 (= 96 err 1) (111 181 (123 1 err)))) (95 (58 (48 err 1) (65 err
    (91 1 err))) (116 (= 96 err 1) (117 182 (123 1 err)))) (95 (58 (48 err
    1) (65 err (91 1 err))) (97 (96 1 err) (98 183 (123 1 err)))) (95 (58
    (48 err 1) (65 err (91 1 err))) (100 (= 96 err 1) (101 184 (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (112 (= 96 err 1) (113
    185 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (115 (= 96
    err 1) (116 186 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err)))
    (104 (= 96 err 1) (105 187 (123 1 err)))) err (= 61 188 err) err (91
    (58 (48 err 104) (65 err 104)) (96 (95 err 104) (97 err (123 104
    err)))) err (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97 err (123
    1 err)))) (= 100 189 err) (95 (58 (48 err 1) (65 err (91 1 err))) (99
    (= 96 err 1) (100 190 (123 1 err)))) err err err (= 36 191 err) (35 (11
    (10 191 err) (34 191 194)) (92 (91 191 193) (= 123 192 191))) err err
    (= 42 195 err) (61 (60 err 196) (62 197 err)) (= 99 198 err) (= 61 199
    err) (62 (61 err 199) (= 112 200 err)) err err (= 115 201 err) err err
    (77 (68 (58 (48 err 1) (65 err (67 1 207))) (70 (69 206 1) (71 205 (76
    1 203)))) (96 (79 (78 1 202) (91 1 (95 err 1))) (104 (97 err 1) (105
    204 (123 1 err))))) (95 (58 (48 err 1) (65 err (91 1 err))) (110 (= 96
    err 1) (111 208 (123 1 err)))) (96 (65 (48 err (58 1 err)) (91 1 (95
    err 1))) (111 (97 err (98 210 1)) (112 209 (123 1 err)))) (96 (65 (48
    err (58 1 err)) (91 1 (95 err 1))) (116 (97 err (115 1 212)) (117 211
    (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (101 (= 96 err
    1) (102 213 (123 1 err)))) err err (= 115 214 err) (= 110 215 err) (=
    116 216 err) (= 98 217 err) (= 110 218 err) (= 108 219 err) (= 111 220
    err) (= 101 221 err) (= 111 222 err) (= 114 223 err) err err (95 (58
    (48 err 1) (65 err (91 1 err))) (100 (= 96 err 1) (101 224 (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (114 (= 96 err 1) (115
    225 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (115 (= 96
    err 1) (116 226 (123 1 err)))) (84 (58 (48 err 1) (65 err (83 1 227)))
    (96 (91 1 (95 err 1)) (97 err (123 1 err)))) (70 (58 (48 err 1) (65 err
    (69 1 228))) (96 (91 1 (95 err 1)) (97 err (123 1 err)))) (77 (58 (48
    err 1) (65 err (76 1 229))) (96 (91 1 (95 err 1)) (97 err (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (108 (= 96 err 1) (109
    230 (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97
    err (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97
    err (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (101 (= 96
    err 1) (102 231 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err)))
    (101 (= 96 err 1) (102 228 (123 1 err)))) (91 (58 (48 err 1) (65 err
    1)) (96 (95 err 1) (97 err (123 1 err)))) (95 (58 (48 err 1) (65 err
    (91 1 err))) (111 (= 96 err 1) (112 232 (123 1 err)))) (95 (58 (48 err
    1) (65 err (91 1 err))) (116 (= 96 err 1) (117 233 (123 1 err)))) (95
    (58 (48 err 1) (65 err (91 1 err))) (116 (= 96 err 1) (117 234 (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (117 (= 96 err 1) (118
    235 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (117 (= 96
    err 1) (118 236 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err)))
    (108 (= 96 err 1) (109 237 (123 1 err)))) (95 (58 (48 err 1) (65 err
    (91 1 err))) (116 (= 96 err 1) (117 238 (123 1 err)))) (96 (65 (48 err
    (58 1 err)) (91 1 (95 err 1))) (111 (97 err (110 1 240)) (119 (118 1
    239) (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (108 (= 96
    err 1) (109 229 (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95
    err 1) (97 err (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95
    err 1) (97 err (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err)))
    (116 (= 96 err 1) (117 241 (123 1 err)))) err err (95 (58 (48 err 1)
    (65 err (91 1 err))) (101 (= 96 err 1) (102 242 (123 1 err)))) (95 (58
    (48 err 1) (65 err (91 1 err))) (101 (= 96 err 1) (102 243 (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (116 (= 96 err 1) (117
    244 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (108 (= 96
    err 1) (109 245 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err)))
    (108 (= 96 err 1) (109 246 (123 1 err)))) (95 (58 (48 err 1) (65 err
    (91 1 err))) (111 (= 96 err 1) (112 247 (123 1 err)))) (95 (58 (48 err
    1) (65 err (91 1 err))) (98 (= 96 err 1) (99 248 (123 1 err)))) (95 (58
    (48 err 1) (65 err (91 1 err))) (115 (= 96 err 1) (116 249 (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (99 (= 96 err 1) (100
    250 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (101 (= 96
    err 1) (102 251 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err)))
    (97 (96 1 err) (98 252 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1
    err))) (101 (= 96 err 1) (102 253 (123 1 err)))) (95 (58 (48 err 1) (65
    err (91 1 err))) (108 (= 96 err 1) (109 254 (123 1 err)))) (101 (91 (58
    (48 err 1) (65 err 1)) (96 (95 err 1) (97 err (100 1 259)))) (115 (103
    (102 1 258) (= 105 257 1)) (119 (116 256 1) (120 255 (123 1 err)))))
    (95 (58 (48 err 1) (65 err (91 1 err))) (116 (= 96 err 1) (117 260 (123
    1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (101 (= 96 err 1)
    (102 261 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (111 (=
    96 err 1) (112 262 (123 1 err)))) err err (95 (58 (48 err 1) (65 err
    (91 1 err))) (108 (= 96 err 1) (109 263 (123 1 err)))) (34 (= 10 err
    191) (91 (35 194 191) (92 193 191))) (35 (11 (10 192 err) (34 192 266))
    (92 (91 192 265) (= 125 264 192))) (91 (11 (10 191 err) (= 34 194 191))
    (101 (92 193 (100 191 268)) (= 119 267 191))) (34 (= 10 err 191) (91
    (35 194 191) (92 193 191))) (11 (10 195 err) (= 42 269 195)) err err (=
    114 270 err) err (= 104 271 err) (= 99 272 err) (91 (58 (48 err 1) (65
    err (66 273 1))) (96 (95 err 1) (97 err (123 1 err)))) (74 (58 (48 err
    1) (65 err (73 1 274))) (96 (91 1 (95 err 1)) (97 err (123 1 err))))
    (95 (58 (48 err 1) (65 err (91 1 err))) (97 (96 1 err) (98 275 (123 1
    err)))) (85 (65 (48 err (58 1 err)) (= 73 277 1)) (95 (86 276 (91 1
    err)) (97 (96 1 err) (123 1 err)))) (74 (58 (48 err 1) (65 err (73 1
    278))) (96 (91 1 (95 err 1)) (97 err (123 1 err)))) (77 (58 (48 err 1)
    (65 err (76 1 279))) (96 (91 1 (95 err 1)) (97 err (123 1 err)))) (96
    (65 (48 err (58 1 err)) (91 1 (95 err 1))) (116 (97 err (115 1 281))
    (117 280 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (110 (=
    96 err 1) (111 282 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1
    err))) (115 (= 96 err 1) (116 283 (123 1 err)))) (95 (58 (48 err 1) (65
    err (91 1 err))) (99 (= 96 err 1) (100 284 (123 1 err)))) (95 (58 (48
    err 1) (65 err (91 1 err))) (101 (= 96 err 1) (102 285 (123 1 err))))
    (95 (58 (48 err 1) (65 err (91 1 err))) (97 (96 1 err) (98 286 (123 1
    err)))) (= 32 287 err) (= 115 288 err) (= 114 289 err) (= 106 290 err)
    (= 116 291 err) (= 111 292 err) (= 117 293 err) (= 97 294 err) (= 111
    295 err) (= 114 296 err) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1)
    (97 err (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (97 (96
    1 err) (98 297 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err)))
    (116 (= 96 err 1) (117 298 (123 1 err)))) (70 (58 (48 err 1) (65 err
    (69 1 299))) (96 (91 1 (95 err 1)) (97 err (123 1 err)))) (91 (58 (48
    err 1) (65 err 1)) (96 (95 err 1) (97 err (123 1 err)))) (91 (58 (48
    err 1) (65 err 1)) (96 (95 err 1) (97 err (123 1 err)))) (95 (58 (48
    err 1) (65 err (91 1 err))) (101 (= 96 err 1) (102 300 (123 1 err))))
    (95 (58 (48 err 1) (65 err (91 1 err))) (116 (= 96 err 1) (117 301 (123
    1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (119 (= 96 err 1)
    (120 302 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (99 (=
    96 err 1) (100 303 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1
    err))) (105 (= 96 err 1) (106 304 (123 1 err)))) (95 (58 (48 err 1) (65
    err (91 1 err))) (114 (= 96 err 1) (115 305 (123 1 err)))) (95 (58 (48
    err 1) (65 err (91 1 err))) (105 (= 96 err 1) (106 306 (123 1 err))))
    (95 (58 (48 err 1) (65 err (91 1 err))) (105 (= 96 err 1) (106 307 (123
    1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (101 (= 96 err 1)
    (102 308 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (97 (96
    1 err) (98 309 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err)))
    (116 (= 96 err 1) (117 310 (123 1 err)))) (91 (58 (48 err 1) (65 err
    1)) (96 (95 err 1) (97 err (123 1 err)))) (95 (58 (48 err 1) (65 err
    (91 1 err))) (116 (= 96 err 1) (117 311 (123 1 err)))) (95 (58 (48 err
    1) (65 err (91 1 err))) (114 (= 96 err 1) (115 312 (123 1 err)))) (95
    (58 (48 err 1) (65 err (91 1 err))) (97 (96 1 err) (98 313 (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (117 (= 96 err 1) (118
    314 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (101 (= 96
    err 1) (102 315 (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95
    err 1) (97 err (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err)))
    (97 (96 1 err) (98 316 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1
    err))) (101 (= 96 err 1) (102 299 (123 1 err)))) (95 (58 (48 err 1) (65
    err (91 1 err))) (116 (= 96 err 1) (117 317 (123 1 err)))) (95 (58 (48
    err 1) (65 err (91 1 err))) (97 (96 1 err) (98 318 (123 1 err)))) (95
    (58 (48 err 1) (65 err (91 1 err))) (108 (= 96 err 1) (109 319 (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (110 (= 96 err 1) (111
    320 (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97
    err (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (104 (= 96
    err 1) (105 321 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err)))
    (119 (= 96 err 1) (120 322 (123 1 err)))) (95 (58 (48 err 1) (65 err
    (91 1 err))) (102 (= 96 err 1) (103 323 (123 1 err)))) (95 (58 (48 err
    1) (65 err (91 1 err))) (111 (= 96 err 1) (112 324 (123 1 err)))) (95
    (58 (48 err 1) (65 err (91 1 err))) (101 (= 96 err 1) (102 325 (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (121 (= 96 err 1) (122
    326 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (105 (= 96
    err 1) (106 327 (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95
    err 1) (97 err (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err)))
    (97 (96 1 err) (98 328 (123 1 err)))) (34 (= 10 err 264) (91 (35 330
    264) (92 329 264))) (92 (34 (= 10 err 192) (35 266 (91 192 265))) (119
    (= 100 332 192) (125 (120 331 192) (126 264 192)))) (35 (11 (10 192
    err) (34 192 266)) (92 (91 192 265) (= 125 264 192))) (35 (11 (10 191
    err) (34 191 194)) (92 (91 191 193) (= 93 333 191))) (35 (11 (10 191
    err) (34 191 194)) (92 (91 191 193) (= 93 334 191))) (42 (= 10 err 195)
    (47 (43 269 195) (48 335 195))) (= 105 336 err) (= 112 337 err) (= 114
    338 err) (78 (58 (48 err 1) (65 err (77 1 339))) (96 (91 1 (95 err 1))
    (97 err (123 1 err)))) (79 (58 (48 err 1) (65 err (78 1 340))) (96 (91
    1 (95 err 1)) (97 err (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1
    err))) (108 (= 96 err 1) (109 341 (123 1 err)))) (79 (58 (48 err 1) (65
    err (78 1 342))) (96 (91 1 (95 err 1)) (97 err (123 1 err)))) (77 (58
    (48 err 1) (65 err (76 1 343))) (96 (91 1 (95 err 1)) (97 err (123 1
    err)))) (83 (58 (48 err 1) (65 err (82 1 344))) (96 (91 1 (95 err 1))
    (97 err (123 1 err)))) (91 (58 (48 err 1) (65 err (66 345 1))) (96 (95
    err 1) (97 err (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err)))
    (105 (= 96 err 1) (106 346 (123 1 err)))) (95 (58 (48 err 1) (65 err
    (91 1 err))) (116 (= 96 err 1) (117 347 (123 1 err)))) (95 (58 (48 err
    1) (65 err (91 1 err))) (101 (= 96 err 1) (102 348 (123 1 err)))) (95
    (58 (48 err 1) (65 err (91 1 err))) (115 (= 96 err 1) (116 349 (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (104 (= 96 err 1) (105
    350 (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97
    err (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (107 (= 96
    err 1) (108 351 (123 1 err)))) err (= 101 352 err) (= 105 353 err) (=
    101 354 err) (42 (41 err 356) (= 101 355 err)) (= 97 357 err) (= 98 358
    err) (= 108 359 err) (= 108 360 err) (= 97 361 err) (95 (58 (48 err 1)
    (65 err (91 1 err))) (121 (= 96 err 1) (122 362 (123 1 err)))) (95 (58
    (48 err 1) (65 err (91 1 err))) (114 (= 96 err 1) (115 363 (123 1
    err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97 err (123 1
    err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97 err (123 1
    err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97 err (123 1
    err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97 err (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (104 (= 96 err 1) (105
    364 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (99 (= 96
    err 1) (100 365 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err)))
    (110 (= 96 err 1) (111 366 (123 1 err)))) (95 (58 (48 err 1) (65 err
    (91 1 err))) (114 (= 96 err 1) (115 367 (123 1 err)))) (95 (58 (48 err
    1) (65 err (91 1 err))) (99 (= 96 err 1) (100 368 (123 1 err)))) (95
    (58 (48 err 1) (65 err (91 1 err))) (99 (= 96 err 1) (100 369 (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (116 (= 96 err 1) (117
    370 (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97
    err (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97
    err (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (102 (= 96
    err 1) (103 371 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err)))
    (110 (= 96 err 1) (111 372 (123 1 err)))) (95 (58 (48 err 1) (65 err
    (91 1 err))) (100 (= 96 err 1) (101 373 (123 1 err)))) (95 (58 (48 err
    1) (65 err (91 1 err))) (109 (= 96 err 1) (110 374 (123 1 err)))) (95
    (58 (48 err 1) (65 err (91 1 err))) (108 (= 96 err 1) (109 375 (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (105 (= 96 err 1) (106
    376 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (99 (= 96
    err 1) (100 377 (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95
    err 1) (97 err (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err)))
    (100 (= 96 err 1) (101 378 (123 1 err)))) (95 (58 (48 err 1) (65 err
    (91 1 err))) (105 (= 96 err 1) (106 379 (123 1 err)))) (95 (58 (48 err
    1) (65 err (91 1 err))) (105 (= 96 err 1) (106 380 (123 1 err)))) (91
    (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97 err (123 1 err)))) (95
    (58 (48 err 1) (65 err (91 1 err))) (114 (= 96 err 1) (115 381 (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (99 (= 96 err 1) (100
    382 (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97
    err (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (102 (= 96
    err 1) (103 383 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err)))
    (114 (= 96 err 1) (115 384 (123 1 err)))) (91 (11 (10 264 err) (= 34
    330 264)) (101 (92 329 (100 264 386)) (= 119 385 264))) (34 (= 10 err
    264) (91 (35 330 264) (92 329 264))) (91 (11 (10 192 err) (= 34 266
    192)) (94 (92 265 (93 192 387)) (= 125 264 192))) (91 (11 (10 192 err)
    (= 34 266 192)) (94 (92 265 (93 192 388)) (= 125 264 192))) (34 (= 10
    err 333) (91 (35 390 333) (92 389 333))) (35 (11 (10 334 err) (34 334
    393)) (40 (39 334 391) (= 91 392 334))) (11 (10 195 err) (= 42 269
    195)) (= 112 394 err) err (= 105 395 err) (70 (58 (48 err 1) (65 err
    (69 1 396))) (96 (91 1 (95 err 1)) (97 err (123 1 err)))) (70 (58 (48
    err 1) (65 err (69 1 397))) (96 (91 1 (95 err 1)) (97 err (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (116 (= 96 err 1) (117
    398 (123 1 err)))) (68 (58 (48 err 1) (65 err (67 1 399))) (96 (91 1
    (95 err 1)) (97 err (123 1 err)))) (70 (58 (48 err 1) (65 err (69 1
    400))) (96 (91 1 (95 err 1)) (97 err (123 1 err)))) (91 (58 (48 err 1)
    (65 err 1)) (96 (95 err 401) (97 err (123 1 err)))) (84 (58 (48 err 1)
    (65 err (83 1 402))) (96 (91 1 (95 err 1)) (97 err (123 1 err)))) (95
    (58 (48 err 1) (65 err (91 1 err))) (110 (= 96 err 1) (111 403 (123 1
    err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97 err (123 1
    err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97 err (123 1
    err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97 err (123 1
    err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97 err (123 1
    err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97 err (123 1
    err)))) (= 116 404 err) (= 110 405 err) (= 99 406 err) (= 103 407 err)
    err (= 116 408 err) (= 108 409 err) (= 41 410 err) (42 (41 err 412) (=
    101 411 err)) (= 121 413 err) (91 (58 (48 err 1) (65 err 1)) (96 (95
    err 1) (97 err (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err)))
    (97 (96 1 err) (98 414 (123 1 err)))) (91 (58 (48 err 1) (65 err 1))
    (96 (95 err 1) (97 err (123 1 err)))) (91 (58 (48 err 1) (65 err 1))
    (96 (95 err 1) (97 err (123 1 err)))) (91 (58 (48 err 1) (65 err 1))
    (96 (95 err 1) (97 err (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1
    err))) (101 (= 96 err 1) (102 415 (123 1 err)))) (91 (58 (48 err 1) (65
    err 1)) (96 (95 err 1) (97 err (123 1 err)))) (95 (58 (48 err 1) (65
    err (91 1 err))) (116 (= 96 err 1) (117 416 (123 1 err)))) (95 (58 (48
    err 1) (65 err (91 1 err))) (101 (= 96 err 1) (102 417 (123 1 err))))
    (95 (58 (48 err 1) (65 err (91 1 err))) (97 (96 1 err) (98 418 (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (99 (= 96 err 1) (100
    419 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (101 (= 96
    err 1) (102 420 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err)))
    (101 (= 96 err 1) (102 421 (123 1 err)))) (91 (58 (48 err 1) (65 err
    1)) (96 (95 err 1) (97 err (123 1 err)))) (95 (58 (48 err 1) (65 err
    (91 1 err))) (111 (= 96 err 1) (112 422 (123 1 err)))) (95 (58 (48 err
    1) (65 err (91 1 err))) (104 (= 96 err 1) (105 423 (123 1 err)))) (95
    (58 (48 err 1) (65 err (91 1 err))) (115 (= 96 err 1) (116 424 (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (108 (= 96 err 1) (109
    425 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (116 (= 96
    err 1) (117 426 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err)))
    (101 (= 96 err 1) (102 427 (123 1 err)))) (95 (58 (48 err 1) (65 err
    (91 1 err))) (108 (= 96 err 1) (109 428 (123 1 err)))) (91 (58 (48 err
    1) (65 err 1)) (96 (95 err 1) (97 err (123 1 err)))) (95 (58 (48 err 1)
    (65 err (91 1 err))) (101 (= 96 err 1) (102 429 (123 1 err)))) (35 (11
    (10 264 err) (34 264 330)) (92 (91 264 329) (= 93 430 264))) (35 (11
    (10 264 err) (34 264 330)) (92 (91 264 329) (= 93 431 264))) (35 (11
    (10 387 err) (34 387 433)) (92 (91 387 432) (= 125 430 387))) (39 (11
    (10 388 err) (= 34 436 388)) (92 (40 434 (91 388 435)) (= 125 431
    388))) (91 (11 (10 333 err) (= 34 390 333)) (101 (92 389 (100 333 438))
    (= 119 437 333))) (34 (= 10 err 333) (91 (35 390 333) (92 389 333)))
    (35 (11 (10 334 err) (34 334 393)) (40 (39 334 391) (= 91 392 334)))
    (40 (34 (= 10 err 334) (35 393 (39 334 391))) (100 (= 91 392 334) (119
    (101 440 334) (120 439 334)))) (35 (11 (10 334 err) (34 334 393)) (40
    (39 334 391) (= 91 392 334))) (= 116 441 err) (= 112 442 err) (84 (58
    (48 err 1) (65 err (83 1 443))) (96 (91 1 (95 err 1)) (97 err (123 1
    err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 444) (97 err (123 1
    err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 445) (97 err (123 1
    err)))) (85 (58 (48 err 1) (65 err (84 1 446))) (96 (91 1 (95 err 1))
    (97 err (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 447)
    (97 err (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 448)
    (97 err (123 1 err)))) (84 (58 (48 err 1) (65 err (83 1 449))) (96 (91
    1 (95 err 1)) (97 err (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1
    err))) (117 (= 96 err 1) (118 450 (123 1 err)))) (= 41 451 err) (= 103
    452 err) (= 116 453 err) (= 101 454 err) (= 41 455 err) (= 101 456 err)
    err (= 97 457 err) err (= 41 458 err) (95 (58 (48 err 1) (65 err (91 1
    err))) (99 (= 96 err 1) (100 459 (123 1 err)))) (91 (58 (48 err 1) (65
    err 1)) (96 (95 err 460) (97 err (123 1 err)))) (95 (58 (48 err 1) (65
    err (91 1 err))) (101 (= 96 err 1) (102 461 (123 1 err)))) (91 (58 (48
    err 1) (65 err 1)) (96 (95 err 1) (97 err (123 1 err)))) (95 (58 (48
    err 1) (65 err (91 1 err))) (99 (= 96 err 1) (100 462 (123 1 err))))
    (95 (58 (48 err 1) (65 err (91 1 err))) (101 (= 96 err 1) (102 463 (123
    1 err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 464) (97 err (123
    1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (110 (= 96 err 1)
    (111 465 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (110 (=
    96 err 1) (111 466 (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96
    (95 err 1) (97 err (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96
    (95 err 1) (97 err (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1
    err))) (101 (= 96 err 1) (102 467 (123 1 err)))) (95 (58 (48 err 1) (65
    err (91 1 err))) (99 (= 96 err 1) (100 468 (123 1 err)))) (95 (58 (48
    err 1) (65 err (91 1 err))) (97 (96 1 err) (98 469 (123 1 err)))) (95
    (58 (48 err 1) (65 err (91 1 err))) (97 (96 1 err) (98 470 (123 1
    err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97 err (123 1
    err)))) (34 (= 10 err 430) (91 (35 472 430) (92 471 430))) (35 (11 (10
    431 err) (34 431 475)) (40 (39 431 473) (= 91 474 431))) (92 (34 (= 10
    err 387) (35 433 (91 387 432))) (119 (= 100 477 387) (125 (120 476 387)
    (126 430 387)))) (35 (11 (10 387 err) (34 387 433)) (92 (91 387 432) (=
    125 430 387))) (39 (11 (10 388 err) (= 34 436 388)) (92 (40 434 (91 388
    435)) (= 125 431 388))) (91 (34 (= 10 err 388) (39 (35 436 388) (40 434
    388))) (119 (100 (92 435 388) (101 479 388)) (125 (120 478 388) (126
    431 388)))) (39 (11 (10 388 err) (= 34 436 388)) (92 (40 434 (91 388
    435)) (= 125 431 388))) (34 (= 10 err 333) (91 (35 390 333) (92 389
    333))) (35 (11 (10 333 err) (34 333 390)) (92 (91 333 389) (= 93 480
    333))) (39 (11 (10 334 err) (= 34 393 334)) (92 (40 391 (91 334 392))
    (= 93 480 334))) (35 (11 (10 334 err) (34 334 393)) (40 (39 334 391) (=
    91 392 334))) (= 32 481 err) (= 116 482 err) (81 (58 (48 err 1) (65 err
    (80 1 483))) (96 (91 1 (95 err 1)) (97 err (123 1 err)))) (91 (58 (48
    err 1) (65 err 1)) (96 (95 err 484) (97 err (123 1 err)))) (95 (58 (48
    err 1) (65 err (91 1 err))) (99 (= 96 err 1) (100 485 (123 1 err))))
    (74 (58 (48 err 1) (65 err (73 1 486))) (96 (91 1 (95 err 1)) (97 err
    (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 487) (97 err
    (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97 err
    (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 488) (97 err
    (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (101 (= 96 err
    1) (102 489 (123 1 err)))) err (= 41 490 err) (= 41 491 err) (= 114 492
    err) err (= 41 493 err) (= 110 494 err) err (95 (58 (48 err 1) (65 err
    (91 1 err))) (116 (= 96 err 1) (117 495 (123 1 err)))) (95 (58 (48 err
    1) (65 err (91 1 err))) (111 (= 96 err 1) (112 496 (123 1 err)))) (95
    (58 (48 err 1) (65 err (91 1 err))) (100 (= 96 err 1) (101 497 (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (101 (= 96 err 1) (102
    498 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (111 (= 96
    err 1) (112 499 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err)))
    (111 (= 96 err 1) (112 500 (123 1 err)))) (95 (58 (48 err 1) (65 err
    (91 1 err))) (116 (= 96 err 1) (117 501 (123 1 err)))) (91 (58 (48 err
    1) (65 err 1)) (96 (95 err 1) (97 err (123 1 err)))) (91 (58 (48 err 1)
    (65 err 1)) (96 (95 err 1) (97 err (123 1 err)))) (95 (58 (48 err 1)
    (65 err (91 1 err))) (104 (= 96 err 1) (105 502 (123 1 err)))) (95 (58
    (48 err 1) (65 err (91 1 err))) (99 (= 96 err 1) (100 503 (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (114 (= 96 err 1) (115
    504 (123 1 err)))) (91 (11 (10 430 err) (= 34 472 430)) (101 (92 471
    (100 430 506)) (= 119 505 430))) (34 (= 10 err 430) (91 (35 472 430)
    (92 471 430))) (35 (11 (10 431 err) (34 431 475)) (40 (39 431 473) (=
    91 474 431))) (40 (34 (= 10 err 431) (35 475 (39 431 473))) (100 (= 91
    474 431) (119 (101 508 431) (120 507 431)))) (35 (11 (10 431 err) (34
    431 475)) (40 (39 431 473) (= 91 474 431))) (35 (11 (10 387 err) (34
    387 433)) (92 (91 387 432) (= 125 430 387))) (91 (11 (10 387 err) (= 34
    433 387)) (94 (92 432 (93 387 509)) (= 125 430 387))) (40 (34 (= 10 err
    388) (35 436 (39 388 434))) (93 (= 91 435 388) (125 (94 509 388) (126
    431 388)))) (39 (11 (10 388 err) (= 34 436 388)) (92 (40 434 (91 388
    435)) (= 125 431 388))) (35 (11 (10 480 err) (34 480 512)) (40 (39 480
    510) (= 91 511 480))) (= 108 513 err) (= 62 125 err) (91 (58 (48 err 1)
    (65 err (66 514 1))) (96 (95 err 1) (97 err (123 1 err)))) (91 (58 (48
    err 1) (65 err 1)) (96 (95 err 1) (97 err (123 1 err)))) (95 (58 (48
    err 1) (65 err (91 1 err))) (111 (= 96 err 1) (112 515 (123 1 err))))
    (80 (58 (48 err 1) (65 err (79 1 516))) (96 (91 1 (95 err 1)) (97 err
    (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97 err
    (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 517) (97 err
    (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97 err
    (123 1 err)))) err err (= 41 518 err) err (= 41 412 err) (91 (58 (48
    err 1) (65 err 1)) (96 (95 err 1) (97 err (123 1 err)))) (95 (58 (48
    err 1) (65 err (91 1 err))) (110 (= 96 err 1) (111 519 (123 1 err))))
    (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97 err (123 1 err))))
    (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97 err (123 1 err))))
    (95 (58 (48 err 1) (65 err (91 1 err))) (102 (= 96 err 1) (103 520 (123
    1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (110 (= 96 err 1)
    (111 521 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (115 (=
    96 err 1) (116 522 (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96
    (95 err 1) (97 err (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1
    err))) (104 (= 96 err 1) (105 523 (123 1 err)))) (95 (58 (48 err 1) (65
    err (91 1 err))) (101 (= 96 err 1) (102 524 (123 1 err)))) (34 (= 10
    err 430) (91 (35 472 430) (92 471 430))) (35 (11 (10 430 err) (34 430
    472)) (92 (91 430 471) (= 93 525 430))) (39 (11 (10 431 err) (= 34 475
    431)) (92 (40 473 (91 431 474)) (= 93 525 431))) (35 (11 (10 431 err)
    (34 431 475)) (40 (39 431 473) (= 91 474 431))) (39 (11 (10 509 err) (=
    34 528 509)) (92 (40 526 (91 509 527)) (= 125 525 509))) (35 (11 (10
    480 err) (34 480 512)) (40 (39 480 510) (= 91 511 480))) (40 (34 (= 10
    err 480) (35 512 (39 480 510))) (100 (= 91 511 480) (119 (101 530 480)
    (120 529 480)))) (35 (11 (10 480 err) (34 480 512)) (40 (39 480 510) (=
    91 511 480))) (= 97 531 err) (68 (58 (48 err 1) (65 err (67 1 532)))
    (96 (91 1 (95 err 1)) (97 err (123 1 err)))) (95 (58 (48 err 1) (65 err
    (91 1 err))) (109 (= 96 err 1) (110 533 (123 1 err)))) (79 (58 (48 err
    1) (65 err (78 1 534))) (96 (91 1 (95 err 1)) (97 err (123 1 err))))
    (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97 err (123 1 err))))
    err (95 (58 (48 err 1) (65 err (91 1 err))) (99 (= 96 err 1) (100 535
    (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97 err
    (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (99 (= 96 err 1)
    (100 536 (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1)
    (97 err (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1)
    (97 err (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1)
    (97 err (123 1 err)))) (35 (11 (10 525 err) (34 525 539)) (40 (39 525
    537) (= 91 538 525))) (39 (11 (10 509 err) (= 34 528 509)) (92 (40 526
    (91 509 527)) (= 125 525 509))) (91 (34 (= 10 err 509) (39 (35 528 509)
    (40 526 509))) (119 (100 (92 527 509) (101 541 509)) (125 (120 540 509)
    (126 525 509)))) (39 (11 (10 509 err) (= 34 528 509)) (92 (40 526 (91
    509 527)) (= 125 525 509))) (35 (11 (10 480 err) (34 480 512)) (40 (39
    480 510) (= 91 511 480))) (35 (11 (10 480 err) (34 480 512)) (40 (39
    480 510) (= 91 511 480))) (= 110 542 err) (70 (58 (48 err 1) (65 err
    (69 1 543))) (96 (91 1 (95 err 1)) (97 err (123 1 err)))) (95 (58 (48
    err 1) (65 err (91 1 err))) (112 (= 96 err 1) (113 544 (123 1 err))))
    (91 (58 (48 err 1) (65 err 1)) (96 (95 err 545) (97 err (123 1 err))))
    (95 (58 (48 err 1) (65 err (91 1 err))) (101 (= 96 err 1) (102 546 (123
    1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (101 (= 96 err 1)
    (102 547 (123 1 err)))) (35 (11 (10 525 err) (34 525 539)) (40 (39 525
    537) (= 91 538 525))) (40 (34 (= 10 err 525) (35 539 (39 525 537)))
    (100 (= 91 538 525) (119 (101 549 525) (120 548 525)))) (35 (11 (10 525
    err) (34 525 539)) (40 (39 525 537) (= 91 538 525))) (39 (11 (10 509
    err) (= 34 528 509)) (92 (40 526 (91 509 527)) (= 125 525 509))) (39
    (11 (10 509 err) (= 34 528 509)) (92 (40 526 (91 509 527)) (= 125 525
    509))) (= 103 550 err) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 551)
    (97 err (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (105 (=
    96 err 1) (106 552 (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96
    (95 err 553) (97 err (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96
    (95 err 1) (97 err (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96
    (95 err 1) (97 err (123 1 err)))) (35 (11 (10 525 err) (34 525 539))
    (40 (39 525 537) (= 91 538 525))) (35 (11 (10 525 err) (34 525 539))
    (40 (39 525 537) (= 91 538 525))) (= 117 554 err) (91 (58 (48 err 1)
    (65 err 1)) (96 (95 err 555) (97 err (123 1 err)))) (95 (58 (48 err 1)
    (65 err (91 1 err))) (108 (= 96 err 1) (109 556 (123 1 err)))) (91 (58
    (48 err 1) (65 err 1)) (96 (95 err 1) (97 err (123 1 err)))) (= 97 557
    err) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97 err (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (101 (= 96 err 1) (102
    558 (123 1 err)))) (= 103 559 err) (95 (58 (48 err 1) (65 err (91 1
    err))) (114 (= 96 err 1) (115 560 (123 1 err)))) (= 101 561 err) (91
    (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97 err (123 1 err)))) (= 61
    562 err) (= 34 563 err) (= 112 564 err) (= 104 565 err) (= 112 566 err)
    (= 34 567 err) (= 62 337 err))
   '#((#f . #f) (141 . 141) (141 . 141) (141 . 141) (141 . 141) (128 . 128)
    (127 . 127) (126 . 126) (125 . 125) (124 . 124) (122 . 122) (#f . #f)
    (118 . 118) (117 . 117) (116 . 116) (141 . 141) (141 . 141) (141 . 141)
    (141 . 141) (141 . 141) (141 . 141) (141 . 141) (141 . 141) (129 . 129)
    (141 . 141) (141 . 141) (76 . 76) (76 . 76) (76 . 76) (141 . 141) (#f .
    #f) (136 . 136) (130 . 130) (141 . 141) (141 . 141) (141 . 141) (141 .
    141) (#f . #f) (133 . 133) (#f . #f) (141 . 141) (131 . 131) (123 .
    123) (20 . 20) (19 . 19) (134 . 134) (132 . 132) (15 . 15) (137 . 137)
    (#f . #f) (135 . 135) (141 . 141) (141 . 141) (141 . 141) (#f . #f)
    (119 . 119) (121 . 121) (#f . #f) (141 . 141) (141 . 141) (141 . 141)
    (141 . 141) (120 . 120) (141 . 141) (141 . 141) (141 . 141) (141 . 141)
    (141 . 141) (141 . 141) (141 . 141) (141 . 141) (141 . 141) (141 . 141)
    (141 . 141) (141 . 141) (141 . 141) (81 . 81) (141 . 141) (78 . 78)
    (141 . 141) (71 . 71) (100 . 100) (69 . 69) (90 . 90) (60 . 60) (141 .
    141) (141 . 141) (141 . 141) (58 . 58) (141 . 141) (141 . 141) (141 .
    141) (141 . 141) (141 . 141) (141 . 141) (141 . 141) (141 . 141) (141 .
    141) (141 . 141) (141 . 141) (141 . 141) (35 . 35) (68 . 68) (31 . 31)
    (114 . 114) (30 . 30) (29 . 29) (#f . #f) (141 . 141) (86 . 86) (23 .
    23) (22 . 22) (#f . #f) (#f . #f) (17 . 17) (26 . 26) (16 . 16) (98 .
    98) (#f . #f) (87 . 87) (87 . 87) (73 . 73) (71 . 71) (#f . #f) (80 .
    80) (14 . 14) (141 . 141) (141 . 141) (141 . 141) (141 . 141) (141 .
    141) (89 . 89) (6 . 6) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (#f .
    #f) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (5 . 5) (1 . 1)
    (141 . 141) (141 . 141) (141 . 141) (141 . 141) (141 . 141) (141 . 141)
    (141 . 141) (113 . 113) (112 . 112) (141 . 141) (141 . 141) (109 . 109)
    (141 . 141) (141 . 141) (141 . 141) (141 . 141) (141 . 141) (141 . 141)
    (141 . 141) (141 . 141) (141 . 141) (83 . 83) (79 . 79) (141 . 141) (72
    . 72) (101 . 101) (141 . 141) (141 . 141) (141 . 141) (141 . 141) (141
    . 141) (141 . 141) (141 . 141) (141 . 141) (141 . 141) (51 . 51) (141 .
    141) (141 . 141) (141 . 141) (141 . 141) (141 . 141) (141 . 141) (141 .
    141) (70 . 70) (27 . 27) (141 . 141) (#f . #f) (#f . #f) (#f . #f) (40
    . 40) (#f . #f) (102 . 102) (99 . 99) (#f . #f) (88 . 88) (#f . #f) (#f
    . #f) (141 . 141) (141 . 141) (141 . 141) (141 . 141) (141 . 141) (141
    . 141) (141 . 141) (141 . 141) (141 . 141) (141 . 141) (141 . 141) (141
    . 141) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (#f
    . #f) (#f . #f) (#f . #f) (#f . #f) (77 . 77) (141 . 141) (141 . 141)
    (141 . 141) (139 . 139) (138 . 138) (141 . 141) (141 . 141) (141 . 141)
    (141 . 141) (141 . 141) (141 . 141) (141 . 141) (141 . 141) (141 . 141)
    (141 . 141) (141 . 141) (75 . 75) (141 . 141) (141 . 141) (141 . 141)
    (141 . 141) (141 . 141) (56 . 56) (141 . 141) (141 . 141) (141 . 141)
    (141 . 141) (141 . 141) (141 . 141) (47 . 47) (141 . 141) (141 . 141)
    (141 . 141) (141 . 141) (141 . 141) (141 . 141) (37 . 37) (36 . 36)
    (141 . 141) (#f . #f) (#f . #f) (40 . 40) (#f . #f) (#f . #f) (#f . #f)
    (#f . #f) (#f . #f) (#f . #f) (141 . 141) (141 . 141) (141 . 141) (141
    . 141) (141 . 141) (141 . 141) (141 . 141) (141 . 141) (141 . 141) (141
    . 141) (141 . 141) (141 . 141) (9 . 9) (141 . 141) (4 . 4) (#f . #f)
    (#f . #f) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (#f . #f)
    (#f . #f) (141 . 141) (141 . 141) (140 . 140) (115 . 115) (110 . 110)
    (108 . 108) (141 . 141) (141 . 141) (141 . 141) (141 . 141) (141 . 141)
    (141 . 141) (141 . 141) (91 . 91) (67 . 67) (141 . 141) (141 . 141)
    (141 . 141) (141 . 141) (141 . 141) (141 . 141) (141 . 141) (50 . 50)
    (141 . 141) (141 . 141) (141 . 141) (44 . 44) (141 . 141) (141 . 141)
    (39 . 39) (141 . 141) (141 . 141) (#f . #f) (40 . 40) (#f . #f) (#f .
    #f) (#f . #f) (#f . #f) (28 . 28) (#f . #f) (87 . 87) (#f . #f) (141 .
    141) (141 . 141) (141 . 141) (141 . 141) (141 . 141) (141 . 141) (141 .
    141) (141 . 141) (18 . 18) (13 . 13) (11 . 11) (10 . 10) (8 . 8) (#f .
    #f) (#f . #f) (#f . #f) (#f . #f) (64 . 64) (#f . #f) (#f . #f) (#f .
    #f) (#f . #f) (#f . #f) (2 . 2) (141 . 141) (107 . 107) (103 . 103) (97
    . 97) (141 . 141) (93 . 93) (141 . 141) (141 . 141) (141 . 141) (141 .
    141) (141 . 141) (141 . 141) (55 . 55) (141 . 141) (141 . 141) (141 .
    141) (141 . 141) (141 . 141) (42 . 42) (141 . 141) (38 . 38) (141 .
    141) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (40 . 40) (84 .
    84) (#f . #f) (40 . 40) (#f . #f) (#f . #f) (141 . 141) (141 . 141)
    (141 . 141) (141 . 141) (141 . 141) (141 . 141) (141 . 141) (141 . 141)
    (#f . #f) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (32 . 32)
    (#f . #f) (7 . 7) (#f . #f) (141 . 141) (95 . 95) (141 . 141) (92 . 92)
    (141 . 141) (141 . 141) (61 . 61) (141 . 141) (141 . 141) (52 . 52) (48
    . 48) (141 . 141) (141 . 141) (141 . 141) (141 . 141) (24 . 24) (#f .
    #f) (#f . #f) (#f . #f) (40 . 40) (84 . 84) (#f . #f) (40 . 40) (#f .
    #f) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (141 . 141) (141
    . 141) (141 . 141) (141 . 141) (141 . 141) (25 . 25) (141 . 141) (141 .
    141) (111 . 111) (#f . #f) (#f . #f) (#f . #f) (34 . 34) (#f . #f) (#f
    . #f) (3 . 3) (141 . 141) (141 . 141) (141 . 141) (141 . 141) (141 .
    141) (141 . 141) (141 . 141) (53 . 53) (46 . 46) (141 . 141) (141 .
    141) (141 . 141) (#f . #f) (40 . 40) (84 . 84) (#f . #f) (40 . 40) (#f
    . #f) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (141
    . 141) (74 . 74) (141 . 141) (141 . 141) (49 . 49) (141 . 141) (21 .
    21) (105 . 105) (85 . 85) (#f . #f) (33 . 33) (#f . #f) (0 . 0) (141 .
    141) (94 . 94) (66 . 66) (141 . 141) (141 . 141) (141 . 141) (45 . 45)
    (141 . 141) (141 . 141) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (#f .
    #f) (84 . 84) (#f . #f) (40 . 40) (#f . #f) (141 . 141) (141 . 141)
    (141 . 141) (12 . 12) (65 . 65) (141 . 141) (63 . 63) (141 . 141) (59 .
    59) (43 . 43) (41 . 41) (#f . #f) (84 . 84) (#f . #f) (40 . 40) (#f .
    #f) (#f . #f) (#f . #f) (141 . 141) (141 . 141) (141 . 141) (141 . 141)
    (141 . 141) (84 . 84) (#f . #f) (40 . 40) (#f . #f) (#f . #f) (#f . #f)
    (141 . 141) (141 . 141) (141 . 141) (96 . 96) (62 . 62) (#f . #f) (#f .
    #f) (#f . #f) (141 . 141) (141 . 141) (54 . 54) (#f . #f) (82 . 82)
    (141 . 141) (#f . #f) (141 . 141) (#f . #f) (57 . 57) (#f . #f) (#f .
    #f) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (#f . #f))))

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
