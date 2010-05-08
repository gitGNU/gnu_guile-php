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
                 (if (eq? parse-mode 'txt) (read-inline-html yygetc yyungetc) (lexer-error (yygetc)))
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
                (read-parse-string #\" yygetc yyungetc) 
        ))
    #f
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yyline)
                (read-const-string #\' yygetc yyungetc)
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
                (make-token 'T_DEFAULT yytext)
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
                (make-token 'T_STRING_CAST yytext)
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
                 (make-token 'period yytext)
        ))
    #t
    (lambda (yycontinue yygetc yyungetc)
      (lambda (yytext yyline)
                 (make-token 'colon yytext)
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
    err 1)) (96 (95 err 1) (97 err (123 1 err)))) (48 err (58 27 err)) (88
    (48 err (56 79 err)) (120 (89 80 err) (121 80 err))) (95 (58 (48 err 1)
    (65 err (91 1 err))) (105 (= 96 err 1) (106 81 (123 1 err)))) (= 61 82
    err) (62 (61 err 84) (63 83 err)) (49 (44 (43 err 86) (48 err 28)) (61
    (58 27 err) (62 85 err))) (102 (91 (58 (48 err 1) (65 err 1)) (96 (95
    err 1) (97 err 1))) (111 (109 (103 90 1) (110 89 88)) (116 (115 1 87)
    (123 1 err)))) (96 (65 (48 err (58 1 err)) (91 1 (95 err 1))) (109 (97
    err (108 1 92)) (112 (111 1 91) (123 1 err)))) (98 (91 (58 (48 err 1)
    (65 err 1)) (96 (95 err 1) (97 err 93))) (112 (106 (105 1 96) (111 1
    95)) (118 (117 1 94) (123 1 err)))) (100 (91 (58 (48 err 1) (65 err 1))
    (96 (95 err 1) (97 err (99 1 102)))) (118 (109 (108 1 101) (110 100
    (111 99 1))) (120 (119 98 1) (121 97 (123 1 err))))) (= 58 103 err) (62
    (61 err 104) (63 105 err)) (95 (56 (55 err 106) (65 err (91 106 err)))
    (97 (96 106 err) (123 106 (124 107 err)))) (95 (48 (= 46 109 err) (65
    (58 1 err) (91 1 err))) (102 (97 (96 1 err) (101 1 110)) (112 (111 1
    108) (123 1 err)))) (49 (46 (45 err 112) (48 err 28)) (62 (58 27 err)
    (63 111 err))) (= 36 113 err) err err (= 61 114 err) (47 (= 42 116 err)
    (61 (48 47 err) (62 115 err))) err (61 (47 (= 37 119 err) (48 123 (60
    err 117))) (64 (62 121 (63 122 120)) (= 115 118 err))) (62 (61 err 124)
    (63 125 err)) (= 62 125 err) (91 (58 (48 err 1) (65 err 1)) (96 (95 err
    126) (97 err (123 1 err)))) (97 (65 (48 err (58 1 err)) (95 (91 1 err)
    (96 1 err))) (109 (98 129 (108 1 128)) (112 (111 1 127) (123 1 err))))
    (95 (58 (48 err 1) (65 err (91 1 err))) (114 (= 96 err 1) (115 130 (123
    1 err)))) (62 (61 err 131) (= 124 132 err)) (= 97 133 err) (105 (100
    (98 (97 err 142) (99 141 err)) (102 (101 139 err) (103 138 err))) (114
    (111 (106 137 err) (112 136 err)) (116 (115 140 135) (= 117 134 err))))
    (39 (38 err 143) (= 61 144 err)) (97 (65 (48 err (58 1 err)) (95 (91 1
    err) (96 1 err))) (111 (99 (98 1 147) (110 1 145)) (115 (114 1 146)
    (123 1 err)))) (77 (58 (48 err 1) (65 err (76 1 148))) (96 (91 1 (95
    err 1)) (97 err (123 1 err)))) (86 (58 (48 err 1) (65 err (85 1 149)))
    (96 (91 1 (95 err 1)) (97 err (123 1 err)))) (77 (58 (48 err 1) (65 err
    (76 1 150))) (96 (91 1 (95 err 1)) (97 err (123 1 err)))) err (95 (58
    (48 err 1) (65 err (91 1 err))) (105 (= 96 err 1) (106 151 (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (114 (= 96 err 1) (115
    152 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (101 (= 96
    err 1) (102 153 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err)))
    (115 (= 96 err 1) (116 154 (123 1 err)))) (96 (65 (48 err (58 1 err))
    (91 1 (95 err 1))) (118 (97 err (117 1 155)) (122 (121 1 156) (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (114 (= 96 err 1) (115
    157 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (105 (= 96
    err 1) (106 158 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err)))
    (97 (96 1 err) (98 159 (123 1 err)))) (96 (65 (48 err (58 1 err)) (91 1
    (95 err 1))) (114 (97 err (113 1 161)) (117 (116 1 160) (123 1 err))))
    (95 (58 (48 err 1) (65 err (91 1 err))) (98 (= 96 err 1) (99 162 (123 1
    err)))) (96 (65 (48 err (58 1 err)) (91 1 (95 err 1))) (106 (97 err
    (105 1 164)) (112 (111 1 163) (123 1 err)))) (95 (58 (48 err 1) (65 err
    (91 1 err))) (108 (= 96 err 1) (109 165 (123 1 err)))) (95 (58 (48 err
    1) (65 err (91 1 err))) (119 (= 96 err 1) (120 166 (123 1 err)))) err
    (95 (58 (48 err 1) (65 err (91 1 err))) (114 (= 96 err 1) (115 167 (123
    1 err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97 err (123 1
    err)))) (48 err (56 79 err)) (65 (48 err (58 168 err)) (97 (71 168 err)
    (103 168 err))) (95 (58 (48 err 1) (65 err (91 1 err))) (115 (= 96 err
    1) (116 169 (123 1 err)))) (= 61 170 err) (= 61 171 err) err err err
    (95 (58 (48 err 1) (65 err (91 1 err))) (115 (= 96 err 1) (116 172 (123
    1 err)))) (97 (65 (48 err (58 1 err)) (95 (91 1 err) (96 1 err))) (115
    (= 99 175 1) (117 (116 174 173) (123 1 err)))) (95 (58 (48 err 1) (65
    err (91 1 err))) (112 (= 96 err 1) (113 176 (123 1 err)))) (91 (58 (48
    err 1) (65 err 1)) (96 (95 err 1) (97 err (123 1 err)))) (95 (58 (48
    err 1) (65 err (91 1 err))) (116 (= 96 err 1) (117 177 (123 1 err))))
    (95 (58 (48 err 1) (65 err (91 1 err))) (111 (= 96 err 1) (112 178 (123
    1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (108 (= 96 err 1)
    (109 179 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (110 (=
    96 err 1) (111 180 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1
    err))) (114 (= 96 err 1) (115 181 (123 1 err)))) (95 (58 (48 err 1) (65
    err (91 1 err))) (110 (= 96 err 1) (111 182 (123 1 err)))) (95 (58 (48
    err 1) (65 err (91 1 err))) (116 (= 96 err 1) (117 183 (123 1 err))))
    (95 (58 (48 err 1) (65 err (91 1 err))) (97 (96 1 err) (98 184 (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (100 (= 96 err 1) (101
    185 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (112 (= 96
    err 1) (113 186 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err)))
    (115 (= 96 err 1) (116 187 (123 1 err)))) (95 (58 (48 err 1) (65 err
    (91 1 err))) (104 (= 96 err 1) (105 188 (123 1 err)))) err (= 61 189
    err) err (91 (58 (48 err 106) (65 err 106)) (96 (95 err 106) (97 err
    (123 106 err)))) err (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97
    err (123 1 err)))) (= 100 190 err) (96 (65 (48 err (58 1 err)) (91 1
    (95 err 1))) (100 (97 err (99 1 192)) (103 (102 1 191) (123 1 err))))
    err err err err err (= 42 193 err) (61 (60 err 194) (62 195 err)) (= 99
    196 err) (= 61 197 err) (62 (61 err 197) (= 112 198 err)) err err (=
    115 199 err) err err (77 (68 (58 (48 err 1) (65 err (67 1 205))) (70
    (69 204 1) (71 203 (76 1 201)))) (96 (79 (78 1 200) (91 1 (95 err 1)))
    (104 (97 err 1) (105 202 (123 1 err))))) (95 (58 (48 err 1) (65 err (91
    1 err))) (110 (= 96 err 1) (111 206 (123 1 err)))) (96 (65 (48 err (58
    1 err)) (91 1 (95 err 1))) (111 (97 err (98 208 1)) (112 207 (123 1
    err)))) (96 (65 (48 err (58 1 err)) (91 1 (95 err 1))) (116 (97 err
    (115 1 210)) (117 209 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1
    err))) (101 (= 96 err 1) (102 211 (123 1 err)))) err err (= 115 212
    err) (= 110 213 err) (= 116 214 err) (= 98 215 err) (= 110 216 err) (=
    108 217 err) (= 111 218 err) (= 101 219 err) (= 111 220 err) (= 114 221
    err) err err (95 (58 (48 err 1) (65 err (91 1 err))) (100 (= 96 err 1)
    (101 222 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (114 (=
    96 err 1) (115 223 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1
    err))) (115 (= 96 err 1) (116 224 (123 1 err)))) (84 (58 (48 err 1) (65
    err (83 1 225))) (96 (91 1 (95 err 1)) (97 err (123 1 err)))) (70 (58
    (48 err 1) (65 err (69 1 226))) (96 (91 1 (95 err 1)) (97 err (123 1
    err)))) (77 (58 (48 err 1) (65 err (76 1 227))) (96 (91 1 (95 err 1))
    (97 err (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (108 (=
    96 err 1) (109 228 (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96
    (95 err 1) (97 err (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96
    (95 err 1) (97 err (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1
    err))) (101 (= 96 err 1) (102 229 (123 1 err)))) (95 (58 (48 err 1) (65
    err (91 1 err))) (101 (= 96 err 1) (102 226 (123 1 err)))) (91 (58 (48
    err 1) (65 err 1)) (96 (95 err 1) (97 err (123 1 err)))) (95 (58 (48
    err 1) (65 err (91 1 err))) (111 (= 96 err 1) (112 230 (123 1 err))))
    (95 (58 (48 err 1) (65 err (91 1 err))) (116 (= 96 err 1) (117 231 (123
    1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (116 (= 96 err 1)
    (117 232 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (117 (=
    96 err 1) (118 233 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1
    err))) (117 (= 96 err 1) (118 234 (123 1 err)))) (95 (58 (48 err 1) (65
    err (91 1 err))) (108 (= 96 err 1) (109 235 (123 1 err)))) (95 (58 (48
    err 1) (65 err (91 1 err))) (116 (= 96 err 1) (117 236 (123 1 err))))
    (96 (65 (48 err (58 1 err)) (91 1 (95 err 1))) (111 (97 err (110 1
    238)) (119 (118 1 237) (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1
    err))) (108 (= 96 err 1) (109 227 (123 1 err)))) (91 (58 (48 err 1) (65
    err 1)) (96 (95 err 1) (97 err (123 1 err)))) (91 (58 (48 err 1) (65
    err 1)) (96 (95 err 1) (97 err (123 1 err)))) (65 (48 err (58 168 err))
    (97 (71 168 err) (103 168 err))) (95 (58 (48 err 1) (65 err (91 1
    err))) (116 (= 96 err 1) (117 239 (123 1 err)))) err err (95 (58 (48
    err 1) (65 err (91 1 err))) (101 (= 96 err 1) (102 240 (123 1 err))))
    (95 (58 (48 err 1) (65 err (91 1 err))) (101 (= 96 err 1) (102 241 (123
    1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (116 (= 96 err 1)
    (117 242 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (108 (=
    96 err 1) (109 243 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1
    err))) (108 (= 96 err 1) (109 244 (123 1 err)))) (95 (58 (48 err 1) (65
    err (91 1 err))) (111 (= 96 err 1) (112 245 (123 1 err)))) (95 (58 (48
    err 1) (65 err (91 1 err))) (98 (= 96 err 1) (99 246 (123 1 err)))) (95
    (58 (48 err 1) (65 err (91 1 err))) (115 (= 96 err 1) (116 247 (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (99 (= 96 err 1) (100
    248 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (101 (= 96
    err 1) (102 249 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err)))
    (97 (96 1 err) (98 250 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1
    err))) (101 (= 96 err 1) (102 251 (123 1 err)))) (95 (58 (48 err 1) (65
    err (91 1 err))) (108 (= 96 err 1) (109 252 (123 1 err)))) (101 (91 (58
    (48 err 1) (65 err 1)) (96 (95 err 1) (97 err (100 1 257)))) (115 (103
    (102 1 256) (= 105 255 1)) (119 (116 254 1) (120 253 (123 1 err)))))
    (95 (58 (48 err 1) (65 err (91 1 err))) (116 (= 96 err 1) (117 258 (123
    1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (101 (= 96 err 1)
    (102 259 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (111 (=
    96 err 1) (112 260 (123 1 err)))) err err (95 (58 (48 err 1) (65 err
    (91 1 err))) (97 (96 1 err) (98 261 (123 1 err)))) (95 (58 (48 err 1)
    (65 err (91 1 err))) (108 (= 96 err 1) (109 262 (123 1 err)))) (11 (10
    193 err) (= 42 263 193)) err err (= 114 264 err) err (= 104 265 err) (=
    99 266 err) (91 (58 (48 err 1) (65 err (66 267 1))) (96 (95 err 1) (97
    err (123 1 err)))) (74 (58 (48 err 1) (65 err (73 1 268))) (96 (91 1
    (95 err 1)) (97 err (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1
    err))) (97 (96 1 err) (98 269 (123 1 err)))) (85 (65 (48 err (58 1
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
    292 (123 1 err)))) (70 (58 (48 err 1) (65 err (69 1 293))) (96 (91 1
    (95 err 1)) (97 err (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96
    (95 err 1) (97 err (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96
    (95 err 1) (97 err (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1
    err))) (101 (= 96 err 1) (102 294 (123 1 err)))) (95 (58 (48 err 1) (65
    err (91 1 err))) (116 (= 96 err 1) (117 295 (123 1 err)))) (95 (58 (48
    err 1) (65 err (91 1 err))) (119 (= 96 err 1) (120 296 (123 1 err))))
    (95 (58 (48 err 1) (65 err (91 1 err))) (99 (= 96 err 1) (100 297 (123
    1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (105 (= 96 err 1)
    (106 298 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (114 (=
    96 err 1) (115 299 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1
    err))) (105 (= 96 err 1) (106 300 (123 1 err)))) (95 (58 (48 err 1) (65
    err (91 1 err))) (105 (= 96 err 1) (106 301 (123 1 err)))) (95 (58 (48
    err 1) (65 err (91 1 err))) (101 (= 96 err 1) (102 302 (123 1 err))))
    (95 (58 (48 err 1) (65 err (91 1 err))) (97 (96 1 err) (98 303 (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (116 (= 96 err 1) (117
    304 (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97
    err (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (116 (= 96
    err 1) (117 305 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err)))
    (114 (= 96 err 1) (115 306 (123 1 err)))) (95 (58 (48 err 1) (65 err
    (91 1 err))) (97 (96 1 err) (98 307 (123 1 err)))) (95 (58 (48 err 1)
    (65 err (91 1 err))) (117 (= 96 err 1) (118 308 (123 1 err)))) (95 (58
    (48 err 1) (65 err (91 1 err))) (101 (= 96 err 1) (102 309 (123 1
    err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97 err (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (97 (96 1 err) (98 310
    (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (101 (= 96 err
    1) (102 293 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (116
    (= 96 err 1) (117 311 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1
    err))) (97 (96 1 err) (98 312 (123 1 err)))) (95 (58 (48 err 1) (65 err
    (91 1 err))) (108 (= 96 err 1) (109 313 (123 1 err)))) (95 (58 (48 err
    1) (65 err (91 1 err))) (110 (= 96 err 1) (111 314 (123 1 err)))) (91
    (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97 err (123 1 err)))) (95
    (58 (48 err 1) (65 err (91 1 err))) (104 (= 96 err 1) (105 315 (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (119 (= 96 err 1) (120
    316 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (102 (= 96
    err 1) (103 317 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err)))
    (111 (= 96 err 1) (112 318 (123 1 err)))) (95 (58 (48 err 1) (65 err
    (91 1 err))) (101 (= 96 err 1) (102 319 (123 1 err)))) (95 (58 (48 err
    1) (65 err (91 1 err))) (121 (= 96 err 1) (122 320 (123 1 err)))) (95
    (58 (48 err 1) (65 err (91 1 err))) (105 (= 96 err 1) (106 321 (123 1
    err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97 err (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (117 (= 96 err 1) (118
    322 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (97 (96 1
    err) (98 323 (123 1 err)))) (42 (= 10 err 193) (47 (43 263 193) (48 324
    193))) (= 105 325 err) (= 112 326 err) (= 114 327 err) (78 (58 (48 err
    1) (65 err (77 1 328))) (96 (91 1 (95 err 1)) (97 err (123 1 err))))
    (79 (58 (48 err 1) (65 err (78 1 329))) (96 (91 1 (95 err 1)) (97 err
    (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (108 (= 96 err
    1) (109 330 (123 1 err)))) (79 (58 (48 err 1) (65 err (78 1 331))) (96
    (91 1 (95 err 1)) (97 err (123 1 err)))) (77 (58 (48 err 1) (65 err (76
    1 332))) (96 (91 1 (95 err 1)) (97 err (123 1 err)))) (83 (58 (48 err
    1) (65 err (82 1 333))) (96 (91 1 (95 err 1)) (97 err (123 1 err))))
    (91 (58 (48 err 1) (65 err (66 334 1))) (96 (95 err 1) (97 err (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (105 (= 96 err 1) (106
    335 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (116 (= 96
    err 1) (117 336 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err)))
    (101 (= 96 err 1) (102 337 (123 1 err)))) (95 (58 (48 err 1) (65 err
    (91 1 err))) (115 (= 96 err 1) (116 338 (123 1 err)))) (95 (58 (48 err
    1) (65 err (91 1 err))) (104 (= 96 err 1) (105 339 (123 1 err)))) (91
    (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97 err (123 1 err)))) (95
    (58 (48 err 1) (65 err (91 1 err))) (107 (= 96 err 1) (108 340 (123 1
    err)))) err (= 101 341 err) (= 105 342 err) (= 101 343 err) (42 (41 err
    345) (= 101 344 err)) (= 97 346 err) (= 98 347 err) (= 108 348 err) (=
    108 349 err) (= 97 350 err) (95 (58 (48 err 1) (65 err (91 1 err)))
    (121 (= 96 err 1) (122 351 (123 1 err)))) (95 (58 (48 err 1) (65 err
    (91 1 err))) (114 (= 96 err 1) (115 352 (123 1 err)))) (91 (58 (48 err
    1) (65 err 1)) (96 (95 err 1) (97 err (123 1 err)))) (91 (58 (48 err 1)
    (65 err 1)) (96 (95 err 1) (97 err (123 1 err)))) (91 (58 (48 err 1)
    (65 err 1)) (96 (95 err 1) (97 err (123 1 err)))) (91 (58 (48 err 1)
    (65 err 1)) (96 (95 err 1) (97 err (123 1 err)))) (95 (58 (48 err 1)
    (65 err (91 1 err))) (104 (= 96 err 1) (105 353 (123 1 err)))) (95 (58
    (48 err 1) (65 err (91 1 err))) (99 (= 96 err 1) (100 354 (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (110 (= 96 err 1) (111
    355 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (114 (= 96
    err 1) (115 356 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err)))
    (99 (= 96 err 1) (100 357 (123 1 err)))) (95 (58 (48 err 1) (65 err (91
    1 err))) (99 (= 96 err 1) (100 358 (123 1 err)))) (95 (58 (48 err 1)
    (65 err (91 1 err))) (116 (= 96 err 1) (117 359 (123 1 err)))) (91 (58
    (48 err 1) (65 err 1)) (96 (95 err 1) (97 err (123 1 err)))) (91 (58
    (48 err 1) (65 err 1)) (96 (95 err 1) (97 err (123 1 err)))) (95 (58
    (48 err 1) (65 err (91 1 err))) (102 (= 96 err 1) (103 360 (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (110 (= 96 err 1) (111
    361 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (100 (= 96
    err 1) (101 362 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err)))
    (109 (= 96 err 1) (110 363 (123 1 err)))) (95 (58 (48 err 1) (65 err
    (91 1 err))) (108 (= 96 err 1) (109 364 (123 1 err)))) (95 (58 (48 err
    1) (65 err (91 1 err))) (105 (= 96 err 1) (106 365 (123 1 err)))) (95
    (58 (48 err 1) (65 err (91 1 err))) (99 (= 96 err 1) (100 366 (123 1
    err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97 err (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (100 (= 96 err 1) (101
    367 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (105 (= 96
    err 1) (106 368 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err)))
    (105 (= 96 err 1) (106 369 (123 1 err)))) (91 (58 (48 err 1) (65 err
    1)) (96 (95 err 1) (97 err (123 1 err)))) (95 (58 (48 err 1) (65 err
    (91 1 err))) (114 (= 96 err 1) (115 370 (123 1 err)))) (95 (58 (48 err
    1) (65 err (91 1 err))) (99 (= 96 err 1) (100 371 (123 1 err)))) (91
    (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97 err (123 1 err)))) (95
    (58 (48 err 1) (65 err (91 1 err))) (102 (= 96 err 1) (103 372 (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (108 (= 96 err 1) (109
    373 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (114 (= 96
    err 1) (115 374 (123 1 err)))) (11 (10 193 err) (= 42 263 193)) (= 112
    375 err) err (= 105 376 err) (70 (58 (48 err 1) (65 err (69 1 377)))
    (96 (91 1 (95 err 1)) (97 err (123 1 err)))) (70 (58 (48 err 1) (65 err
    (69 1 378))) (96 (91 1 (95 err 1)) (97 err (123 1 err)))) (95 (58 (48
    err 1) (65 err (91 1 err))) (116 (= 96 err 1) (117 379 (123 1 err))))
    (68 (58 (48 err 1) (65 err (67 1 380))) (96 (91 1 (95 err 1)) (97 err
    (123 1 err)))) (70 (58 (48 err 1) (65 err (69 1 381))) (96 (91 1 (95
    err 1)) (97 err (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95
    err 382) (97 err (123 1 err)))) (84 (58 (48 err 1) (65 err (83 1 383)))
    (96 (91 1 (95 err 1)) (97 err (123 1 err)))) (95 (58 (48 err 1) (65 err
    (91 1 err))) (110 (= 96 err 1) (111 384 (123 1 err)))) (91 (58 (48 err
    1) (65 err 1)) (96 (95 err 1) (97 err (123 1 err)))) (91 (58 (48 err 1)
    (65 err 1)) (96 (95 err 1) (97 err (123 1 err)))) (91 (58 (48 err 1)
    (65 err 1)) (96 (95 err 1) (97 err (123 1 err)))) (91 (58 (48 err 1)
    (65 err 1)) (96 (95 err 1) (97 err (123 1 err)))) (91 (58 (48 err 1)
    (65 err 1)) (96 (95 err 1) (97 err (123 1 err)))) (= 116 385 err) (=
    110 386 err) (= 99 387 err) (= 103 388 err) err (= 116 389 err) (= 108
    390 err) (= 41 391 err) (42 (41 err 393) (= 101 392 err)) (= 121 394
    err) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97 err (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (97 (96 1 err) (98 395
    (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97 err
    (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97 err
    (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97 err
    (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (101 (= 96 err
    1) (102 396 (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err
    1) (97 err (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (116
    (= 96 err 1) (117 397 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1
    err))) (101 (= 96 err 1) (102 398 (123 1 err)))) (95 (58 (48 err 1) (65
    err (91 1 err))) (97 (96 1 err) (98 399 (123 1 err)))) (95 (58 (48 err
    1) (65 err (91 1 err))) (99 (= 96 err 1) (100 400 (123 1 err)))) (95
    (58 (48 err 1) (65 err (91 1 err))) (101 (= 96 err 1) (102 401 (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (101 (= 96 err 1) (102
    402 (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97
    err (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (111 (= 96
    err 1) (112 403 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err)))
    (104 (= 96 err 1) (105 404 (123 1 err)))) (95 (58 (48 err 1) (65 err
    (91 1 err))) (115 (= 96 err 1) (116 405 (123 1 err)))) (95 (58 (48 err
    1) (65 err (91 1 err))) (108 (= 96 err 1) (109 406 (123 1 err)))) (95
    (58 (48 err 1) (65 err (91 1 err))) (116 (= 96 err 1) (117 407 (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (101 (= 96 err 1) (102
    408 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (108 (= 96
    err 1) (109 409 (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95
    err 1) (97 err (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err)))
    (116 (= 96 err 1) (117 410 (123 1 err)))) (95 (58 (48 err 1) (65 err
    (91 1 err))) (101 (= 96 err 1) (102 411 (123 1 err)))) (= 116 412 err)
    (= 112 413 err) (84 (58 (48 err 1) (65 err (83 1 414))) (96 (91 1 (95
    err 1)) (97 err (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95
    err 415) (97 err (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95
    err 416) (97 err (123 1 err)))) (85 (58 (48 err 1) (65 err (84 1 417)))
    (96 (91 1 (95 err 1)) (97 err (123 1 err)))) (91 (58 (48 err 1) (65 err
    1)) (96 (95 err 418) (97 err (123 1 err)))) (91 (58 (48 err 1) (65 err
    1)) (96 (95 err 419) (97 err (123 1 err)))) (84 (58 (48 err 1) (65 err
    (83 1 420))) (96 (91 1 (95 err 1)) (97 err (123 1 err)))) (95 (58 (48
    err 1) (65 err (91 1 err))) (117 (= 96 err 1) (118 421 (123 1 err))))
    (= 41 422 err) (= 103 423 err) (= 116 424 err) (= 101 425 err) (= 41
    426 err) (= 101 427 err) err (= 97 428 err) err (= 41 429 err) (95 (58
    (48 err 1) (65 err (91 1 err))) (99 (= 96 err 1) (100 430 (123 1
    err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 431) (97 err (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (101 (= 96 err 1) (102
    432 (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97
    err (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (99 (= 96
    err 1) (100 433 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err)))
    (101 (= 96 err 1) (102 434 (123 1 err)))) (91 (58 (48 err 1) (65 err
    1)) (96 (95 err 435) (97 err (123 1 err)))) (95 (58 (48 err 1) (65 err
    (91 1 err))) (110 (= 96 err 1) (111 436 (123 1 err)))) (95 (58 (48 err
    1) (65 err (91 1 err))) (110 (= 96 err 1) (111 437 (123 1 err)))) (91
    (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97 err (123 1 err)))) (91
    (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97 err (123 1 err)))) (95
    (58 (48 err 1) (65 err (91 1 err))) (101 (= 96 err 1) (102 438 (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (99 (= 96 err 1) (100
    439 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (97 (96 1
    err) (98 440 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (97
    (96 1 err) (98 441 (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96
    (95 err 1) (97 err (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96
    (95 err 1) (97 err (123 1 err)))) (= 32 442 err) (= 116 443 err) (81
    (58 (48 err 1) (65 err (80 1 444))) (96 (91 1 (95 err 1)) (97 err (123
    1 err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 445) (97 err (123
    1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (99 (= 96 err 1) (100
    446 (123 1 err)))) (74 (58 (48 err 1) (65 err (73 1 447))) (96 (91 1
    (95 err 1)) (97 err (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96
    (95 err 448) (97 err (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96
    (95 err 1) (97 err (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96
    (95 err 449) (97 err (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1
    err))) (101 (= 96 err 1) (102 450 (123 1 err)))) err (= 41 451 err) (=
    41 452 err) (= 114 453 err) err (= 41 454 err) (= 110 455 err) err (95
    (58 (48 err 1) (65 err (91 1 err))) (116 (= 96 err 1) (117 456 (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (111 (= 96 err 1) (112
    457 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (100 (= 96
    err 1) (101 458 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err)))
    (101 (= 96 err 1) (102 459 (123 1 err)))) (95 (58 (48 err 1) (65 err
    (91 1 err))) (111 (= 96 err 1) (112 460 (123 1 err)))) (95 (58 (48 err
    1) (65 err (91 1 err))) (111 (= 96 err 1) (112 461 (123 1 err)))) (95
    (58 (48 err 1) (65 err (91 1 err))) (116 (= 96 err 1) (117 462 (123 1
    err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97 err (123 1
    err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97 err (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (104 (= 96 err 1) (105
    463 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (99 (= 96
    err 1) (100 464 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err)))
    (114 (= 96 err 1) (115 465 (123 1 err)))) (= 108 466 err) (= 62 125
    err) (91 (58 (48 err 1) (65 err (66 467 1))) (96 (95 err 1) (97 err
    (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97 err
    (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (111 (= 96 err
    1) (112 468 (123 1 err)))) (80 (58 (48 err 1) (65 err (79 1 469))) (96
    (91 1 (95 err 1)) (97 err (123 1 err)))) (91 (58 (48 err 1) (65 err 1))
    (96 (95 err 1) (97 err (123 1 err)))) (91 (58 (48 err 1) (65 err 1))
    (96 (95 err 470) (97 err (123 1 err)))) (91 (58 (48 err 1) (65 err 1))
    (96 (95 err 1) (97 err (123 1 err)))) err err (= 41 471 err) err (= 41
    393 err) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97 err (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (110 (= 96 err 1) (111
    472 (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97
    err (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97
    err (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (102 (= 96
    err 1) (103 473 (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err)))
    (110 (= 96 err 1) (111 474 (123 1 err)))) (95 (58 (48 err 1) (65 err
    (91 1 err))) (115 (= 96 err 1) (116 475 (123 1 err)))) (91 (58 (48 err
    1) (65 err 1)) (96 (95 err 1) (97 err (123 1 err)))) (95 (58 (48 err 1)
    (65 err (91 1 err))) (104 (= 96 err 1) (105 476 (123 1 err)))) (95 (58
    (48 err 1) (65 err (91 1 err))) (101 (= 96 err 1) (102 477 (123 1
    err)))) (= 97 478 err) (68 (58 (48 err 1) (65 err (67 1 479))) (96 (91
    1 (95 err 1)) (97 err (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1
    err))) (109 (= 96 err 1) (110 480 (123 1 err)))) (79 (58 (48 err 1) (65
    err (78 1 481))) (96 (91 1 (95 err 1)) (97 err (123 1 err)))) (91 (58
    (48 err 1) (65 err 1)) (96 (95 err 1) (97 err (123 1 err)))) err (95
    (58 (48 err 1) (65 err (91 1 err))) (99 (= 96 err 1) (100 482 (123 1
    err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97 err (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (99 (= 96 err 1) (100
    483 (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97
    err (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97
    err (123 1 err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97
    err (123 1 err)))) (= 110 484 err) (70 (58 (48 err 1) (65 err (69 1
    485))) (96 (91 1 (95 err 1)) (97 err (123 1 err)))) (95 (58 (48 err 1)
    (65 err (91 1 err))) (112 (= 96 err 1) (113 486 (123 1 err)))) (91 (58
    (48 err 1) (65 err 1)) (96 (95 err 487) (97 err (123 1 err)))) (95 (58
    (48 err 1) (65 err (91 1 err))) (101 (= 96 err 1) (102 488 (123 1
    err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (101 (= 96 err 1) (102
    489 (123 1 err)))) (= 103 490 err) (91 (58 (48 err 1) (65 err 1)) (96
    (95 err 491) (97 err (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1
    err))) (105 (= 96 err 1) (106 492 (123 1 err)))) (91 (58 (48 err 1) (65
    err 1)) (96 (95 err 493) (97 err (123 1 err)))) (91 (58 (48 err 1) (65
    err 1)) (96 (95 err 1) (97 err (123 1 err)))) (91 (58 (48 err 1) (65
    err 1)) (96 (95 err 1) (97 err (123 1 err)))) (= 117 494 err) (91 (58
    (48 err 1) (65 err 1)) (96 (95 err 495) (97 err (123 1 err)))) (95 (58
    (48 err 1) (65 err (91 1 err))) (108 (= 96 err 1) (109 496 (123 1
    err)))) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97 err (123 1
    err)))) (= 97 497 err) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1)
    (97 err (123 1 err)))) (95 (58 (48 err 1) (65 err (91 1 err))) (101 (=
    96 err 1) (102 498 (123 1 err)))) (= 103 499 err) (95 (58 (48 err 1)
    (65 err (91 1 err))) (114 (= 96 err 1) (115 500 (123 1 err)))) (= 101
    501 err) (91 (58 (48 err 1) (65 err 1)) (96 (95 err 1) (97 err (123 1
    err)))) (= 61 502 err) (= 34 503 err) (= 112 504 err) (= 104 505 err)
    (= 112 506 err) (= 34 507 err) (= 62 326 err))
   '#((#f . #f) (140 . 140) (140 . 140) (140 . 140) (140 . 140) (127 . 127)
    (126 . 126) (123 . 123) (122 . 122) (121 . 121) (119 . 119) (#f . #f)
    (115 . 115) (114 . 114) (113 . 113) (140 . 140) (140 . 140) (140 . 140)
    (140 . 140) (140 . 140) (140 . 140) (140 . 140) (140 . 140) (128 . 128)
    (140 . 140) (140 . 140) (76 . 76) (76 . 76) (76 . 76) (140 . 140) (#f .
    #f) (135 . 135) (129 . 129) (140 . 140) (140 . 140) (140 . 140) (140 .
    140) (125 . 125) (132 . 132) (#f . #f) (140 . 140) (130 . 130) (120 .
    120) (20 . 20) (19 . 19) (124 . 124) (131 . 131) (15 . 15) (136 . 136)
    (#f . #f) (134 . 134) (140 . 140) (140 . 140) (140 . 140) (#f . #f)
    (116 . 116) (118 . 118) (#f . #f) (140 . 140) (140 . 140) (140 . 140)
    (140 . 140) (117 . 117) (140 . 140) (140 . 140) (140 . 140) (140 . 140)
    (140 . 140) (140 . 140) (140 . 140) (140 . 140) (140 . 140) (140 . 140)
    (140 . 140) (140 . 140) (140 . 140) (81 . 81) (140 . 140) (78 . 78) (76
    . 76) (#f . #f) (140 . 140) (71 . 71) (99 . 99) (69 . 69) (89 . 89) (60
    . 60) (140 . 140) (140 . 140) (140 . 140) (58 . 58) (140 . 140) (140 .
    140) (140 . 140) (140 . 140) (140 . 140) (140 . 140) (140 . 140) (140 .
    140) (140 . 140) (140 . 140) (140 . 140) (140 . 140) (36 . 36) (68 .
    68) (32 . 32) (111 . 111) (31 . 31) (30 . 30) (#f . #f) (140 . 140) (85
    . 85) (23 . 23) (22 . 22) (17 . 17) (27 . 27) (16 . 16) (97 . 97) (#f .
    #f) (86 . 86) (86 . 86) (73 . 73) (71 . 71) (#f . #f) (80 . 80) (14 .
    14) (140 . 140) (140 . 140) (140 . 140) (140 . 140) (140 . 140) (88 .
    88) (6 . 6) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (#f . #f)
    (#f . #f) (#f . #f) (#f . #f) (#f . #f) (5 . 5) (1 . 1) (140 . 140)
    (140 . 140) (140 . 140) (140 . 140) (140 . 140) (140 . 140) (140 . 140)
    (110 . 110) (109 . 109) (140 . 140) (140 . 140) (106 . 106) (140 . 140)
    (140 . 140) (140 . 140) (140 . 140) (140 . 140) (140 . 140) (140 . 140)
    (140 . 140) (140 . 140) (83 . 83) (79 . 79) (76 . 76) (140 . 140) (72 .
    72) (100 . 100) (140 . 140) (140 . 140) (140 . 140) (140 . 140) (140 .
    140) (140 . 140) (140 . 140) (140 . 140) (140 . 140) (51 . 51) (140 .
    140) (140 . 140) (140 . 140) (140 . 140) (140 . 140) (140 . 140) (140 .
    140) (70 . 70) (28 . 28) (140 . 140) (140 . 140) (#f . #f) (101 . 101)
    (98 . 98) (#f . #f) (87 . 87) (#f . #f) (#f . #f) (140 . 140) (140 .
    140) (140 . 140) (140 . 140) (140 . 140) (140 . 140) (140 . 140) (140 .
    140) (140 . 140) (140 . 140) (140 . 140) (140 . 140) (#f . #f) (#f .
    #f) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (#f .
    #f) (#f . #f) (77 . 77) (140 . 140) (140 . 140) (140 . 140) (138 . 138)
    (137 . 137) (140 . 140) (140 . 140) (140 . 140) (140 . 140) (140 . 140)
    (140 . 140) (140 . 140) (140 . 140) (140 . 140) (140 . 140) (140 . 140)
    (75 . 75) (140 . 140) (140 . 140) (140 . 140) (140 . 140) (140 . 140)
    (56 . 56) (140 . 140) (140 . 140) (140 . 140) (140 . 140) (140 . 140)
    (140 . 140) (47 . 47) (140 . 140) (140 . 140) (140 . 140) (140 . 140)
    (140 . 140) (140 . 140) (38 . 38) (37 . 37) (140 . 140) (140 . 140) (#f
    . #f) (#f . #f) (#f . #f) (#f . #f) (140 . 140) (140 . 140) (140 . 140)
    (140 . 140) (140 . 140) (140 . 140) (140 . 140) (140 . 140) (140 . 140)
    (140 . 140) (140 . 140) (140 . 140) (9 . 9) (140 . 140) (4 . 4) (#f .
    #f) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (#f .
    #f) (#f . #f) (140 . 140) (140 . 140) (139 . 139) (112 . 112) (107 .
    107) (105 . 105) (140 . 140) (140 . 140) (140 . 140) (140 . 140) (140 .
    140) (140 . 140) (140 . 140) (90 . 90) (67 . 67) (140 . 140) (140 .
    140) (140 . 140) (140 . 140) (140 . 140) (140 . 140) (140 . 140) (50 .
    50) (140 . 140) (140 . 140) (140 . 140) (44 . 44) (140 . 140) (140 .
    140) (40 . 40) (140 . 140) (140 . 140) (140 . 140) (29 . 29) (#f . #f)
    (86 . 86) (#f . #f) (140 . 140) (140 . 140) (140 . 140) (140 . 140)
    (140 . 140) (140 . 140) (140 . 140) (140 . 140) (18 . 18) (13 . 13) (11
    . 11) (10 . 10) (8 . 8) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (64 .
    64) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (2 . 2) (140 .
    140) (104 . 104) (102 . 102) (96 . 96) (140 . 140) (92 . 92) (140 .
    140) (140 . 140) (140 . 140) (140 . 140) (140 . 140) (140 . 140) (55 .
    55) (140 . 140) (140 . 140) (140 . 140) (140 . 140) (140 . 140) (42 .
    42) (140 . 140) (39 . 39) (140 . 140) (140 . 140) (#f . #f) (#f . #f)
    (140 . 140) (140 . 140) (140 . 140) (140 . 140) (140 . 140) (140 . 140)
    (140 . 140) (140 . 140) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (#f .
    #f) (#f . #f) (33 . 33) (#f . #f) (7 . 7) (#f . #f) (140 . 140) (94 .
    94) (140 . 140) (91 . 91) (140 . 140) (140 . 140) (61 . 61) (140 . 140)
    (140 . 140) (52 . 52) (48 . 48) (140 . 140) (140 . 140) (140 . 140)
    (140 . 140) (25 . 25) (24 . 24) (#f . #f) (#f . #f) (140 . 140) (140 .
    140) (140 . 140) (140 . 140) (140 . 140) (26 . 26) (140 . 140) (140 .
    140) (108 . 108) (#f . #f) (#f . #f) (#f . #f) (35 . 35) (#f . #f) (#f
    . #f) (3 . 3) (140 . 140) (140 . 140) (140 . 140) (140 . 140) (140 .
    140) (140 . 140) (140 . 140) (53 . 53) (46 . 46) (140 . 140) (140 .
    140) (140 . 140) (#f . #f) (#f . #f) (140 . 140) (74 . 74) (140 . 140)
    (140 . 140) (49 . 49) (140 . 140) (21 . 21) (103 . 103) (84 . 84) (#f .
    #f) (34 . 34) (#f . #f) (0 . 0) (140 . 140) (93 . 93) (66 . 66) (140 .
    140) (140 . 140) (140 . 140) (45 . 45) (140 . 140) (140 . 140) (#f .
    #f) (140 . 140) (140 . 140) (140 . 140) (12 . 12) (65 . 65) (140 . 140)
    (63 . 63) (140 . 140) (59 . 59) (43 . 43) (41 . 41) (#f . #f) (140 .
    140) (140 . 140) (140 . 140) (140 . 140) (140 . 140) (#f . #f) (140 .
    140) (140 . 140) (140 . 140) (95 . 95) (62 . 62) (#f . #f) (140 . 140)
    (140 . 140) (54 . 54) (#f . #f) (82 . 82) (140 . 140) (#f . #f) (140 .
    140) (#f . #f) (57 . 57) (#f . #f) (#f . #f) (#f . #f) (#f . #f) (#f .
    #f) (#f . #f) (#f . #f))))

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
