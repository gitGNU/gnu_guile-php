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
  #:export (make-tokenizer clean-php-env))

(define parse-mode 'txt)

(define (make-tokenizer port)
  (lexer-init 'port port)
  lexer)

(define (clean-php-env)
  (set! parse-mode 'txt))

(define (make-token tok txt)
  ;(display parse-mode)(display " ------ ")(display tok)(newline)(display txt)(newline)
  (if (eq? parse-mode 'txt)
    (make-lexical-token 'T_INLINE_HTML #f txt)
    (make-lexical-token tok #f txt)))

(define (lexer-error tok)
  (set! parse-mode 'txt)
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

(define (read-single-line-comment txt yygetc yyungetc)
  (define (stop-reading c)
    (if (or (eq? c 'eof) (not (char? c)))
	#t
	(if (char=? c #\newline)
	    #t
	    (if (or (char=? c #\?) (char=? c #\%))
		(let ((nc (yygetc)))
		  (if (and (char? nc) (char=? nc #\>))
		      (begin
			(yyungetc)(yyungetc) #t)
		      (begin
			(yyungetc) #f)))
		#f))))
  (make-token 'T_COMMENT (string-append txt (read-til stop-reading yygetc yyungetc))))

(define (read-multi-line-comment tok txt yygetc yyungetc)
  (define (stop-reading c)
    (if (or (eq? c 'eof) (not (char? c)))
	#t
	(if (eq? c #\*)
	    (let ((nc (yygetc)))
	      (if (or (eq? c 'eof) (and (char? nc) (char=? nc #\/)))
		  #t
		  (begin (yyungetc) #f)))
	    #f)))
  (make-token tok (string-append txt (read-til stop-reading yygetc yyungetc))))

(define (read-inline-html yygetc yyungetc)
  (define (stop-reading c)
    (if (or (eq? c 'eof) (and (char? c) (char=? c #\<)))
	(begin (yyungetc) #t)
	#f))
  (make-token 'T_INLINE_HTML (read-til stop-reading yygetc yyungetc)))

(define (read-string str-char yygetc yyungetc)
  (define (stop-reading c)
    (if (eq? c 'eof)
	(if (eq? parse-mode 'php)
	    (syntax-error "Unexpected eof inside string")
	    #t)
	(char=? c str-char)))
  (read-til stop-reading yygetc yyungetc))
	
(define (read-const-string str-char yygetc yyungetc)
  (make-token 'T_CONSTANT_ENCAPSED_STRING (read-string str-char yygetc yyungetc)))


(define (read-parse-string str-char yygetc yyungetc)
  (let ((str (read-string str-char yygetc yyungetc)))
;    (display str)(newline)(newline)
;    (let ((lex (call-with-input-string str make-tokenizer)))
;      (let loop ((tok (lex)))
;	(display tok)(newline)
;	(if (not (eq? tok '*eoi*)) (loop (lex))))
      (make-token 'T_CONSTANT_ENCAPSED_STRING str)))

;;; below merged in from tokenize-silex.scm


