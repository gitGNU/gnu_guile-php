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


