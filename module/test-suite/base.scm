;;; PHP for GNU Guile

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

(define-module (test-suite base)
  #:use-module (test-suite lib)
  #:use-module (language php parse)
  #:use-module (language php tokenize)
  #:use-module (system base compile)
  #:export (read-php-str parse-php-str compile-php-str))

(define (read-php-str str)
  (call-with-input-string str read-php))

(define-syntax parse-php-str
  (syntax-rules ()
    ((_ expression expected)
     (begin
       (clean-php-env)
       (pass-if expression (equal? expected (read-php-str expression)))))))

(define-syntax compile-php-str
  (syntax-rules ()
    ((_ expression expected)
     (begin
       (clean-php-env)
       (pass-if expression
        (equal? expected
	  (with-output-to-string
	    (lambda ()
	      (compile (read-php-str expression) #:from 'php #:to 'value)))))))))


