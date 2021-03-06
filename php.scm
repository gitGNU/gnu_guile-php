#!/usr/bin/env guile 
!#

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

(use-modules 
  (language php compile-tree-il)  
  (language php parse))

(define (err msg)
  (display "guile-php err: ")
  (display msg)
  (newline)
  (exit))

(define (compile-php-file file)
  (compile (call-with-input-file file read-php) #:from 'php #:to 'value))

(define (main args)
  (let ((args (cdr args)))
    (if (= 1 (length args))
	(compile-php-file (list-ref args 0))
	(err "guile-php currently requires 1 and only 1 param, a file name."))))

;;;;;;;;

(main (command-line))