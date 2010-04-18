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

(include "compile-tree-il.scm")
(include "parse.scm")

(define-module (language php spec)
  #:use-module (language php compile-tree-il)
  #:use-module (language php parse)
  #:use-module (system base language)
  #:export (php))

(define-language php
  #:title	"Guile PHP"
  #:version	"1.0"
  #:reader	(lambda (port env) (read-php port))
  #:compilers	`((tree-il . ,compile-tree-il))
  #:printer	write)
