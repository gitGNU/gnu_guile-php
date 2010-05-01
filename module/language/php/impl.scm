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

(define-module (language php impl)
  #:use-module (system base language)
  #:export (php/print php/echo 
	    php/== php/=== php/< php/> php/<= php/>=))

;;
;;  Currently all these methods are just stubs, not even close to complete...
;;

(define (php/echo arg)
  (php/print arg))

(define (php/print arg)
  (if (not (eq? arg #nil))
    (display arg)))

(define (php/== a b)
  (cond
   ((and (string? a) (number? b))
    (string=? a (number->string b)))
   ((and (number? a) (string? b))
    (string=? (number->string a) b))
   (else
    (php/=== a b))))
    

(define (php/=== a b)
  (cond
   ((and (number? a) (number? b))
    (= a b))
   ((and (string? a) (string? b))
    (string=? a b))
   (else
    #f)))

(define (php/< a b)
  (< a b))

(define (php/> a b)
  (> a b))

(define (php/<= a b)
  (<= a b))

(define (php/>= a b)
  (>= a b))