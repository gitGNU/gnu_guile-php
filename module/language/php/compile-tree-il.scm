;;; PHP for GNU Guile

;; Copyright (C) 2009 Free Software Foundation, Inc.

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

;;; Commentary:

;;; Code:

(define-module (language php compile-tree-il)
  #:use-module (language tree-il)
  #:use-module (ice-9 format)
  #:use-module (system base pmatch)
  #:export (compile-tree-il comp))

(define-syntax ->
  (syntax-rules ()
    ((_ (type arg ...))
     `(type ,arg ...))))

(define-syntax @implv
  (syntax-rules ()
    ((_ sym)
     (-> (@ '(language php impl) 'sym)))))

(define-syntax @impl
  (syntax-rules ()
    ((_ sym arg ...)
     (-> (apply (@implv sym) arg ...)))))

(define (econs name gensym env)
  (let ((name (string->symbol name)))
    (acons name (-> (lexical name gensym)) env)))

(define (lookup name env)
  (let ((name (string->symbol name)))
    (or (assq-ref env name) (-> (toplevel name)))))

(define (empty-lexical-environment)
  '())

(define (compile-tree-il exp env opts)
  (values
   (let ((il (parse-tree-il
    (comp exp (empty-lexical-environment)))))
      ;(display il)(newline)
      il)
   env
   env))

(define (location x)
  (and (pair? x)
       (let ((props (source-properties x)))
         (and (not (null? props))
              props))))

(define-syntax pmatch/source
  (syntax-rules ()
    ((_ x clause ...)
     (let ((x x))
       (let ((res (pmatch x
                    clause ...)))
	 (let ((loc (location x)))
           (if loc
               (set-source-properties! res (location x))))
	 res)))))

(define (comp exp env)

  (pmatch/source exp
    
    ((begin ,form)
      (comp form env))
    ((begin . ,forms)
      `(begin ,@(map (lambda (x) (comp x env)) forms)))
    ((num ,n)
      (-> (const (string->number n))))
    ((string ,s)
      (-> (const s)))
    ((print ,x)
      (@impl php/print (-> (const x))))
    ((print-var ,x)
      (@impl php/print (lookup x env))) 
    ((var ,varname ,val)
      (-> (define (string->symbol varname) (comp val env))))
    ((var ,varname)
      (-> (define (string->symbol varname) (-> (const 'NULL)))))
    ((var-resolve ,varname)
      (lookup varname env))
    ((lambda ,formals ,body)
      (let ((syms (map (lambda (p)
			 (let ((sym (gensym)))
			   (set! env (econs p sym env))
			   sym))
		       formals)))
	(let ((defaults (map (lambda (p) (-> (const 'NULL))) formals)))
	  `(lambda '()
	     (lambda-case ((() ,formals #f #f ,defaults ,syms) ,(comp body env)))))))
    ((call ,proc)
      (-> (apply (-> (toplevel (string->symbol proc))))))
    ((call ,proc ,args)
      `(apply (toplevel ,(string->symbol proc)) ,@(map (lambda(x) (comp x env)) args)))
    ((if ,cond ,then)
      (-> (if (comp cond env) (comp then env) (-> (void)))))
    ((if ,cond ,then ,else)
      (-> (if (comp cond env) (comp then env) (comp else env))))
    ((equal ,a ,b)
      ;(-> (apply (-> (primitive '=)) (comp a env) (comp b env))))
      (@impl php/== (comp a env) (comp b env)))
    ((not ,x)
      (-> (apply (-> (primitive 'not)) (comp x env))))
    ((void)
      (-> (void)))

    (else
      (apply throw 'CompileError "Exp not implemented: " exp))))


