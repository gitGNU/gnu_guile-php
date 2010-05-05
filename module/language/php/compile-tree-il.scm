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

;;; Commentary:

;;; Code:

(define-module (language php compile-tree-il)
  #:use-module (language tree-il)
  #:use-module (ice-9 format)
  #:use-module (system base pmatch)
  #:export (compile-tree-il comp handle-break-continue))

(define-syntax ->
  (syntax-rules ()
    ((_ (type arg ...))
     `(type ,arg ...))))

(define-syntax handle-break-continue
  (syntax-rules ()
    ((_ vars syms construct entry continue-handler)
     (let ((%key (gensym "%catch-key ")) (%args (gensym "%catch-args ")))
       `(letrec vars syms
		construct
		(begin
		  (apply (toplevel catch) (const #t)
			 (lambda () (lambda-case ((() #f #f #f () ()) entry)))
			 (lambda ()
			   (lambda-case (((%key) #f (%args) #f () (,%key ,%args))
					 (if (apply (toplevel eq?)
						    (lexical %key ,%key) (const ,(loop-break-symbol)))
					     (const #f)
					     (if (apply (toplevel eq?)
							(lexical %key ,%key) (const ,(loop-continue-symbol)))
						 continue-handler
						 (begin (apply (toplevel throw) (lexical %key ,%key)) (const #f)))))))
			 (const #f))))))))

(define-syntax @implv
  (syntax-rules ()
    ((_ sym)
     (-> (@ '(language php impl) 'sym)))))

(define-syntax @impl
  (syntax-rules ()
    ((_ sym arg ...)
     (-> (apply (@implv sym) arg ...)))))

(define (econs name gensym env)
  (let ((name (if (string? name) (string->symbol name) name)))
    (acons name (-> (lexical name gensym)) env)))

(define (lookup name env)
  (let ((name (if (string? name) (string->symbol name) name)))
    (or (assq-ref env name) (-> (toplevel name)))))

(define (empty-lexical-environment)
  '())

(define (loop-break-str)
  " %loop_break% ")

(define (loop-continue-str)
  " %loop_continue% ")

(define (loop-break-symbol)
  (string->symbol (loop-break-str)))

(define (loop-continue-symbol)
  (string->symbol (loop-continue-str)))

(define (compile-tree-il exp env opts)
  (values
   (let ((il (comp exp '())))
     ;(newline)(display il)(newline)
     (parse-tree-il il))
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
     `(const ,(string->number n)))
    ((string ,s)
     `(const ,s))
    ((null)
     `(const #nil))
    ((true)
     `(const #t))
    ((false)
     `(const #f))
    ((return)
     `(apply (primitive return)))
    ((return ,x)
     `(apply (primitive return) ,(comp x env)))
    ((echo ,arg)
     (@impl php/echo (comp arg env)))
    ((print ,x)
     (@impl php/print (comp x env)))
    ((var ,varname ,val)
     `(define ,(string->symbol varname) ,(comp val env)))
    ((var ,varname)
     `(define ,(string->symbol varname) (const #nil)))
    ((var-resolve ,varname)
     (lookup varname env))
    ((break)
     `(apply (toplevel throw) (const ,(loop-break-symbol)) (const 1)))
    ((break ,x)
     `(apply (toplevel throw) (const ,(loop-break-symbol)) ,(comp x env)))
    ((continue)
     `(apply (toplevel throw) (const ,(loop-continue-symbol)) (const 1)))
    ((continue ,x)
     `(apply (toplevel throw) (const ,(loop-continue-symbol)) ,(comp x env)))
    ((do ,body ,test)
     (let ((%body (gensym "%body ")) (%test (gensym "%test "))
	   (%catch-key (gensym "%catch-key ")) (%catch-args (gensym "%catch-args ")))
       (let ((e (econs '%body %body (econs '%test %test env))))
	 `(letrec (%body %test) (,%body ,%test)
		  ((lambda ()
		     (lambda-case ((() #f #f #f () ())
				   (begin ,(comp body e) (apply (lexical %test ,%test))))))
		   (lambda ()
		     (lambda-case ((() #f #f #f () ())
				   (if ,(comp test e)
				       (apply (lexical %body ,%body))
				       (const #f))))))
		  (begin
		    (apply (toplevel catch) (const #t)
			   (lambda ()
			     (lambda-case ((() #f #f #f () ())
					   (apply (lexical %body ,%body)))))
			   (lambda ()
			     (lambda-case (((%catch-key) #f (%catch-args) #f () (,%catch-key ,%catch-args))
					   (if (apply (primitive eq?)
						      (lexical %catch-key ,%catch-key)
						      (const ,(loop-break-symbol)))
					       (const #f)
					       (if (apply (primitive eq?)
							  (lexical %catch-key ,%catch-key)
							  (const ,(loop-continue-symbol)))
						   (apply (lexical %test ,%test))
						   (const #f))))))
			   (const #f)))))))
    ((while ,test ,body)
     (let ((%body (gensym "%body ")))
       (let ((e (econs '%body %body env)))
	 (handle-break-continue
	  (%body) (,%body)
	  ((lambda ()
	     (lambda-case ((() #f #f #f () ())
			   (if ,(comp test e)
			       (begin ,(comp body e) (apply (lexical %body ,%body)))
			       (const #f))))))
	  (apply (lexical %body ,%body))
	  (apply (lexical %body ,%body))))))
    ((for ,init ,test ,inc ,body)
     (let ((%body (gensym "%body ")))
       (let ((e (econs '%body %body env)))
	 `(letrec (%body) (,%body)
		  ((lambda ()
		     (lambda-case ((() #f #f #f () ())
				   (if ,(comp (or test '(true)) e)
				       (begin ,(comp body e)
					      ,(comp (or inc '(begin)) e)
					      (apply (lexical %body ,%body)))
				       (const #f))))))
		  (begin
		    ,(comp (or init '(begin)) e)
		    (apply (lexical %body ,%body)))))))
    ((lambda ,formals ,body)
      (let ((syms (map (lambda (p)
			 (let ((sym (gensym)))
			   (set! env (econs p sym env))
			   sym))
		       formals)))
	(let ((defaults (map (lambda (p) (-> (const #nil))) formals)))
	  `(lambda ()
	     (lambda-case ((() ,formals #f #f ,defaults ,syms) ,(comp body env)))))))
    ((call ,proc)
     `(apply (toplevel ,(string->symbol proc))))
    ((call ,proc ,args)
     `(apply (toplevel ,(string->symbol proc)) ,@(map (lambda(x) (comp x env)) args)))
    ((if ,cond ,then)
     `(if ,(comp cond env) ,(comp then env) (void)))
    ((if ,cond ,then ,else)
     `(if ,(comp cond env) ,(comp then env) ,(comp else env)))
    ((and ,a ,b)
     `(if ,(comp a env) ,(comp b env) (const #f)))
    ((or ,a ,b)
     `(if ,(comp a env) (const #t) ,(comp b env)))
    ((equal ,a ,b)
     (@impl php/== (comp a env) (comp b env)))
    ((greater-than ,a ,b)
     (@impl php/> (comp a env) (comp b env)))
    ((less-than ,a ,b)
     (@impl php/< (comp a env) (comp b env)))
    ((greater-or-equal ,a ,b)
     (@impl php/>= (comp a env) (comp b env)))
    ((less-or-equal ,a ,b)
     (@impl php/<= (comp a env) (comp b env)))
    ((identical ,a ,b)
     (@impl php/=== (comp a env) (comp b env)))
    ((not ,x)
     `(apply (primitive not) ,(comp x env)))
    ((pre-inc ,var)
     (let ((v (comp var env)))
       `(begin (set! ,v (apply (primitive +) ,v (const 1))) ,v)))
    ((post-inc ,var)
     (let ((v (comp var env)) (tmp (gensym "post-inc ")))
       `(let (,tmp) (,tmp) (,v)
	     (begin
	       (set! ,v (apply (primitive +) ,v (const 1)))
	       (lexical ,tmp ,tmp)))))
    ((pre-dec ,var)
     (let ((v (comp var env)))
       `(begin (set! ,v (apply (primitive -) ,v (const 1))) ,v)))
    ((post-dec ,var)
     (let ((v (comp var env)) (tmp (gensym "pre-inc ")))
       `(let (,tmp) (,tmp) (,v)
	     (begin
	       (set! ,v (apply (primitive -) ,v (const 1)))
	       (lexical ,tmp ,tmp)))))
    ((->bool ,x)
     (@impl php/->bool (comp x env)))
    ((void)
     `(void))

    (else
     (apply throw 'CompileError "Exp not implemented: " exp))))


