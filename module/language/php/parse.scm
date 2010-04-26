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

;(include "tokenize.scm")

(define-module (language php parse)
  #:use-module (language php tokenize)
  #:use-module (system base lalr)
  #:export (read-php))

(define (syntax-error message . args)
  (apply throw 'ParseSyntaxError message args))

(define *eof-object* 
  (call-with-input-string "" read-char))

(define (read-php port)
  (let ((parse (make-parser)))
    (parse (make-tokenizer port) syntax-error)))

(define (make-parser)
  (lalr-parser 
    (T_ABSTRACT T_AND_EQUAL T_ARRAY T_ARRAY_CAST T_AS T_BAD_CHARACTER 
      T_BOOLEAN_AND T_BOOLEAN_OR T_BOOL_CAST T_BREAK T_CASE T_CATCH 
      T_CLASS T_CLASS_C T_CLONE T_CLOSE_TAG T_COMMENT T_CONCAT_EQUAL 
      T_CONST T_CONSTANT_ENCAPSULATED_STRING T_CONTINUE T_CURLY_OPEN 
      T_DEC T_DECLARE T_DIR T_DIV_EQUAL T_DNUMBER T_DOC_COMMENT 
      T_DO T_DOLLAR_OPEN_CURLY_BRACES T_DOUBLE_ARROW T_DOUBLE_CAST 
      T_DOUBLE_COLON T_ECHO T_ELSE T_ELSEIF T_EMPTY T_ENCAPSULATED_AND_WHITESPACE 
      T_ENDDECLARE T_ENDFOR T_ENDFOREACH T_ENDIF T_ENDSWITCH T_ENDWHILE 
      T_END_HEREDOC T_EVAL T_EXTENDS T_FILE T_FINAL T_FOR T_FOREACH 
      T_FUNCTION T_FUNC_C T_GLOBAL T_GOTO T_HALT_COMPILER T_IF 
      T_IMPLEMENTS T_INC T_INCLUDE T_INCLUDE_ONCE T_INLINE_HTML T_INSTANCEOF T_INT_CAST 
      T_INTERFACE T_ISSET T_IS_EQUAL T_IS_GREATER_OR_EQUAL T_IS_IDENTICAL 
      T_IS_NOT_EQUAL T_IS_NOT_IDENTICAL T_IS_SMALLER_OR_EQUAL T_LINE T_LIST 
      T_LNUMBER T_LOGICAL_AND T_LOGICAL_OR T_LOGICAL_XOR T_MOD_EQUAL T_MUL_EQUAL 
      T_NS_C T_NUM_STRING T_OBJECT_CAST T_OBJECT_OPERATOR T_OPEN_TAG 
      T_OPEN_TAG_WITH_ECHO T_OR_EQUAL T_PLUS_EQUAL T_PRINT T_PRIVATE T_PUBLIC 
      T_PROTECTED T_REQUIRE T_REQUIRE_ONCE T_RETURN T_SL T_SL_EQUAL T_SR 
      T_SR_EQUAL T_START_HEREDOC T_STATIC T_STRING T_STRING_CAST T_SWITCH 
      T_THROW T_TRY T_UNSET T_UNSET_CAST T_USE T_VAR T_VARIABLE T_WHILE 
      T_WHITESPACE T_XOR_EQUAL 

      open-paren close-paren open-brace close-brace open-bracket close-bracket 
      comma semi asteriks plus minus divide equals dot qmark null word)

    (Program   
      (SourceElements) : $1
      (*eoi*) : *eof-object*)

    (SourceElements  
      (SourceElement) : $1 
      (SourceElements SourceElement) : (if (and (pair? $1) (eq? (car $1) 'begin))
                                          `(begin ,@(cdr $1) ,$2)
                                          `(begin ,$1 ,$2)))

    (SourceElement 
      (Statement) : $1
      (FunctionDeclaration) : $1

      (T_INLINE_HTML) : `(print ,$1)
      (T_OPEN_TAG) : `(void) 
      (T_CLOSE_TAG) : `(void) 
      (T_WHITESPACE) : `(void)
      (T_COMMENT) : `(void)
      
      )

    (FunctionDeclaration 
      (T_FUNCTION word open-paren close-paren open-brace FunctionBody close-brace) : `(var ,$2 (lambda () ,$6))
      (T_FUNCTION word open-paren FormalParameterList close-paren open-brace FunctionBody close-brace) : `(var ,$2 (lambda ,$4 ,$7))
      )

    (FormalParameterList 
      (T_VARIABLE) : `(,$1)
      (FormalParameterList comma T_VARIABLE) : `(,@$1 ,$3))

    (ValueList
      (Value) : `(,$1)
      (ValueList comma Value) : `(,@$1 ,$3))
    
    (Value
      (T_CONSTANT_ENCAPSULATED_STRING) : `(string ,$1)
      (T_LNUMBER) : `(num ,$1)
      (null) : `(const 'NULL)
      (T_VARIABLE) : `(var-resolve ,$1))

    (FunctionBody 
      (SourceElements) : $1)

    (Statement 
      (Print) : $1
      (Var) : $1
      (IfBlock) : $1 
      (word open-paren close-paren semi) : `(call ,$1)
      (word open-paren ValueList close-paren semi) : `(call ,$1 ,$3))

    (Print 
      (T_PRINT T_CONSTANT_ENCAPSULATED_STRING semi) : `(print ,$2) 
      (T_PRINT T_VARIABLE semi) : `(print-var ,$2))

    (Var 
      (T_VARIABLE equals null semi) : `(var ,$1)
      (T_VARIABLE equals T_LNUMBER semi) : `(var ,$1 (num ,$3))
      (T_VARIABLE equals T_CONSTANT_ENCAPSULATED_STRING semi) : `(var ,$1 (string ,$3)))
    
    (IfBlock
      (T_IF open-paren Comparison close-paren open-brace SourceElements close-brace) : `(if ,$3 ,$6))

    (Comparison
      (Value T_IS_EQUAL Value) : `(equal ,$1 ,$3)
      (Value T_IS_NOT_EQUAL Value) : `(not (equal ,$1 ,$3)))

    ))