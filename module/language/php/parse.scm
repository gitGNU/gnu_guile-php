;; PHP for GNU Guile.

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
    ((nonassoc: T_COMMENT T_DOC_COMMENT)
     T_ABSTRACT T_ARRAY T_AS T_BAD_CHARACTER 
      T_CATCH T_CLASS T_CLASS_C T_CLONE 
      T_CONST 
      T_CURLY_OPEN 
      T_DECLARE T_DIR T_DNUMBER 
      T_DOLLAR_OPEN_CURLY_BRACES 
      T_DOUBLE_COLON T_EMPTY T_ENCAPSED_AND_WHITESPACE 
      T_ENDDECLARE T_ENDFOR T_ENDFOREACH T_ENDIF T_ENDSWITCH T_ENDWHILE 
      T_END_HEREDOC T_EVAL T_EXTENDS T_FILE T_FINAL 
      T_FUNC_C T_GLOBAL T_GOTO T_HALT_COMPILER 
      T_IMPLEMENTS T_INCLUDE T_INCLUDE_ONCE T_INSTANCEOF 
      T_INTERFACE T_ISSET T_LINE T_LIST 
      T_NS_C T_NUM_STRING T_OBJECT_OPERATOR 
      T_PRIVATE T_PUBLIC 
      T_PROTECTED T_REQUIRE T_REQUIRE_ONCE 
      T_START_HEREDOC T_STATIC T_STRING
      T_THROW T_TRY T_UNSET T_UNSET_CAST T_USE T_VAR 
      T_WHITESPACE 

      semi colon

      (nonassoc: open-paren close-paren open-brace close-brace
		 open-bracket close-bracket null label true false)

      T_OPEN_TAG
      (nonassoc: T_CLOSE_TAG)
      (nonassoc: T_ELSE T_ELSEIF)
      (nonassoc: T_CONSTANT_ENCAPSED_STRING T_LNUMBER T_VARIABLE)
      (left: T_FUNCTION)
      (left: T_SWITCH T_CASE T_DEFAULT T_BREAK T_CONTINUE)
      (left: T_OPEN_TAG_WITH_ECHO T_ECHO T_PRINT T_INLINE_HTML)
      (left: T_DO T_FOR T_FOREACH T_WHILE)
      (left: T_IF)
      (left: T_RETURN)
      (left: comma)
      (left: T_LOGICAL_OR T_LOGICAL_XOR T_LOGICAL_AND)
      (right: equals T_PLUS_EQUAL T_MINUS_EQUAL T_MUL_EQUAL T_DIV_EQUAL T_CONCAT_EQUAL
	      T_MOD_EQUAL T_AND_EQUAL T_OR_EQUAL T_XOR_EQUAL T_SL_EQUAL T_SR_EQUAL
	      T_DOUBLE_ARROW)
      (left: qmark)
      (left: T_BOOLEAN_OR)
      (left: T_BOOLEAN_AND)
      (left: pipe)
      (left: caret)
      (left: amp)
      (nonassoc: T_IS_EQUAL T_IS_NOT_EQUAL T_IS_IDENTICAL T_IS_NOT_IDENTICAL)
      (nonassoc: less-than T_IS_SMALLER_OR_EQUAL greater-than T_IS_GREATER_OR_EQUAL) ; T_IS_NOT_EQUAL goes here also... <>
      (left: T_SL T_SR)
      (left: plus minus period)
      (left: asteriks divide mod)
      (right: exclaimation)
      (right: T_INT_CAST T_DOUBLE_CAST T_STRING_CAST T_ARRAY_CAST T_OBJECT_CAST T_BOOL_CAST)
      (nonassoc: T_INC T_DEC))

    (Program
     (SourceElements *eoi*) : $1
     (*eoi*) : `(void))

    (SourceElements
     (SourceElement) : $1
     (SourceElements SourceElement) : (if (and (pair? $1) (eq? (car $1) 'begin))
                                  `(begin ,@(cdr $1) ,$2)
                                  `(begin ,$1 ,$2)))

    (Statements
     (Statement) : $1
     (Statements Statement) : (if (and (pair? $1) (eq? (car $1) 'begin))
				  `(begin ,@(cdr $1) ,$2)
				  `(begin ,$1 ,$2)))

    (SourceElement
     (InlineHTMLs) : $1
     (T_OPEN_TAG T_CLOSE_TAG) : `(void)
     (T_OPEN_TAG Statements) : $2
     (T_OPEN_TAG Statements T_CLOSE_TAG) : $2)
    
    (Statement
     (FunctionDeclaration) : $1
     (IfStatement) : $1
     (IterationStatement) : $1
     (Comment) : $1
     (ExpressionStatement) : $1
     (GroupedStatements) : $1
     (Echo) : $1
     (Print) : $1
     (Return) : $1
     (ContinueStatement) : $1
     (BreakStatement) : $1
     (SwitchStatement) : $1
     ;(TextModeStatement) : $1
     )

    (BreakStatement
     (T_BREAK semi) : `(break)
     (T_BREAK T_LNUMBER semi) : `(break ,$2))

    (CaseClause
     (T_CASE Expression colon) : `(void)
     (T_CASE Expression colon Statements) : `(case ,$2 ,$4)
     )
    
    (CaseClauses
     (CaseClause) : $1
     (CaseClauses CaseClause) : (if (and (pair? $1) (eq? (car $1) 'begin))
				    `(begin ,@(cdr $1) ,$2)
				    `(begin ,$1 ,$2)))

    (CaseStatements
     (open-brace close-brace) : `(void)
     (open-brace CaseClauses close-brace) : $2
     (open-brace CaseClauses DefaultClause close-brace) : `(begin ,$2 ,$3)
     (open-brace DefaultClause close-brace) : $2
     )
    
    (Comment (T_COMMENT) : `(void))

    (ContinueStatement
     (T_CONTINUE semi) : `(continue)
     (T_CONTINUE T_LNUMBER semi) : `(continue ,$2))

    (DefaultClause
      (T_DEFAULT colon) : `(void)
      (T_DEFAULT colon Statements) : `(case-default ,$3)
      )
    
    (Echo (T_ECHO ExpressionList semi) : `(echo ,@$2))

    (ElseStatements
     (T_ELSE Statement) : $2
     (T_ELSEIF open-paren Expression close-paren Statement) : `(if ,$3 ,$5)
     (T_ELSEIF open-paren Expression close-paren Statement ElseStatements) : `(if ,$3 ,$5 ,$6))
    
    (ExpressionStatement (Expression semi) : $1)

    (Expression
     (AssignmentExpression) : $1
     (Expression comma AssignmentExpression) : `(begin ,$1 ,$3)) 

    (ExpressionList
     (AssignmentExpression) : `(,$1)
     (ExpressionList comma AssignmentExpression) : `(,@$1 ,$3)) 
    
    (FunctionBody
     (GroupedStatements) : $1)

    (FunctionDeclaration
     (T_FUNCTION label open-paren close-paren FunctionBody) : `(var (var ,$2) (lambda () ,$5))
     (T_FUNCTION label open-paren FunctionParamList close-paren FunctionBody) : `(var (var ,$2) (lambda ,$4 ,$6)))

    (FunctionParam
     (T_VARIABLE) : $1)
    
    (FunctionParamList
     (FunctionParam) : `(,$1)
     (FunctionParamList comma FunctionParam) : `(,@$1 ,$3))

    (GroupedStatements
     (open-brace close-brace) : `(void)
     (open-brace Statements close-brace) : $2)

    (IfStatement
     (T_IF open-paren Expression close-paren Statement) : `(if ,$3 ,$5)
     (T_IF open-paren Expression close-paren Statement ElseStatements) : `(if ,$3 ,$5 ,$6))

    (InlineHTML
     (T_INLINE_HTML) : `(echo (string ,$1)))

    (InlineHTMLs
     (InlineHTML) : $1
     (InlineHTMLs InlineHTML) : (if (and (pair? $1) (eq? (car $1) 'begin))
				    `(begin ,@(cdr $1) ,$2)
				    `(begin ,$1 ,$2)))

    (IterationStatement
     (T_DO Statement T_WHILE open-paren Expression close-paren semi) : `(do ,$2 ,$5)
     (T_FOR open-paren semi semi close-paren Statement) : `(for #f #f #f ,$6)
     (T_FOR open-paren semi semi Expression close-paren Statement) : `(for #f #f ,$5 ,$7)
     (T_FOR open-paren semi Expression semi close-paren Statement) : `(for #f ,$5 #f ,$7)
     (T_FOR open-paren Expression semi semi close-paren Statement) : `(for ,$3 #f #f ,$7)
     (T_FOR open-paren Expression semi Expression semi close-paren Statement) : `(for ,$3 ,$5 #f ,$8)
     (T_FOR open-paren Expression semi Expression semi Expression close-paren Statement) : `(for ,$3 ,$5 ,$7 ,$9)
     (T_WHILE open-paren Expression close-paren Statement) : `(while ,$3 ,$5))
    
    (Print (T_PRINT AssignmentExpression semi) : `(print ,$2))

    (Return
     (T_RETURN semi) : `(return)
     (T_RETURN Expression semi) : `(return ,$2))

    (SwitchStatement
     (T_SWITCH open-paren Expression close-paren CaseStatements) : `(switch ,$3 ,$5))

    (TextModeStatement
     (T_CLOSE_TAG InlineHTMLs T_OPEN_TAG) : $2)

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    (PrimaryExpression
     (null) : `(null)
     (true) : `(true)
     (false) : `(false)
     (T_CONSTANT_ENCAPSED_STRING) : `(string ,$1)
     (T_LNUMBER) : `(num ,$1)
     (T_VARIABLE) : `(var ,$1)
     (CallExpression) : $1
     (open-paren Expression close-paren) : $2)
         
    (CallExpression
     (label open-paren close-paren) : `(call ,$1)
     (label open-paren ArgumentList close-paren) : `(call ,$1 ,$3))
    
    (ArgumentList
     (AssignmentExpression) : `(,$1)
     (ArgumentList comma AssignmentExpression) : `(,@$1 ,$3))

    (LeftHandSideExpression
     (PrimaryExpression) : $1)

    (PostfixExpression
     (LeftHandSideExpression) : $1
     (PostfixExpression T_INC) : `(post-inc ,$1)
     (PostfixExpression T_DEC) : `(post-dec ,$1))
    
    (UnaryExpression
     (PostfixExpression) : $1
     (T_INC UnaryExpression) : `(pre-inc ,$2)
     (T_DEC UnaryExpression) : `(pre-dec ,$2))
    
    (TypeExpression
     (UnaryExpression) : $1
     (T_BOOL_CAST TypeExpression) : `(->bool ,$2)
     (T_INT_CAST TypeExpression) : `(->int ,$2)
     (T_STRING_CAST TypeExpression) : `(->string ,$2))
    
    (InstanceOfExpression
     (TypeExpression) : $1)
    
    (NotExpression
     (InstanceOfExpression) : $1
     (exclaimation NotExpression))

    (MultiplicativeExpression
     (NotExpression) : $1
     (MultiplicativeExpression asteriks NotExpression) : `(mul ,$1 ,$3)
     (MultiplicativeExpression divide NotExpression) : `(div ,$1 ,$3)
     (MultiplicativeExpression mod NotExpression) : `(mod ,$1 ,$3))
    
    (AdditiveExpression
     (MultiplicativeExpression) : $1
     (AdditiveExpression plus MultiplicativeExpression) : `(add ,$1 ,$3)
     (AdditiveExpression minus MultiplicativeExpression) : `(sub ,$1 ,$3)
     (AdditiveExpression period MultiplicativeExpression) : `(concat ,$1 ,$3))

    (BitwiseShiftExpression
     (AdditiveExpression) : $1
     (BitwiseShiftExpression T_SL AdditiveExpression) : `(bit-sl ,$1 ,$3)
     (BitwiseShiftExpression T_SR AdditiveExpression) : `(bit-sr ,$1 ,$3))

    (RelationalComparisonExpression
     (BitwiseShiftExpression) : $1
     (RelationalComparisonExpression less-than BitwiseShiftExpression) : `(less-than ,$1 ,$3)
     (RelationalComparisonExpression T_IS_SMALLER_OR_EQUAL BitwiseShiftExpression) : `(less-or-equal ,$1 ,$3)
     (RelationalComparisonExpression greater-than BitwiseShiftExpression) : `(greater-than ,$1 ,$3)
     (RelationalComparisonExpression T_IS_GREATER_OR_EQUAL BitwiseShiftExpression) : `(greater-or-equal ,$1 ,$3))
    
    (EqualityComparisonExpression
     (RelationalComparisonExpression) : $1
     (EqualityComparisonExpression T_IS_EQUAL RelationalComparisonExpression) : `(equal ,$1 ,$3)
     (EqualityComparisonExpression T_IS_NOT_EQUAL RelationalComparisonExpression) : `(not-equal ,$1 ,$3)
     (EqualityComparisonExpression T_IS_IDENTICAL RelationalComparisonExpression) : `(identical ,$1 ,$3)
     (EqualityComparisonExpression T_IS_NOT_IDENTICAL RelationalComparisonExpression) : `(not-identical ,$1 ,$3))

    (BitwiseXOrExpression
     (EqualityComparisonExpression) : $1
     (BitwiseXOrExpression caret EqualityComparisonExpression) : `(bit-xor ,$1 ,$3))
    
    (BitwiseAndExpression
     (BitwiseXOrExpression) : $1
     (BitwiseAndExpression amp BitwiseXOrExpression) : `(bit-and ,$1 ,$3))

    (BitwiseOrExpression
     (BitwiseAndExpression) : $1
     (BitwiseOrExpression pipe BitwiseAndExpression) : `(bit-or ,$1 ,$3))
    
    (AndExpression
     (BitwiseOrExpression) : $1
     (AndExpression T_BOOLEAN_AND BitwiseOrExpression) : `(and ,$1 ,$3))
    
    (OrExpression
     (AndExpression) : $1
     (OrExpression T_BOOLEAN_OR AndExpression) : `(or ,$1 ,$3))
    
    (TernaryExpression
     (OrExpression) : $1
     ;; 5 Shift/Reduce conflicts here...
     (OrExpression qmark AssignmentExpression colon AssignmentExpression) : `(if ,$1 ,$3 ,$5)) 

    (LogicalAndExpression
     (TernaryExpression) : $1
     (LogicalAndExpression T_LOGICAL_AND TernaryExpression) : `(and ,$1 ,$3))

    (LogicalXOrExpression
     (LogicalAndExpression) : $1
     (LogicalXOrExpression T_LOGICAL_XOR LogicalAndExpression) : `(xor ,$1 ,$2))

    (LogicalOrExpression
     (LogicalXOrExpression) : $1
     (LogicalOrExpression T_LOGICAL_OR LogicalXOrExpression) : `(or ,$1 ,$2))
    
    (AssignmentExpression
     (LogicalOrExpression) : $1
     (LeftHandSideExpression equals AssignmentExpression) : `(var ,$1 ,$3)
     (LeftHandSideExpression T_PLUS_EQUAL AssignmentExpression) : `(var ,$1 (add ,$1 ,$3))
     (LeftHandSideExpression T_MINUS_EQUAL AssignmentExpression) : `(var ,$1 (sub ,$1 ,$3))
     (LeftHandSideExpression T_MUL_EQUAL AssignmentExpression) : `(var ,$1 (mul ,$1 ,$3))
     (LeftHandSideExpression T_DIV_EQUAL AssignmentExpression) : `(var ,$1 (div ,$1 ,$3))
     (LeftHandSideExpression T_CONCAT_EQUAL AssignmentExpression) : `(var ,$1 (concat ,$1 ,$3))
     (LeftHandSideExpression T_MOD_EQUAL AssignmentExpression) : `(var ,$1 (mod ,$1 ,$3))
     (LeftHandSideExpression T_AND_EQUAL AssignmentExpression) : `(var ,$1 (bit-and ,$1 ,$3))
     (LeftHandSideExpression T_OR_EQUAL AssignmentExpression) : `(var ,$1 (bit-or ,$1 ,$3))
     (LeftHandSideExpression T_XOR_EQUAL AssignmentExpression) : `(var ,$1 (bit-xor ,$1 ,$3))
     (LeftHandSideExpression T_SL_EQUAL AssignmentExpression) : `(var ,$1 (bit-sl ,$1 ,$3))
     (LeftHandSideExpression T_SR_EQUAL AssignmentExpression) : `(var ,$1 (bit-sr ,$1 ,$3))
     (LeftHandSideExpression T_DOUBLE_ARROW AssignmentExpression) : `(double-arrow ,$1 ,$3))
     
    ))