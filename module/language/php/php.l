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


%%

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  The following token names are defined @:
;;;
;;;    http://php.net/manual/tokens.php
;;;
;;;  Names have been kept the same to help validate the implementation.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; todo: don't make all these strings, consolidate like items, etc...

"abstract"  (make-token 'T_ABSTRACT yytext)
"&="  (make-token 'T_AND_EQUAL yytext)
"array"  (make-token 'T_ARRAY yytext)
"(array)"  (make-token 'T_ARRAY_CAST yytext)
" as "  (make-token 'T_AS yytext)

;todo:          (make-token 'T_BAD_CHARACTER yytext)

"&&"  (make-token 'T_BOOLEAN_AND yytext)
"||"  (make-token 'T_BOOLEAN_OR yytext)
"(bool)"|"(boolean)"  (make-token 'T_BOOL_CAST yytext)
"break"  (make-token 'T_BREAK yytext)
"case"  (make-token 'T_CASE yytext)
"catch"  (make-token 'T_CATCH yytext)
"class"  (make-token 'T_CLASS yytext)
"__CLASS__"  (make-token 'T_CLASS_C yytext)
"clone"  (make-token 'T_CLONE yytext)

"?>"|"%>"|"</script>"  (begin (let ((token (make-token 'T_CLOSE_TAG yytext))) (set! parse-mode 'txt) token))

"#"|"//"  (read-single-line-comment yygetc yyungetc)
"/*"      (read-multi-line-comment yygetc yyungetc)
".="  (make-token 'T_CONCAT_EQUAL yytext)
"const"          (make-token 'T_CONST yytext)
"\""            (read-string #\" yygetc yyungetc) 
"'"             (read-string #\' yygetc yyungetc)
"continue"      (make-token 'T_CONTINUE yytext)
"{$"            (make-token 'T_CURLY_OPEN yytext)
"--"            (make-token 'T_DEC yytext)
"declare"        (make-token 'T_DECLARE yytext)
"__DIR__"        (make-token 'T_DIR yytext)
"/="            (make-token 'T_DIV_EQUAL yytext)
\d\.\d          (make-token 'T_DNUMBER yytext)

\/\*\*(.*)\*\/  (make-token 'T_DOC_COMMENT yytext)
"do"            (make-token 'T_DO yytext)
"${"            (make-token 'T_DOLLAR_OPEN_CURLY_BRACES yytext)
"=>"            (make-token 'T_DOUBLE_ARROW yytext)
"(real)"        (make-token 'T_DOUBLE_CAST yytext)
"(double)"      (make-token 'T_DOUBLE_CAST yytext)
"(float)"        (make-token 'T_DOUBLE_CAST yytext)
"::"            (make-token 'T_DOUBLE_COLON yytext) ; also T_PAAMAYIM_NEKUDOTAYIM
"echo"          (make-token 'T_ECHO yytext)
"else"          (make-token 'T_ELSE yytext)
"elseif"        (make-token 'T_ELSEIF yytext)
"empty"          (make-token 'T_EMPTY yytext)
\"(.?)\$(.*)\"  (make-token 'T_ENCAPSED_AND_WHITESPACE yytext)
"enddeclare"    (make-token 'T_ENDDECLARE yytext)
"endfor"        (make-token 'T_ENDFOR yytext)
"endforeach"    (make-token 'T_ENDFOREACH yytext)
"endif"          (make-token 'T_ENDIF yytext)
"endswitch"      (make-token 'T_ENDSWITCH yytext)
"endwhile"      (make-token 'T_ENDWHILE yytext)

;todo:          (make-token 'T_END_HEREDOC yytext)

"eval"          (make-token 'T_EVAL yytext)
"extends"        (make-token 'T_EXTENDS yytext)
"__FILE__"      (make-token 'T_FILE yytext)
"final"          (make-token 'T_FINAL yytext)
"for"            (make-token 'T_FOR yytext)
"foreach"        (make-token 'T_FOREACH yytext)
"function"      (make-token 'T_FUNCTION yytext)
"__FUNCTION__"  (make-token 'T_FUNC_C yytext)
"global"        (make-token 'T_GLOBAL yytext)
"goto"          (make-token 'T_GOTO yytext)

"__halt_compiler" (make-token 'T_HALT_COMPILER yytext)

"if"            (make-token 'T_IF yytext)
"implements"    (make-token 'T_IMPLEMENTS yytext)
"++"            (make-token 'T_INC yytext)
"include"        (make-token 'T_INCLUDE yytext)
"include_once"  (make-token 'T_INCLUDE_ONCE yytext)
"instanceof"    (make-token 'T_INSTANCEOF yytext)
"(int)"          (make-token 'T_INT_CAST yytext)
"(integer)"      (make-token 'T_INT_CAST yytext)
"interface"      (make-token 'T_INTERFACE yytext)
"isset"          (make-token 'T_ISSET yytext)
"=="            (make-token 'T_IS_EQUAL yytext)
">="            (make-token 'T_IS_GREATER_OR_EQUAL yytext)
"==="            (make-token 'T_IS_IDENTICAL yytext)
"!="|"<>"        (make-token 'T_IS_NOT_EQUAL yytext)
"!=="            (make-token 'T_IS_NOT_IDENTICAL yytext)
"<="            (make-token 'T_IS_SMALLER_OR_EQUAL yytext)
"__LINE__"      (make-token 'T_LINE yytext)
"list"          (make-token 'T_LIST yytext)
0|[1-9][0-9]*    (make-token 'T_LNUMBER yytext)
"and"            (make-token 'T_LOGICAL_AND yytext)
"or"            (make-token 'T_LOGICAL_OR yytext)
"xor"            (make-token 'T_LOGICAL_XOR yytext)
"%="            (make-token 'T_MOD_EQUAL yytext)
"*="            (make-token 'T_MUL_EQUAL yytext)

"__NAMESPACE__"  (make-token 'T_NS_C yytext)

"new"            (make-token 'T_NEW yytext)

\"(.?)\$(.*)\[\d\](.*)\' (make-token 'T_NUM_STRING yytext)

"(object)"      (make-token 'T_OBJECT_CAST yytext)
"->"            (make-token 'T_OBJECT_OPERATOR yytext)

"<?"|"<?php"|"<%"|"<script language=\"php\">"            (begin (set! parse-mode 'php) (make-token 'T_OPEN_TAG yytext))

"<?="|"<%="      (begin (set! parse-mode 'php) (make-token 'T_OPEN_TAG_WITH_ECHO yytext))

"|="            (make-token 'T_OR_EQUAL yytext)
"+="            (make-token 'T_PLUS_EQUAL yytext)
"print"          (make-token 'T_PRINT yytext)
"private"        (make-token 'T_PRIVATE yytext)
"public"        (make-token 'T_PUBLIC yytext)
"protected"      (make-token 'T_PROTECTED yytext)
"require"        (make-token 'T_REQUIRE yytext)
"require_once"  (make-token 'T_REQUIRE_ONCE yytext)
"return"        (make-token 'T_RETURN yytext)
"<<"            (make-token 'T_SL yytext)
"<<="            (make-token 'T_SL_EQUAL yytext)
">>"            (make-token 'T_SR yytext)
">>="            (make-token 'T_SR_EQUAL yytext)
"<<<"            (make-token 'T_START_HEREDOC yytext)
"static"        (make-token 'T_STATIC yytext)

\"(.?)\$(.*)\[\w\](.*)\" (make-token 'T_STRING yytext)

"(string)"      (make-token 'T_STRING_CAST yytext)

\"\$\{(.*)\}(.*)\" (make-token 'T_STRING_VARNAME yytext)

"switch"        (make-token 'T_SWITCH yytext)
"throw"          (make-token 'T_THROW yytext)
"try"            (make-token 'T_TRY yytext)
"unset"          (make-token 'T_UNSET yytext)
"(unset)"        (make-token 'T_UNSET_CAST yytext)
"use"            (make-token 'T_USE yytext)
"var"            (make-token 'T_VAR yytext)
\$[a-zA-Z]+      (make-token 'T_VARIABLE yytext)  ;todo: handle all legal var name
"while"          (make-token 'T_WHILE yytext)
\9+              (if (eq? parse-mode 'txt) (make-token 'T_WHITESPACE yytext) (yycontinue))
\10+            (if (eq? parse-mode 'txt) (make-token 'T_WHITESPACE yytext) (yycontinue))
\13+            (if (eq? parse-mode 'txt) (make-token 'T_WHITESPACE yytext) (yycontinue))
[ ]              (if (eq? parse-mode 'txt) (make-token 'T_WHITESPACE yytext) (yycontinue))
"^="            (make-token 'T_XOR_EQUAL yytext)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Other tokens
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

"("              (make-token 'open-paren yytext)
")"              (make-token 'close-paren yytext)

"{"              (make-token 'open-brace yytext)
"}"              (make-token 'close-brace yytext)

"["              (make-token 'open-bracket yytext)
"]"              (make-token 'close-bracket yytext)

","              (make-token 'comma yytext)
";"              (make-token 'semi yytext)
"*"              (make-token 'asteriks yytext)
"+"              (make-token 'plus yytext)
"-"              (make-token 'minus yytext)
"/"              (make-token 'divide yytext)
"="              (make-token 'equals yytext)
"."              (make-token 'dot yytext)
"?"              (make-token 'qmark yytext)
">"              (make-token 'greater-than yytext)
"<"              (make-token 'less-than yytext)

"null"|"NULL"    (make-token 'null yytext)
"true"|"TRUE"    (make-token 'true yytext)
"false"|"FALSE"  (make-token 'false yytext)

([a-zA-Z]|_)([a-zA-Z]|_|[0-9])* (make-token 'word yytext)

<<EOF>>          '*eoi*
<<ERROR>>        (lexer-error (yygetc))
