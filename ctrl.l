%{
#include <stdio.h>
#include <string.h>
#include "Value.h"
#include "AST.h"

#include "Statement.h"
#include "Assignment.h"
#include "PrintStatement.h"
/*#include "SymbolTable.h"*/
#include "parser.tab.h"

%}

LETTER  [a-zA-Z]
DIGIT   [0-9]
%option noyywrap

%%

"int"|"bool"|"float"|"char"|"string"|"void" { yylval.string = strdup(yytext); return TYPE; }
"true"      { yylval.boolean = true; return TRUE;}
"false"     { yylval.boolean = false; return FALSE;}
"ctrl()"    {return BGIN;}
"call"      {return CALL; }
"class"    {return OBJECT_DEF;}
"function"  {return FUNCTION_DEF;}
"for"       {return FOR;}
"if"        {return IF;}
"else"      {return ELSE;}
"then"      {return THEN;}
"while"     {return WHILE;}
"print"     {return PRINT;}
"return"    {return RETURN;}
"Eval"      {return EVAL;}
"TypeOf"    {return TYPE_OF;}
"="  { return ASSIGN; }
"+"  { return PLUS; }
"-"   { return MINUS; }
"*"  { return MULT; }
"/"  { return DIV; }
"==" { return EQUAL; }
"<"  { return LESS; }
">"  { return GREATER; }
"<=" { return LESSEQUAL; }
">=" { return GREATEREQUAL; }
"!=" { return NOTEQUAL; }
"."  { return DOT; }


{LETTER}({LETTER}|{DIGIT}|_)*   { yylval.string = strdup(yytext); return ID;}
-?{DIGIT}+\.{DIGIT}+            { yylval.real = atof(yytext); return FLOAT_LITERAL;} 
-?{DIGIT}+                      { yylval.integer = atoi(yytext);  return INT_LITERAL;}
\"(\\.|[^"])*\"                 { yylval.string = strndup(yytext + 1, strlen(yytext) - 2); return STRING_LITERAL;}
\'.\'                           { yylval.string= strndup(yytext + 1, 1); return CHAR_LITERAL;}
[ \r\t]*    { continue; /* Skip blanks. */ }
\n          {yylineno++;}
. {return yytext[0];}
%%
