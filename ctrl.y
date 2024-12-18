%{
#include <iostream>
#include <vector>
#include "SymTable.h"
extern FILE* yyin;
extern char* yytext;
extern int yylineno;
extern int yylex();
void yyerror(const char * s);
class SymTable* current;
int errorCount = 0;      
%}
%union {
     char* string;
}
%left '+' '-'
%left '*' '/' '%'
%token  BGIN ASSIGN NR FLOAT CLASS FUNCTION RETURN WHILE IF ELSE FOR ENDWHILE THEN ENDFOR TRUE FALSE ENDIF PRINT TYPEOF NEW LEQ EQ GEQ NE OR AND
%token<string> VAR TYPE PRIVACY
%start progr
%%
progr : declarations main { 
            if (errorCount == 0) cout << "The program is correct!" << endl; 
        }
      ;

declarations : declaration_list
             ;

declaration_list : /* epsilon */
                 | declaration_list declaration
                 ;

declaration : decl
            | functions
            | classes
            ;


decl       :  TYPE VAR ';' { 
                              if(!current->existsId($2)) {
                                    current->addVar($1,$2);
                              } else {
                                   errorCount++; 
                                   yyerror("Variable already defined");
                              }
                          }
              | TYPE VAR  '(' list_param ')' ';'
              | TYPE VAR  '('')' ';'
              | TYPE VAR  '[' NR ']' ';'
           ;
 
e : e '+' e  
  | e '*' e   
  | e '-' e    
  | e '/' e    
  | e '%' e    
  | '(' e ')' 
  | NR
  | VAR 
  | FLOAT
  | VAR '.' VAR //proprietate obiect
  | VAR '.' VAR '(' array_list ')' //metoda obiect
  | VAR '.' VAR '(' ')' //metoda obiect fara parametri
  | VAR '(' array_list ')' //apelare functie
  | VAR '(' ')' //apelare functie fara parametri
  | VAR '[' NR ']' //element array
  ;

condition : e '<' e
          | e '>' e
          | e LEQ e
          | e GEQ e
          | e NE e
          | e EQ e
          | TRUE
          | FALSE
          ;

expb : condition
     | expb AND expb
     | expb OR expb
     | '!' expb
     | '(' expb ')'

ret : e 
    | condition 
  ;

list_param : param
            | list_param ','  param 
            ;
            
param : TYPE VAR 
      ; 
      

main : BGIN '{' list '}'
     ;

list :  statement ';'
     | list statement ';'    
     | list decl
     | decl
     ;

function_list : /* epsilon */ //pentru functii care au doar return
              | function_list statement ';'
              | function_list decl

              ;
statement:  VAR ASSIGN e //atribuire
         | VAR '(' array_list')' //apelare functie
         | VAR '(' array_list',' function_in_function ')' //apelare functie cu parametri functie
         | VAR '(' function_in_function ')' //apelare functie doar cu parametru functie
         | VAR '('')' //apelare functie fara parametri
         | VAR '['']' ASSIGN '{' array_list '}' //atribuire array
         | VAR '[' NR ']' ASSIGN e //atribuire element al unui array
         | IF '(' expb ')' THEN list ENDIF
         | IF '(' expb ')' THEN list ELSE list ENDIF 
         | WHILE '(' expb ')' list ENDWHILE 
         | FOR '(' VAR ASSIGN e ';' expb ';' VAR ASSIGN e ')' list ENDFOR
         | VAR VAR ASSIGN NEW VAR //instantiere obiect
         | VAR '.' VAR ASSIGN e //apelare camp
         | VAR '.' VAR '('array_list')' //apelare metoda cu parametri
         | VAR '.' VAR '('')' //apelare metoda fara parametri
         | decl //declaratii in blockuri
         | PRINT '(' e ')'
         | TYPEOF '(' e ')'
         | TYPEOF  '(' condition ')'
         ;

array_list : e
           | array_list ',' e
           ;

classes : CLASS VAR '{' class_members '}'  
          ;

class_members : class_member
              | class_members class_member
              ;

class_member : TYPE VAR ';'  //declaratii           
              | TYPE VAR '(' list_param ')' '{' function_list RETURN ret ';' '}' //metode
              | TYPE VAR '('')' '{' function_list RETURN ret ';' '}' //metode fara parametri
              | VAR '(' TYPE VAR ')' ';' //constructor
              | '#' VAR '('')'';' //destructor
              | PRIVACY':'
              ;

functions : FUNCTION TYPE VAR '(' list_param ')' '{' function_list RETURN ret ';' '}' 
         | FUNCTION TYPE VAR '('')' '{' function_list RETURN ret ';' '}' 
         | FUNCTION TYPE VAR '(' list_param ',' function_in_function')' '{'  function_list RETURN ret ';' '}' 
         ;

function_in_function : FUNCTION VAR '(' list_param ')'
                    | FUNCTION VAR '('')'
                    ;
%%
void yyerror(const char * s){
     cout << "error:" << s << " at line: " << yylineno << endl;
}

int main(int argc, char** argv){
     yyin=fopen(argv[1],"r");
     current = new SymTable("global");
     yyparse();
     cout << "Variables:" <<endl;
     current->printVars();
     delete current;
}
