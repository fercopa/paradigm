/* Archivo:: formula.y
 * Descripcion:: Archivo de especificacion utilizado por bison. 
 */

/* Describir esto*/

%name-prefix "f_"

%{
#define _POSIX_C_SOURCE 1
 #include <stdio.h>
 #include "misc.h"
 #include "ast.h" 
 extern int f_lex();
 extern int f_error(char *s);
 extern int f_lex_destroy(); 
 extern FILE *f_in;
 extern int f_lineno;
 ASTNode *ast;
 char *fname;
 /*nodo temporal que se usa para crear el AST. */
 ASTNode *n;
%}

/* Declaracion de yyval: yyval es la variable que  */
%union {
  ASTNode *a;
  int v;
}

/* Tokens*/
%token <v> TK_PROP
%token TK_BEGIN
%token TK_END
%token TK_NOT
%token TK_OR
%token TK_AND
%token TK_IMPL
%token TK_IFF
%token TK_TRUE
%token TK_FALSE
%token TK_OP
%token TK_OC

 /* Define el tipo de datos que retorna la bnf*/
%type <a> forms phi prop
/* Precedencias */
%left TK_PROP
%left TK_IFF
%left TK_IMPL
%left TK_OR
%left TK_AND
%right TK_NOT
%right TK_OP
%left TK_CP

%%
/* Simbolo inicial */
forms: TK_BEGIN phi TK_END {ast = $2;};

/* COMPLETAR ACA */
phi: prop
   | phi TK_OR phi              {ASTNODE_OR(n,$1,$3); $$=n;}
   | phi TK_AND phi             {ASTNODE_AND(n,$1,$3); $$=n;}
   | phi TK_IFF phi             {ASTNODE_IFF(n,$1,$3); $$=n;}
   | phi TK_IMPL phi            {ASTNODE_IMPL(n,$1,$3); $$=n;}
   | TK_NOT phi                 {ASTNODE_NOT(n,$2); $$=n;}
   | TK_OP phi TK_CP            {$$=n;}
   | TK_TRUE                    {ASTNODE_TRUE(n); $$=n;}
   | TK_FALSE                   {ASTNODE_FALSE(n); $$=n;};

prop: TK_PROP                   {ASTNODE_PROP(n,$1); $$ =n ;} ;

%%

/* Funcion que se provee para parsear una formula en un archivo.*/
void parse_formula(FILE * input)
{
  f_lineno = 1;
  f_in = input;
  if (!f_in)
    log_error("parse_formula: no input file.");
  f_parse();
  f_lex_destroy();
}

/* Funcion que se ejecuta cuando ocurre un error durante el parseo.*/
int f_error(char *s)
{
    log_error(" %s - parser error - %s\n",fname,s);
    return 0;
}
