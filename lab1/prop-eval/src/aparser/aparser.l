/* Archivo:: asignacion.l 
 * Descripcion:: Archivo de especificacion utilizado por lex. A partir de este
 * archivo lex genera el analizador lexico para parsear las asignaciones.
 */

%option  noyywrap case-insensitive yylineno
%option prefix="a_"

%{
#include "misc.h"
#include "aparser.tab.h"
#include <stdio.h>
/*int a_lval;*/
extern char* aname;
%}

ID      [1-9]
NUMBER  [0-9]*

			
%%
(,|\n){1}                   {/* Separador de var. */ return TK_SEP;}
:{1}                        {/* Asignacion de type. */ return TK_COLON;}
(true|1)                    {return TK_TRUE;}
(false|0)                   {return TK_FALSE;}
(p|P){ID}{NUMBER}           {a_lval.v = get_prop_var(a_text); return TK_PROP;}
[ /t]*                      {}
.                           { log_error(" %s - lexer error - at line %d: %s\n",aname,a_lineno,a_text); }      
%%
