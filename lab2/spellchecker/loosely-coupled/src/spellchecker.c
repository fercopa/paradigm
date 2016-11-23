#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "document.h"
#include "dictionary.h"


/* tamaño maximo de una palabra */
#define MAX_WORD_SIZE 30

/* diccionario principal */
Dictionary main_dict;
/* diccionario de palabras ignoradas */
Dictionary ignored;
/* Documento a procesar */
Document doc;


/*******************************************************************
 * NAME :            int is_known(char *word)
 *
 * DESCRIPTION :     Verifica si una palabra es "conocida", ya sea 
 *                   porque esta en el diccionario principal o en el 
 *                   diccionario de palabras ignoradas. 
 *
 * PARAMETERS:
 *      INPUT:
 *           char    *word       Palabra a verificar.
 *
 * RETURN :
 *           Type: int
 *           Values: 1 si la palabra es conocida
 *                   0 si la palabra no es conocida
 *******************************************************************/
int is_known(char *word){
    /* completar aca  */
    int a = 0,
        b = 0,
        res = 0;
    if (word != NULL) {
        a = dict_contains(main_dict, word);
        b = dict_contains(ignored, word);
    }
    res = (a == 1 || b == 1)? 1: 0;
    return res;
}

/*******************************************************************
 * NAME :            void consult_user(char *word)
 *
 * DESCRIPTION :     Consulta al usuario sobre que accion realizar 
 *                   (aceptar, ignorar o reemplazar) con la palabra w.
 *                   Una vez que el usuario elige, realiza la accion 
 *                   elegida.
 *
 * PARAMETERS:
 *      INPUT:
 *           char    *word       Palabra sobre la cual se consulta la 
 *                            accion a realizar.
 *
 * RETURN :
 *           Type: void
 *******************************************************************/
void consult_user(char *word){
    char ans[2];
    if (word != NULL) {
        do{
            printf("Palabra no reconocida: %s\n Aceptar (a) - Ignorar (i) - Reemplazar (r): ", word);
            scanf("%s", ans);
        }while((strcmp(ans,"r") != 0) && (strcmp(ans,"a") != 0) && (strcmp(ans,"i") != 0));
        /* completar aca  */
        switch(ans[0]) {
            case 'r':
                /* limpio word */
                for (int i=0; i<MAX_WORD_SIZE; i++) { word[i] = '\0'; }
                printf("Corregir a: ");
                scanf("%s", word);
                break;
            case 'a':
                dict_add(main_dict, word);
                break;
            case 'i':
                dict_add(ignored, word);
                break;
            default:
                printf("Opcion incorrecta.\n");
        }
    }
}

/*******************************************************************
 * NAME :            void process_document(char *fname)
 *
 * DESCRIPTION :     Procesa el documento fname, palabra por palabra, 
 *                   consultando al usuario sobre la accion a realizar 
 *                   si la palabra no es conocida.
 * PARAMETERS:
 *      INPUT:
 *           char    *fname   Nombre del archivo a procesar.
 *
 * RETURN :
 *           Type: void
 *******************************************************************/
void process_document(char *fname){
    char current_word[MAX_WORD_SIZE];
    /* completar aca */
    if (fname == NULL) {
        printf("Nombre del documento invalido.\n");
        exit(1);
    }
    /* Creo los documentos */
    doc = calloc(1, sizeof(Document)+10);
    if (doc != NULL) {
        /* Abro los documentos */
        doc_open(doc, fname, "out.txt");

        while(doc_get_word(doc, current_word) == 1) {
            if (is_known(current_word) == 0 && (int)strlen(current_word) > 0) {
                consult_user(current_word);
            }
            doc_put_word(doc, current_word);
            for(int i=0; i<MAX_WORD_SIZE; i++) { current_word[i] = '\0'; }
        }
        doc_close(doc);
        free(doc);
        doc = NULL;
    }
}

/*******************************************************************
 * NAME :            int main(int argc, char **argv)
 *
 * DESCRIPTION :     Punto de entrada principal. Abre el diccionario 
 *                   principal, procesa el archivo especificado y 
 *                   guarda los cambios realizados en el diccionario 
 *                   principal.
 *******************************************************************/
int main(int argc, char **argv){
    char *dict;
    /* Verificamos el nro de argumentos. */
    if (argc < 2)
    {
        printf("spellchecker.c: nro de argumentos erroneo. Deben ser <documento> [<diccionario>].\n");
        return (1);
    }
    /* si se especifico un diccionario lo usamos,  */
    /* caso contrario usamos el diccionario por defecto */
    dict = (argc >=3) ? argv[2] : "dict.txt";

    /* completar aca */
    main_dict = dict_new();
    ignored = dict_new();
    dict_load(main_dict, dict);
    process_document(argv[1]);
    dict_save(main_dict, dict);
    printf("El documento %s ha sido procesado. Resultados en out.txt\n", argv[1]);
    main_dict = dict_destroy(main_dict);
    ignored = dict_destroy(ignored);
    return 0;
}
