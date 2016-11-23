#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

//tamaño maximo de una palabra
#define MAX_WORD_SIZE 30

/* diccionario principal */
char **dict_main;
/*tamaño inicial del diccionario principal*/
int main_size = 10;

/* diccionario de palabras ignoradas */
char **dict_ignored;
/*tamaño inicial del diccionario de palabras ignoradas*/
int ignored_size = 0;

/* Documento de entrada */
FILE *doc_in;
/* Documento de salida */
FILE *doc_out;

/*******************************************************************
 * NAME :            void dict_load(char *fname)
 *
 * DESCRIPTION :     Carga en memoria el diccionario principal desde 
 *                   el archivo fname
 *
 * PARAMETERS:
 *      INPUT:
 *           char    *fname       Nombre del archivo que contiene el 
 *                                diccionario
 * RETURN :
 *           Type: void
 *
 * OBSERVATIONS :
 *    1) El tamaño de la lista que contiene representa al diccionario 
 *       debe coincidir con el nro de palabras en el diccionario. 
 *       Esto implica que si es necesario hay que "agrandar" la lista, 
 *       y si sobra espacio en la lista hay que "achicarla".
 *    2) Recordar borrar el \n y \r al final de cada palabra y que los 
 *       strings en C terminan en \0.
 *******************************************************************/
void dict_load(char *fname){
    /* completar aca */
    int i = 0,
        j = 0;
    char c;
    FILE *f;
    char *word = NULL;

    dict_main = calloc(main_size, sizeof(char*));
    if (dict_main != NULL) {
        f = fopen(fname, "r");
        if (f != NULL) {
            word = calloc(MAX_WORD_SIZE, sizeof(char));
            if (word != NULL) {
                /* Read a word */
                while((c=fgetc(f)) != EOF) {
                    if (c != '\r' && c != '\n' && c != '\0') {
                        if (i == MAX_WORD_SIZE-1) { // Reservo el ultimo lugar para el '\0'.
                            free(word);
                            word = NULL;
                            free(dict_main);
                            dict_main = NULL;
                            printf("El diccionario contiene palabras con mas de %i caracteres permitidos.\n", MAX_WORD_SIZE);
                            exit(1);
                        } else {
                            word[i] = c;
                            i++;
                        }
                    } else {
                        word[i] = '\0';
                        dict_main[j] = word;
                        i = 0;
                        j++;
                        word = NULL;
                        word = calloc(MAX_WORD_SIZE, sizeof(char));
                        if (j == main_size) {
                            // Duplico main_size
                            main_size = main_size * 2;
                            dict_main = realloc(dict_main, main_size*sizeof(char*));
                        }
                    }
                }
            } else {
                free(dict_main);
                dict_main = NULL;
                printf("Memoria insuficiente\n");
                exit(1);
            }
        } else {
            printf("Debe crear un archivo diccionario. Ej: touch dict.txt\n");
            exit(1);
        }
        // Ajusto la lista al tamano de la cantidad de palabras.
        // j lleva la cantidad de palabras que hay en la lista
        if (j != main_size) {
            main_size = j;
            dict_main = realloc(dict_main, main_size*sizeof(char*));
        }
        fclose(f);
        free(word);
    } else {
        printf("Memoria insuficiente\n");
        exit(1);
    }
}

/*******************************************************************
 * NAME :            void dict_save(char *fname)
 *
 * DESCRIPTION :     Guarda el diccionario principal en el archivo 
 *                   fname
 *
 * PARAMETERS:
 *      INPUT:
 *           char    *fname       Nombre del archivo donde se guardara
 *                                el diccionario
 * RETURN :
 *           Type: void
 *******************************************************************/
void dict_save(char *fname){
    /* completar aca  */
    FILE *f;
    if (fname == NULL) {
        printf("Nombre de archivo invalido.\n");
        exit(1);
    }
    f = fopen(fname, "w");
    for (int i=0; i<main_size; i++) {
        fputs(dict_main[i], f);
        fputs("\n", f);
    }
    fclose(f);
}

/*******************************************************************
 * NAME :            void dict_add(char *word)
 *
 * DESCRIPTION :     Agrega una palabra al diccionario principal.
 *
 * PARAMETERS:
 *      INPUT:
 *           char    *word       Palabra a ser agregada.
 *
 * RETURN :
 *           Type: void
 *
 * OBSERVATIONS :
 *    1) Recordar que la lista que representa el diccionario no tiene 
 *       lugar "de sobra".
 *******************************************************************/
void dict_add(char *word){
    /* completar aca */
    int n = 0; // len de word
    char *wordcpy = NULL;

    if (word != NULL) {
        n = strlen(word);
        wordcpy = calloc(n+1, sizeof(char));
        if (wordcpy == NULL) {
            printf("Memoria insuficiente\n");
            exit(1);
        }
        strncpy(wordcpy, word, n+1);
        // Agrando el tamano del dict_main
        main_size += 1;
        dict_main = realloc(dict_main, main_size*sizeof(char*));
        if (dict_main == NULL) {
            main_size -= 1;
            printf("Memoria insuficiente\n");
            exit(1);
        }
        dict_main[main_size-1] = wordcpy;
    }
}

/*******************************************************************
 * NAME :            void ignored_add(char *word)
 *
 * DESCRIPTION :     Agrega una palabra al diccionario de palabras 
 *                   ignoradas.
 *
 * PARAMETERS:
 *      INPUT:
 *           char    *word       Palabra a ser agregada.

 * RETURN :
 *           Type: void
 * OBSERVATIONS :
 *    1) La lista que representa el diccionario de palabras ignoradas 
 *       debe agrandarse a medida que se agregan palabras.
 *******************************************************************/
void ignored_add(char *word){
    /* completar aca  */
    int n = 0; // len de word
    char *wordcpy = NULL;

    if (word != NULL) {
        ignored_size += 1;
        if (ignored_size - 1 == 0) {
            dict_ignored = calloc(ignored_size, sizeof(char*));
            if (dict_ignored == NULL) {
                printf("Memoria insuficiente\n");
                exit(1);
            }
        } else {
            dict_ignored = realloc(dict_ignored, ignored_size*sizeof(char*));
            if (dict_ignored == NULL) {
                printf("Memoria insuficiente\n");
                exit(1);
            }
        }
        n = strlen(word);
        wordcpy = calloc(n+1, sizeof(char));
        if (wordcpy == NULL) {
            printf("Memoria insuficiente\n");
            exit(1);
        }
        strncpy(wordcpy, word, n+1);
        dict_ignored[ignored_size-1] = wordcpy;
    }
}

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
        resA = 0,
        resB = 0,
        res = 0;

    if (word != NULL) {
        // Buscamos en el diccionario principal
        for(int i=0; i<main_size; i++) {
            a = strcmp(dict_main[i], word);
            if (a == 0) {
                resA = 1;
                break;
            }
        }
        // Buscamos en el diccionario de ignorados
        for(int i=0; i<ignored_size; i++) {
            b = strcmp(dict_ignored[i], word);
            if (b == 0) {
                resB = 1;
                break;
            }
        }
    }
    res = (resA == 1 || resB == 1)? 1 : 0;
    return res;
}


/*******************************************************************
 * NAME :            int get_word(char *w)
 *
 * DESCRIPTION :     Lee una palabra del archivo de entrada, copiando 
 *                   todo caracter de puntuacion precedente al archivo
 *                   de salida.
 * PARAMETERS:
 *      OUTPUT:
 *           char    *word       Palabra que se lee desde el archivo.
 *
 * RETURN :
 *           Type: int
 *           Values: 0 si no hay mas palabras para leer.  
 *                   1 si hay mas palabras para leer.
 *******************************************************************/
int get_word(char *word){
    /* completar aca */
    char c;
    int i = 0,
        res = 0;

    if (doc_in == NULL || word == NULL) {
        exit(1);
    }
    c = fgetc(doc_in);
    while (!isalpha(c) && c != EOF && c != '\r') {
        fputc(c, doc_out);
        c = fgetc(doc_in);
    }
    while (isalpha(c)) {
        word[i] = c;
        i++;
        c = fgetc(doc_in);
        if (!isalpha(c)) {
            word[i] = '\0';
            fseek(doc_in, -1, SEEK_CUR);
        }
    }
    res = (c == EOF)? 0: 1;
    return res;
}

/*******************************************************************
 * NAME :            void put_word(char *word)
 *
 * DESCRIPTION :     Escribe la palabra w al archivo de salida.
 *
 * PARAMETERS:
 *      INPUT:
 *           char    *word       Palabra a escribir.
 *
 * RETURN :
 *           Type: void
 *******************************************************************/
void put_word(char *word){
    /* completar aca  */
    if (word != NULL) {
        if (strlen(word) > 0) {
            fputs(word, doc_out);
        }
    }
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
    do{
        printf("Palabra no reconocida: %s\n Aceptar (a) - Ignorar (i) - Reemplazar (r): ", word);
        scanf("%s", ans);
    }while((strcmp(ans,"r") != 0) && (strcmp(ans,"a") != 0) && (strcmp(ans,"i") != 0));
    /* completar aca  */
    switch(ans[0]) {
        case 'r':
            /* limpio word */
            for(int i=0; i<MAX_WORD_SIZE; i++) { word[i] = '\0'; }
            printf("Corregir a: ");
            scanf("%s", word);
            break;
        case 'a':
            dict_add(word);
            break;
        case 'i':
            ignored_add(word);
            break;
        default:
            printf("Respuesta invalida\n");
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
    doc_in = fopen(fname, "r");
    if (doc_in == NULL){
        printf("Error: el archivo %s no existe.\n", fname);
        exit(1);
    }

    while(get_word(current_word) == 1) {
        /* current_word debe tener al menos un caracter valido */
        if (is_known(current_word) == 0 && (int)strlen(current_word) > 0) {
            consult_user(current_word);
        }
        put_word(current_word);
        /* Limpio current_word */
        for(int i=0; i<MAX_WORD_SIZE; i++) {
            current_word[i] = '\0';
        }
    }
    fclose(doc_in);
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
    dict_load(dict);
    doc_out = fopen("out.txt", "w");
    process_document(argv[1]);
    dict_save(dict);
    free(dict_ignored);
    printf("El documento %s ha sido procesado. Resultados en out.txt\n", argv[1]);
    fclose(doc_out);
    for(int i=0; i<main_size; i++) { free(dict_main[i]); }
    free(dict_main);
    return 0;
}
