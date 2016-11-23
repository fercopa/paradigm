#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "dictionary.h"

#define TAM_INIC_DIC 10
#define TAM_MAX_WORD 30

struct sDictionary {
    char **dic;
    int tam;
};

void error_dict(char *msg) {
    printf("%s\n", msg);
    exit(1);
}

void dict_load(Dictionary d, char *fname) {
    int i = 0,
        j = 0;
    char c;
    FILE *f = NULL;
    char *word = NULL;

    if (fname == NULL) {
        error_dict("Nombre de archivo invalido.\n");
    }
    d->dic = calloc(TAM_INIC_DIC, sizeof(char*));
    if (d->dic == NULL) {
        error_dict("Memoria insuficiente\n");
    }
    d->tam = TAM_INIC_DIC;
    f = fopen(fname, "r");
    if (f == NULL) {
        printf("Debe crear un archivo diccionario. Ej touch dict.txt\n");
        exit(1);
    }
    word = calloc(TAM_MAX_WORD, sizeof(char));
    if (word == NULL) {
        error_dict("Memoria insuficiente\n");
    }
    /* Read a word */
    while((c=fgetc(f)) != EOF) {
        if (c != '\r' && c != '\n' && c != '\0') {
            if (i == TAM_MAX_WORD-1) {
                free(word);
                free(d->dic);
                error_dict("El diccionario contiene palabras demasiadas largas.");
            }
            word[i] = c;
            i++;
        } else {
            // realloc if len(word) == MAX_WORD_SIZE
            word[i] = '\0';
            d->dic[j] = word;
            i = 0;
            j++;
            word = NULL;
            word = calloc(TAM_MAX_WORD, sizeof(char));
            if (word == NULL) {
                error_dict("Memoria insuficiente\n");
            }
            if (j == d->tam) {
                // Duplico main_size
                d->tam = d->tam * 2;
                d->dic = realloc(d->dic, d->tam*sizeof(char*));
                if (d->dic == NULL) {
                    error_dict("Memoria insuficiente\n");
                }
            }
        }
    }
    // Ajusto la lista al tamano de la cantidad de palabras.
    // j lleva la cantidad de palabras que hay en la lista
    if (j != d->tam) {
        d->tam = j;
        d->dic = realloc(d->dic, d->tam*sizeof(char*));
    }
    fclose(f);
    free(word);
}

void dict_save(Dictionary d, char *fname) {
    FILE *f = NULL;
    f = fopen(fname, "w");
    if (f == NULL) {
        error_dict("Ocurrio un error al intentar abrir el archivo.\n");
    }
    for (int i=0; i<d->tam; i++) {
        fputs(d->dic[i], f);
        fputs("\n", f);
    }
    fclose(f);
}

void dict_add(Dictionary d, char *word) {
    int n = 0; // len de word
    char *wordcpy = NULL;

    if (d != NULL && word != NULL) {
        n = strlen(word);
        wordcpy = calloc(n+1, sizeof(char));
        if (wordcpy == NULL) {
            printf("Memoria insuficiente\n");
        } else {
            strncpy(wordcpy, word, n+1);
            // Agrando el tamano del dict_main
            d->tam += 1;
            d->dic = realloc(d->dic, d->tam*sizeof(char*));
            if (d->dic == NULL) {
                printf("Memoria insuficiene.\n");
                free(wordcpy);
            }
            d->dic[d->tam-1] = wordcpy;
        }
    }
}

int dict_contains(Dictionary d, char *word) {
    int b = 0,
        res = 0;

    if (word == NULL) {
        printf("Palabra nula.\n");
    } else {
        for(int i=0; i<d->tam; i++) {
            b = strcmp(d->dic[i], word);
            if (b == 0) {
                res = 1;
                break;
            }
        }
    }
    return res;
}

Dictionary dict_new(void) {
    Dictionary d = NULL;
    d = calloc(1, sizeof(struct sDictionary));
    if (d != NULL) {
        d->tam = 0;
    } else {
        error_dict("Memoria insuficiente\n");
    }
    return d;
}

Dictionary dict_destroy(Dictionary d) {
    if (d != NULL) {
        if (d->dic != NULL) {
            for (int i=0; i<d->tam; i++) { free(d->dic[i]); }
            free(d->dic);
        }
        free(d);
        d = NULL;
    }
    return d;
}
