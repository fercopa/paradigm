#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "document.h"

struct sDocument {
    FILE *doc_in;
    FILE *doc_out;
};

int doc_get_word(Document d, char *word) {
    char c;
    int i = 0,
        res = 0;

    if (d == NULL || d->doc_in == NULL || word == NULL || d->doc_out == NULL) {
        return 0;
    }
    c = fgetc(d->doc_in);
    while (!isalpha(c) && c != EOF) {
        fputc(c, d->doc_out);
        c = fgetc(d->doc_in);
    }
    while (isalpha(c)) {
        word[i] = c;
        i++;
        c = fgetc(d->doc_in);
        if (!isalpha(c)) {
            word[i] = '\0';
            fseek(d->doc_in, -1, SEEK_CUR);
        }
    }
    res = (c == EOF)? 0: 1;
    return res;
}

void doc_put_word(Document d, char *word) {
    if (d != NULL && d->doc_out != NULL && word != NULL) {
        if (strlen(word) > 0) {
            fputs(word, d->doc_out);
        }
    }
}

void doc_open(Document d, char *file_read, char *file_write) {
    if (d == NULL) {
        printf("Documento nulo.\n");
    } else {
        d->doc_in = fopen(file_read, "r");
        if (d->doc_in == NULL) {
            printf("El documento '%s' no existe.\n", file_read);
            exit(1);
        }
        d->doc_out = fopen(file_write, "w");
    }
}

void doc_close(Document d) {
    fclose(d->doc_in);
    fclose(d->doc_out);
}
