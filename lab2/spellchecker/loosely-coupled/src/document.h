#ifndef DOCUMENT_H
#define DOCUMENT_H

typedef struct sDocument *Document;

int doc_get_word(Document d, char *word);
void doc_put_word(Document d, char *word);
void doc_open(Document d, char *fread, char *fwrite);
void doc_close(Document d);

#endif
