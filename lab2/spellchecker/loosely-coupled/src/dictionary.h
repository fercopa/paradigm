#ifndef DICTIONARY_H
#define DICTIONARY_H

typedef struct sDictionary *Dictionary;

void dict_load(Dictionary d, char *fname);
void dict_save(Dictionary d, char *fname);
void dict_add(Dictionary d, char *word);
int dict_contains(Dictionary d, char *word);
Dictionary dict_new(void);
Dictionary dict_destroy(Dictionary d);

#endif
