#ifndef _HUNPOS_H_
#define _HUNPOS_H_

typedef  void* Hunpos;
typedef  void* TagList;

/**
 * @param modelfile
 * @param morphtable
 * @param max_guessed_tags
 * @param theta
 * @param error pointer to an integer to report errors or NULL
 *
 * @return an Hunpos tagger or NULL on errors
 */
Hunpos hunpos_tagger_new(const char* modelfile, const char* morphtable, int max_guessed_tags, int theta, int* error);

typedef const char* (*get_string_callback_t)(void*,int,int*);

typedef void (*set_string_callback_t)(void*,int,const char*,int*);

/**
 * @param hp
 * @param n tokens number
 * @param tokens the tokens to be tagged
 * @param get_token function that given @p tokens and an index, returns the token content.
 * @param tags output tags
 * @param add_tag function that given @p tags, an index and the tag content, sets the tag content.
 * @param error pointer to an integer to report errors or NULL
 */
void hunpos_tagger_tag(Hunpos hp, int n, void* tokens, get_string_callback_t get_token, void* tags, set_string_callback_t add_tag, int* error);

/**
 * @param hp
 * @param error pointer to an integer to report errors or NULL
 */
void hunpos_tagger_destroy(Hunpos hp, int* error);

#endif /* _HUNPOS_H_ */
