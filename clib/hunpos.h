#ifndef _HUNPOS_H_
#define _HUNPOS_H_

typedef  void* Hunpos;
typedef  void* TagList;

typedef const char* (*get_string_callback_t)(void*,int,int*);

typedef void (*set_string_callback_t)(void*,int,const char*,int*);

#undef  HUNPOS_BEGIN_C_DECLS
#undef  HUNPOS_END_C_DECLS
#ifdef __cplusplus
#  define HUNPOS_BEGIN_C_DECLS extern "C" {
#  define HUNPOS_END_C_DECLS }
#else /* !__cplusplus */
#  define HUNPOS_BEGIN_C_DECLS
#  define HUNPOS_END_C_DECLS
#endif  /* __cplusplus */

#if defined _WIN32 || defined __CYGWIN__
#  define HUNPOS_HELPER_DLL_IMPORT __declspec(dllimport)
#  define HUNPOS_HELPER_DLL_EXPORT __declspec(dllexport)
#  define HUNPOS_HELPER_DLL_LOCAL
#else
#  if __GNUC__ >= 4
#    define HUNPOS_HELPER_DLL_IMPORT __attribute__ ((visibility("default")))
#    define HUNPOS_HELPER_DLL_EXPORT __attribute__ ((visibility("default")))
#    define HUNPOS_HELPER_DLL_LOCAL  __attribute__ ((visibility("hidden")))
#  else
#    define HUNPOS_HELPER_DLL_IMPORT
#    define HUNPOS_HELPER_DLL_EXPORT
#    define HUNPOS_HELPER_DLL_LOCAL
#  endif
#endif

#ifdef HUNPOS_LIBRARIES_EXPORTS
#  ifdef HUNPOS_SRC
#    define HUNPOS_API HUNPOS_HELPER_DLL_EXPORT
#  else
#    define HUNPOS_API HUNPOS_HELPER_DLL_IMPORT
#  endif
#  define HUNPOS_LOCAL HUNPOS_HELPER_DLL_LOCAL
#else
#  define HUNPOS_API
#  define HUNPOS_LOCAL
#endif

HUNPOS_BEGIN_C_DECLS

/**
 * @param modelfile
 * @param morphtable
 * @param max_guessed_tags
 * @param theta
 * @param error pointer to an integer to report errors or NULL
 *
 * @return an Hunpos tagger or NULL on errors
 */
HUNPOS_API Hunpos hunpos_tagger_new(const char* modelfile, const char* morphtable, int max_guessed_tags, int theta, int* error);

/**
 * @param hp
 * @param n tokens number
 * @param tokens the tokens to be tagged
 * @param get_token function that given @p tokens and an index, returns the token content.
 * @param tags output tags
 * @param add_tag function that given @p tags, an index and the tag content, sets the tag content.
 * @param error pointer to an integer to report errors or NULL
 */
HUNPOS_API void hunpos_tagger_tag(Hunpos hp, int n, void* tokens, get_string_callback_t get_token, void* tags, set_string_callback_t add_tag, int* error);

/**
 * @param hp
 * @param error pointer to an integer to report errors or NULL
 */
HUNPOS_API void hunpos_tagger_destroy(Hunpos hp, int* error);

HUNPOS_END_C_DECLS

#endif
