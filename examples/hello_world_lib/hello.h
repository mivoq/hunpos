#ifndef _HELLO_H_
#define _HELLO_H_

#undef  HELLO_BEGIN_C_DECLS
#undef  HELLO_END_C_DECLS
#ifdef __cplusplus
#  define HELLO_BEGIN_C_DECLS extern "C" {
#  define HELLO_END_C_DECLS }
#else /* !__cplusplus */
#  define HELLO_BEGIN_C_DECLS
#  define HELLO_END_C_DECLS
#endif  /* __cplusplus */

#if defined _WIN32 || defined __CYGWIN__
#  define HELLO_HELPER_DLL_IMPORT __declspec(dllimport)
#  define HELLO_HELPER_DLL_EXPORT __declspec(dllexport)
#  define HELLO_HELPER_DLL_LOCAL
#else
#  if __GNUC__ >= 4
#    define HELLO_HELPER_DLL_IMPORT __attribute__ ((visibility("default")))
#    define HELLO_HELPER_DLL_EXPORT __attribute__ ((visibility("default")))
#    define HELLO_HELPER_DLL_LOCAL  __attribute__ ((visibility("hidden")))
#  else
#    define HELLO_HELPER_DLL_IMPORT
#    define HELLO_HELPER_DLL_EXPORT
#    define HELLO_HELPER_DLL_LOCAL
#  endif
#endif

#ifdef HELLO_LIBRARIES_EXPORTS
#  ifdef HELLO_SRC
#    define HELLO_API HELLO_HELPER_DLL_EXPORT
#  else
#    define HELLO_API HELLO_HELPER_DLL_IMPORT
#  endif
#  define HELLO_LOCAL HELLO_HELPER_DLL_LOCAL
#else
#  define HELLO_API
#  define HELLO_LOCAL
#endif

HELLO_BEGIN_C_DECLS

HELLO_API void hello_world();

HELLO_END_C_DECLS

#endif
