#include <string>
#include <vector>
extern "C" {
#include <caml/mlvalues.h>
#include <caml/callback.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
}  
#include "hunpos.h"

using namespace std;

  
  
  // one should add some other constructor with default values
HunPos::HunPos(const string model_file, const string morph_table_file, int max_guessed_tags, int theta)
  {
    // notice: ocaml is a function language
    // we call a function init that returns a new function which can do the tagging
     static value* init_fun;
     if (init_fun == NULL) {
           init_fun = caml_named_value("init_from_aff_dic");
     }
    
     CAMLparam0();
     // we pass some argument to the function
     CAMLlocalN ( args, 1 );
     args[0] = caml_copy_string(model_file.c_str());
     args[1] = caml_copy_string(morph_table_file.c_str());
     args[2] = caml_copy_nativeint(max_guessed_tags);
     args[3] = caml_copy_nativeint(theta);
     
     // due to the garbage collector we have to register the
     // returned value not to be deallocated
     caml_register_global_root( ((value*) tagger_fun));
     
     tagger_fun = (long*) caml_callbackN( *init_fun, 4, args );
     
     
    
  }


 
