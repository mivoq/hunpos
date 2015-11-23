#include <string.h>
#include <caml/mlvalues.h>
#include <caml/callback.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include "stdio.h"
#include "hunpos.h"
 
  // one should add some other constructor with default values

Hunpos hunpos_tagger_new(const char* model_file, const char* morph_table_file, int max_guessed_tags, int theta, int* error)
{
    CAMLparam0();
    if(model_file == NULL) {
	return NULL;
    }
    if(morph_table_file == NULL) {
	morph_table_file = "";
    }

    char* dummyargv[2];
    dummyargv[0]="";
    dummyargv[1]=NULL;
    caml_startup(dummyargv);
    
    // notice: ocaml is a functional language
    // we call a function init that returns a new function which can do the tagging
     static value* init_fun;
     if (init_fun == NULL) 
     {
           init_fun = caml_named_value("init_from_files");
     }
    
     Hunpos tagger_fun = (Hunpos) malloc(sizeof(value));

     // we pass some argument to the function
     CAMLlocalN ( args, 4 );
     args[0] = caml_copy_string(model_file);
     args[1] = caml_copy_string(morph_table_file);
     args[2] = Val_int(max_guessed_tags);
     args[3] = Val_int(theta);
     
     // due to the garbage collector we have to register the
     // returned value not to be deallocated
     caml_register_global_root(tagger_fun);
     value* t = tagger_fun;
     *t =  caml_callbackN( *init_fun, 4, args );

     // CAMLreturn1(tagger_fun)
     CAMLreturnT(Hunpos,tagger_fun);
    
  }
 
void hunpos_tagger_tag(Hunpos hp, int n, void* tokens, const char* (*get_token)(void*,int), void* tags, int (*add_tag)(void*,int,const char*), int* error)
{
  CAMLparam0();
  CAMLlocal3 (return_value,list, v);
  int i;
  list = Val_emptylist;  /* the [] */
  for(i = 0; i< n; i ++)
  {
  	 	 /* Allocate a cons cell */
  		 v = caml_alloc_small(2, 0); 
  		 Field(v, 0) = caml_copy_string(get_token(tokens,i));
  		 Field(v, 1) = list; /* add to the list as head */								
  		 list = v;
  }

  return_value = caml_callback(*((value*)hp), list);
  return_value = Field(return_value,1);

  i = 0;
  while(return_value != Val_emptylist) {
	  char* s = String_val(Field(return_value, Tag_cons));
	  add_tag(tags, i++, s);
	  return_value = Field(return_value, 1);
  }

  CAMLreturn0;
  
}



 
