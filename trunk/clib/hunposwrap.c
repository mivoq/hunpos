#include <string.h>
#include <caml/mlvalues.h>
#include <caml/callback.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include "stdio.h"
#include "hunpos.h"
 // lehet, hogy kell strncpy
 
/* converts an ocaml string list to C++ string vector. In Ocaml a binary predicate context
   is given by a string list
*/
void string_list_to_C_string_array(value context, char** r) {
	CAMLparam1(context);
	CAMLlocal1(tail);

	/* context is a string list */
	tail = context;
  int i = 0;
	while(tail != Val_emptylist) {
		char* s = String_val(Field(tail, Tag_cons));
    r[i++] = s;
		tail = Field(tail, 1);
	}
	CAMLreturn0;	
}

 
  // one should add some other constructor with default values

hunpos init_hunpos(char* model_file, char* morph_table_file, int max_guessed_tags, int theta)
{
    
    CAMLparam0();

    char* dummyargv[2];
    dummyargv[0]="cmorph";
    dummyargv[1]=0;
    caml_startup(dummyargv);
    
    // notice: ocaml is a function language
    // we call a function init that returns a new function which can do the tagging
     static value* init_fun;
     if (init_fun == NULL) 
     {
           init_fun = caml_named_value("init_from_files");
     }
    
     hunpos tagger_fun = (hunpos) malloc(sizeof(value));

     // we pass some argument to the function
     CAMLlocalN ( args, 4 );
     args[0] = caml_copy_string(model_file);
     args[1] = caml_copy_string(morph_table_file);
     args[2] = Val_int(max_guessed_tags);
     args[3] = Val_int(theta);
     
     // due to the garbage collector we have to register the
     // returned value not to be deallocated
     caml_register_global_root(& tagger_fun);
     tagger_fun =  caml_callbackN( *init_fun, 4, args );

     // CAMLreturn1(tagger_fun)
     CAMLreturn( tagger_fun);
    
  }
 
void tag(hunpos hp, int n, char** tokens, char** tags) 
{
  CAMLparam0();
  CAMLlocal3 (return_value,list, v);
  int i;
  list = Val_emptylist;  /* the [] */

  // we construct an ocaml string list
  // notice the first token is at the head
  // we spare on List.rev
  for(i = n-1; i > -1; i --)
  {
  	 	 /* Allocate a cons cell */
  		 v = caml_alloc_small(2, 0); 
  		 Field(v, 0) = caml_copy_string(tokens[i]);  
  		 Field(v, 1) = list; /* add to the list as head */								
  		 list = v;
  }
 
  return_value = caml_callback(hp, list);
  string_list_to_C_string_array(return_value, tags);
  CAMLreturn0;
  
}



 
