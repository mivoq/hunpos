#include <string.h>
#include <caml/mlvalues.h>
#include <caml/callback.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include "stdio.h"
#include "hunpos.h"

static int is_initialized = 0;

Hunpos hunpos_tagger_new(const char* model_file, const char* morph_table_file, int max_guessed_tags, int theta, int* error)
{
    *error = 0;
    if(model_file == NULL) {
	*error = 3;
	return NULL;
    }
    if(morph_table_file == NULL) {
	morph_table_file = "";
    }

    /* Startup OCaml */
    if (is_initialized == 0)
    {
	is_initialized = 1;
	char* dummyargv[2];
	dummyargv[0]="";
	dummyargv[1]=NULL;
	caml_startup(dummyargv);
    }
    CAMLparam0();

    /* get hunpos init function from ocaml */
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

     /* due to the garbage collector we have to register the */
     /* returned value not to be deallocated                 */
     caml_register_global_root(tagger_fun);
     value* t = tagger_fun;
     *t =  caml_callbackN_exn( *init_fun, 4, args );
     if (Is_exception_result(*t))
     {
	*error = 1;
	CAMLreturnT(Hunpos, NULL);
     }

     // CAMLreturn1(tagger_fun)
     CAMLreturnT(Hunpos,tagger_fun);

  }

void hunpos_tagger_tag(Hunpos hp, int n, void* tokens, const char* (*get_token)(void*,int, int*), void* tags, int (*add_tag)(void*,int,const char*), int* error)
{
	CAMLparam0();
	CAMLlocal3 (return_value, list, v);
	int i;
	list = Val_emptylist;  /* the [] */
	*error = 0;
	for(i = 0; i< n; i ++)
	{
		/* Allocate a cons cell */
		v = caml_alloc_small(2, 0);
		const char* token = get_token(tokens, i, error);
		if (*error != 0) CAMLreturn0;
		Store_field (v, 0, caml_copy_string(token) );
		Store_field (v, 1, list );
		list = v;
	}

	return_value = caml_callback(*((value*)hp), list);
	return_value = Field(return_value,1);

	i = 0;
	while(return_value != Val_emptylist) {
		char* s = String_val(Field(return_value, Tag_cons));
		*error = add_tag(tags, i++, s);
		if (*error != 0) CAMLreturn0;
		return_value = Field(return_value, 1);
	}

	CAMLreturn0;

}

/* hunpos destruction */
void hunpos_tagger_destroy(Hunpos hp, int* error)
{
	CAMLparam0();
	*error = 0;
	caml_remove_global_root((value*) hp);
	free(hp);
	CAMLreturn0;
}


