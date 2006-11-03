
#include <maxent/maxentmodel.hpp> 
#include <string>
#include <vector>

extern "C" {
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>

	
static struct custom_operations maxent_model_ops = {
    "maxent modell",
	  custom_finalize_default,
	  custom_compare_default,
	  custom_hash_default,
	  custom_serialize_default,
	  custom_deserialize_default
};

/* Accessing the Maxent::MaxentModel * part of a Caml custom block */
#define Model_val(v) (*((maxent::MaxentModel  **) Data_custom_val(v)))


/* Allocating a Caml custom block to hold the given MaxentModell * */
static value alloc_model(maxent::MaxentModel * w)
{
  value v = alloc_custom(&maxent_model_ops, sizeof(maxent::MaxentModel *), 0, 1);
  Model_val(v) = w;
  return v;
}

/* converts an ocaml string list to C++ string vector. In Ocaml a binary predicate context
   is given by a string list
*/
std::vector<std::string> to_vector(value context) {
	std::vector<std::string> ctx ;
	/* context is a string list */
	value tail = context;
	while(tail != Val_emptylist) {
		ctx.push_back(String_val(Field(tail, Tag_cons)));
		tail = Field(tail, 1);
	}
	return ctx;	
}


value maxent_new ()
{
	CAMLparam0 ();
	CAMLlocal1(r);
	maxent::MaxentModel* m = new maxent::MaxentModel;
	m->begin_add_event();
	r=alloc_model(m);
	
	CAMLreturn (r);
}



void maxent_add_event(value model, value context, value outcome, value n) {
	CAMLparam4(model, context, outcome, n);
	Model_val(model)->add_event(to_vector(context), String_val(outcome), 1); 
	
	
}



void maxent_train(value model) {
	CAMLparam1(model);
	
	Model_val(model)->end_add_event();
	maxent::verbose=1;
	Model_val(model)->train(10, "gis", 2); // train the model with 100 iterations of GIS method 

	CAMLreturn0;
}

void maxent_save(value model, value filename) {
	CAMLparam2 (model, filename);
		

	Model_val(model)->save(String_val(filename), false); 
	CAMLreturn0;
}

value maxent_load(value filename) {
	CAMLparam1 (filename);
	CAMLlocal1(r);
	maxent::MaxentModel* m = new maxent::MaxentModel;
	m->load(String_val(filename));
	r=alloc_model(m);
	
	CAMLreturn (r);
	
}
value maxent_eval(value model, value context, value outcome) {
	CAMLparam3 (model, context, outcome);
	CAMLlocal1(r);
	
	double p = Model_val(model)->eval(to_vector(context), String_val(outcome)); 
	r = caml_copy_double(p);
	CAMLreturn(r) ;
}

value maxent_eval_all(value model, value context) 
{
	CAMLparam2 (model, context);
	CAMLlocal3 (r, t, v);
	
	std::vector<pair<std::string, double> > probs; 
	Model_val(model)->eval_all(to_vector(context), probs); 
	
	r = Val_int(0);  /* the [] */
	
	for (std::vector<pair<std::string, double> >::iterator it = probs.begin(); it!=probs.end(); ++it) {
		/* this is the same as in ocaml
			let r = ((*it).first , (*it).second ):: r
		
		   first create a tuple */
		 t = caml_alloc_tuple(2);
		 Store_field(t, 0, caml_copy_string(((*it).first).c_str ()));
		 Store_field(t, 1, caml_copy_double((*it).second));
		
	 	 /* Allocate a cons cell */
		 v = caml_alloc_small(2, 0); 
		 Field(v, 0) = t;  
		 Field(v, 1) = r; /* add to the list as head */								
		 r = v;
	}
	CAMLreturn (r);
}



}