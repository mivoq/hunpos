#ifndef _HUNPOS_H_
#define _HUNPOS_H_
#include <caml/mlvalues.h>
typedef  value hunpos ;

hunpos init_hunpos(char* modelfile, char* morphtable, int x, int y);

void tag(hunpos hp, int n, char** tokens, char** tags);



#endif /* _HUNPOS_H_ */