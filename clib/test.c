#include <stdio.h>  //for printf
#include <stdlib.h> //for malloc
#include <hunpos.h> // for the analyzer

#define MAX_SENT_LENGTH 100
#define MAX_TAG_LENGTH 100
#define MAX_TOKEN_LENGTH 100
void cerr(char* txt)
{
  fprintf(stderr,txt);
}

// reads a sentence from the stdin, stores the values in tokens (caller allocates)
// and returns the number of tokens
int read_sentence(char ** tokens)
{
  int i, j;
  char * token;
  for(i = 0; i < MAX_SENT_LENGTH; i++)
  {
    if(!fgets(tokens[i], MAX_TOKEN_LENGTH, stdin)) {
      return i;
    }
    // don't like to type a lot
    token = tokens[i];

    // chomp(token)
    for (j = 0; token[j] != '\012'; ++j) {};
    token[j] = 0;
	  if (j == 0)
	  {
      return i;
	  }
  }
  return i-1;
}

static const char* get_token(void*tokens,int i, int* error) {
	return ((char**)tokens)[i];
}
static void add_tag(void*tokens,int i,const char* tag, int* error) {
	printf ("%s\t%s\n", get_token(tokens,i, error), tag);
}
int main(int argc, char ** argv)
{
  if(argc < 2)
  {
    cerr("usage: test model_file morph_table\n");
    return 1;
  }
  int error = 0;
  Hunpos hp = hunpos_tagger_new(argv[1], argv[2], 3, 1000, &error);
  char* tokens[MAX_SENT_LENGTH];
  int i, n;
  for (i=0; i<MAX_SENT_LENGTH;i++)
  {
    tokens[i] =  (char *) malloc(MAX_TOKEN_LENGTH * sizeof(char));
  }
  while((n = read_sentence(tokens)) > 0)
  {
    error = 0;
    //int j;
    //for(j = 0; j < 10000000; j++)
    hunpos_tagger_tag(hp, n, tokens, get_token, tokens, add_tag, &error);
    printf ("\n");
  }
}
