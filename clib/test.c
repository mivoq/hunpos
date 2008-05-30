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
}
int main(int argc, char ** argv)
{
  if(argc < 2)
  {
    cerr("usage: test model_file morph_table\n");
    return 1;
  }
  
  hunpos hp = init_hunpos(argv[1], argv[2], 3, 1000);
  char* tokens[MAX_SENT_LENGTH];
  char* tags[MAX_SENT_LENGTH];
  
  int i, n;
  for (i=0; i<MAX_SENT_LENGTH;i++) 
  {
    tokens[i] =  (char *) malloc(MAX_TOKEN_LENGTH * sizeof(char));
    tags[i] = (char *) malloc(MAX_TAG_LENGTH * sizeof(char));
  }
  while((n = read_sentence(tokens)) > 0)
  {
    tag(hp, n, tokens, tags);
    for(i = 0; i < n; i++)
    {
      printf ("%s\t%s\n", tokens[i], tags[i]);
    }
    printf ("\n");
  }
}
