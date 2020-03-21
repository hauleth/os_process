#include <stdlib.h>
#include <string.h>

#include <erl_nif.h>

typedef ERL_NIF_TERM gen_func(ErlNifEnv *env);

struct Entry {
  char *key;
  gen_func* gen;
};

struct Entry* process_info_functions(size_t *size);
