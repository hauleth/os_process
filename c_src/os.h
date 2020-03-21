#include <stdlib.h>
#include <string.h>

#include <erl_nif.h>

#define PROCESS_INFO_FUNCTIONS(array) \
  struct Entry *process_info_functions = array; \
  size_t process_info_functions_size = sizeof(array) / sizeof(struct Entry);

typedef ERL_NIF_TERM gen_func(ErlNifEnv *env);

struct Entry {
  char *key;
  gen_func* gen;
};

extern struct Entry* process_info_functions;
extern size_t process_info_functions_size;
