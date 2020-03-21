#include <Windows.h>

#include "os.h"

static ERL_NIF_TERM get_pid(ErlNifEnv* env) { return enif_make_int(env, GetCurrentProcessId()); }

struct Entry functions[] = {
  {"pid", get_pid},
};

struct Entry* process_info_functions(size_t *size) {
  *size = sizeof(functions) / sizeof(struct Entry);

  return functions;
}
