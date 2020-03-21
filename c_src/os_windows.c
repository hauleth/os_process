#include <Windows.h>

#include "os.h"

static ERL_NIF_TERM get_pid(ErlNifEnv* env) { return enif_make_int(env, GetCurrentProcessId()); }
static ERL_NIF_TERM get_user_name(ErlNifEnv* env) {
  ErlNifBinary bin;
  enif_alloc_binary(UNLEN + 1, &bin);

  if (!GetUserNameA(bin.data, &bin.size)) {
    bin.size = 0;
  }

  return enif_make_binary(env, &bin);
}

static struct Entry functions[] = {
  {"pid", get_pid},
  {"user_name", get_user_name},
};

PROCESS_INFO_FUNCTIONS(functions)
