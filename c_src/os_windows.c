#include <Windows.h>

#include "os.h"

static ERL_NIF_TERM get_pid(ErlNifEnv* env) { return enif_make_int(env, GetCurrentProcessId()); }

static struct Entry functions[] = {
  {"pid", get_pid},
};

PROCESS_INFO_FUNCTIONS(functions)
