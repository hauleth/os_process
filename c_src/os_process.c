#include "os.h"

static void add(ErlNifEnv *env, ERL_NIF_TERM *map, struct Entry entry) {
  ERL_NIF_TERM atom;

  if (!enif_make_existing_atom(env, entry.key, &atom, ERL_NIF_LATIN1))
    atom = enif_make_atom(env, entry.key);

  enif_make_map_put(env, *map, atom, (entry.gen)(env), map);
}

static ERL_NIF_TERM process_info_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  ERL_NIF_TERM map = enif_make_new_map(env);
  size_t size = 0;

  for (size_t i = 0; i < process_info_functions_size; ++i) {
    add(env, &map, process_info_functions[i]);
  }

  return map;
}

static ErlNifFunc nif_funcs[] = {
  {"info", 0, process_info_nif}
};

ERL_NIF_INIT(os_process, nif_funcs, NULL, NULL, NULL, NULL)
