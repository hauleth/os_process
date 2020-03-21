#include <sys/time.h>
#include <sys/resource.h>
#include <sys/types.h>
#include <unistd.h>

#include "os.h"

static ERL_NIF_TERM get_pid(ErlNifEnv* env) { return enif_make_int(env, getpid()); }
static ERL_NIF_TERM get_pgid(ErlNifEnv* env) { return enif_make_int(env, getpgid(0)); }
static ERL_NIF_TERM get_parent_pid(ErlNifEnv* env) { return enif_make_int(env, getppid()); }
static ERL_NIF_TERM get_uid(ErlNifEnv* env) { return enif_make_int(env, getuid()); }
static ERL_NIF_TERM get_gid(ErlNifEnv* env) { return enif_make_int(env, getgid()); }
static ERL_NIF_TERM get_effective_uid(ErlNifEnv* env) { return enif_make_int(env, geteuid()); }
static ERL_NIF_TERM get_effective_gid(ErlNifEnv* env) { return enif_make_int(env, getegid()); }
static ERL_NIF_TERM get_priority_process(ErlNifEnv* env) { return enif_make_int(env, getpriority(PRIO_PROCESS, 0)); }
static ERL_NIF_TERM get_priority_pgroup(ErlNifEnv* env) { return enif_make_int(env, getpriority(PRIO_PGRP, 0)); }
static ERL_NIF_TERM get_priority_user(ErlNifEnv* env) { return enif_make_int(env, getpriority(PRIO_USER, 0)); }

struct Entry functions[] = {
  {"pid", get_pid},
  {"pgid", get_pgid},
  {"parent_pid", get_parent_pid},
  {"uid", get_uid},
  {"gid", get_gid},
  {"effective_uid", get_effective_uid},
  {"effective_gid", get_effective_gid},
  {"priority_process", get_priority_process},
  {"priority_pgroup", get_priority_pgroup},
  {"priority_user", get_priority_user}
};

struct Entry* process_info_functions(size_t *size) {
  *size = sizeof(functions) / sizeof(struct Entry);

  return functions;
}
