/*
 * SPDX-FileCopyrightText: 2025 Łukasz Niemier <#@hauleth.dev>
 *
 * SPDX-License-Identifier: MIT
 */

#include <erl_nif.h>
#include <limits.h>

#define M_ADD(env, map, key, value) enif_make_map_put(env, map, key, value, &map)

#if defined(__WIN32__) /* Windows */

#include <Windows.h>

#define ENTRIES \
  ENTRY(pid) \
  ENTRY(priority) \
  ENTRY(user_name)

#define PRIORITY \
  ENTRY(class) \
  ENTRY(thread)

#else /* UNIX */

#include <sys/time.h>
#include <sys/resource.h>
#include <sys/types.h>
#include <unistd.h>

#define ENTRIES \
  ENTRY(pid) \
  ENTRY(priority) \
  ENTRY(pgid) \
  ENTRY(parent_pid) \
  ENTRY(uid) \
  ENTRY(gid) \
  ENTRY(effective_uid) \
  ENTRY(effective_gid)

#define PRIORITY \
  ENTRY(process) \
  ENTRY(process_group) \
  ENTRY(user)

#endif

#define ATOMS \
  ENTRY(byte) \
  ENTRY(short) \
  ENTRY(int) \
  ENTRY(long) \
  ENTRY(long_long) \
  ENTRY(ptr)

#define ENTRY(X) static ERL_NIF_TERM atom_##X;
ENTRIES;
PRIORITY;
ATOMS;
#undef ENTRY

#if defined(__WIN32__) /* Windows */

static ERL_NIF_TERM get_pid(ErlNifEnv* env) { return enif_make_int(env, GetCurrentProcessId()); }
static ERL_NIF_TERM get_user_name(ErlNifEnv* env) {
  ErlNifBinary bin;
  enif_alloc_binary(UNLEN + 1, &bin);

  if (!GetUserNameA(bin.data, &bin.size)) {
    bin.size = 0;
  }

  return enif_make_binary(env, &bin);
}

static ERL_NIF_TERM get_priority(ErlNifEnv *env) {
  HANDLE proc_h = GetCurrentProcess();
  HANDLE thread_h = GetCurrentThread();

  DWORD priority_class = GetPriorityClass(proc_h);
  DWORD thread_priority = GetThreadPriority(thread_h);

  ERL_NIF_TERM map = enif_make_new_map(env);

  M_ADD(env, map, atom_class, enif_make_int(env, priority_class));
  M_ADD(env, map, atom_thread, enif_make_int(env, thread_priority));

  return map;
}

#else /* UNIX */

static ERL_NIF_TERM get_pid(ErlNifEnv* env) { return enif_make_int(env, getpid()); }
static ERL_NIF_TERM get_pgid(ErlNifEnv* env) { return enif_make_int(env, getpgid(0)); }
static ERL_NIF_TERM get_parent_pid(ErlNifEnv* env) { return enif_make_int(env, getppid()); }
static ERL_NIF_TERM get_uid(ErlNifEnv* env) { return enif_make_int(env, getuid()); }
static ERL_NIF_TERM get_gid(ErlNifEnv* env) { return enif_make_int(env, getgid()); }
static ERL_NIF_TERM get_effective_uid(ErlNifEnv* env) { return enif_make_int(env, geteuid()); }
static ERL_NIF_TERM get_effective_gid(ErlNifEnv* env) { return enif_make_int(env, getegid()); }

static ERL_NIF_TERM get_priority(ErlNifEnv* env) {
  int process = getpriority(PRIO_PROCESS, 0);
  int process_group = getpriority(PRIO_PGRP, 0);
  int user = getpriority(PRIO_USER, 0);

  ERL_NIF_TERM map = enif_make_new_map(env);

  M_ADD(env, map, atom_process, enif_make_int(env, process));
  M_ADD(env, map, atom_process_group, enif_make_int(env, process_group));
  M_ADD(env, map, atom_user, enif_make_int(env, user));

  return map;
}

#endif

static ERL_NIF_TERM process_info_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  ERL_NIF_TERM map = enif_make_new_map(env);

#define ENTRY(X) M_ADD(env, map, atom_##X, get_##X(env));
  ENTRIES;
#undef ENTRY

  return map;
}

ERL_NIF_TERM sizes_map;

static ERL_NIF_TERM sizes_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  return sizes_map;
}

static ErlNifFunc nif_funcs[] = {
  {"info", 0, process_info_nif},
  {"sizes", 0, sizes_nif},
};

static int os_process_init(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info) {
#define ENTRY(X) atom_##X = enif_make_atom(env, #X);
  ENTRIES;
  PRIORITY;
  ATOMS;
#undef ENTRY

  /* Setup sizes map, as these will not change in runtime */
  sizes_map = enif_make_new_map(env);

  M_ADD(env, sizes_map, atom_byte, enif_make_int(env, CHAR_BIT));
  M_ADD(env, sizes_map, atom_short, enif_make_int(env, sizeof(short) * CHAR_BIT));
  M_ADD(env, sizes_map, atom_int, enif_make_int(env, sizeof(int) * CHAR_BIT));
  M_ADD(env, sizes_map, atom_long, enif_make_int(env, sizeof(long) * CHAR_BIT));
  M_ADD(env, sizes_map, atom_long_long, enif_make_int(env, sizeof(long long) * CHAR_BIT));
  M_ADD(env, sizes_map, atom_ptr, enif_make_int(env, sizeof(void*) * CHAR_BIT));

  return 0;
}

ERL_NIF_INIT(os_process, nif_funcs, os_process_init, NULL, NULL, NULL)
