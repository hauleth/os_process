% SPDX-FileCopyrightText: 2025 Łukasz Niemier <#@hauleth.dev>
%
% SPDX-License-Identifier: MIT

-module(os_process).

-export([info/0, sizes/0]).

-on_load(init/0).

%% @hidden
init() ->
    SoName = case code:priv_dir(os_process) of
        {error, bad_name} ->
            case filelib:is_dir(filename:join(["..", priv])) of
                true ->
                    filename:join(["..", priv, ?MODULE]);
                _ ->
                    filename:join([priv, ?MODULE])
            end;
        Dir ->
            filename:join(Dir, ?MODULE)
    end,
    erlang:load_nif(SoName, 0).

%% @doc
%% Return map with OS-specific information about current process.
%%
%% On UNIX-like OSes the map will contain:
%%
%% <ul>
%%      <li>`pid' - process ID for the VM, returned value should be logically the same
%%      as {@link os:getpid/0. `os:getpid/0'}, however it will be integer
%%      instead of string there.
%%
%%      For more information see `man 2 getpid'.</li>
%%      <li>`priority' - map containing priorities for given process. It will
%%      contain 3 keys:
%%      <ul>
%%              <li>`process'</li>
%%              <li>`process_group'</li>
%%              <li>`user'</li>
%%      </ul>
%%
%%      For more info see `man 2 getpriority'.</li>
%%      <li>`pgid' - process group ID.
%%
%%      For more info see `man 2 getpgid'.</li>
%%      <li>`parent_pid' - process ID of the parent process.
%%
%%      For more info see `man 2 getppid'.</li>
%%      <li>`uid' - user ID.
%%
%%      For more info see `man 2 getuid'.</li>
%%      <li>`gid' - group ID.
%%
%%      For more info see `man 2 getgid'.</li>
%%      <li>`effective_uid' - effective user ID.
%%
%%      For more info see `man 2 geteuid'.</li>
%%      <li>`effective_gid' - effective group ID.
%%
%%      For more info see `man 2 getegid'.</li>
%% </ul>
%%
%% On Windows it will contain:
%%
%% <ul>
%%      <li>`pid' - process ID for the VM, returned value should be logically the same
%%      as {@link os:getpid/0. `os:getpid/0'}, however it will be integer
%%      instead of string there.</li>
%%      <li>`priority' - map containing priorities for given process. It will
%%      contain 3 keys:
%%      <ul>
%%              <li>`class'</li>
%%              <li>`thread'</li>
%%      </ul>
%%      </li>
%%      <li>`user_name' - name of the owner of the process as binary.</li>
%% </ul>
%% @end
-spec info() -> #{pid => pos_integer(),
                  priority => map(),
                  atom() := term()}.
info() -> erlang:nif_error(not_loaded).

%% @doc
%% Return sizes (in bits) of the primitive values for a given system.
%%
%% <ul>
%%      <li>`byte' - amount of bits in a byte, where byte is defined as minimal
%%      addressable piece of the memory.</li>
%%      <li>`int' - amount of bits in `int' C type.</li>
%%      <li>`long' - amount of bits in `long' C type.</li>
%%      <li>`long_long' - amount of bits in `long long' C type.</li>
%%      <li>`ptr' - amount of bits in C pointer type.</li>
%% </ul>
%% @end
-spec sizes() -> #{byte => pos_integer(),
                   int => pos_integer(),
                   long => pos_integer(),
                   long_long => pos_integer(),
                   ptr => pos_integer()}.
sizes() -> erlang:nif_error(not_loaded).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").

info_pid_is_equal_os_getpid_test() ->
    #{pid := Pid} = info(),
    ?assertEqual(integer_to_list(Pid), os:getpid()).

sizes_test_() ->
    #{byte := ByteSize,
      int := IntSize,
      long := LongSize,
      long_long := LongLongSize,
      ptr := PtrSize} = sizes(),
    [% Check C constraints on these types
     ?_assert(ByteSize >= 8),
     ?_assert(IntSize >= ByteSize),
     ?_assert(LongSize >= IntSize),
     ?_assert(LongLongSize >= LongSize),
     % Sizes are divisible by byte size
     ?_assertEqual(0, IntSize rem ByteSize),
     ?_assertEqual(0, LongSize rem ByteSize),
     ?_assertEqual(0, LongLongSize rem ByteSize),
     ?_assertEqual(0, PtrSize rem ByteSize)
    ].
-endif.
