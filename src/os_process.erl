-module(os_process).

-export([info/0]).

-on_load(init/0).

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

info() -> erlang:nif_error(not_loaded).
