-module(logflare_logger_handler).

%% The main entry point
-export([log/2]).

%% Optional callbacks
-export([adding_handler/1, removing_handler/1, changing_config/3]).

-spec adding_handler(logger:handler_config()) -> {ok, logger:handler_config()} | {error, term()}.
adding_handler(Config) ->
    case supervisor:start_child(logflare_logger_handler_sup,
                                [maps:get(config, Config, [])]) of
        {ok, Pid} ->
            NewConfig = maps:put(pid, Pid, Config),
            {ok, NewConfig};
        {error, Error} ->
            {error, Error}
    end.

-spec removing_handler(logger:handler_config()) -> ok.
removing_handler(Config) ->
    case maps:get(pid, Config) of
        undefined ->
            ok;
        Pid ->
            supervisor:terminate_child(logflare_logger_handler_sup, Pid),
            ok
    end.

-spec changing_config(set | update, logger:handler_config(), logger:handler_config()) ->
          {ok, logger:handler_config()} | {error, term()}.
changing_config(_, OldConfig, NewConfig) ->
    removing_handler(OldConfig),
    adding_handler(NewConfig).

%% Log handler
-spec log(term(), logger:handler_config()) -> ok.
log(#{ msg := Msg, meta := Meta}, #{ pid := Pid }) ->
    Metadata = maps:filter(fun (K, _) -> is_binary(K) end, Meta),
    logflare:async(Pid, #{ <<"message">> => message_to_binary(Msg),
                           <<"metadata">> => Metadata
                         }),
    ok;
log(_,_) ->
    ok.

message_to_binary({string, String}) ->
    iolist_to_binary(String);
message_to_binary({report, Term}) ->
    iolist_to_binary(io_lib:format("~p", [Term]));
message_to_binary({Format, Args}) ->
    try iolist_to_binary(io_lib:format(Format, Args))
    catch error:badarg ->
            io:format(user, "FORMAT ERROR: \"~s\" - ~p~n", [Format, Args]),
            message_to_binary({report, {Format, Args}})
    end;
message_to_binary(_V) ->
    <<"">>.
