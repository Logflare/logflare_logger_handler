%%%-------------------------------------------------------------------
%% @doc logflare_logger_handler public API
%% @end
%%%-------------------------------------------------------------------

-module(logflare_logger_handler_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    logflare_logger_handler_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
