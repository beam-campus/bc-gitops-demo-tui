-module(demo_tui_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    case application:get_env(demo_tui, auto_start, false) of
        true ->
            %% Auto-start the TUI
            demo_tui_sup:start_link();
        false ->
            %% Just start the supervisor, TUI can be launched manually
            io:format("[demo_tui] Started. Use demo_tui:start() to launch TUI~n"),
            demo_tui_sup:start_link()
    end.

stop(_State) ->
    ok.
