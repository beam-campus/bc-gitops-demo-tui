-module(demo_tui).

%% API
-export([start/0, start/1, stop/0]).
-export([get_executable/0, get_priv_dir/0]).

-define(EXECUTABLE, "demo-tui").

%%====================================================================
%% API
%%====================================================================

%% @doc Start the TUI with default counter URL
-spec start() -> {ok, port()} | {error, term()}.
start() ->
    CounterUrl = application:get_env(demo_tui, counter_url, "http://localhost:8080"),
    start(CounterUrl).

%% @doc Start the TUI with a specific counter URL
-spec start(string()) -> {ok, port()} | {error, term()}.
start(CounterUrl) ->
    case get_executable() of
        {ok, Executable} ->
            io:format("[demo_tui] Launching ~s~n", [Executable]),
            Env = [{"COUNTER_URL", CounterUrl}],
            Port = open_port(
                {spawn_executable, Executable},
                [
                    {env, Env},
                    use_stdio,
                    exit_status,
                    binary
                ]
            ),
            {ok, Port};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Stop the TUI (if running)
-spec stop() -> ok.
stop() ->
    %% Find and close the port - simplified implementation
    ok.

%% @doc Get the path to the TUI executable
-spec get_executable() -> {ok, string()} | {error, term()}.
get_executable() ->
    case get_priv_dir() of
        {ok, PrivDir} ->
            Executable = filename:join([PrivDir, platform_dir(), ?EXECUTABLE]),
            case filelib:is_regular(Executable) of
                true ->
                    {ok, Executable};
                false ->
                    {error, {executable_not_found, Executable}}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Get the priv directory for this application
-spec get_priv_dir() -> {ok, string()} | {error, term()}.
get_priv_dir() ->
    case code:priv_dir(demo_tui) of
        {error, bad_name} ->
            %% Fallback for development
            case code:which(?MODULE) of
                Filename when is_list(Filename) ->
                    AppDir = filename:dirname(filename:dirname(Filename)),
                    {ok, filename:join(AppDir, "priv")};
                _ ->
                    {error, cannot_find_priv_dir}
            end;
        PrivDir ->
            {ok, PrivDir}
    end.

%%====================================================================
%% Internal
%%====================================================================

%% @doc Get the platform-specific directory name
-spec platform_dir() -> string().
platform_dir() ->
    Os = os_name(),
    Arch = arch_name(),
    Os ++ "-" ++ Arch.

os_name() ->
    case os:type() of
        {unix, linux} -> "linux";
        {unix, darwin} -> "macos";
        {win32, _} -> "windows";
        _ -> "unknown"
    end.

arch_name() ->
    case erlang:system_info(system_architecture) of
        "x86_64" ++ _ -> "x86_64";
        "aarch64" ++ _ -> "aarch64";
        "arm64" ++ _ -> "aarch64";
        Arch -> Arch
    end.
