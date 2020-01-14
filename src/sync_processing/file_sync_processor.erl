%% ----------------------------------------------------------------------
%%
%% @doc synchronous file processing
%% @end
%% ----------------------------------------------------------------------

-module(file_sync_processor).

-behaviour(gen_server).

% API
-export([
	 start_link/0,
	 process_file/0
	]).

% gen_server callbacks
-export([
	 init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3
	]).

-define(SERVER, file_processing_benchmark).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc starts and links the server
start_link() ->
	gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

%% @doc process file
process_file() ->
	gen_server:cast({global, ?MODULE}, process_file_request).

%%%===================================================================
%%% internal function
%%%===================================================================

%% @doc file processing
sync_file_processing() ->
	StartTime = erlang:monotonic_time(),
	{ok, InputFilePath} = application:get_env(?SERVER, input_file_path),
	{ok, Data} = file:read_file(InputFilePath),
	processing_util:process_text(Data),
	TimeTaken = erlang:convert_time_unit(erlang:monotonic_time() - StartTime, native, nanosecond),
	lager:info("Time taken: ~p nanoseconds, size of data: ~p", [TimeTaken, string:length(Data)]).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc initializes the server
init([]) ->
	{ok, #{}}.

%% @private
%% @doc handles call messages
handle_call(Msg, From, State) ->
	lager:info("Call request received: ~p, From process ID: ~p, Module: ~p", [Msg, From, ?MODULE]),
	Reply = ok,
	{reply, Reply, State}.

%% @private
%% @doc handles cast messages
handle_cast(process_file_request, State) ->
	sync_file_processing(),
	{noreply, State};
handle_cast(Msg, State) ->
	lager:info("Cast request received: ~p, Module: ~p", [Msg, ?MODULE]),
	{noreply, State}.

%% @private
%% @doc handles all non call/cast messages
handle_info(Msg, State) ->
	lager:info("Info request received: ~p, Module: ~p", [Msg, ?MODULE]),
	{noreply, State}.

%% @private
%% @doc opposite of init
terminate(_Reason, _State) ->
	ok.

%% @private
%% @doc convert process state when code is changed
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

