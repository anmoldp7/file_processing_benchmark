%% ----------------------------------------------------------------------
%%
%% @doc generates input stream for asynchronous processing
%% @end
%% ----------------------------------------------------------------------

-module(file_async_input_stream_generator).

-behaviour(gen_server).

% API
-export([
	 start_link/0,
	 generate_input_stream/0
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

%% @doc starts the server
start_link() ->
	gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

%% @doc generates input stream
generate_input_stream() ->
	gen_server:cast({global, ?MODULE}, generate_input_stream).

%%%===================================================================
%%% internal functions
%%%===================================================================

%% @doc transmit input stream
transmit_input_stream() ->
	{ok, ChunkSize} = application:get_env(?SERVER, block_size),
	{ok, InputFilePath} = application:get_env(?SERVER, input_file_path),
	{ok, Data} = file:read_file(InputFilePath),
	file_async_message_queue:start_processes(),
	transmit_chunks(Data, ChunkSize),
	file_async_message_queue:stop_transmission().

%% @doc transmit chunks of data
transmit_chunks(Data, ChunkSize) ->
	BodyLength = string:length(Data),
	if
		BodyLength =< ChunkSize ->
			file_async_message_queue:send_message(Data);
		BodyLength > ChunkSize ->
			<<Head:ChunkSize/binary, Tail/binary>> = Data,
			file_async_message_queue:send_message(Head),
			transmit_chunks(Tail, ChunkSize)
	end.

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
handle_cast(generate_input_stream, State) ->
	transmit_input_stream(),
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

