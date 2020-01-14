%% ----------------------------------------------------------------------
%%
%% @doc asynchronous file processing
%% @end
%% ----------------------------------------------------------------------

-module(file_async_processor).

-behavior(gen_server).

% API
-export([
	 start/1,
	 start_link/1
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

-record(state, {
	  message = <<"">>,
	  id_no = undefined
	 }).

-define(SERVER, file_processing_benchmark).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc starts the server
start(Id) ->
	gen_server:start(?MODULE, [Id], []).

%% @doc starts and links the server
start_link(Id) ->
	gen_server:start_link(?MODULE, [Id], []).

%%%===================================================================
%%% internal functions
%%%===================================================================

%% @doc file processing
process_data(Id, Data) ->
	StartTime = erlang:monotonic_time(),
	processing_util:process_text(Data),
	TimeTaken = erlang:convert_time_unit(erlang:monotonic_time() - StartTime, native, nanosecond),
	lager:info("Time taken: ~p, process ID: ~p, size of data: ~p", [TimeTaken, Id, string:length(Data)]).

%% @doc fetch data from message queue
fetch_data_from_queue() ->
	gen_server:cast(self(), fetch_data_from_queue).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc initializes the server
init([Id]) ->
	fetch_data_from_queue(),
	lager:info("Async file processor initialized: ~p", [Id]),
	{ok, #state{message = <<"">>, id_no = Id}}.

%% @private
%% @doc handles call messages
handle_call(Msg, From, State) ->
	lager:info("Call request received: ~p, From process ID: ~p, Module: ~p", [Msg, From, ?MODULE]),
	Reply = ok,
	{reply, Reply, State}.

%% @private
%% @doc handles cast messages
handle_cast(fetch_data_from_queue, State) ->
	Chunk = file_async_message_queue:get_message(),
	case Chunk of
		process_data ->
			process_data(State#state.id_no, State#state.message),
			{stop, normal, State};
		ChunkedData ->
			fetch_data_from_queue(),
			MsgSoFar = State#state.message,
			{noreply, State#state{message = <<MsgSoFar/binary, ChunkedData/binary>>}}
	end;
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

