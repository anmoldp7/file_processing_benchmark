%% ----------------------------------------------------------------------
%%
%% @doc message queue for getting chunked data from input stream
%% @end
%% ----------------------------------------------------------------------

-module(file_async_message_queue).

-behaviour(gen_server).

% API
-export([
	 start_link/0,
	 send_message/1,
	 get_message/0,
	 start_processes/0,
	 stop_transmission/0
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
	  message_queue = undefined,
	  is_processing = false
	 }).

-define(SERVER, file_processing_benchmark).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc starts and links the server
start_link() ->
	gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

%% @doc adds message to queue
send_message(Msg) ->
	gen_server:cast({global, ?MODULE},{add_message, Msg}).

%% @doc get message from queue
get_message() ->
	gen_server:call({global, ?MODULE}, get_message).

%% @doc send request to start async processes
start_processes() ->
	gen_server:cast({global, ?MODULE}, start_processes).

%% @doc send request to stop transmission
stop_transmission() ->
	gen_server:call({global, ?MODULE}, stop_transmission).

%%%===================================================================
%%% internal function
%%%===================================================================

%% @doc start async processes
start_async_processes() ->
	{ok, NumberOfProcesses} = application:get_env(?SERVER, number_of_processes),
	lists:foreach(fun(Num) -> file_async_processor:start(Num) end, lists:seq(1, NumberOfProcesses)).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc initializes the server
init([]) ->
	{ok, #state{message_queue = queue:new(), is_processing = false}}.

%% @private
%% @doc handles call messages
handle_call(get_message, _From, State) ->
	case queue:is_empty(State#state.message_queue) of
		true ->
			case State#state.is_processing of
				true ->
					{reply, <<"">>, State};
				false ->
					{reply, process_data, State}
			end;
		false ->
			{{value, Chunk}, NewQueue} = queue:out(State#state.message_queue),
			{reply, Chunk, State#state{message_queue = NewQueue}}
	end;
handle_call(stop_transmission, _From, State) ->
	{reply, ok, State#state{is_processing = false}};
handle_call(Msg, From, State) ->
	lager:info("Call request received: ~p, From process ID: ~p, Module: ~p", [Msg, From, ?MODULE]),
	Reply = ok,
	{reply, Reply, State}.

%% @private
%% @doc handles cast messages
handle_cast(start_processes, State) ->
	start_async_processes(),
	{noreply, State#state{is_processing = true}};
handle_cast({add_message, Msg}, State) ->
	{noreply, State#state{message_queue = queue:in(Msg, State#state.message_queue)}};
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

