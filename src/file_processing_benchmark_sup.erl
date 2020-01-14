%% ----------------------------------------------------------------------
%%
%% @doc Supervisor for all processes
%% @end
%% ----------------------------------------------------------------------

-module(file_processing_benchmark_sup).

-behaviour(supervisor).

% API
-export([start_link/0]).

% supervisor callback
-export([init/1]).

%%%===================================================================
% API
%%%===================================================================

%% @doc starts and links supervisor
start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%===================================================================
% supervisor callback
%%%===================================================================

%% @doc initializes the supervisor
init([]) ->
	FileSyncProcessor =
		{file_sync_processor,
		 {file_sync_processor, start_link, []},
		permanent,
		infinity,
		worker,
		[file_sync_processor]},
	FileAsyncInputStreamGenerator =
		{file_async_input_stream_generator,
		 {file_async_input_stream_generator, start_link, []},
		 permanent,
		 infinity,
		 worker,
		 [file_async_input_stream_generator]},
	FileAsyncMessageQueue =
		{file_async_message_queue,
		 {file_async_message_queue, start_link, []},
		permanent,
		infinity,
		worker,
		[file_async_message_queue]},
	Procs = [FileSyncProcessor, FileAsyncInputStreamGenerator, FileAsyncMessageQueue],
	{ok, {{one_for_one, 1, 5}, Procs}}.

