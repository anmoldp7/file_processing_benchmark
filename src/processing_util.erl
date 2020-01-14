%% ----------------------------------------------------------------------
%%
%% @doc processing utils
%% @end
%% ----------------------------------------------------------------------

-module(processing_util).

% API
-export([
	 process_text/1,
	 process_file_async/0,
	 process_file_sync/0
	]).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc text processing done at process level
process_text(Data) ->
	% uncomment below line to show data
	% lager:info("Data: ~p", [Data]),
	lager:info("Number of Tokens: ~p", [erlang:length(string:tokens(binary_to_list(Data)), " ")]).

%% @doc asynchronous processing
process_file_async() ->
	file_async_input_stream_generator:generate_input_stream().

%% @doc synchronous processing
process_file_sync() ->
	file_sync_processor:process_file().

