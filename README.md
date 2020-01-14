# file_processing_benchmark
Implemented to compare synchronous and asynchronous file processing strategy using Erlang.

## Requirement
* Erlang

## Usage
* Clone this repository
* Execute `make run`
* Once CLI is accessible, run `process_util:process_file_sync()` for synchronous processing metrics or run `process_util:process_file_async()` for asynchronous processing metrics
* Text processing function can be modified by editing function processing_util:process_text/1 at `src/processing_util.erl`
* Input file can be modified at `priv/input.txt`
* Number of processes and size of block for asynchronous processing can be modified at `config/sys.config`