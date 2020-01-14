PROJECT = file_processing_benchmark
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.1.0

DEPS = lager gproc

ERLC_OPTS += +'{parse_transform, lager_transform}'

include erlang.mk
