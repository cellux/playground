SHELL := /bin/bash

CLOJURE ?= clojure
UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S),Linux)
DEV_ALIASES ?= dev:linux
else
DEV_ALIASES ?= dev
endif

.PHONY: up

up:
	@mkdir -p .dev
	@port="$$(tr -d '[:space:]' < .nrepl-port 2>/dev/null || true)"; \
	if [[ "$$port" =~ ^[0-9]+$$ ]]; then \
		port_number=$$((10#$$port)); \
		if (( port_number > 0 && port_number <= 65535 )) && { : >"/dev/tcp/127.0.0.1/$$port_number"; } 2>/dev/null; then \
			echo "Clojure development nREPL is already running on port $$port_number."; \
			exit 0; \
		fi; \
	fi; \
	exec "$(CLOJURE)" -M:$(DEV_ALIASES) \
		> >(tee .dev/stdout.log) \
		2> >(tee .dev/stderr.log >&2)
