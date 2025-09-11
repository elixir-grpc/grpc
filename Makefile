LOCAL_CLEAN=$(shell git diff-index --quiet HEAD -- && echo 1)

test:
	@mix deps.get
      mix test

release:
ifeq ($(LOCAL_CLEAN), 1)
	mix hex.publish
else
	@echo "Please keep your git local clean."
endif

test-prepare:
	mix deps.get
	cd interop && mix deps.get

test-all:
	mix test
	cd interop && mix run script/run.exs

.PHONY: test release test-prepare test-all ci-cron
