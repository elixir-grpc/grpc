LOCAL_CLEAN=$(shell git diff-index --quiet HEAD -- && echo 1)

test:
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

# This is heavy
ci-cron:
	cd interop && mix deps.get && mix run script/run.exs --rounds 1000 --concurrency 30 && cd -
	mix deps.get && bash .ci/build-plt-cache.sh && mix dialyzer


.PHONY: test release test-prepare test-all ci-cron
