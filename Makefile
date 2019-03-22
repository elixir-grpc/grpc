LOCAL_CLEAN=$(shell git diff-index --quiet HEAD -- && echo 1)

release:
ifeq ($(LOCAL_CLEAN), 1)
	mix hex.publish
else
	@echo "Please keep your git local clean."
endif

.PHONY: release
