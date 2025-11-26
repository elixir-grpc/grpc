# See LICENSE for licensing information.

PROJECT = cowlib
PROJECT_DESCRIPTION = Support library for manipulating Web protocols.
PROJECT_VERSION = 2.16.0

# Options.

#ERLC_OPTS += +bin_opt_info
DIALYZER_OPTS = -Werror_handling -Wunmatched_returns

# Dependencies.

LOCAL_DEPS = crypto

DOC_DEPS = asciideck

TEST_DEPS = $(if $(CI_ERLANG_MK),ci.erlang.mk) base32 horse proper jsx \
	decimal structured-header-tests uritemplate-tests
dep_base32 = git https://github.com/dnsimple/base32_erlang main
dep_horse = git https://github.com/ninenines/horse.git master
dep_jsx = git https://github.com/talentdeficit/jsx v2.10.0
dep_decimal = git https://github.com/egobrain/decimal 0.6.2
dep_structured-header-tests = git https://github.com/httpwg/structured-header-tests faed1f92942abd4fb5d61b1f9f0dc359f499f1d7
dep_uritemplate-tests = git https://github.com/uri-templates/uritemplate-test master

# CI configuration.

dep_ci.erlang.mk = git https://github.com/ninenines/ci.erlang.mk master
DEP_EARLY_PLUGINS = ci.erlang.mk

AUTO_CI_OTP ?= OTP-LATEST-24+
AUTO_CI_WINDOWS ?= OTP-LATEST-24+

# Hex configuration.

define HEX_TARBALL_EXTRA_METADATA
#{
	licenses => [<<"ISC">>],
	links => #{
		<<"Function reference">> => <<"https://ninenines.eu/docs/en/cowlib/2.16/manual/">>,
		<<"GitHub">> => <<"https://github.com/ninenines/cowlib">>,
		<<"Sponsor">> => <<"https://github.com/sponsors/essen">>
	}
}
endef

# Standard targets.

include erlang.mk

# Always rebuild from scratch in CI because OTP-25.0+ can't use the older build.

ci-setup:: distclean-deps
	-$(verbose) rm -rf $(ERLANG_MK_TMP)/rebar

# Compile options.

TEST_ERLC_OPTS += +'{parse_transform, eunit_autoexport}' +'{parse_transform, horse_autoexport}'

# Mimetypes module generator.

GEN_URL = http://svn.apache.org/repos/asf/httpd/httpd/trunk/docs/conf/mime.types
GEN_SRC = src/cow_mimetypes.erl.src
GEN_OUT = src/cow_mimetypes.erl

.PHONY: gen

gen:
	$(gen_verbose) cat $(GEN_SRC) \
		| head -n `grep -n "%% GENERATED" $(GEN_SRC) | cut -d : -f 1` \
		> $(GEN_OUT)
	$(gen_verbose) wget -qO - $(GEN_URL) \
		| grep -v ^# \
		| awk '{for (i=2; i<=NF; i++) if ($$i != "") { \
			split($$1, a, "/"); \
			print "all_ext(<<\"" $$i "\">>) -> {<<\"" \
				a[1] "\">>, <<\"" a[2] "\">>, []};"}}' \
		| sort \
		| uniq -w 25 \
		>> $(GEN_OUT)
	$(gen_verbose) cat $(GEN_SRC) \
		| tail -n +`grep -n "%% GENERATED" $(GEN_SRC) | cut -d : -f 1` \
		>> $(GEN_OUT)

# Performance testing.

ifeq ($(MAKECMDGOALS),perfs)
.NOTPARALLEL:
endif

.PHONY: perfs

perfs: test-build
	$(gen_verbose) erl -noshell -pa ebin -eval 'horse:app_perf($(PROJECT)), erlang:halt().'

# Prepare for the release.

prepare_tag:
	$(verbose) $(warning Hex metadata: $(HEX_TARBALL_EXTRA_METADATA))
	$(verbose) echo
	$(verbose) echo -n "Most recent tag:            "
	$(verbose) git tag --sort taggerdate | tail -n1
	$(verbose) git verify-tag `git tag --sort taggerdate | tail -n1`
	$(verbose) echo -n "MAKEFILE: "
	$(verbose) grep -m1 PROJECT_VERSION Makefile
	$(verbose) echo -n "APP:                 "
	$(verbose) grep -m1 vsn ebin/$(PROJECT).app | sed 's/	//g'
	$(verbose) echo
	$(verbose) echo -n "LICENSE: " ; head -n1 LICENSE
	$(verbose) echo
	$(verbose) echo "Dependencies:"
	$(verbose) grep ^DEPS Makefile || echo "DEPS ="
	$(verbose) grep ^dep_ Makefile || true
