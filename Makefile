VERSION ?= $(shell git rev-parse --short HEAD)

.PHONY: build erl test
build:
	psc-package sources | xargs pserlc --no-opts 'src/**/*.purs' 'etest/**/*.purs'

erl: build
	rm -f otp/mylib/src/ps/*
	mkdir -p otp/mylib/src/ps
	cp output/*/*.erl otp/mylib/src/ps/
	cd otp/mylib && rebar3 release && rebar3 compile && rebar3 run

test: erl
	erl -pa ebin -noshell -eval '(test_main@ps:main@c())()' -eval 'init:stop()'

.PHONY: docs
docs:
	make -C docs release

.PHONY: examples
examples:
	# Disabled! This part of the docs should be moved to the respective
	# repository instead.
	# make -C docs/src/extensions/type-level-routing/examples build
	#
	pulp build -I docs/src/topic-guides
	pulp build -I examples
