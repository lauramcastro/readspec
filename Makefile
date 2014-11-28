REPO       ?= readspec
TAG         = $(shell git describe --tags)
REVISION   ?= $(shell echo $(TAG) | sed -e 's/^$(REPO)-//')
VERSION    ?= $(shell echo $(REVISION) | tr - .)
 
REBAR=./rebar
 
all: compile
 
compile:
	@$(REBAR) compile
 
clean:
	@$(REBAR) clean
 
docs:
	@$(REBAR) doc
 
tests: eunit qc

eunit:
	@rm -rf .eunit
	@$(REBAR) eunit

qc:
	@rm -rf .qc
	@$(REBAR) qc
