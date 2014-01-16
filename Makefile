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
 
test:
	@rm -rf .eunit
	@mkdir -p .eunit
	@$(REBAR) eunit
