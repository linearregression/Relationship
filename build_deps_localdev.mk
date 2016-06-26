# dependencies can be local so cp
# database shall be ne4j-stage.sqor.com
$(DEPENDECIES)/csi:
	git clone -n -- git@github.com:Sqor/CommonServiceInterface.git $@
	(cd $@ && git checkout -q dev && make DEPS_DIR=$(DEPS_DIR))

$(DEPENDECIES)/neo4j:
	git clone -n -- git@github.com:dmitriid/neo4j-erlang.git $@
	(cd $@ && git checkout -q master && make deps DEPS_DIR=$(DEPS_DIR) && make app DEPS_DIR=$(DEPS_DIR))

$(DEPENDECIES)/hackney:
	git clone -n -- https://github.com/benoitc/hackney.git $@
	(cd $@ && git checkout  -q 1.3.1 && ./rebar3 update && make DEPS_DIR=$(DEPS_DIR)) )

$(DEPENDECIES)/lager:
	git clone -n -- https://github.com/basho/lager.git $@
	(cd $@ && git checkout -q 3.0.1 && ./rebar deps_dir="$(DEPS_DIR)" get-deps compile)
