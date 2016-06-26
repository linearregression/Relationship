# Make file for erlang.mk

PROJECT = rls
PROJECT_DESCRIPTION = Ralationship service
PROJECT_VERSION = 0.1.0

LOCAL_DEPS =
SQOR_DEPS = csi
DEPS = lager neo4j ${SQOR_DEPS}
TEST_DEPS = meck

dep_csi = git git@github.com:Amplify-Social/SQ2_CommonServiceInterface.git
dep_neo4j = git git@github.com:dmitriid/neo4j-erlang.git

PLT_APPS = lager csi neo4j
# When we are using lager, we cannot dialyze from source
DIALYZER_DIRS = ebin

# Since we extended erlang.mk with rel:: rule, we need to specify default target
.DEFAULT_GOAL = all

BUILD_ENV ?= dev
CONFIG_TEMPLATE = config/template.sys.config
PACKAGE = ${RELX_RELEASE}_${BUILD_ENV}

app:: deps
	$(gen_verbose) config/generate-config.sh \
		--deps ${SQOR_DEPS} \
		--build-env ${BUILD_ENV}

tests:: app
	$(gen_verbose)

include erlang.mk

# Don't fail on warnings right now
ERLC_OPTS := $(filter-out -Werror,$(ERLC_OPTS))

# Use lager in csi and use parse transform
ERLC_OPTS += +'{parse_transform, lager_transform}' -Dlager
TEST_ERLC_OPTS += +'{parse_transform, lager_transform}' -Dlager

# In local env we want to mock the external calls
ifeq ($(BUILD_ENV), local)
	ERLC_OPTS += -DMOCK_CALLS
endif

push: package
	@# this assumes the s3cmd is install and a file exists at ~/.s3cfg
	s3cmd put ${PACKAGE}.tar.bz2 s3://erlang_releasess

package: deps app rel
	git rev-parse HEAD > _rel/${RELX_RELEASE}/gitrev.txt
	cd _rel; tar cvjf ../${PACKAGE}.tar.bz2 ${RELX_RELEASE}; cd ..;

deploy: distclean push
	echo "Deploying"
