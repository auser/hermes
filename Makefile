LIBDIR					= `erl -eval 'io:format("~s~n", [code:lib_dir()])' -s init stop -noshell`
VERSION					= $(shell cat VERSION | tr -d '\n')
CC						= erlc
ERL						= erl
EBIN					= ebin
CFLAGS					= +debug_info -W0 -I include -pa $(EBIN) -I gen-erl/
COMPILE					= $(CC) $(CFLAGS) -o $(EBIN)
EBIN_DIRS				= $(wildcard deps/*/ebin)
WEB_DIR					= web/
TEST_DIR				= test
TEST_EBIN_DIR		= $(TEST_DIR)/ebin
DEPS_DIR = deps
STOPLIGHT_DIR	= $(DEPS_DIR)/stoplight
STOPLIGHT_VERSION = $(shell cat $(STOPLIGHT_DIR)/VERSION | tr -d '\n')
APP							= hermes

all: $(TEST_EBIN_DIR) ebin compile
all_boot: all boot
wonderland_boot: wonderland all_boot
start: all start_all
rstakeout: wonderland compile

deps: mochi thrift gen_cluster stoplight

mochi:
	@(cd deps/mochiweb;$(MAKE))
gen_cluster:
	@(cd deps/gen_cluster;$(MAKE))
thrift:
	@(cd deps/thrift;$(MAKE))
	thrift --gen erl thrift/hermes.thrift
	@(mv gen-erl/hermes*.hrl include)
stoplight:
	[ -d $(STOPLIGHT_DIR) ] || (mkdir -p $(DEPS_DIR) && cd $(DEPS_DIR) && git clone git://github.com/jashmenn/stoplight.git)
	cd $(STOPLIGHT_DIR) && git pull origin master
	cd $(STOPLIGHT_DIR) && rake

compile:
	@$(ERL) -pa $(EBIN_DIRS) -pa $(EBIN) -noinput +B -eval 'case make:all() of up_to_date -> halt(0); error -> halt(1) end.'

edoc:
	@echo Generating $(APP) documentation from srcs
	@$(ERL) -noinput -eval 'edoc:application($(APP), "./", [{doc, "doc/"}, {files, "src/"}])' -s erlang halt

eunit:
	cd test/include/eunit && make

shell: compile
	$(ERL) 	-sname shell -setcookie test \
					-pa $(EBIN) \
					-pa deps/*/ebin

test: $(TEST_EBIN_DIR) compile
	$(ERL) 	-noshell -pa $(EBIN) \
					-pa deps/*/ebin \
					-pa $(TEST_EBIN_DIR) \
					-pa test/include/gen_server_mock/ebin \
					-s test_suite test \
					-s init stop
	
boot:
	(cd $(EBIN); erl -pa ../$(EBIN) -pz ../$(STOPLIGHT_DIR)/ebin -noshell -run make_boot write_scripts hermes $(VERSION) stoplight $(STOPLIGHT_VERSION))

release:
	(cd $(EBIN); erl -pa ../$(EBIN) -pz ../$(STOPLIGHT_DIR)/ebin -noshell -run make_boot write_release_scripts hermes $(VERSION) stoplight $(STOPLIGHT_VERSION))

target_system:
	(cd $(EBIN); erl -pa ../$(EBIN) -pz ../$(STOPLIGHT_DIR)/ebin -noshell -run target_system create "hermes-$(VERSION)" -s init stop)


start_all:
	(cd $(EBIN); erl -pa $(EBIN) -noshell -sname hermes -boot hermes)

$(EBIN):
	@mkdir $(EBIN)

clean:
	echo $(TEST_EBIN_DIR)
	rm -rf $(EBIN)/*.beam $(EBIN)/erl_crash.dump erl_crash.dump $(EBIN)/*.boot $(EBIN)/*.rel $(EBIN)/*.script $(TEST_EBIN_DIR)/*.beam

clean_mochiweb:
	rm -rf deps/mochiweb/ebin/*.beam

$(TEST_EBIN_DIR):
	@mkdir $(TEST_EBIN_DIR)