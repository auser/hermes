LIBDIR					= `erl -eval 'io:format("~s~n", [code:lib_dir()])' -s init stop -noshell`
VERSION					= 0.0.1
CC							= erlc
ERL							= erl
EBIN						= ebin
CFLAGS					= +debug_info -W0 -I include -pa $(EBIN) -I gen-erl/
COMPILE					= $(CC) $(CFLAGS) -o $(EBIN)
EBIN_DIRS				= $(wildcard deps/*/ebin)
WEB_DIR					= web/
TEST_DIR				= test/
WONDERLAND_DIR	= $(WEB_DIR)/wonderland
DEPS_DIR = deps
STOPLIGHT_DIR	= $(DEPS_DIR)/stoplight
APP							= hermes

all: ebin compile
all_boot: all boot
wonderland_boot: wonderland all_boot
start: all start_all
rstakeout: wonderland compile

wonderland:
	[ -d $(WONDERLAND_DIR) ] || (mkdir $(WEB_DIR) && cd $(WEB_DIR) && git clone git://github.com/auser/wonderland.git)
	cd $(WONDERLAND_DIR) && git pull origin master

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
	
test: compile
	$(ERL) -noshell -pa $(EBIN) -pa test/ebin -pa test/include/gen_server_mock/ebin -s test_suite test -s init stop
	
boot:
	(cd ebin; erl -pa ebin -noshell -run make_boot write_scripts hermes)

start_all:
	(cd ebin; erl -pa ebin -noshell -sname hermes -boot hermes)

ebin:
	@mkdir ebin

clean:
	rm -rf ebin/*.beam ebin/erl_crash.dump erl_crash.dump ebin/*.boot ebin/*.rel ebin/*.script test/ebin/*.beam

clean_mochiweb:
	rm -rf deps/mochiweb/ebin/*.beam