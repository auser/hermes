LIBDIR					= `erl -eval 'io:format("~s~n", [code:lib_dir()])' -s init stop -noshell`
VERSION					= 0.0.1
CC							= erlc
ERL							= erl
EBIN						= ebin
CFLAGS					= -I include -pa $(EBIN)
COMPILE					= $(CC) $(CFLAGS) -o $(EBIN)
EBIN_DIRS				= $(wildcard deps/*/ebin)
WEB_DIR					= web/
WONDERLAND_DIR	= $(WEB_DIR)/wonderland
APP							= hermes

all: deps ebin compile
all_boot: all boot
wonderland_boot: wonderland all_boot
start: all start_all
rstakeout: wonderland compile

wonderland:
	[ -d $(WONDERLAND_DIR) ] || (mkdir $(WEB_DIR) && cd $(WEB_DIR) && git clone git://github.com/auser/wonderland.git)
	cd $(WONDERLAND_DIR) && git pull origin master

deps: mochi thrift

mochi:
	@(cd deps/mochiweb;$(MAKE))
thrift:
	@(cd deps/thrift;$(MAKE))
	
compile:
	@$(ERL) -pa $(EBIN_DIRS) -noinput +B -eval 'case make:all() of up_to_date -> halt(0); error -> halt(1) end.'

edoc:
	@echo Generating $(APP) documentation from srcs
	@$(ERL) -noinput -eval 'edoc:application($(APP), "./", [{doc, "doc/"}, {files, "src/"}])' -s erlang halt
	
boot:
	(cd ebin; erl -pa ebin -noshell -run make_boot write_scripts hermes)

start_all:
	(cd ebin; erl -pa ebin -noshell -sname hermes -boot hermes)

ebin:
	@mkdir ebin

clean:
	rm -rf ebin/*.beam ebin/erl_crash.dump erl_crash.dump ebin/*.boot ebin/*.rel ebin/*.script 

clean_mochiweb:
	rm -rf deps/mochiweb/ebin/*.beam