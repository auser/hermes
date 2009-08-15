LIBDIR					= `erl -eval 'io:format("~s~n", [code:lib_dir()])' -s init stop -noshell`
VERSION					= 0.0.1
CC							= erlc
ERL							= erl
EBIN						= ebin
CFLAGS					= +debug_info -W0 -I include -I ./../include -pa $(EBIN) -pa ./../ebin -I ./../gen-erl/
COMPILE					= $(CC) $(CFLAGS) -o $(EBIN)
EBIN_DIRS				= $(wildcard deps/*/ebin)
TEST_DIR				= test/
TEST_OPTS 			= -b start_sasl -noshell -s init stop

all: compile_src compile_tests test_run

compile_src:
	echo "COMPILING SOURCE"
	cd ../; $(ERL) -pa $(EBIN_DIRS) -noinput +B -eval 'case make:all() of up_to_date -> halt(0); error -> halt(1) end.'
	
compile_tests:
	@$(ERL) -pa $(EBIN_DIRS) -noinput +B -eval 'case make:all() of up_to_date -> halt(0); error -> halt(1) end.'
	
test_run:
	@$(ERL) -pa $(EBIN_DIRS) $(TEST_OPTS) -noinput +B -eval 'test_suite:test().'

clean:
	rm -rf ebin/*.beam ebin/erl_crash.dump erl_crash.dump ebin/*.boot ebin/*.rel ebin/*.script 
