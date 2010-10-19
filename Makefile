# Makefile
APP			= baker
LIBDIR		= `erl -eval \ 'io:format("~s~n", [code:lib_dir()])' -s init stop -noshell`
VERSION		= 0.0.1
CC  		= erlc
ERL     	= erl
EBIN		= ebin
SRC			= src
CFLAGS  	= -I include
COMPILE		= $(CC) $(CFLAGS) -o $(EBIN)/ $(SRC)/*.erl
EBIN_DIRS	= "$(wildcard deps/*/ebin)"

all: ebin compile
all_boot: all make_boot
start: all start_all

compile:
	@$(ERL) -pa $(EBIN_DIRS) -noinput +B -eval 'case make:all() of up_to_date -> halt(0); error -> halt(1) end.'
	@$(COMPILE)
edoc:
	@echo "Generating $(APP) documentation from srcs"
	@erl -noinput -eval 'edoc:application($(APP), "./", [{doc, "doc/"}, {files, "src/"}])' -s erlang halt
  
make_boot:
	(cd ebin; erl -eval 'systools:make_script("$(APP)",[local])' -s erlang halt)

start_all:
	(cd ebin; erl -pa ebin -noshell -sname _name_ -boot _name_)

ebin:
	@mkdir ebin

clean:
	rm -rf ebin/*.beam ebin/erl_crash.dump erl_crash.dump
	rm -rf ebin/*.boot ebin/*.script
	rm -rf doc/*.html doc/*.css doc/erlang.png doc/edoc-info