BINDIR=./bin
export TERM=xterm

.PHONY: all test update-serial-db hmip-serial-db hmrf-serial-db update install qa perlcritic

test:
	prove -I lib/ -s -r -v t/*