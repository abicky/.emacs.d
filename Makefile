all:

setup:
	@git submodule update --init
	@./script/install-packages
