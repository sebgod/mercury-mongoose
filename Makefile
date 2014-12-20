include Make.options

.PHONY: default
default:
	cd src && $(MAKE) default

.PHONY: clean
clean:
	cd src && $(MAKE) clean

.PHONY: tests
tests:
	cd tests && $(MAKE) runtests

.PHONY: all
all: default tests
