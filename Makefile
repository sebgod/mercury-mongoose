include Make.options

.PHONY: default
default:
	cd src && $(MAKE) default

.PHONY: clean
clean:
	cd src && $(MAKE) clean
	cd tests && $(MAKE) clean

.PHONY: tests
tests:
	cd tests && $(MAKE) runtests

.PHONY: stop
stop:
	cd tests && $(MAKE) $(MONGOOSE_TEST_SERVER).stop

.PHONY: all
all: default tests
