include ../Make.options

EXPS := $(wildcard internal/*.exp)
RESS := $(patsubst %.exp,%.res,$(EXPS))

.PHONY: runtests
runtests: $(MONGOOSE_TEST_SERVER).start $(RESS)
	$(MAKE) $(MONGOOSE_TEST_SERVER).stop

%.stop:
	@echo Stopping all running $* instances
	@($(KILLALL) $*$(EXE_EXT) || \
		( $(SLEEP) 1 && $(KILLALL_FORCE) $*$(EXE_EXT) ) ) || true

%.start: %.build
	@echo Starting $* as a deamon
	MONGOOSE_TEST_ADDRESS=$(MONGOOSE_TEST_ADDRESS) ./$* &
	@echo Wait a second until server is operational
	@$(SLEEP) 1

%.build: %.stop lib
	$(MMC) $(MCFLAGS) --make $* --mld lib/mercury \
		--ml mercury_mongoose $(MLLIBS)

lib: $(wildcard ../src/*.m) ../Make.options
	cd ../src && $(MAKE) install INSTALL_PREFIX="$(CURDIR)"

%.res: %.exp
	@echo testing $*
	$(CURL) $(CURL_FLAGS) http://$(MONGOOSE_TEST_ADDRESS)/$* | \
		$(DIFF) $(DIFF_FLAGS) $< - >$*.res

.PHONY: tests.clean
tests.clean:
	$(DEL_FILE) $(RESS)

.PHONY: clean
clean: $(MONGOOSE_TEST_SERVER).stop tests.clean
	$(MMC) --make $(MONGOOSE_TEST_SERVER).clean
	$(DEL_DIR) Mercury
	$(DEL_FILE) Mercury.modules
	$(DEL_FILE) FAILED_TESTS ABORTED_TESTS
	$(DEL_FILE) $(MONGOOSE_OBJ)
	$(DEL_FILE) $(MONGOOSE_TEST_SERVER)$(EXE_EXT)
