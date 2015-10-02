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
	MONGOOSE_TEST_PORT=$(MONGOOSE_TEST_PORT) ./$* &
	@echo Wait a second until server is operational
	@$(SLEEP) 1

Mercury.modules: $(wildcard *.m) $(wildcard ../src/*.[hmc])
	@echo Call test clean to ensure testing of changed code
	$(MAKE) tests.clean
	$(MMC) -f $(filter %.m,$^)

%.build: %.stop Mercury.modules $(MONGOOSE_OBJ)
	$(MMC) $(MCFLAGS) --make $* --link-object $(MONGOOSE_OBJ) $(MLLIBS)

$(MONGOOSE_OBJ): $(wildcard ../src/*.hc) ../Make.options
	cd ../src && $(MAKE) $@
	$(HARDLINK) ../src/$@ $@

%.res: %.exp
	@echo testing $*
	$(CURL) $(CURL_FLAGS) http://127.0.0.1:$(MONGOOSE_TEST_PORT)/$* | \
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
