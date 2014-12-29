include ../Make.options

.PHONY: runtests
runtests: $(MONGOOSE_TEST_SERVER).start

%.stop:
	@echo Stopping all running $* instances
	@($(KILLALL) $*$(EXE_EXT) || \
		( $(SLEEP) 1 && $(KILLALL_FORCE) $*$(EXE_EXT) ) ) || true

%.start: %.build
	@echo Starting $* as a deamon
	./$* &

Mercury.modules: $(wildcard *.m) $(wildcard ../src/*.m)
	$(MMC) -f $(wildcard *.m) $(wildcard ../src/*.m)

%.build: %.stop Mercury.modules $(MONGOOSE_OBJ)
	$(MMC) $(MCFLAGS) --make $* --link-object $(MONGOOSE_OBJ) $(MLLIBS)

$(MONGOOSE_OBJ):
	cd ../src && $(MAKE) $@
	$(HARDLINK) ../src/$@ $@

.PHONY: clean
clean: $(MONGOOSE_TEST_SERVER).stop
	$(MMC) --make $(MONGOOSE_TEST_SERVER).clean
	$(DEL_DIR) Mercury
	$(DEL_FILE) Mercury.modules
	$(DEL_FILE) FAILED_TESTS ABORTED_TESTS
	$(DEL_FILE) $(MONGOOSE_OBJ)
