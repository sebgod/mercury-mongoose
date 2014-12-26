include ../Make.options

TEST_SERVER := test_mercury_mongoose

.PHONY: runtests
runtests: $(TEST_SERVER).start

%.stop:
	@echo Stopping all running $* instances
	@($(KILLALL) $*$(EXE_EXT) || \
		( $(SLEEP) 1 && $(KILLALL_FORCE) $*$(EXE_EXT) ) ) || true

%.start: %.stop %
	@echo Starting $* as a deamon
	./$* &

Mercury.modules: $(wildcard *.m) $(wildcard ../src/*.m)
	$(MMC) -f $(wildcard *.m) $(wildcard ../src/*.m)

$(TEST_SERVER): Mercury.modules $(MONGOOSE_OBJ)
	$(MMC) --make $@ -O5 --link-object $(MONGOOSE_OBJ) $(MLLIBS)

$(MONGOOSE_OBJ):
	cd ../src && $(MAKE) $@
	$(HARDLINK) ../src/$@ $@

.PHONY: clean
clean: $(TEST_SERVER).stop
	$(MMC) --make $(TEST_SERVER).clean
	$(DEL_DIR) Mercury
	$(DEL_FILE) Mercury.modules
	$(DEL_FILE) FAILED_TESTS ABORTED_TESTS
	$(DEL_FILE) $(MONGOOSE_OBJ)
