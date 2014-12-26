include ../Make.options

TEST_SERVER := test_mercury_mongoose
MONGOOSE_OBJ := mongoose$(OBJ_EXT)

.PHONY: runtests
runtests: $(TEST_SERVER).start

%.stop:
	@echo Stopping all running $* instances
	@($(TASKKILL) $*$(EXE_EXT) || ( $(SLEEP) 1 && $(TASKKILL_FORCE) $*$(EXE_EXT) ) ) || true

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
clean:
	$(MMC) --make $(TEST_SERVER).clean
	$(DEL_DIR) Mercury
	$(DEL_FILE) Mercury.modules
	$(DEL_FILE) FAILED_TESTS ABORTED_TESTS
	$(DEL_FILE) mongoose$(OBJ_EXT)
