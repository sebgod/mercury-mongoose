include ../Make.options

TEST_SERVER := test_server

.PHONY: runtests
runtests: $(TEST_SERVER)
	@./$^ $(DIFF_FLAGS)

.PHONY: runtests-verbose
runtests-verbose: $(TEST_SERVER)
	@./$(TEST_SERVER) $(DIFF_FLAGS) -v

Mercury.modules: $(wildcard *.m) $(wildcard ../src/*.m)
	$(MMC) -f $(wildcard *.m) $(wildcard ../src/*.m)

$(TEST_SERVER): Mercury.modules mongoose.$(OBJ_EXT)
	$(MMC) --make $@ -O5 --link-object mongoose.$(OBJ_EXT) $(MLLIBS)

mongoose.$(OBJ_EXT):
	cd ../src && $(MAKE) $@
	$(HARDLINK) ../src/$@ $@

.PHONY: clean
clean:
	$(MMC) --make $(TEST_SERVER).clean
	$(DEL_DIR) Mercury
	$(DEL_FILE) Mercury.modules
	$(DEL_FILE) FAILED_TESTS ABORTED_TESTS
	$(DEL_FILE) mongoose.$(OBJ_EXT)
