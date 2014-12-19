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

$(TEST_SERVER): Mercury.modules
	$(MMC) --make $@

.PHONY: realclean
realclean:
	$(MMC) --make $(TEST_SERVER).realclean
	$(DEL_DIR) Mercury
	$(DEL_FILE) Mercury.modules
	$(DEL_FILE) FAILED_TESTS ABORTED_TESTS
