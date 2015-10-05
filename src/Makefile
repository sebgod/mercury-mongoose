# vim: ft=make tw=78 ts=4 sw=4 noet
include ../Make.options

.PHONY: default
default: $(MONGOOSE_OBJ)
	$(MMC) $(MCFLAGS) --cflags "$(MONGOOSE_OPTS)" --link-object $^ -m

install: $(MONGOOSE_OBJ) default
	$(MMC) $(MCFLAGS) --no-detect-libgrades -s asm_fast.gc \
		-m libmercury_mongoose.install --link-object $< \
		--install-prefix $(INSTALL_PREFIX)

$(MONGOOSE_OBJ): mongoose.c mongoose.h ../Make.options
	$(CC) $(MONGOOSE_CFLAGS) -c $<

mongoose.%: mongoose/mongoose.%
	$(HARDLINK) $^ $@

.PHONY: clean
clean:
	$(DEL_DIR) Mercury
	$(foreach EXT,gch err mh so a lib dylib init,$(DEL_FILE) *.$(EXT);)
	$(DEL_FILE) mongoose.[och] mongoose.obj
