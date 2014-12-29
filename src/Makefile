include ../Make.options

.PHONY: default
default: $(MONGOOSE_OBJ)
	$(MMC) $(MCFLAGS) --link-object $^ -m

$(MONGOOSE_OBJ): mongoose.c mongoose.h
	$(CC) -c $(MONGOOSE_CFLAGS) $^

mongoose.%: mongoose/mongoose.%
	$(HARDLINK) $^ $@

.PHONY: clean
clean:
	$(DEL_DIR) Mercury
	$(DEL_FILE) mongoose.[och]
	$(DEL_FILE) *.gch
	$(DEL_FILE) *.err
	$(DEL_FILE) *.mh
	$(DEL_FILE) *.so
	$(DEL_FILE) *.a
