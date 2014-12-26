include ../Make.options

.PHONY: default
default: $(MONGOOSE_OBJ)
	$(MMC) --link-object $^ -m

mongoose.%: mongoose/mongoose.%
	$(HARDLINK) $^ $@

$(MONGOOSE_OBJ): mongoose.c mongoose.h
	$(CC) -c $(CFLAGS) $^

.PHONY: clean
clean:
	$(DEL_DIR) Mercury
	$(DEL_FILE) mongoose.[och]
	$(DEL_FILE) *.gch
	$(DEL_FILE) *.so
	$(DEL_FILE) *.a
	$(DEL_FILE) *.err
