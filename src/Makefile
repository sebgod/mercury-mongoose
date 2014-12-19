include ../Make.options

.PHONY: default
default: mongoose.o
	$(MMC) --link-object $^ -m

mongoose.%: mongoose/mongoose.%
	$(HARDLINK) $^ $@

mongoose.o: mongoose.c mongoose.h
	$(CC) -c $(CFLAGS) $^

.PHONY: clean
clean:
	rm -f mongoose.[och]
	rm -f *.gch
	rm -f *.so
	rm -f *.a
	rm -f *.err
