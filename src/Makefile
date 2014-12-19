include ../Make.options

.PHONY: default
default: mongoose.h mongoose.c
	$(MMC) -m

mongoose.%: mongoose/mongoose.%
	$(HARDLINK) $^ $@
