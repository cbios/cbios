# $Id$

ROMS:=main sub
ROMS_FULLPATH:=$(addprefix derived/bin/cbios_,$(addsuffix .rom,$(ROMS)))

.PHONY: all clean
.DELETE_ON_ERROR: $(ROMS_FULLPATH)

all: $(ROMS_FULLPATH)

derived/bin/cbios_%.rom: src/%.asm
	@echo "Assembling: $<"
	@mkdir -p $(@D)
	@mkdir -p derived/lst
	@sjasm -l $< $@ $(@:derived/bin/%.rom=derived/lst/%.lst)

clean:
	@rm -rf derived
