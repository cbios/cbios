# $Id: Makefile,v 1.1.1.1 2004/11/27 02:23:54 mthuurne Exp $

ROMS:=main sub
ROMS_FULLPATH:=$(addprefix derived/bin/cbios_,$(addsuffix .rom,$(ROMS)))
DEPS_FULLPATH:=$(addprefix derived/dep/,$(addsuffix .dep,$(ROMS)))

# Mark all logical targets as such.
.PHONY: all clean

all: $(ROMS_FULLPATH)

# Workaround for SjASM producing output file even if assembly failed.
.DELETE_ON_ERROR: $(ROMS_FULLPATH)

derived/bin/cbios_%.rom: src/%.asm
	@echo "Assembling: $<"
	@mkdir -p $(@D)
	@mkdir -p derived/lst
	@sjasm -l $< $@ $(@:derived/bin/%.rom=derived/lst/%.lst)

# Include dependency files.
ifeq ($(filter clean,$(MAKECMDGOALS)),)
  -include $(DEPS_FULLPATH)
endif

derived/dep/%.dep: src/%.asm
	@echo "Depending: $<"
	@mkdir -p $(@D)
	@echo "$(<:src/%.asm=derived/bin/cbios_%.rom): \\" > $@
	@sed -n '/include/s/^[\t ]*include[\t ]*"\(.*\)".*$$/  src\/\1 \\/p' \
		< $< >> $@
	@echo "  $<" >> $@

clean:
	@rm -rf derived

