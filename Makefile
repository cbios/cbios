# $Id: Makefile,v 1.2 2004/12/05 16:53:16 mthuurne Exp $

# Select your assembler:
Z80_ASSEMBLER?=pasmo
#Z80_ASSEMBLER?=sjasm

ROMS:=main sub
ROMS_FULLPATH:=$(addprefix derived/bin/cbios_,$(addsuffix .rom,$(ROMS)))
DEPS_FULLPATH:=$(addprefix derived/dep/,$(addsuffix .dep,$(ROMS)))

# Mark all logical targets as such.
.PHONY: all clean

all: $(ROMS_FULLPATH)

ifeq ($(Z80_ASSEMBLER),sjasm)
# Workaround for SjASM producing output file even if assembly failed.
.DELETE_ON_ERROR: $(ROMS_FULLPATH)
endif

derived/bin/cbios_%.rom: src/%.asm
	@echo "Assembling: $<"
	@mkdir -p $(@D)
	@mkdir -p derived/lst
ifeq ($(Z80_ASSEMBLER),sjasm)
	@sjasm -l $< $@ $(@:derived/bin/%.rom=derived/lst/%.lst)
endif
ifeq ($(Z80_ASSEMBLER),pasmo)
	@pasmo -I $(<D) $< $@ $(@:derived/bin/%.rom=derived/lst/%.lst)
endif

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

