# $Id: Makefile,v 1.3 2004/12/11 04:37:54 mthuurne Exp $

# Select your assembler:
Z80_ASSEMBLER?=pasmo
#Z80_ASSEMBLER?=sjasm

PACKAGE:=cbios
VERSION:=0.18
PACKAGE_FULL:=$(PACKAGE)-$(VERSION)

ROMS:=main sub
ROMS_FULLPATH:=$(addprefix derived/bin/cbios_,$(addsuffix .rom,$(ROMS)))
DEPS_FULLPATH:=$(addprefix derived/dep/,$(addsuffix .dep,$(ROMS)))

# Mark all logical targets as such.
.PHONY: all dist clean

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

dist: all
	@rm -rf derived/dist
	@mkdir -p derived/dist/$(PACKAGE_FULL)
	@find . -type f '!' -path '*/CVS/*' '!' -path '*/derived/*' '!' -name '.*' \
		-exec cp --parents "{}" derived/dist/$(PACKAGE_FULL) ';'
	@find configs/openMSX/* -maxdepth 0 -type d '!' -name 'CVS' \
		-exec mkdir "derived/dist/$(PACKAGE_FULL)/{}/roms" ';'
	@SCRIPT=`mktemp` \
		&& sha1sum $(ROMS_FULLPATH) | sed -nf tools/subst_sha1.sed > $$SCRIPT \
		&& sed -s -i -f $$SCRIPT \
			derived/dist/$(PACKAGE_FULL)/configs/openMSX/*/hardwareconfig.xml \
		&& rm $$SCRIPT
	@mkdir -p derived/dist/$(PACKAGE_FULL)/roms
	@cp $(ROMS_FULLPATH) derived/dist/$(PACKAGE_FULL)/roms
	@cd derived/dist ; zip -9 -r $(PACKAGE_FULL).zip $(PACKAGE_FULL)

