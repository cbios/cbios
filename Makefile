# $Id: Makefile,v 1.6 2004/12/26 21:42:51 bifimsx Exp $

# Select your assembler:
Z80_ASSEMBLER?=pasmo
#Z80_ASSEMBLER?=sjasm

PACKAGE:=cbios
VERSION:=0.19
PACKAGE_FULL:=$(PACKAGE)-$(VERSION)

ROMS:=main sub music disk
ROMS_FULLPATH:=$(ROMS:%=derived/bin/cbios_%.rom)

# Mark all logical targets as such.
.PHONY: all dist clean

all: $(ROMS_FULLPATH)

ifeq ($(Z80_ASSEMBLER),sjasm)
# Workaround for SjASM producing output file even if assembly failed.
.DELETE_ON_ERROR: $(ROMS_FULLPATH)
endif

derived/bin/cbios_%.rom: vdep/%.asm
	@echo "Assembling: $(<:vdep/%=src/%)"
	@mkdir -p $(@D)
	@mkdir -p derived/lst
ifeq ($(Z80_ASSEMBLER),sjasm)
	@sjasm -l $(<:vdep/%=src/%) $@ $(@:derived/bin/%.rom=derived/lst/%.lst)
endif
ifeq ($(Z80_ASSEMBLER),pasmo)
	@pasmo -I src $(<:vdep/%=src/%) $@ $(@:derived/bin/%.rom=derived/lst/%.lst)
endif

# Include main dependency files.
-include $(ROMS:%=derived/dep/%.dep)

ifeq ($(filter clean,$(MAKECMDGOALS)),)
# Incremental build -> create dependency files.
derived/dep/%.dep: src/%.asm
	@echo "Depending: $<"
	@mkdir -p $(@D)
	@echo "INCLUDES:=" > $@
	@sed -n '/include/s/^[\t ]*include[\t ]*"\(.*\)".*$$/INCLUDES+=\1/p' \
		< $< >> $@
	@echo ".SECONDARY: $(<:src/%=vdep/%)" >> $@
	@echo "$(<:src/%=vdep/%): $< \$$(INCLUDES:%=vdep/%)" >> $@
	@echo "ifneq (\$$(INCLUDES),)" >> $@
	@echo "-include \$$(INCLUDES:%.asm=derived/dep/%.dep)" >> $@
	@echo "endif" >> $@
else
# Clean build -> treat all dependencies as outdated.
.PHONY: $(ROMS:%=vdep/%.asm)
endif

clean:
	@rm -rf derived

dist: all
	@rm -rf derived/dist
	@mkdir -p derived/dist/$(PACKAGE_FULL)
	@find . -type f '!' -path '*/CVS/*' \
		'!' -path './derived/*' '!' -path './debian/*' \
		'!' -name '.*' \
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

