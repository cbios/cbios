; Copyright (c) 2026 Joost Yervante Damad
; SPDX-License-Identifier: BSD-2-Clause

; GENERATED FILE -- do not hand-edit. Produced by
; tools/gen_resident_abi.py from build/basic-reloc.sym (Makefile rule);
; regenerated on every build so a page-0-low shift can never leave this
; disk ROM calling stale addresses. docs/spec-basic-subrom-mathpack.md §4.
;
; Clean-room: every value below is an address inside THIS project's own
; main ROM, read out of our own build's symbol file. Nothing here is derived
; from a disassembly or byte-copy of any reference ROM.

widen_int_to equ 03945H
widen_fac_to equ 038F7H
round_single_and_pack equ 03577H
round_and_finalize equ 03526H
pu_deref_body equ 02C7EH
subrom_call equ 03C24H
ARGA equ 0F806H   ; operand A / working result, FPNUM record (18)
ZB_HOST equ 0FD09H   ; (2 B) ZB_HOST_SIG while zerobas BASIC hosts the disk ROM: slot 0's own SLTWRK
ZB_HOST_SIG equ 0425AH   ; NOT-A-CELL: "ZB", little-endian
ENDFLAG equ 0F6FFH   ; 1 = END/STOP reached, stop the run (1 B)
ONEFLG equ 0F6BBH   ; 1 B: $FF = currently inside a handler (no RESUME
STRSCR equ 0F55EH   ; = BUF ($F55E): [len][bytes:STRMAX] staging (256 B)
FN_RESUME equ 0F6BCH   ; D-FNEXPR: cursor past a filename expr (2 B) -- published TEMP2 (D-WSZERO)
DISKOP_STATUS equ 0FCAEH   ; tenant -> main: the disposition (1 B)
DISK_FCB_NAME equ 0EF49H   ; DISK-ONLY: +1..+11: 8.3 name field (11 bytes)
FILES_HASPAT equ 0F09DH   ; DISK-ONLY: FILES: 1 = a filespec pattern is in DISK_FCB_NAME (1)
FAC equ 0F7F6H   ; float accumulator = DAC: value bytes as tokenised (8)
FPERR equ 0F7B9H   ; runtime numeric-error flag: 0 none / 1 overflow / (1 B)
FACTYP equ 0F663H   ; 2=int / 4=single / 8=double (1)
FSECTOR_BUF equ 0E8ADH   ; DISK-ONLY: file data / read sector buffer ($E8AD..$EAAC)
FWBUF equ 0EAADH   ; DISK-ONLY: FAT/dir metadata sector buffer ($EAAD..$ECAC)
STRPTR equ 0FD0EH   ; pointer to the [len][bytes] descriptor (2 bytes)
DISKOP_OP equ 0FCACH   ; main -> tenant: DISKOP_SEL_* primitive selector (1)
DISKOP_ERR equ 0FC9AH   ; tenant -> main: ERR code of the last DSKIO failure, 0 = none (1)
CLPTR equ 0F6EDH   ; CLOAD/LOAD: store cursor into the program area (2)
CLINK equ 0F6EFH   ; CLOAD/LOAD: saved link word of the current line
TXTBASE equ 08001H   ; text base (oracle: TXTTAB value after boot)
TXTTAB equ 0F676H   ; sysvar: pointer to the BASIC text base
TXTMAX equ 0E7ADH   ; NOT-A-CELL: the text ceiling (= the workspace floor)
TXTMAX_NODISK equ 0F380H   ; NOT-A-CELL: the diskless build's text ceiling
PRGEND equ 0FC96H   ; addr of the $0000 end-of-program marker (2 bytes)
DSV_PTR equ 0EF59H   ; DISK-ONLY: SAVE: current source byte pointer (2)
DSV_END equ 0EF5BH   ; DISK-ONLY: SAVE/BSAVE: last source byte addr, inclusive (2)
DISKSLOT_OK equ 0FB21H   ; nonzero = DISKSLOT valid (1 B; the disk ROM's drive count once INIT ran -- D-DRVCOUNT)
CURPTR equ 0F7BCH   ; current load pointer (2 bytes) -- SWPTMP+0
EXECPTR equ 0F7C0H   ; execution address   (2 bytes) -- SWPTMP+4
VRAM_FLAG equ 0F664H   ; 1 = ",S" VRAM save/load (1) -- published DORES, idle (D-WSZERO s6)
ENDPTR equ 0F7BEH   ; final load address  (2 bytes) -- SWPTMP+2
BL_OFS equ 0F69FH   ; 2 B: main -> tenant, BLOAD's offset (0 = none) -- published TEMP8 (D-WSZERO; was SW_ADDR's)
COPY_SRC equ 0EEEEH   ; DISK-ONLY: the source's 11-byte 8.3 name field
COPY_CLUS equ 0EEF9H   ; DISK-ONLY: the source's first cluster (word)
COPY_LEFT equ 0EEFBH   ; DISK-ONLY: bytes still to copy (4-byte LE)
FILES_ENTIDX equ 0EF63H   ; DISK-ONLY: FILES: dir entry index 0..15 within a sector (1)
LPTPOS equ 0F415H   ; printer head column, 0-based (1)
FCH_ACTIVE equ 0F733H   ; channel live in the engine globals, 0 = none (1)
FCH_MODES equ 0F71CH   ; 16 B ($F71C..$F72B)
FCH_STATESZ equ 00032H   ; $E9C9..$E9FA inclusive = 50 bytes
FCH_CEIL equ 0000FH   ; repack: the MEASURED reference ceiling (§3, exactly
LPT_MODE equ 00005H   ; FCH_MODES value: channel open to the LPT: printer device
DOUT_MODE equ 00003H   ; FCH_MODES value: disk OUTPUT through disk.rom's FCB
DOUT_BLK equ 00025H   ; its row: +0..36 the FCB, +37..38 the channel's
FCH_RECMAX equ 00100H   ; the largest record (the OPEN LEN= domain is 1..256)
DOUT_FLG equ 00027H   ; 265 B block, +39 flags (DOUT_RFULL / DOUT_DEAD)
DOUT_RFULL equ 00000H   ; bit: the record is full and not yet written
DOUT_DEAD equ 00001H   ; bit: a CLOSE failed; the next CLOSE frees it
FCH_RECLENS equ 0EE4DH   ; DISK-ONLY: 32 B ($EE4D..$EE6C). ~~$EA92..$EAFF free (110 B)~~
GP_RECNO equ 0EE7DH   ; DISK-ONLY: 4 B ($EE7D..$EE80): GET / PUT's record number,
GP_SEC equ 0F07BH   ; DISK-ONLY: file logical-sector index of the record (2)
GP_WITHIN equ 0F07DH   ; DISK-ONLY: byte offset of the record within its sector (0/256, 2) (2 B)
GP_CLUS equ 0F07FH   ; DISK-ONLY: current cluster during the chain walk (2)
GP_CLIDX equ 0F081H   ; DISK-ONLY: cluster steps still to walk (2)
GP_SECINCL equ 0F083H   ; DISK-ONLY: sector index within the final cluster (1)
GP_PHYS equ 0F084H   ; DISK-ONLY: resolved absolute physical sector (2)
GP_FLAGS equ 0F088H   ; DISK-ONLY: bit0 = extend (PUT allocates; GET does not) (1)
GP_RECLEN equ 0F09AH   ; DISK-ONLY: active channel's record length (word), loaded per calc
GP_SRC equ 0EE77H   ; DISK-ONLY: pass cursor into FSECTOR_BUF (2)
GP_LEFT equ 0EE79H   ; DISK-ONLY: record bytes still to move, ends $EA3E (2 B)
FCH_RDMODE equ 0F6A6H   ; current read: 0=INPUT# (stop at ','/CR), 1=LINE (1)
STRMAX equ 000FFH   ; repack build: string heap (slice-4a §8)
LRSET_JUST equ 0EEC6H   ; DISK-ONLY: LSET/RSET: the verb's token, bit 0 = right-justify (1)
MIDS_DEST equ 0F65EH   ; MID$ statement: the target descriptor's address (2 B)
EQ_TOKEN equ 000EFH   ; '='
FLD_CUROFF equ 0EEC3H   ; DISK-ONLY: FIELD: the next field's first byte, in the channel's record at block+9 (2)
FLD_END equ 0F019H   ; DISK-ONLY: FIELD: the end of the channel's record, block+9+reclen -- ERR 50's bound (2)
RVDESC equ 0F698H   ; [len:1][ptr:2] scratch descriptor (3 B)
FMT_GEOMSEL equ 0EF61H   ; DISK-ONLY: main -> tenant: 0 = 360k, 1 = 720k (1)
FMT_RESULT equ 0EF62H   ; DISK-ONLY: tenant -> main: 0 = ok, 1 = DSKIO write error (1)
LINEBUF equ 0F55EH   ; repack: ASCII input line = the reference's BUF (LINEMAX B) (1 B)
SUBROM_ENTRY_BASE_P1 equ 04010H   ; sub-ROM page-1 jp-table base
SUBROM_IDX_FORMAT equ 0000AH   ; format_tenant (CALL FORMAT eviction, sub/

; --- page-1 call-back targets: reached by an INTER-SLOT call (MSX2 TH), not an absolute one ---
fname_expr equ 06D25H
pdfcb_resume equ 06D1EH
parse_disk_fcb equ 066B6H
eval equ 04A94H
fch_check equ 06F23H
fch_mode_class equ 04F7CH
field_item equ 072F6H
tgt_store_desc equ 05BC7H
req_letter equ 040F9H
var_str_type equ 0463FH
tgt_parse_req equ 05298H
tgt_desc equ 0477DH
skip_eq equ 0423DH
req_operand equ 0413CH
str_eval equ 048EFH
type_mismatch_error equ 04271H
stmt_error equ 04263H
lrset_finish equ 0718FH
fch_ctx_addr equ 06EA7H
disk_error equ 06990H
read_line equ 0452FH
chget_getbyte equ 0700AH
fch_close_all equ 06F67H
eval_int16_checked equ 044A5H
check_expr_errors equ 04381H
eval_byte_arg equ 044B9H
skip_comma equ 04237H
raise_error equ 04283H
