#!/usr/bin/env ruby

# $Id: basic.rb,v 1.4 2005/04/18 18:54:43 andete Exp $
#
# ruby script for generating BASIC parsing related assembler
#
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# !This is Experimental Work In Progress, and might turn out not be useful!
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#
# Copyright (c) 2005 Joost Yervante Damad.  All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions
# are met:
# 1. Redistributions of source code must retain the above copyright
#    notice, this list of conditions and the following disclaimer.
# 2. Redistributions in binary form must reproduce the above copyright
#    notice, this list of conditions and the following disclaimer in the
#    documentation and/or other materials provided with the distribution.
#
# THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
# IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
# OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
# IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
# INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
# NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES# LOSS OF USE,
# DATA, OR PROFITS# OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
# THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
# THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#


class ByteEncoding
    def initialize(string, token, extended)
        @string = string        # characters representing the token
        @token = token          # the token byte
        @extended = extended    # extended (0xFF prefix)
    end
    attr_reader :string, :token, :extended

    def hex
        sprintf "%02x", @token
    end
end

encodings = [
    #
    ByteEncoding.new("0",    0x11,    false),
    ByteEncoding.new("1",    0x12,    false),
    ByteEncoding.new("2",    0x13,    false),
    ByteEncoding.new("3",    0x14,    false),
    ByteEncoding.new("4",    0x15,    false),
    ByteEncoding.new("5",    0x16,    false),
    ByteEncoding.new("6",    0x17,    false),
    ByteEncoding.new("7",    0x18,    false),
    ByteEncoding.new("8",    0x19,    false),
    ByteEncoding.new("0",    0x1A,    false),
    #
    ByteEncoding.new(" ",    0x20,    false),
    ByteEncoding.new("!",    0x21,    false),
    ByteEncoding.new('"',    0x22,    false),
    ByteEncoding.new("#",    0x23,    false),
    ByteEncoding.new("$",    0x24,    false),
    ByteEncoding.new("%",    0x25,    false),
    ByteEncoding.new("&",    0x27,    false),
    #
    ByteEncoding.new(":",    0x3A,    false),
    ByteEncoding.new(";",    0x3B,    false),
    #
    ByteEncoding.new("A",    0x41,    false),
    ByteEncoding.new("B",    0x42,    false),
    ByteEncoding.new("C",    0x43,    false),
    ByteEncoding.new("D",    0x44,    false),
    ByteEncoding.new("E",    0x45,    false),
    ByteEncoding.new("F",    0x46,    false),
    ByteEncoding.new("G",    0x47,    false),
    ByteEncoding.new("H",    0x48,    false),
    ByteEncoding.new("I",    0x49,    false),
    ByteEncoding.new("J",    0x4A,    false),
    ByteEncoding.new("K",    0x4B,    false),
    ByteEncoding.new("L",    0x4C,    false),
    ByteEncoding.new("M",    0x4D,    false),
    ByteEncoding.new("N",    0x4E,    false),
    ByteEncoding.new("O",    0x4F,    false),
    ByteEncoding.new("P",    0x50,    false),
    ByteEncoding.new("Q",    0x51,    false),
    ByteEncoding.new("R",    0x52,    false),
    ByteEncoding.new("S",    0x53,    false),
    ByteEncoding.new("T",    0x54,    false),
    ByteEncoding.new("U",    0x55,    false),
    ByteEncoding.new("V",    0x56,    false),
    ByteEncoding.new("W",    0x57,    false),
    ByteEncoding.new("X",    0x58,    false),
    ByteEncoding.new("Y",    0x59,    false),
    ByteEncoding.new("Z",    0x5A,    false),
    #
    ByteEncoding.new("END",     0x81,    false),
    ByteEncoding.new("LEFT$",   0x81,    true),
    ByteEncoding.new("FOR",     0x82,    false),
    ByteEncoding.new("RIGHT$",  0x82,    true),
    ByteEncoding.new("NEXT",    0x83,    false),
    ByteEncoding.new("MID$",    0x83,    true),
    ByteEncoding.new("DATA",    0x84,    false),
    ByteEncoding.new("SGN",     0x84,    true),
    ByteEncoding.new("INPUT",   0x85,    false),
    ByteEncoding.new("INT",     0x85,    true),
    ByteEncoding.new("DIM",     0x86,    false),
    ByteEncoding.new("ABS",     0x86,    true),
    ByteEncoding.new("READ",    0x87,    false),
    ByteEncoding.new("SQR",     0x87,    true),
    ByteEncoding.new("LET",     0x88,    false),
    ByteEncoding.new("RND",     0x88,    true),
    ByteEncoding.new("GOTO",    0x89,    false),
    ByteEncoding.new("SIN",     0x89,    true),
    ByteEncoding.new("RUN",     0x8A,    false),
    ByteEncoding.new("LOG",     0x8A,    true),
    ByteEncoding.new("IF",      0x8B,    false),
    ByteEncoding.new("EXP",     0x8B,    true),
    ByteEncoding.new("RESTORE", 0x8C,    false),
    ByteEncoding.new("COS",     0x8C,    true),
    ByteEncoding.new("GOSUB",   0x8D,    false),
    ByteEncoding.new("TAN",     0x8D,    true),
    ByteEncoding.new("RETURN",  0x8E,    false),
    ByteEncoding.new("ATN",     0x8E,    true),
    ByteEncoding.new("REM",     0x8F,    false),
    ByteEncoding.new("FRE",     0x8F,    true),
    ByteEncoding.new("STOP",    0x90,    false),
    ByteEncoding.new("INP",     0x90,    true),
    ByteEncoding.new("PRINT",   0x91,    false),
    ByteEncoding.new("POS",     0x91,    true),
    ByteEncoding.new("CLEAR",   0x92,    false),
    ByteEncoding.new("LEN$",    0x92,    true),
    ByteEncoding.new("LIST",    0x93,    false),
    ByteEncoding.new("STR$",    0x93,    true),
    ByteEncoding.new("NEW",     0x94,    false),
    ByteEncoding.new("VAL",     0x94,    true),
    ByteEncoding.new("ON",      0x95,    false),
    ByteEncoding.new("ASC",     0x95,    true),
    ByteEncoding.new("WAIT",    0x96,    false),
    ByteEncoding.new("CHR$",    0x96,    true),
    ByteEncoding.new("DEF",     0x97,    false),
    ByteEncoding.new("PEEK",    0x97,    true),
    ByteEncoding.new("POKE",    0x98,    false),
    ByteEncoding.new("VPEEK",   0x98,    true),
    ByteEncoding.new("CONT",    0x99,    false),
    ByteEncoding.new("SPACE$",  0x99,    true),
    ByteEncoding.new("CSAVE",   0x9A,    false),
    ByteEncoding.new("OCT$",    0x9A,    true),
    ByteEncoding.new("CLOAD",   0x9B,    false),
    ByteEncoding.new("HEX$",    0x9B,    true),
    ByteEncoding.new("OUT",     0x9C,    false),
    ByteEncoding.new("LPOS",    0x9C,    true),
    ByteEncoding.new("LPRINT",  0x9D,    false),
    ByteEncoding.new("BIN$",    0x9D,    true),
    ByteEncoding.new("LLIST",   0x9E,    false),
    ByteEncoding.new("CINT",    0x9E,    true),
    ByteEncoding.new("CLS",     0x9F,    false),
    ByteEncoding.new("CSNG",    0x9F,    true),
    ByteEncoding.new("WIDTH",   0xA0,    false),
    ByteEncoding.new("CDBL",    0xA0,    true),
    ByteEncoding.new("ELSE",    0xA1,    false),
    ByteEncoding.new("FIX",     0xA1,    true),
    ByteEncoding.new("TRON",    0xA2,    false),
    ByteEncoding.new("STICK",   0xA2,    true),
    ByteEncoding.new("TROFF",   0xA3,    false),
    ByteEncoding.new("STRIG",   0xA3,    true),
    ByteEncoding.new("SWAP",    0xA4,    false),
    ByteEncoding.new("PDL",     0xA4,    true),
    ByteEncoding.new("ERASE",   0xA5,    false),
    ByteEncoding.new("PAD",     0xA5,    true),
    ByteEncoding.new("ERROR",   0xA6,    false),
    ByteEncoding.new("DSKF",    0xA6,    true),
    ByteEncoding.new("RESUME",  0xA7,    false),
    ByteEncoding.new("FPOS",    0xA7,    true),
    ByteEncoding.new("DELETE",  0xA8,    false),
    ByteEncoding.new("CVI",     0xA8,    true),
    ByteEncoding.new("AUTO",    0xA9,    false),
    ByteEncoding.new("CVS",     0xA9,    true),
    ByteEncoding.new("RENUM",   0xAA,    false),
    ByteEncoding.new("CVD",     0xAA,    true),
    ByteEncoding.new("DEFSTR",  0xAB,    false),
    ByteEncoding.new("EOF",     0xAB,    true),
    ByteEncoding.new("DEFINT",  0xAC,    false),
    ByteEncoding.new("LOC",     0xAC,    true),
    ByteEncoding.new("DEFSNG",  0xAD,    false),
    ByteEncoding.new("LOF",     0xAD,    true),
    ByteEncoding.new("DEFDBL",  0xAE,    false),
    ByteEncoding.new("MKI$",    0xAE,    true),
    ByteEncoding.new("LINE",    0xAF,    false),
    ByteEncoding.new("MKS$",    0xAF,    true),
    ByteEncoding.new("OPEN",    0xB0,    false),
    ByteEncoding.new("MKD$",    0xB0,    true),
    ByteEncoding.new("FIELD",   0xB1,    false),
    ByteEncoding.new("GET",     0xB2,    false),
    ByteEncoding.new("PUT",     0xB3,    false),
    ByteEncoding.new("CLOSE",   0xB4,    false),
    ByteEncoding.new("LOAD",    0xB5,    false),
    ByteEncoding.new("MERGE",   0xB6,    false),
    ByteEncoding.new("FILES",   0xB7,    false),
    ByteEncoding.new("LSET",    0xB8,    false),
    ByteEncoding.new("RSET",    0xB9,    false),
    ByteEncoding.new("SAVE",    0xBA,    false),
    ByteEncoding.new("LFILES",  0xBB,    false),
    ByteEncoding.new("CIRCLE",  0xBC,    false),
    ByteEncoding.new("COLOR",   0xBD,    false),
    ByteEncoding.new("DRAW",    0xBE,    false),
    ByteEncoding.new("PAINT",   0xBF,    false),
    ByteEncoding.new("BEEP",    0xC0,    false),
    ByteEncoding.new("PLAY",    0xC1,    false),
    ByteEncoding.new("PSET",    0xC2,    false),
    ByteEncoding.new("PRESET",  0xC3,    false),
    ByteEncoding.new("SOUND",   0xC4,    false),
    ByteEncoding.new("SCREEN",  0xC5,    false),
    ByteEncoding.new("VPOKE",   0xC6,    false),
    ByteEncoding.new("SPRITE",  0xC7,    false),
    ByteEncoding.new("VDP",     0xC8,    false),
    ByteEncoding.new("BASE",    0xC9,    false),
    ByteEncoding.new("CALL",    0xCA,    false),
    ByteEncoding.new("TIME",    0xCB,    false),
    ByteEncoding.new("KEY",     0xCC,    false),
    ByteEncoding.new("MAX",     0xCD,    false),
    ByteEncoding.new("MOTOR",   0xCE,    false),
    ByteEncoding.new("BLOAD",   0xCF,    false),
    ByteEncoding.new("BSAVE",   0xD0,    false),
    ByteEncoding.new("DSKO$",   0xD1,    false),
    ByteEncoding.new("SET",     0xD2,    false),
    ByteEncoding.new("NAME",    0xD3,    false),
    ByteEncoding.new("KILL",    0xD4,    false),
    ByteEncoding.new("IPL",     0xD5,    false),
    ByteEncoding.new("COPY",    0xD6,    false),
    ByteEncoding.new("CMD",     0xD7,    false),
    ByteEncoding.new("LOCATE",  0xD8,    false),
    ByteEncoding.new("TO",      0xD9,    false),
    ByteEncoding.new("THEN",    0xDA,    false),
    ByteEncoding.new("TAB(",    0xDB,    false),
    ByteEncoding.new("STEP",    0xDC,    false),
    ByteEncoding.new("USR",     0xDD,    false),
    ByteEncoding.new("FN",      0xDE,    false),
    ByteEncoding.new("SPCL",    0xDF,    false),
    ByteEncoding.new("NOT",     0xE0,    false),
    ByteEncoding.new("ERL",     0xE1,    false),
    ByteEncoding.new("ERR",     0xE2,    false),
    ByteEncoding.new("STRING$", 0xE3,    false),
    ByteEncoding.new("USING",   0xE4,    false),
    ByteEncoding.new("INSTR",   0xE5,    false),
    ByteEncoding.new("'",       0xE6,    false),
    ByteEncoding.new("VARPTR",  0xE7,    false),
    ByteEncoding.new("CSRLIN",  0xE8,    false),
    ByteEncoding.new("ATTR$",   0xE9,    false),
    ByteEncoding.new("DSKI$",   0xEA,    false),
    ByteEncoding.new("OFF",     0xEB,    false),
    ByteEncoding.new("INKEY$",  0xEC,    false),
    ByteEncoding.new("POINT",   0xED,    false),
    ByteEncoding.new(">",       0xEE,    false),
    ByteEncoding.new("=",       0xEF,    false),
    ByteEncoding.new("<",       0xF0,    false),
    ByteEncoding.new("+",       0xF1,    false),
    ByteEncoding.new("-",       0xF2,    false),
    ByteEncoding.new("*",       0xF3,    false),
    ByteEncoding.new("/",       0xF4,    false),
    ByteEncoding.new("^",       0xF5,    false),
    ByteEncoding.new("AND",     0xF6,    false),
    ByteEncoding.new("OR",      0xF7,    false),
    ByteEncoding.new("XOR",     0xF8,    false),
    ByteEncoding.new("EQV",     0xF9,    false),
    ByteEncoding.new("IMP",     0xFA,    false),
    ByteEncoding.new("MOD",     0xFB,    false),
    ByteEncoding.new("\\",      0xFC,    false),
]

# 0 preparation

# 0.1 generate token => string maps and the other way around
$token_string_map = {}
$ext_token_string_map = {}
$string_token_map = {}
$ext_string_token_map = {}
encodings.each {|encoding|
    if encoding.extended
        $ext_token_string_map[encoding.token] = encoding.string
        $ext_string_token_map[encoding.string] = encoding.token
    else
        $token_string_map[encoding.token] = encoding.string
        $string_token_map[encoding.string] = encoding.token
    end
}

# 1 generate token => string tables

# 1.1 generate token string table
encodings.each {|encoding|
    ext = ""
    if encoding.extended
        ext ="ext_"
    end
    puts "#{ext}token_string_#{encoding.hex}"
    puts "        db \"#{encoding.string}\", $#{encoding.hex}"
}

# 1.2 generate token string address table
puts "\ntoken_string_address_table:"
(0..0xFF).each {|i|
    hex = sprintf "%02x", i
    if $token_string_map.has_key?(i)
        puts "       dw token_string_#{hex}"
    else
        puts "       dw $0000                ; $#{hex}"
    end
}

# 1.3 generate extended token string address table
puts "\next_token_string_address_table:"
(0..0xFF).each {|i|
    hex = sprintf "%02x", i
    if $ext_token_string_map.has_key?(i)
        puts "       dw ext_token_string_#{hex}"
    else
        puts "       dw $0000                ; $#{hex}"
    end
}

# 2 generate string => token parse tree

class ParseTreeElement < Hash
    def initialize(parent, char, token)
        @parent = parent
        @char = char
        @token = token
    end

    def dump(indent)
        (0..indent).each {|x|
            print "    "
        }
        puts "|#{@char} #{$token_string_map[@token]}|"
        each_value {|child|
            child.dump(indent+1)
        }
    end
end

parseTreeRoot = {}
extParseTreeRoot = {}

def parse(node, stringTail, token)
    len = stringTail.length
    stringTail = stringTail[1..len]
    return if stringTail == ""
    char = stringTail[0..0]
    if not node.has_key?(char)
        node[char] = ParseTreeElement.new(node, char, token)
    end
    node2 = node[char]
    parse(node2, stringTail, token)
end

$string_token_map.each{|string, token|
    char = string[0..0]
    if not parseTreeRoot.has_key?(char)
        parseTreeRoot[char] = ParseTreeElement.new(nil, char, token)
    end
    parse(parseTreeRoot[char], string, token)
}

parseTreeRoot.each_value{|val|
    val.dump(0)
}

# vim:ts=4:expandtab:
