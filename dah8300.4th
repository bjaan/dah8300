\ H8/330 series processor disassembler  Version 1.0.0
\ Copyright (C) 1999  Risto A. Karola
\
\ This program is free software; you can redistribute it and/or
\ modify it under the terms of the GNU General Public License
\ as published by the Free Software Foundation; either version 2
\ of the License, or (at your option) any later version.
\
\ This program is distributed in the hope that it will be useful,
\ but WITHOUT ANY WARRANTY; without even the implied warranty of
\ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
\ GNU General Public License for more details.
\
\ You should have received a copy of the GNU General Public License
\ along with this program; if not, write to the Free Software
\ Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
\
\ Risto A. Karola
\ e-mail: rak (at) pcuf.fi
\ s-mail: Teerimaentie 9A
\         FIN-04200 Kerava

.OPTIONS /OPT /TAILOPTON

HANDLE CodeFile
\ HANDLE AssemblerFile
VARIABLE InputByte
VARIABLE Address
CREATE CodeDump 5 ALLOT
VARIABLE ASCII?

: ReadAbyte  ( -- c )
    \ Read a byte to the "InputByte", if EOF then abort
    InputByte 1 CodeFile HREAD
    0= IF ." END" CR
          CodeFile HCLOSE
          ABORT
    THEN
    InputByte C@
    Address INCR  ;

: FlipByte  ( b1 -- b2 )
  \ Flips the high nibble and low nibble
  $FF AND
  DUP
  $0F AND 2* 2* 2* 2*
  SWAP
  2/ 2/ 2/ 2/
  OR  ;

: SplitByte  ( b -- b1 b2 )
  \ Splits the high nibble and low nibble
  \ b is the original byte
  \ b1 is the low nibble
  \ b2 is the high nibble
  $FF AND
  DUP
  $0F AND
  SWAP
  2/ 2/ 2/ 2/  ;

: .HEXbyte  ( b -- )
  \ Prints two hex characters, high and low nibbles of the byte
  SplitByte
  HEX 1 .R 1 .R DECIMAL  ;

: $AddByte  ( b -- b )
  \ Adds a byte to the string "CodeDump"
  DUP                                   \ Duplicate the byte
  CodeDump C@                           \ String length
  1+ DUP CodeDump C!                    \ Increase the string length
  CodeDump + C!  ;                      \ Store the byte

: ReadBytes  ( n -- c1 c2 .. cn )
  \ n is 1 or more, how many bytes is to be read
  0 DO
    ReadAbyte
    $AddByte                            \ Add to the string
  LOOP  ;

: .DC.W  ( -- )
  ." DC.W" TAB ."  H'"
  CodeDump 1+ COUNT
  .HEXbyte C@ .HEXbyte                  \ Print two bytes
  CodeDump C@ 4 <> ?EXIT                \ Is the dump string length 4?
  TAB TAB TAB
  ';' EMIT
  Address @ 2- 2- SPLIT
  .HEXbyte .HEXbyte                     \ Print the address
  2 SPACES
  CodeDump 1+ COUNT
  .HEXbyte C@
  .HEXbyte                              \ Print two bytes
  TAB                                   \ Print the ASCII string
  CodeDump 1+ COUNT
  DUP 32 126 BETWEEN
  NOT IF DROP '.' THEN EMIT
  C@
  DUP 32 126 BETWEEN
  NOT IF DROP '.' THEN EMIT
  CodeDump 3 + @                        \ Move 3rd & 4th bytes to as 1st & 2nd
  CodeDump 1+ !
  2 CodeDump C!                         \ The string length is 2
  CR
  BL EMIT
  ." DC.W" TAB ."  H'"
  CodeDump 1+ COUNT
  .HEXbyte C@ .HEXbyte  ;               \ Print two bytes

: .Rx?
  $0F AND
  CASE
    $00 OF ." R0H" ENDOF
    $01 OF ." R1H" ENDOF
    $02 OF ." R2H" ENDOF
    $03 OF ." R3H" ENDOF
    $04 OF ." R4H" ENDOF
    $05 OF ." R5H" ENDOF
    $06 OF ." R6H" ENDOF
    $07 OF ." R7H" ENDOF
    $08 OF ." R0L" ENDOF
    $09 OF ." R1L" ENDOF
    $0A OF ." R2L" ENDOF
    $0B OF ." R3L" ENDOF
    $0C OF ." R4L" ENDOF
    $0D OF ." R5L" ENDOF
    $0E OF ." R6L" ENDOF
    $0F OF ." R7L" ENDOF
  ENDCASE  ;

: .Rx
  $07 AND
  CASE
    $00 OF ." R0" ENDOF
    $01 OF ." R1" ENDOF
    $02 OF ." R2" ENDOF
    $03 OF ." R3" ENDOF
    $04 OF ." R4" ENDOF
    $05 OF ." R5" ENDOF
    $06 OF ." R6" ENDOF
    $07 OF ." R7" ENDOF
  ENDCASE  ;

: .OpCode_.Rs?,Rd?  ( a n -- )
  1 ReadBytes
  -ROT
  TYPE TAB BL EMIT                      \ Type the mnemonic
  SplitByte                             \ Split the Rs and Rd
  .Rx?                                  \ Print Rs
  ." , "
  .Rx?  ;                               \ Print Rd

: .OpCode_.Rs,Rd  ( a n -- )
  1 ReadBytes
  DUP
  $88 AND                               \ The bits 3 and 7 must be 0
  IF 3DROP .DC.W EXIT THEN
  -ROT
  TYPE TAB BL EMIT
  SplitByte
  .Rx
  ." , "
  .Rx  ;

: Bxxx_#?,@Rd  ( b a n -- )
  \ a and n are the address and count of the string Bxxx
  \ b is the register Rd and bit ?
  \ Prints "Bxxx #?, @Rd"
  TYPE TAB
  ."  #"
  SplitByte
  1 .R                                  \ Print the 'bit'
  ." , @"
  .Rx  ;

: (BitManipulationEnd)  ( a n b -- )
  \ a and n are the address and count of the string Bxxx
  \ b is the register Rd and bit ?
  \ Prints "Bxxx #?, Rd"
  -ROT
  TYPE TAB
  ."  #"
  SplitByte
  1 .R                                  \ Print the 'bit'
  ." , "
  .Rx?  ;

: .Bit_#:3,Rd_manipulation  ( a n -- )
  \ a and n are the address and count of the string
  1 ReadBytes
  DUP $80 AND
  IF 3DROP .DC.W TAB EXIT THEN
  (BitManipulationEnd)  ;

: .Bit(I)_#:3,Rd_manipulation  ( a0 n0 a1 n1 -- )
  \ a0 and n0 are the address and count of the string with bit 7 = 0
  \ a1 and n1 are the address and count of the string with bit 7 = 1
  1 ReadBytes
  DUP>R
  $80 AND IF 2SWAP THEN 2DROP           \ Drop the unneeded a and n
  R>
  $7F AND                               \ Clear the bit 8
  (BitManipulationEnd)  ;

: .Rs?,@Rd  ( b -- )
  \ b has bit 7=0, bits 4..6 tell Rd, bits 0..3 tell Rs
    SplitByte
    SWAP
    .Rx?
    ." , @"
    .Rx  ;

: .@(disp:16,R)  ( b1 b2 b3 -- )
  \ b1 is the register R
  \ b2 is the LSB of displacement
  \ b3 is the MSB of displacement
    ." @(H'" .HEXbyte .HEXbyte
    ." , " .Rx ')' EMIT  ;

: .Fixed2ndByte  ( a n n1 -- )
  \ a and n are the address and count of the string
  \ n1 is the fixed second byte
  1 ReadBytes
  <> IF 2DROP .DC.W EXIT THEN           \ The second byte must be n1
  TYPE TAB  ;

: .Branches  ( a n -- )
  \ a and n are the address and count of the string
  1 ReadBytes
  DUP $7F >                             \ Is the relative address negative?
  IF $100 - THEN
  Address @ +                           \ Add displacement to the address
  DUP $0001 AND                         \ The address must not be odd
  IF   3DROP .DC.W
  ELSE -ROT
       TYPE TAB ."  H'"
       SPLIT .HEXbyte .HEXbyte
  THEN  ;

: .Rotations  ( a0 n0 a1 n1 -- )
  \ a0 and n0 are the address and count of the string with bit 7 = 0
  \ a1 and n1 are the address and count of the string with bit 7 = 1
  1 ReadBytes
  DUP>R
  $80 AND IF 2SWAP THEN 2DROP           \ Drop the unneeded a and n
  R>
  $7F AND                               \ Clear bits 8
  DUP $70 AND                           \ Bits 4 .. 6 must be 0
  IF 3DROP .DC.W EXIT THEN
  -ROT
  TYPE BL EMIT .Rx?  ;

: ShowHelp
  CR
  ." ÚÄÄ DisAssembler for H8/300: DAH8300" CR
  ." ³   24 Feb 1995   by Risto A. Karola" CR
    '³' EMIT CR
  ." ³   USAGE:" CR
  ." ³          Disassemble on the screen:  DAH8300 file" CR
  ." ³   Disassemble on the file 'DA.LST':  DAH8300 file > DA.LST" CR
    '³' EMIT CR
  ." ³   ESC interrupts the disassembling" CR  ;

: ShowNoInput
    '³' EMIT CR
  ." ³   ERROR: No input file given"
  CR  ;

: ShowNoOpen
    '³' EMIT CR
  ." ³   ERROR: Could not open the input file"
  CR  ;

: .#:3,@FFaa:8  ( b2 b1 a n -- )
  \ b2 is the address aa
  \ b1 is the bit ?
  \ a and n are the address and count of the string
  TYPE TAB ."  #" 1 .R ." , @H'FF" .HEXbyte ." :8"  ;

: .Rn?,@FFaa:8  ( b2 b1 a n -- )
  \ b2 is the address aa
  \ b1 is the register Rn?
  \ a and n are the address and count of the string
  TYPE TAB BL EMIT .Rx? ." , @H'FF" .HEXbyte ." :8"  ;

: .@H'FFaa,Rd?  ( b2 b1 -- )
  \ b2 is the register Rd?
  \ b1 is the address aa
  TYPE TAB ."  @H'FF" .HEXbyte ." :8, " .Rx?  ;

: .#H'XX,Rd?  ( a n -- )
  \ a and n are the address and count of the string
  InputByte C@                          \ Read the register
  $0F AND
  1 ReadBytes                           \ The immediate data
  2SWAP
  TYPE TAB ."  #H'" .HEXbyte ." , " .Rx?  ;

: .#H'XX,CCR  ( a n -- )
  \ a and n are the address and count of the string
  1 ReadBytes
  -ROT
  TYPE TAB ."  #H'" .HEXbyte ." , CCR"  ;

: .#H'XXXX  ( b2 b1 -- )
  TAB ."  #H'" .HEXbyte .HEXbyte ." , "  ;

: .@H'XXXX  ( b2 b1 -- )
  TAB ."  @H'" .HEXbyte .HEXbyte  ;

\ OP CODE HANDLING: ---------------------------------------------------------

: NOP  ( 00: )
  " NOP" $00 .Fixed2ndByte TAB TAB TAB  ;

: SLEEP  ( 01: )
  " SLEEP" $80 .Fixed2ndByte TAB TAB TAB  ;

: STC  ( 02: )
  1 ReadBytes
  DUP $F0 AND                                   \ The bits 4..7 must be 0
  IF   DROP .DC.W TAB
  ELSE ." STC" TAB ."  CCR, " .Rx?
  THEN
  TAB TAB  ;

: LDC_Rs,CCR  ( 03: )
  1 ReadBytes
  DUP $F0 AND                                   \ The bits 4..7 must be 0
  IF   DROP .DC.W TAB
  ELSE ." LDC" TAB BL EMIT .Rx? ." , CCR"
  THEN
  TAB TAB  ;

: ORC  ( 04: )
  " ORC" .#H'XX,CCR TAB TAB  ;

: XORC  ( 05: )
  " XORC" .#H'XX,CCR TAB TAB  ;

: ANDC  ( 06: )
  " ANDC" .#H'XX,CCR TAB TAB  ;

: LDC_#:8,CCR  ( 07: )
  " LDC" .#H'XX,CCR TAB TAB  ;

: ADD.B_Rs,Rd  ( 08: )
  " ADD.B" .OpCode_.Rs?,Rd? TAB TAB  ;

: ADD.W  ( 09: )
  " ADD.W" .OpCode_.Rs,Rd TAB TAB TAB  ;

: INC.B  ( 0A: )
  1 ReadBytes
  SplitByte
  IF   DROP .DC.W
  ELSE ." INC.B" TAB BL EMIT .Rx?
  THEN
  TAB TAB TAB  ;

: ADDS.W  ( 0B: )
  1 ReadBytes
  DUP
  $78 AND                               \ Bits 3 .. 6 must be 0
  IF   DROP .DC.W
  ELSE SplitByte
       ." ADDS.W" TAB ."  #"
       0= IF '1'                        \ If high nibble is 0
       ELSE  '2'                        \ If high nibble is 8
       THEN
       EMIT ." , " .Rx
  THEN
  TAB TAB TAB  ;

: MOV.B_Rs,Rd  ( 0C: )
  " MOV.B" .OpCode_.Rs?,Rd? TAB TAB  ;

: MOV.W_Rs,Rd  ( 0D: )
  " MOV.W" .OpCode_.Rs,Rd TAB TAB TAB  ;

: ADDX.B(reg)  ( 0E: )
  " ADDX.B" .OpCode_.Rs?,Rd? TAB TAB  ;

: DAA  ( 0F: )
  1 ReadBytes
  SplitByte
  IF   DROP .DC.W
  ELSE ." DAA" TAB BL EMIT .Rx?
  THEN
  TAB TAB TAB  ;

: {SHAL.B_Rd}|{SHLL.B_Rd}  ( 10: )
  " SHLL.B "                            \ If high nibble is 0
  " SHAL.B "                            \ If high nibble is 8
  .Rotations TAB TAB TAB  ;

: {SHAR.B_Rd}|{SHLR.B_Rd}  ( 11: )
  " SHLR.B "                            \ If high nibble is 0
  " SHAR.B "                            \ If high nibble is 8
  .Rotations TAB TAB TAB  ;

: {ROTL.B_Rd}|{ROTXL.B_Rd}  ( 12: )
  " ROTXL.B"                            \ If high nibble is 0
  " ROTL.B "                            \ If high nibble is 8
  .Rotations TAB TAB TAB  ;

: {ROTR.B_Rd}|{ROTXR.B_Rd}  ( 13: )
  " ROTXR.B"                            \ If high nibble is 0
  " ROTR.B "                            \ If high nibble is 8
  .Rotations TAB TAB TAB  ;

: OR.B_Rs,Rd  ( 14: )
  " OR.B" .OpCode_.Rs?,Rd? TAB TAB  ;

: XOR.B_Rs,Rd  ( 15: )
  " XOR.B" .OpCode_.Rs?,Rd? TAB TAB  ;

: AND.B_Rs,Rd  ( 16: )
  " AND.B" .OpCode_.Rs?,Rd? TAB TAB  ;

: {NEG.B}|{NOT.B}  ( 17: )
  " NOT.B  "                            \ If high nibble is 0
  " NEG.B  "                            \ If high nibble is 8
  .Rotations TAB TAB TAB  ;

: SUB.B  ( 18: )
  " SUB.B" .OpCode_.Rs?,Rd? TAB TAB  ;

: SUB.W  ( 19: )
  " SUB.W" .OpCode_.Rs,Rd TAB TAB TAB  ;

: DEC.B  ( 1A: )
  1 ReadBytes
  SplitByte
  IF   DROP .DC.W
  ELSE ." DEC.B" TAB BL EMIT .Rx?
  THEN
  TAB TAB TAB  ;

: SUBS.W  ( 1B: )
  1 ReadBytes
  DUP
  $78 AND                               \ Bits 3 .. 6 must be 0
  IF   DROP .DC.W
  ELSE SplitByte
       ." SUBS.W" TAB ."  #"
       0= IF '1'                        \ If high nibble is 0
       ELSE  '2'                        \ If high nibble is 8
       THEN
       EMIT ." , " .Rx
  THEN
  TAB TAB TAB  ;

: CMP.B_Rs,Rd  ( 1C: )
  " CMP.B" .OpCode_.Rs?,Rd? TAB TAB  ;

: CMP.W  ( 1D: )
  " CMP.W" .OpCode_.Rs,Rd TAB TAB TAB  ;

: SUBX.B_Rs,Rd  ( 1E: )
  " SUBX.B" .OpCode_.Rs?,Rd? TAB TAB  ;

: DAS  ( 1F: )
  1 ReadBytes
  SplitByte
  IF   DROP .DC.W
  ELSE ." DAS" TAB BL EMIT .Rx?
  THEN
  TAB TAB TAB  ;

: MOV.B_@aa:8,Rd  ( 20-2F: )
  InputByte C@
  $0F AND
  1 ReadBytes
  " MOV.B" .@H'FFaa,Rd? TAB TAB  ;

: MOV.B_Rs,@aa:8  ( 30-3F: )
  InputByte C@
  $0F AND
  1 ReadBytes
  SWAP
  " MOV.B" .Rn?,@FFaa:8 TAB TAB  ;

: BRA  ( 40: )
  " BRA" .Branches TAB TAB TAB  ;

: BRN  ( 41: )
  " BRN" .Branches TAB TAB TAB  ;

: BHI  ( 42: )
  " BHI" .Branches TAB TAB TAB  ;

: BLS  ( 43: )
  " BLS" .Branches TAB TAB TAB  ;

: BCC  ( 44: )
  " BCC" .Branches TAB TAB TAB  ;

: BCS  ( 45: )
  " BCS" .Branches TAB TAB TAB  ;

: BNE  ( 46: )
  " BNE" .Branches TAB TAB TAB  ;

: BEQ  ( 47: )
  " BEQ" .Branches TAB TAB TAB  ;

: BVC  ( 48: )
  " BVC" .Branches TAB TAB TAB  ;

: BVS  ( 49: )
  " BVS" .Branches TAB TAB TAB  ;

: BPL  ( 4A: )
  " BPL" .Branches TAB TAB TAB  ;

: BMI  ( 4B: )
  " BMI" .Branches TAB TAB TAB  ;

: BGE  ( 4C: )
  " BGE" .Branches TAB TAB TAB  ;

: BLT  ( 4D: )
  " BLT" .Branches TAB TAB TAB  ;

: BGT  ( 4E: )
  " BGT" .Branches TAB TAB TAB  ;

: BLE  ( 4F: )
  " BLE" .Branches TAB TAB TAB  ;

: MULXU  ( 50: )
  1 ReadBytes
  DUP $08 AND                           \ The bit 3 must be 0
  IF   DROP .DC.W TAB
  ELSE SplitByte
       ." MULXU" TAB BL EMIT
       .Rx? ." , " .Rx
  THEN
  TAB TAB  ;

: DIVXU  ( 51: )
  1 ReadBytes
  DUP $08 AND                           \ The bit 3 must be 0
  IF   DROP .DC.W TAB
  ELSE SplitByte
       ." DIVXU" TAB BL EMIT
       .Rx? ." , " .Rx
  THEN
  TAB TAB  ;

: RTS  ( 54: )
  " RTS" $70 .Fixed2ndByte TAB TAB TAB  ;

: BSR  ( 55: )
  " BSR" .Branches TAB TAB TAB  ;

: RTE  ( 56: )
  " RTE" $70 .Fixed2ndByte TAB TAB TAB  ;

: JMP_@Rn  ( 59: )
  1 ReadBytes
  DUP $8F AND                           \ The bits 0..3 and 7 must be 0
  IF   DROP .DC.W
  ELSE SplitByte NIP
       ." JMP" TAB ."  @" .Rx
  THEN
  TAB TAB TAB  ;

: JMP_@aaaa:16  ( 5A: )
  3 ReadBytes
  SWAP ROT
  IF   2DROP .DC.W TAB
  ELSE ." JMP" .@H'XXXX
  THEN
  TAB TAB  ;

: JMP_@@aaaa:8  ( 5B: )
  1 ReadBytes
  ." JMP" TAB ."  @@H'"
  .HEXbyte TAB TAB TAB  ;

: JSR_@Rn  ( 5D: )
  1 ReadBytes
  DUP $8F AND                           \ The bits 0..3 and 7 must be 0
  IF   DROP .DC.W
  ELSE SplitByte NIP
       ." JSR" TAB ."  @" .Rx
  THEN
  TAB TAB TAB  ;

: JSR_@aaaa:16  ( 5E: )
  3 ReadBytes
  SWAP ROT
  IF   2DROP .DC.W TAB
  ELSE ." JSR" .@H'XXXX
  THEN
  TAB TAB  ;

: JSR_@@aaaa:8  ( 5F: )
  1 ReadBytes
  ." JSR" TAB ."  @@H'"
  .HEXbyte TAB TAB TAB  ;

: BSET_Rn,Rd  ( 60: )
  " BSET" .OpCode_.Rs?,Rd? TAB TAB  ;

: BNOT_Rn,Rd  ( 61: )
  " BNOT" .OpCode_.Rs?,Rd? TAB TAB  ;

: BCLR_Rn,Rd  ( 62: )
  " BCLR" .OpCode_.Rs?,Rd? TAB TAB  ;

: BTST_Rn,Rd  ( 63: )
  " BTST" .OpCode_.Rs?,Rd? TAB TAB  ;

: B(I)ST_#:3,Rd  ( 67: )
  " BST"
  " BIST"
  .Bit(I)_#:3,Rd_manipulation TAB TAB  ;

: MOV.B_{@Rs,Rd}|{Rs,@Rd}  ( 68: )
  1 ReadBytes
  DUP $80 AND
  ." MOV.B" TAB BL EMIT
  IF                                    \ MOV.B Rs, @Rd
    $7F AND
    .Rs?,@Rd
  ELSE                                  \ MOV.B @Rs, Rd
    SplitByte
    '@' EMIT .Rx
    ." , "
    .Rx?
  THEN TAB TAB  ;

: MOV.W_{@Rs,Rd}|{Rs,@Rd}  ( 69: )
  1 ReadBytes
  DUP $08 AND
  IF   2DROP .DC.W TAB
  ELSE ." MOV.W" TAB BL EMIT
       DUP $80 AND
       IF                               \ MOV.W Rs, @Rd
         $7F AND
         SplitByte
         SWAP
         .Rx ." , @"
       ELSE                                     \ MOV.W @Rs, Rd
         SplitByte
          '@' EMIT .Rx ." , "
       THEN
       .Rx
  THEN
  TAB TAB  ;

: {MOV.B}|{MOVFPE}|{MOVTPE}  ( 6A: )
  3 ReadBytes
  SWAP ROT
  SplitByte
  CASE
    $00 OF -ROT ." MOV.B" .@H'XXXX ." , " .Rx? ENDOF
    $04 OF -ROT ." MOVFPE"  .@H'XXXX ." , " .Rx? ENDOF
    $08 OF ." MOV.B" TAB BL EMIT .Rx? ." , @H'" .HEXbyte .HEXbyte ENDOF
    $0C OF ." MOVTPE" TAB BL EMIT .Rx? ." , @H'" .HEXbyte .HEXbyte ENDOF
    2DROP 2DROP .DC.W TAB
  ENDCASE TAB TAB  ;

: MOV.W_{@aa:16,Rd}|{Rs,@aa:16}  ( 6B: )
  3 ReadBytes
  SWAP ROT
  DUP $78 AND
  IF   3DROP .DC.W TAB
  ELSE ." MOV.W" TAB BL EMIT
       DUP $80 AND
       IF                               \ MOV.W Rs, @aaaa:16
         $07 AND
         .Rx ." , @H'" .HEXbyte .HEXbyte
       ELSE                             \ MOV.W @aaaa:16, Rd
         $07 AND
         -ROT ." @H'" .HEXbyte .HEXbyte
         ." , " .Rx
       THEN
  THEN
  TAB TAB  ;

: MOV.B_{Rs+,Rd}|{Rs,-Rd}  ( 6C: )
  1 ReadBytes
  DUP $80 AND
  ." MOV.B" TAB BL EMIT
  IF                                    \ MOV.B Rs, @-Rd
    $7F AND
    SplitByte
    SWAP
    .Rx?
    ." , @-"
    .Rx
  ELSE                                  \ MOV.B @Rs+, Rd
    SplitByte
    '@' EMIT .Rx
    ." +, "
    .Rx?
  THEN TAB TAB  ;

: MOV.W_{Rs+,Rd}|{Rs,-Rd}  ( 6D: )
  1 ReadBytes
  DUP $08 AND
  IF   DROP .DC.W TAB
  ELSE ." MOV.W" TAB BL EMIT
       DUP $80 AND
       IF                               \ MOV.W Rs, @-Rd
         $7F AND
         SplitByte
         SWAP .Rx ." , @-"
       ELSE                             \ MOV.W @Rs+, Rd
         SplitByte
         '@' EMIT .Rx ." +, "
       THEN
       .Rx
  THEN
  TAB TAB  ;

: MOV.B_@(:16,Rs),Rd|Rs,@(:16,Rd)  ( 6E: )
  3 ReadBytes
  SWAP ROT
  DUP $80 AND
  ." MOV.B" TAB BL EMIT
  IF                                    \ MOV.B Rs, @(disp:16, Rd)
    $7F AND
    SplitByte
    SWAP
    .Rx?
    -ROT
    ." , " .@(disp:16,R)
  ELSE                                  \ MOV.B @(disp:16, Rs), Rd
    SplitByte
    2SWAP
    .@(disp:16,R)
    ." , "
    .Rx?
  THEN TAB  ;

: MOV.W_@(:16,Rs),Rd|Rs,@(:16,Rd)  ( 6F: )
  3 ReadBytes
  SWAP ROT
  DUP $08 AND
  IF   3DROP .DC.W TAB TAB
  ELSE ." MOV.W" TAB BL EMIT
       DUP $80 AND
       IF                               \ MOV.W Rs, @(disp:16, Rd)
         SplitByte
         SWAP .Rx -ROT
         ." , " .@(disp:16,R)
       ELSE                             \ MOV.W @(disp:16, Rs), Rd
         SplitByte
         2SWAP .@(disp:16,R)
         ." , " .Rx
       THEN
  THEN
  TAB  ;

: BSET_#:3,Rd  ( 70: )
  " BSET" .Bit_#:3,Rd_manipulation TAB TAB  ;

: BNOT_#:3,Rd  ( 71: )
  " BNOT" .Bit_#:3,Rd_manipulation TAB TAB  ;

: BCLR_#:3,Rd  ( 72: )
  " BCLR" .Bit_#:3,Rd_manipulation TAB TAB  ;

: BTST_#:3,Rd  ( 73: )
  " BTST" .Bit_#:3,Rd_manipulation TAB TAB  ;

: B(I)OR_#:3,Rd  ( 74: )
  " BOR"
  " BIOR"
  .Bit(I)_#:3,Rd_manipulation TAB TAB  ;

: B(I)XOR_#:3,Rd  ( 75: )
  " BXOR"
  " BIXOR"
  .Bit(I)_#:3,Rd_manipulation TAB TAB  ;

: B(I)AND_#:3,Rd  ( 76: )
  " BAND"
  " BIAND"
  .Bit(I)_#:3,Rd_manipulation TAB TAB  ;

: B(I)LDrd  ( 77: )
  " BLD"
  " BILD"
  .Bit(I)_#:3,Rd_manipulation TAB TAB  ;

: MOV.W_#:16,Rd  ( 79: )
  3 ReadBytes
  SWAP ROT
  DUP
  $F8 AND                               \ f = TRUE if one of bits 3 .. 7 <> 0
  IF   3DROP .DC.W TAB
  ELSE ." MOV.W"
       -ROT .#H'XXXX .Rx
  THEN
  TAB TAB  ;

: EEPMOV  ( 7B: )
  3 ReadBytes
  SWAP ROT
  $5C <> -ROT                           \ The second byte must be 5C
  $59 <> -ROT                           \ The third byte must be 59
  $8F <>                                \ The fourth byte must be 8F
  OR OR                                 \ f = TRUE if not correct
  IF   .DC.W
  ELSE ." EEPMOV" TAB
  THEN
  TAB TAB TAB  ;

: 7Cinstr  ( 7C: )
  3 ReadBytes
  PLUCK $8F AND OVER $0F AND OR
  IF 3DROP .DC.W TAB TAB TAB EXIT THEN
  ROT
  SplitByte NIP                         \ Drop the lower nibble away
  OR                                    \ Form from bytes 2 and 4 [Rs,0,Rd]
  SWAP
  CASE                                  \ Check the third byte
    $63 OF FlipByte ." BTST" TAB BL EMIT .Rs?,@Rd ENDOF
    $73 OF DUP $80 AND IF   DROP .DC.W
                       ELSE " BTST" Bxxx_#?,@Rd
                       THEN TAB ENDOF
    $74 OF DUP $80 AND IF $7F AND " BIOR" ELSE " BOR" THEN
           Bxxx_#?,@Rd TAB ENDOF
    $75 OF DUP $80 AND IF $7F AND " BIXOR" ELSE " BXOR" THEN
           Bxxx_#?,@Rd TAB ENDOF
    $76 OF DUP $80 AND IF $7F AND " BIAND" ELSE " BAND" THEN
           Bxxx_#?,@Rd TAB ENDOF
    $77 OF DUP $80 AND IF $7F AND " BILD" ELSE " BLD" THEN
           Bxxx_#?,@Rd TAB ENDOF
           2DROP .DC.W TAB
  ENDCASE TAB TAB  ;

: 7Dinstr  ( 7D: )
  3 ReadBytes
  PLUCK $8F AND OVER $0F AND OR
  IF 3DROP .DC.W TAB TAB TAB EXIT THEN
  ROT
  SplitByte NIP                         \ Drop the lower nibble away
  OR                                    \ Form from bytes 2 and 4 [Rs,0,Rd]
  SWAP
  CASE                                  \ Check the third byte
    $60 OF FlipByte ." BSET" TAB BL EMIT .Rs?,@Rd ENDOF
    $61 OF FlipByte ." BNOT" TAB BL EMIT .Rs?,@Rd ENDOF
    $62 OF FlipByte ." BCLR" TAB BL EMIT .Rs?,@Rd ENDOF
    $67 OF DUP $80 AND IF $7F AND " BIST" ELSE " BST" THEN
           Bxxx_#?,@Rd TAB ENDOF
    $70 OF " BSET" Bxxx_#?,@Rd TAB ENDOF
    $71 OF " BNOT" Bxxx_#?,@Rd TAB ENDOF
    $72 OF " BCLR" Bxxx_#?,@Rd TAB ENDOF
           2DROP .DC.W TAB
  ENDCASE TAB TAB  ;

: 7Einstr  ( 7E: )
  3 ReadBytes DUP $0F AND
  IF 3DROP .DC.W TAB TAB TAB EXIT THEN
  SplitByte NIP                         \ Drop the lower nibble away
  SWAP
  CASE                                  \ Check the third byte
    $63 OF " BTST" .Rn?,@FFaa:8 ENDOF
    $73 OF DUP $08 AND IF   2DROP .DC.W TAB
                       ELSE " BTST" .#:3,@FFaa:8
                       THEN ENDOF
    $74 OF DUP $08 AND IF $07 AND " BIOR" ELSE " BOR" THEN
           .#:3,@FFaa:8 ENDOF
    $75 OF DUP $08 AND IF $07 AND " BIXOR" ELSE " BXOR" THEN
           .#:3,@FFaa:8 ENDOF
    $76 OF DUP $08 AND IF $07 AND " BIAND" ELSE " BAND" THEN
           .#:3,@FFaa:8 ENDOF
    $77 OF DUP $08 AND IF $07 AND " BILD" ELSE " BLD" THEN
           .#:3,@FFaa:8 ENDOF
           3DROP .DC.W TAB
  ENDCASE TAB TAB  ;

: 7Finstr  ( 7F: )
  3 ReadBytes  DUP $0F AND
  IF 3DROP .DC.W TAB TAB TAB EXIT THEN
  SplitByte NIP                         \ Drop the lower nibble away
  SWAP
  CASE                                  \ Check the third byte
    $60 OF " BSET" .Rn?,@FFaa:8 ENDOF
    $61 OF " BNOT" .Rn?,@FFaa:8 ENDOF
    $62 OF " BCLR" .Rn?,@FFaa:8 ENDOF
    $67 OF DUP $08 AND IF $07 AND " BIST" ELSE " BST" THEN
           .#:3,@FFaa:8 ENDOF
    $70 OF DUP $08 AND IF   2DROP .DC.W TAB
                       ELSE " BSET" .#:3,@FFaa:8
                       THEN ENDOF
    $71 OF DUP $08 AND IF   2DROP .DC.W TAB
                       ELSE " BNOT" .#:3,@FFaa:8
                       THEN ENDOF
    $72 OF DUP $08 AND IF   2DROP .DC.W TAB
                       ELSE " BCLR" .#:3,@FFaa:8
                       THEN ENDOF
           3DROP .DC.W TAB
  ENDCASE TAB TAB  ;

: ADD.B_#:8,Rd  ( 80-8F: )
  " ADD.B" .#H'XX,Rd? TAB TAB  ;

: ADDX.B_#:8,Rd  ( 90-9F: )
  " ADDX.B" .#H'XX,Rd? TAB TAB  ;

: CMP.B_#:8,Rd  ( A0-AF: )
  " CMP.B" .#H'XX,Rd? TAB TAB  ;

: SUBX.B_#:8,Rd  ( B0-BF: )
  " SUBX.B" .#H'XX,Rd? TAB TAB  ;

: OR.B_#:8,Rd  ( C0-CF: )
  " OR.B" .#H'XX,Rd? TAB TAB  ;

: XOR.B_#:8,Rd  ( D0-DF: )
  " XOR.B" .#H'XX,Rd? TAB TAB  ;

: AND.B_#:8,Rd  ( E0-EF: )
  " AND.B" .#H'XX,Rd? TAB TAB  ;

: MOV.B_#:8,Rd  ( F0-FF: )
  " MOV.B" .#H'XX,Rd? TAB TAB  ;

: Unknown  ( -- )
  1 ReadBytes
  DROP .DC.W TAB TAB TAB  ;

\ OP CODE JUMP TABLE: -------------------------------------------------------

: DecodeJumps  ( n -- )
  \ n is the first byte of the opcode
  EXEC:  ( n -- )                       \ execute the n-th CALL following
  ( 00: )    NOP
  ( 01: )    SLEEP
  ( 02: )    STC
  ( 03: )    LDC_Rs,CCR
  ( 04: )    ORC
  ( 05: )    XORC
  ( 06: )    ANDC
  ( 07: )    LDC_#:8,CCR
  ( 08: )    ADD.B_Rs,Rd
  ( 09: )    ADD.W
  ( 0A: )    INC.B
  ( 0B: )    ADDS.W
  ( 0C: )    MOV.B_Rs,Rd
  ( 0D: )    MOV.W_Rs,Rd
  ( 0E: )    ADDX.B(reg)
  ( 0F: )    DAA
  ( 10: )    {SHAL.B_Rd}|{SHLL.B_Rd}
  ( 11: )    {SHAR.B_Rd}|{SHLR.B_Rd}
  ( 12: )    {ROTL.B_Rd}|{ROTXL.B_Rd}
  ( 13: )    {ROTR.B_Rd}|{ROTXR.B_Rd}
  ( 14: )    OR.B_Rs,Rd
  ( 15: )    XOR.B_Rs,Rd
  ( 16: )    AND.B_Rs,Rd
  ( 17: )    {NEG.B}|{NOT.B}
  ( 18: )    SUB.B
  ( 19: )    SUB.W
  ( 1A: )    DEC.B
  ( 1B: )    SUBS.W
  ( 1C: )    CMP.B_Rs,Rd
  ( 1D: )    CMP.W
  ( 1E: )    SUBX.B_Rs,Rd
  ( 1F: )    DAS
  ( 20-23: ) MOV.B_@aa:8,Rd  MOV.B_@aa:8,Rd  MOV.B_@aa:8,Rd  MOV.B_@aa:8,Rd
  ( 24-27: ) MOV.B_@aa:8,Rd  MOV.B_@aa:8,Rd  MOV.B_@aa:8,Rd  MOV.B_@aa:8,Rd
  ( 28-2B: ) MOV.B_@aa:8,Rd  MOV.B_@aa:8,Rd  MOV.B_@aa:8,Rd  MOV.B_@aa:8,Rd
  ( 2C-2F: ) MOV.B_@aa:8,Rd  MOV.B_@aa:8,Rd  MOV.B_@aa:8,Rd  MOV.B_@aa:8,Rd
  ( 30-33: ) MOV.B_Rs,@aa:8  MOV.B_Rs,@aa:8  MOV.B_Rs,@aa:8  MOV.B_Rs,@aa:8
  ( 34-37: ) MOV.B_Rs,@aa:8  MOV.B_Rs,@aa:8  MOV.B_Rs,@aa:8  MOV.B_Rs,@aa:8
  ( 38-3B: ) MOV.B_Rs,@aa:8  MOV.B_Rs,@aa:8  MOV.B_Rs,@aa:8  MOV.B_Rs,@aa:8
  ( 3C-3F: ) MOV.B_Rs,@aa:8  MOV.B_Rs,@aa:8  MOV.B_Rs,@aa:8  MOV.B_Rs,@aa:8
  ( 40: )    BRA
  ( 41: )    BRN
  ( 42: )    BHI
  ( 43: )    BLS
  ( 44: )    BCC
  ( 45: )    BCS
  ( 46: )    BNE
  ( 47: )    BEQ
  ( 48: )    BVC
  ( 49: )    BVS
  ( 4A: )    BPL
  ( 4B: )    BMI
  ( 4C: )    BGE
  ( 4D: )    BLT
  ( 4E: )    BGT
  ( 4F: )    BLE
  ( 50: )    MULXU
  ( 51: )    DIVXU
  ( 52: )    Unknown
  ( 53: )    Unknown
  ( 54: )    RTS
  ( 55: )    BSR
  ( 56: )    RTE
  ( 57: )    Unknown
  ( 58: )    Unknown
  ( 59: )    JMP_@Rn
  ( 5A: )    JMP_@aaaa:16
  ( 5B: )    JMP_@@aaaa:8
  ( 5C: )    Unknown
  ( 5D: )    JSR_@Rn
  ( 5E: )    JSR_@aaaa:16
  ( 5F: )    JSR_@@aaaa:8
  ( 60: )    BSET_Rn,Rd
  ( 61: )    BNOT_Rn,Rd
  ( 62: )    BCLR_Rn,Rd
  ( 63: )    BTST_Rn,Rd
  ( 64: )    Unknown
  ( 65: )    Unknown
  ( 66: )    Unknown
  ( 67: )    B(I)ST_#:3,Rd
  ( 68: )    MOV.B_{@Rs,Rd}|{Rs,@Rd}
  ( 69: )    MOV.W_{@Rs,Rd}|{Rs,@Rd}
  ( 6A: )    {MOV.B}|{MOVFPE}|{MOVTPE}
  ( 6B: )    MOV.W_{@aa:16,Rd}|{Rs,@aa:16}
  ( 6C: )    MOV.B_{Rs+,Rd}|{Rs,-Rd}
  ( 6D: )    MOV.W_{Rs+,Rd}|{Rs,-Rd}
  ( 6E: )    MOV.B_@(:16,Rs),Rd|Rs,@(:16,Rd)
  ( 6F: )    MOV.W_@(:16,Rs),Rd|Rs,@(:16,Rd)
  ( 70: )    BSET_#:3,Rd
  ( 71: )    BNOT_#:3,Rd
  ( 72: )    BCLR_#:3,Rd
  ( 73: )    BTST_#:3,Rd
  ( 74: )    B(I)OR_#:3,Rd
  ( 75: )    B(I)XOR_#:3,Rd
  ( 76: )    B(I)AND_#:3,Rd
  ( 77: )    B(I)LDrd
  ( 78: )    Unknown
  ( 79: )    MOV.W_#:16,Rd
  ( 7A: )    Unknown
  ( 7B: )    EEPMOV
  ( 7C: )    7Cinstr
  ( 7D: )    7Dinstr
  ( 7E: )    7Einstr
  ( 7F: )    7Finstr
  ( 80-83: ) ADD.B_#:8,Rd  ADD.B_#:8,Rd  ADD.B_#:8,Rd  ADD.B_#:8,Rd
  ( 84-87: ) ADD.B_#:8,Rd  ADD.B_#:8,Rd  ADD.B_#:8,Rd  ADD.B_#:8,Rd
  ( 88-8B: ) ADD.B_#:8,Rd  ADD.B_#:8,Rd  ADD.B_#:8,Rd  ADD.B_#:8,Rd
  ( 8C-8F: ) ADD.B_#:8,Rd  ADD.B_#:8,Rd  ADD.B_#:8,Rd  ADD.B_#:8,Rd
  ( 90-93: ) ADDX.B_#:8,Rd ADDX.B_#:8,Rd ADDX.B_#:8,Rd ADDX.B_#:8,Rd
  ( 94-97:)  ADDX.B_#:8,Rd ADDX.B_#:8,Rd ADDX.B_#:8,Rd ADDX.B_#:8,Rd
  ( 98-9B: ) ADDX.B_#:8,Rd ADDX.B_#:8,Rd ADDX.B_#:8,Rd ADDX.B_#:8,Rd
  ( 9C-9F: ) ADDX.B_#:8,Rd ADDX.B_#:8,Rd ADDX.B_#:8,Rd ADDX.B_#:8,Rd
  ( A0-A3: ) CMP.B_#:8,Rd  CMP.B_#:8,Rd  CMP.B_#:8,Rd  CMP.B_#:8,Rd
  ( A4-A7: ) CMP.B_#:8,Rd  CMP.B_#:8,Rd  CMP.B_#:8,Rd  CMP.B_#:8,Rd
  ( A8-AB: ) CMP.B_#:8,Rd  CMP.B_#:8,Rd  CMP.B_#:8,Rd  CMP.B_#:8,Rd
  ( AC-AF: ) CMP.B_#:8,Rd  CMP.B_#:8,Rd  CMP.B_#:8,Rd  CMP.B_#:8,Rd
  ( B0-B3: ) SUBX.B_#:8,Rd SUBX.B_#:8,Rd SUBX.B_#:8,Rd SUBX.B_#:8,Rd
  ( B4-B7: ) SUBX.B_#:8,Rd SUBX.B_#:8,Rd SUBX.B_#:8,Rd SUBX.B_#:8,Rd
  ( B8-BB: ) SUBX.B_#:8,Rd SUBX.B_#:8,Rd SUBX.B_#:8,Rd SUBX.B_#:8,Rd
  ( BC-BF: ) SUBX.B_#:8,Rd SUBX.B_#:8,Rd SUBX.B_#:8,Rd SUBX.B_#:8,Rd
  ( C0-C3: ) OR.B_#:8,Rd   OR.B_#:8,Rd   OR.B_#:8,Rd   OR.B_#:8,Rd
  ( C4-C7: ) OR.B_#:8,Rd   OR.B_#:8,Rd   OR.B_#:8,Rd   OR.B_#:8,Rd
  ( C8-CB: ) OR.B_#:8,Rd   OR.B_#:8,Rd   OR.B_#:8,Rd   OR.B_#:8,Rd
  ( CC-CF: ) OR.B_#:8,Rd   OR.B_#:8,Rd   OR.B_#:8,Rd   OR.B_#:8,Rd
  ( D0-D3: ) XOR.B_#:8,Rd  XOR.B_#:8,Rd  XOR.B_#:8,Rd  XOR.B_#:8,Rd
  ( D4-D7: ) XOR.B_#:8,Rd  XOR.B_#:8,Rd  XOR.B_#:8,Rd  XOR.B_#:8,Rd
  ( D8-DC: ) XOR.B_#:8,Rd  XOR.B_#:8,Rd  XOR.B_#:8,Rd  XOR.B_#:8,Rd
  ( DC-DF: ) XOR.B_#:8,Rd  XOR.B_#:8,Rd  XOR.B_#:8,Rd  XOR.B_#:8,Rd
  ( E0-E3: ) AND.B_#:8,Rd  AND.B_#:8,Rd  AND.B_#:8,Rd  AND.B_#:8,Rd
  ( E4-E7: ) AND.B_#:8,Rd  AND.B_#:8,Rd  AND.B_#:8,Rd  AND.B_#:8,Rd
  ( E8-EC: ) AND.B_#:8,Rd  AND.B_#:8,Rd  AND.B_#:8,Rd  AND.B_#:8,Rd
  ( EB-EF: ) AND.B_#:8,Rd  AND.B_#:8,Rd  AND.B_#:8,Rd  AND.B_#:8,Rd
  ( F0-F3: ) MOV.B_#:8,Rd  MOV.B_#:8,Rd  MOV.B_#:8,Rd  MOV.B_#:8,Rd
  ( F4-F7: ) MOV.B_#:8,Rd  MOV.B_#:8,Rd  MOV.B_#:8,Rd  MOV.B_#:8,Rd
  ( F8-FB: ) MOV.B_#:8,Rd  MOV.B_#:8,Rd  MOV.B_#:8,Rd  MOV.B_#:8,Rd
  ( FC-FF: ) MOV.B_#:8,Rd  MOV.B_#:8,Rd  MOV.B_#:8,Rd  MOV.B_#:8,Rd
;

: Initialize
  BL WORD COUNT DUP
  0= IF ShowHelp ShowNoInput ABORT THEN
  CodeFile ">HANDLE
  READ-ONLY DEF-RWMODE
  CodeFile HOPEN
  IF ShowHelp ShowNoOpen ABORT THEN
  0 Address !
  0 CodeDump C!                 \ Clear the string
;

: DAH8300
  Initialize
  CR
  BL EMIT
  ." ORG" TAB ."  H'0000"
  CR                                    \ Start new line
  ReadAbyte                             \ Read the 1st word
  $AddByte                              \ Add to the string
  ReadAbyte
  $AddByte                              \ Add to the string
  2DUP
  SWAP
  256 * + $0100 MIN                     \ Code starts latest at 100
  -ROT
  BEGIN
    BL EMIT
    ." DC.W" TAB ."  H'" SWAP .HexByte .HexByte
    TAB TAB TAB ';' EMIT
    Address @ 2- SPLIT
    .HEXbyte .HEXbyte                   \ Print the address
    2 SPACES
    CodeDump 1+ COUNT                   \ Print the string
    .HEXbyte C@ .HEXbyte
    TAB                                 \ Print the ASCII string
    CodeDump 1+ COUNT
    DUP 32 126 BETWEEN
    NOT IF DROP '.' THEN EMIT
    C@
    DUP 32 126 BETWEEN
    NOT IF DROP '.' THEN EMIT
    0 CodeDump C!                       \ Clear the string
    Address @ OVER <
  WHILE
    CR                                  \ Start new line
    ReadAbyte
    $AddByte                            \ Add to the string
    ReadAbyte
    $AddByte                            \ Add to the string
  REPEAT
  DROP
  BEGIN
    KEY? IF KEY 27 = IF ABORT THEN THEN \ If ESC pressed then ABORT
    CR                                  \ Start new line
    BL EMIT
    ReadAbyte                           \ Read the 1st byte of the opcode
    $AddByte                            \ Add to the string
    DecodeJumps
    ';' EMIT
    Address @ CodeDump C@ - SPLIT       \ Address = variable - code length
    .HEXbyte .HEXbyte                   \ Print the address
    2 SPACES
    CodeDump COUNT                      \ Print the string
    0 DO
      COUNT .HEXbyte
    LOOP DROP
    TAB                                 \ Print the ASCII string
    CodeDump COUNT
    0 DO
      COUNT DUP 32 126 BETWEEN
      NOT IF DROP '.' THEN EMIT
    LOOP DROP
    0 CodeDump C!                       \ Clear the string
  AGAIN
;
