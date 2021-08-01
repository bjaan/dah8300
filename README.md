# dah8300
Hitachi H8/300 Disassembler

**Originally published by Risto A. Karola in 1999**

# Original publication

Available here: http://www.karola.fi/rak/sw/dah8300/

## About
Hitachi H8/300 code disassembler dah8300 runs in DOS. It reads binary image of H8/300 code and types it in assembly language to the screen. The output can be redirected into a file. 

dah8300 was written in Forth programming language and compiled with [TCOM](http://www.karola.fi/rak/attic/about_tcom/), a public domain Forth compiler from Tom Zimmer. It is very handy with small programs as dah8300.

## Usage
Just type `dah8300` and you get help.

![help](https://raw.githubusercontent.com/bjaan/dah8300/main/sw_dah8300_help.png)

disassembling file to the screen: `dah8300 file`

disassembling file to the screen with pauses: `dah8300 file | more`

disassembling file1 to file2: `dah8300 file1 >file2`

`Esc` interrupts the disassembling and causes return to DOS.

## Output
The output is in five columns:

* H8/300 assembly instruction
* parameter(s) for the instruction
* program counter value (hexadecimal)
* machine code of the instruction (hexadecimal)
ASCII representation of the machine code (ASCII codes outside 32...126 range are displayed as dots)

![output](https://raw.githubusercontent.com/bjaan/dah8300/main/sw_dah8300_screenshot.png)

## Disclaimer
`dah8300` version 1.0.0, Copyright © 1999 Risto A. Karola.

`dah8300` comes with ABSOLUTELY NO WARRANTY; click here for details. This is free software, and you are welcome to redistribute it under certain conditions; See file copying.txt.

`dah8300` is released under the terms of the GNU General Public License as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later version. See file copying.txt. 

# Contents
* `dah8300.zip` the zipped package ver 1.0.0 
* `dah8300.4th` source in Forth ver 1.0.0 
* `h8300.pdf` Hitachi H8/300 Programming Manual