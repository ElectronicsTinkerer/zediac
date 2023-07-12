# Z/SM: The Software/Monitor for the ZEDIA Computer

(C) Ray Clemens 2023

The system monitor provides a number of basic memory management utilities like copying data or performing a memset. It also includes a hex monitor which is a derivative of the Apple 1's famous Wozmon. Here's the command list (accessible by typing `help`):
```
Available commands:
 > args [arg1] [...]        Print stack info for passed arguments
 > clear                    Clear the terminal
 > copy ssssss dddddd cccc  Copy c bytes from s to d
 > ecopy ssssss dddddd cccc Copy c bytes from s to d (EEPROM)
 > go xxxxxx                JML to an address
 > gosub xxxxxx             JSL to an address (RTL to MONITOR)
 > help                     Display available commands
 > memmap                   Display the system's memory map
 > memset aaaaaa bb cccc    Fill c bytes at address a with value b
 > xrecv                    XMODEM receive to address $020000
 > xsend aaaaaa cccc        XMODEM send c bytes from address a
 > a.b                      Hexdump from a to b
 > a:b [c] [...]            Store b at address a
```

If your terminal emulator supports ANSI color escape codes, then you'll get a few slashes of color here and there as well.

## NOTES

### EEPROM programming

* The MiniPro v6.70 programmer incorrectly handles the AT28HC256F chip. Use the AT28HC256 chip instead (in software).
* Make sure that "protect after programming" is not checked otherwise the `ecopy` command will not function and the syste will hang.

### Version Numbering

Example: `vA.B.C`

* `A` - Complete rewrite number (>50% of code has been rewritten)
* `B` - Physical ROM removal and program
* `C` - ROM patches applied on system

