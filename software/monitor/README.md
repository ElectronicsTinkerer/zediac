# Z/SM: The Software/Monitor for the ZEDIA Computer

(C) Ray Clemens 2023

# NOTES

### EEPROM programming

* The MiniPro v6.70 programmer incorrectly handles the AT28HC256F chip. Use the AT28HC256 chip instead (in software).
* Make sure that "protect after programming" is not checked otherwise the `ecopy` command will not function and the syste will hang.


