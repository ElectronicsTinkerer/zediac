# ZEDIA Computer System

("Zee" Electronic Desktop Instrumentation and Analysis Computer)

![Top-down photo of the PCB. From left to right: GPIO connector, VIA, CPU, RAM, ROM, DUART. Below the CPU RAM and ROM are several 74xx series ICs for address decoding. The right edge of the PCB features the power and USB connectors. Note the incorrect date on the PCB-when I changed the date over from December, 2022 to January 2023, I managed to change the month and date, but not the year. Oops](./pcb1.0.jpg)

## System stats

* W65C816 CPU
* 32k EEPROM (with write capability)
* 512K RAM
* 65C22 VIA, providing ~20 GPIOs (really 16 + 4 handshake lines)
* ST16C2752 Dual UART
* 5+ MHz operation
* Partial system bus expansion header with address decoding
* USB-C UART interface via a CP2102
* 2 power delivery methods (unregulated voltage in via barrel jack or terminal block. Also power over USB)
* ESD protection on all IO headers

## Software

All software is available in the `software` directory. At the moment, there is a system monitor which provides many memory manipulation utilities.

