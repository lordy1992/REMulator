## NES Emulator in Rust 
This project is (Yet Another) NES Emulator. It is written in Rust, which was chosen because it is low-level enough, safe, with a strong type system - and because I've been meaning to learn Rust!

### Testing

The CPU has been tested using [nestest](https://www.qmtpro.com/~nes/misc/nestest.txt). I manually set the PC to start at address $C000, ran it, and compared the output to the expected output of nestest. The CPU passes this test, giving us confidence in the CPU (which also supports unofficial opcodes).

Future test ROMs will be used to test other components, such as the PPU.