# Mega CD Error Handler
Experimental modification of version 2.5 of [Vladikcomper's error handler](https://github.com/vladikcomper/md-modules) that adds support for handling exceptions on the Mega CD Sub CPU. Primarily designed for Mode 1, though theoretically it should work in Mode 2 (disc boot) as well with a few changes. 

This handler was developed with the sub CPU BIOS in mind, and as such it assumes that the sub CPU's stack has not been moved, and that it is in the first program RAM bank. Symbol tables should be usable on the main CPU, but there is no support for them on the sub CPU. Some labels in the code have been changed to Hivebrain style-ones (mainly to make it easier for myself to work with the code); equates for all are provided in the code files. Only targets ASM68K for the moment.

Tested and confirmed working on real hardware (North American Model 1 VA2 + North American Sony/Funai Model 2) and Blastem. Also works in Genesis Plus GX, though sub CPU address error handling is broken in the macOS LibRetro version.
Test ROMs and their source can be found [here](https://github.com/OrionNavattan/MegaCDErrorHandlerTest).

See the Sonic Retro thread for more information: https://forums.sonicretro.org/index.php?threads/experimental-mega-cd-error-handler-plus-other-mode-1-tools.41922/

## Installation instructions (steps 4 and beyond will require modifying parts of your program):

1. Include "Debugger Macros and Common Defs.asm" in both the Main and Sub CPU programs.
2. Insert "MainCPU: equ 1" before the Debugger include in the Main CPU program (used to enable some console program features only on the Main CPU).
3. Include "Mega CD Exception Handler (Main CPU).asm" at the very end of the Main CPU program.
4. Include "Mega CD Exception Handler (Sub CPU).asm" within the first $1A000 bytes of your sub CPU program (that is, in the first program RAM bank).
5. Modify your sub CPU program's init routine to set up the exception vectors. SP Init Example.asm contains an example of how this can be done.
6. Set one of the user traps (0 by default) in the vector table to point to SubCPUError, and another to point to SubCPUTimeout (1 by default).
7. Ensure your interprocessor communication has some means for the sub CPU to inform the main CPU that it has crashed. The system used in the code here is a sample, and can be customized if necessary.
