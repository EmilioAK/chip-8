# CHIP-8 Emulator!
To run this program simply run the file `src/chip8/game/Chip8Game`.

Inside this file you are also able to load your game, and select a color scheme from a wide variety! 
You can find all available schemes in the method `getColorScheme`

The emulator has been tested using [Timendus' Test suite](https://github.com/Timendus/chip8-test-suite) and passes all of these tests.

If you want compatibility with older CHIP-8 titles you can set `delayDisplay` to true. 
Beware that most modern games expect this option to be turned off as it makes them run slower.

![Spacejam running](resources/Screenshot%202023-10-29%20at%2015.36.35.jpg)
![Breakout running](resources/Screenshot%202023-10-29%20at%2015.30.26.png)