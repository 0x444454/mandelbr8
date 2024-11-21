# mandelbr8
A fast Mandelbrot generator for 8 bit computers.
Currently supported:
- Commodore 64 (with or without Kawari acceleration).
- Commodore 128 (VIC-IIe and VDC modes).
- Commodore TED machines (Plus/4 and C16 with 64 KB).
- [more 8 bit machines in the future]

![screenshots](media/mandel8-20241120.jpg)

# CONTROLS

The app is simply controlled using a joystick:
- C64/C128 use port 2.
- TED machines use port 1.

Actions:
- Up, Down, Left, Right: Move around in complex plane.
- Button + Up: Zoom in.
- Button + Down: Zoom out.
- Button + Right: Increment max iterations (upper limit of 255).
- Button + Left: Decrement max iterations (lower limit of 1).

Note: Zooming in over the precision limit can cause a solid color screen to be rendered. In that case, zoom out until you see the image rendered correctly again.

# LICENSE

Creative Commons, CC BY

https://creativecommons.org/licenses/by/4.0/deed.en

Please add a link to this github project.
