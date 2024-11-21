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
- C64/C128: Joystick port 2.
- TED machines: Joystick port 1.

Actions:
- Up, Down, Left, Right: Move around in complex plane.
- Button + Up: Zoom in.
- Button + Down: Zoom out.
- Button + Right: Increment max iterations (upper limit of 255).
- Button + Left: Decrement max iterations (lower limit of 1).

Note: Zooming in over the precision limit can cause a solid color screen to be rendered. In that case, zoom out until you see the image rendered correctly again.

# SUPPORTED RESOLUTIONS
##C64, C128 40 column mode
- First pass: 40x25, 16 colors
- Second pass: 160x200, 16 colors (multicolor bitmap)

##C128 80 column mode:
- First pass: 80x50, 16 colors
- Second pass: [not yet supported]

# ALGORITHM

[TBD]
The rendering is done in two passes.

The first pass is low-resolution and serves two purposes:
- Quick preview of rendered image.
- Buffer iterations for second pass optimization.

The second pass is high resolution. Each low-resolution pixel in the first pass is either skipped (based on boundaries check) or treated as a tile composed of high-resolution pixels.  
The number of tile pixels varies depending on the high-res video mode. In Commodore multicolor bitmap mode, each tile is composed by 4x8 double-width pixels.


# LICENSE

Creative Commons, CC BY

https://creativecommons.org/licenses/by/4.0/deed.en

Please add a link to this github project.
