# Graphv

## Overview

Port of the [NanoVG](https://github.com/memononen/nanovg) C library to (mostly) pure OCaml. This library is still a work in progress, the documentation is not complete and there could be more examples. Some context flags are also not passed down to the backend renderer yet.

## Docs

Docs can be found [here](https://wlitwin.github.io/docs/graphv/graphv). They are still a work in progress.

## Screenshots

![Screenshot of native GLES2 demo](/screenshots/graphv-native.png?raw=true "Native Demo")
![Screenshot of chrome web demo](/screenshots/graphv-web.png?raw=true "Web Demo (Chrome)")

## Demo

A live web demo can be found [here](https://wlitwin.github.io/demos/graphv). If the fonts look wrong the first time, refresh.

Performance varies quite a lot between browsers, Chrome tends to be the fastest. The demo supports mouse and touch events. Clicking/tapping will zoom the top-left widget, hovering the text will highlight rows, and two fingers on mobile will allow zooming/panning. Dragging a single finger on mobile will make the eyes follow the finger.

## Performance

Below is a table of eye-ball measurements of frame times across platforms compared with the native C implementation of NanoVG. All times are in millseconds (ms). Measurements were taken on a MacBook Pro Late 2013 laptop. The program being tested is the one shown in the screenshots above.

| Platform      | OCaml         | NanoVG (GLES2) | NanoVG JS   |
| ------------- | ------------- | -------------- | ---------   |
| Linux Native  | 1.05 +/- 0.05 | 1.00 +/- 0.05  |  N/A        |
| Linux Chrome  | 5.50 +/- 2    | N/A            |  5.50 +/- 2 |
| Linux Firefox | 12.00 +/ 5    | N/A            |  8.00 +/- 2 |

## Licenses

Project is licensed under MIT. Fonts used in examples:

* Roboto licensed under Apache license
* Entypo licensed under CC BY-SA 4.0.
* Noto Emoji licensed under SIL Open Font License, Version 1.1

