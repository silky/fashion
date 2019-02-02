# Contour-Maps

The scheme here worked kind of okay, but we're left with the problem of
finding coherent lines from the bitmap.

Also, I had to do this imagemagick stuff:

```
convert ab.bmp  -dither None -colors 64 -set colorspace RGB less.bmp
```

Say, to change down the colour space.

Some things to investigate:

- https://github.com/fogleman/ln
- https://github.com/haskell-repa/repa/blob/5731ddbdda14be3e81e2f89ab385756797e9187b/repa-examples/examples/Canny/src-repa/Main.hs
- http://www.imagemagick.org/Usage/quantize/#colors
- https://imagemagick.org/script/color-management.php
