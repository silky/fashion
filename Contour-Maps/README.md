# Contour-Maps

The scheme here worked kind of okay, but we're left with the problem of
finding coherent lines from the bitmap.

Also, I had to do this imagemagick stuff:

```
convert ab.bmp  -dither None -colors 64 -set colorspace RGB less.bmp
```

Say, to change down the colour space.
