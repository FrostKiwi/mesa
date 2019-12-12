# Microsoft

This folder contains a couple of things that are Microsoft specific:

1. A compiler back-end that writes DXIL
2. An OpenCL C compiler front-end

_Note_: Both of these are experimental and under heavy development. Do not
        expect them to actually work yet.

In order to compile this, you build the normal way you would otherwise do,
except you might want to set the `microsoft-clc` Meson-option. See
the general Mesa and Meson documentation for how to do that.
