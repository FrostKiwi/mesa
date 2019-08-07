# OpenGL on D3D12 emulation layer

This is a gallium driver that outputs D3D12 command buffers rather than
hardware-specific command buffers. This allows to expose a single OpenGL
implementation on top of any D3D12 driver.

## Building

This currently builds using MSVC using either Meson or SCons. Meson is
strongly recommended for performance reasons.

### Meson

See [Mesa's meson documentation](../../../../docs/meson.html) for details on
how to set up meson and all dependencies. Essentially this is:

1. Python 3.x
2. Meson
3. pkg-config-lite
4. Ninja (unless building with Visual Studio)

#### Ninja

Here's how to create a Ninja build-system for Mesa that enables D3D12 support:

```
meson setup <build-dir> --default-library=static -Dgallium-drivers=swrast,d3d12
```

If you're using Ninja, you execute ninja by doing:
```
ninja -C <build-dir>
```

As a result, you should have `opengl32.dll` in the
`<build-dir>/src/gallium/targets/libgl-gdi/`-directory. This should in theory
be suitable either as an `opengl32.dll`-replacement, or as an OpenGL ICD
driver.

#### Visual Studio

If instead you want to build using Visual Studio, just add `--backend=vs` to
your command. You'll find `mesa.sln` in `<build-dir>`, and proceed as usual
from there.

### SCons

You can build this using SCons by simply executing `scons`. But it's
discouraged, as the build is dog-slow even for no-op builds. Also,
installing SCons is a bit tricky, due to both Python 2.x and 3.x
dependencies.
