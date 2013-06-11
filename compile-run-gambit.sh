#!/bin/sh

# compile the application using gambit scheme. It is straightforward to
# modify the script to handle whichever flavor of scheme you wish to use.

cat src/parameters.scm src/constants.scm src/random.scm \
  src/read.scm src/eval.scm \
  src/projection.scm \
  src/point.scm src/vector.scm src/bounding-box.scm src/AABB.scm \
  src/intersection-object.scm src/phong.scm src/ray.scm \
  src/camera.scm src/fisheye.scm \
  src/scene.scm src/sphere.scm src/plane.scm src/triangle.scm src/cylinder.scm \
  src/light.scm src/octree.scm src/shader.scm \
  src/preliminaries.scm src/gamma.scm src/plot.scm src/initscene.scm \
  src/writedims.scm src/mainpart.scm src/launch.scm \
  > render.scm

cat src/sdl-primitives.scm src/sdl-simple.scm >sdl-scene.scm

gsc -c render.scm
gsc -link render.scm
gcc \
  -O1 \
  -D___SINGLE_HOST \
  -m32 \
  -I/Library/Gambit-C/v4.6.1/include \
  -L/Library/Gambit-C/v4.6.1/lib \
  -lgambc -lm render.c render_.c

# now run the renderer on a sample scene

mv a.out render
./render <sdl-scene.scm | ~/bin/data2bmp x.bmp
