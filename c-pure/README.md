# c-pure

An implementation which only functions on Linux, mostly due to its build
system.

## CoolBuild

This project builds with CoolBuild, a tiny build system written in pure C.
To bootstrap the build system:

    gcc -o coolbuild coolbuild.c

You only need run that once; if you edit coolbuild.c, the system will detect
this and recompile itself.

To compile the program:

    ./coolbuild
