# Morth
Porth but More.

This repo is kind of a challenge to myself to implement the Porth stack-based concatenative programming language in as many host environments as I can manage. This ended up including some which target C, and can therefore run on anything where C is supported (after compilation).

The implementations are all at varying levels of feature-completeness, so take from that what you will.

The most complete version is probably a tie between [C Pure](c-pure) and [C to C](c-to-c). The latter targets C and is also hosted in it, whereas the former compiles to NASM x86_64 assembly targeting Linux.
