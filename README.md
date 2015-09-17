fasta
============

**Gregory W. Schwartz**

Defines the Fasta type and some parsing and translation associated with it. The motivation for this library was to make a general use fasta library making no assumptions about the data comprised of many language types as possible for String, Text, ByteString, and all laziness inbetween, as there was a lack of choice in this regard. Futhermore, the streaming capabilities of pipes allows for much needed efficiency improvements.

While you can use the `parseFasta` function, it is highly recommended to instead use pipes with `pipesFasta` as the former function is slower and uses up much more memory due to lookaheads. An example of the `pipesFasta` usage can be seen in the `diversity` executable package located at https://github.com/GregorySchwartz/diversity/blob/master/src/src-exec/Main.hs in the `pipesPositionMap` function.

To install:
```
stack install fasta
```
