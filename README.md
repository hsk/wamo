# The WAM in OCaml

[![Build Status](https://travis-ci.org/hsk/wamo.svg?branch=master)](https://travis-ci.org/hsk/wamo)

This is simple WAM (Warren Abstract Machine) implementation and compilation.

Original source is https://github.com/acharal/wam .

## Building

```bash
$ apt install ocaml
$ make && make test
```

## Run

```bash
$ ./wamo -i pl/prelude.pl
X=cons(b, cons(a, nil))
```

## License GPLv2

