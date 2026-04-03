# mini-langs

This repository contains the source code for the exam of Languages, Compilers and Interpreters (LCI) course at the University of Pisa. It includes the implementation of an interpreter for a simple function language, `miniTyFun`, and both a compiler and an interpreter for a simple imperative language, `miniImp`.

## Usage

- Clone the repository:

```bash
git clone https://github.com/francescoborri/mini-langs
cd mini-langs
```

- To run the `miniTyFun` interpreter:

```bash
dune exec -- mtfun <src>
```

- To run the `miniImp` interpreter:

```bash
dune exec -- mimp <src>
```

- To compile a `miniImp` program in `src` to `out` using `num_regs` registers, with optional flags for checking undefined variables, liveness optimization, and constant propagation:

```bash
dune exec -- mimpc <num_regs> <src> -o <out> [--check-undef-vars] [--liveness-optimization] [--propagate-constant]
```

- To run the compiled `miniImp` program in `src` using `num_regs` registers:

```bash
dune exec -- mrisc <num_regs> <src>
```

There are also additional debug flags available for every command, which can be used to print the abstract syntax tree or the control-flow graph of the program in [dot](https://en.wikipedia.org/wiki/DOT_%28graph_description_language%29) format: they are better explained by the `--help` flag of each command. There are also available some test programs for both languages under the folders `programs/miniImp` and `programs/miniTyFun`.
