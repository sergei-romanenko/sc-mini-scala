# sc-mini: A Minimalistic Supercompiler written in Idris

The goal is to stress the main features of supercompilation for a working 
functional programmer.

Its design is an attempt to illustrate the following formula:

> Supercompiler = Driving + Positive info propagation +
> Folding + Simplification + Generalization

However, it turns out to be not so easy task. So here we illustrate this 
formula step by step:

1. Prototype = Driving + Generalization + Folding
2. Deforester = Prototype + Simplification of graph of configurations
3. Supercompiler = Deforester + Positive Information Propagation

In order to catch the idea of this supercompiler, just look into the following 
modules into the following order:

1. Prototype
2. Deforester
3. Supercompiler

The initial version of **sc-mini**, written in Haskell was described in the 
paper

> Ilya Klyuchnikov. **The ideas and methods of supercompilation.**
> _Practice of Functional Programming_, 7, 2011. In Russian.
> <http://fprog.ru/2011/issue7/>
