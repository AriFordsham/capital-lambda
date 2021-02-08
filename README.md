# capital-lambda

A small lambda calculus, based on Benjamin Pierce's *Types and Programming Languages,* with the following features:

- Support for different variable naming schemes

  Two naming schemes are provided - de Bruijn integer indexes, and regular ('Church') names, which can be any (equality-comparable) type. A routine is provided for converting Church terms to de Bruijn terms.

- An evaluation routine (for de Bruijn terms,) supporting different evaluation schemes. Strict and lazy schemes are provided.
