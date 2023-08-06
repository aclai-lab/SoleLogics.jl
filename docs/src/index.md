```@meta
CurrentModule = SoleLogics
```

# SoleLogics

Welcome to the documentation for [SoleLogics](https://github.com/aclai-lab/SoleLogics.jl). SoleLogics.jl lays the logical foundations for [Sole.jl](https://github.com/aclai-lab/Sole.jl), an open-source framework for symbolic machine learning. Specifically, SoleLogics.jl provides a fresh codebase for [computational logic](https://en.wikipedia.org/wiki/Computational_logic).


```@contents
```

## Installation

To install SoleLogics.jl, use the Julia package manager:
```julia
using Pkg
Pkg.add("SoleLogics")
```

## Feature Summary

A general list of features provided by SoleLogics.jl is:

- Propositional and (multi)modal logics (propositions, logical constants, alphabet, grammars, crisp/fuzzy algebras);
- [Logical formulas](https://en.wikipedia.org/wiki/Well-formed_formula) (random generation, parsing, minimization);
- [Logical interpretations](https://en.wikipedia.org/wiki/Interpretation_(logic)) (or models, e.g., [Kripke structures](https://en.wikipedia.org/wiki/Kripke_structure_(model_checking)));
- Algorithms for [model checking](https://en.wikipedia.org/wiki/Model_checking), that is, checking that a formula is satisfied by an interpretation.

