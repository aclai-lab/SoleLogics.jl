```@meta
CurrentModule = SoleLogics.jl
DocTestSetup = quote
    using SoleLogics
end
```

# SoleLogics

Welcome to the documentation for [SoleLogics](https://github.com/aclai-lab/SoleLogics.jl).

SoleLogics.jl provides a fresh codebase for computational logic, featuring easy manipulation of:

- Propositional and (multi)modal logics (propositions, logical constants, alphabet, grammars, crisp/fuzzy algebras);
- Logical formulas (random generation, parsing, minimization);
- Logical interpretations (e.g., propositional valuations, Kripke structures);
- Algorithms for model checking, that is, checking that a formula is satisfied by an interpretation.

SoleLogics.jl lays the logical foundations for Sole.jl, an open-source framework for symbolic machine learning.

```@index
```

```@autodocs
Modules = [SoleModels]
```
