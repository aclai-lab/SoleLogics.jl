```@meta
CurrentModule = SoleLogics
```

# SoleLogics

## Introduction

Welcome to the documentation for [SoleLogics](https://github.com/aclai-lab/SoleLogics.jl), a Julia package for [computational logic](https://en.wikipedia.org/wiki/Computational_logic). SoleLogics.jl lays the logical foundations for [Sole.jl](https://github.com/aclai-lab/Sole.jl), an open-source framework for symbolic machine learning.


## Installation

To install SoleLogics.jl, use the Julia package manager:
```julia
using Pkg
Pkg.add("SoleLogics")
```

## Feature Summary

SoleLogics.jl allows easy manipulation of:

- Syntax tokens (e.g., atoms, logical constants/connectives, etc.);
- Alphabets, grammars, algebras (e.g., crisp, fuzzy), logics (e.g., propositional and (multi)modal);
- [Formulas](https://en.wikipedia.org/wiki/Well-formed_formula) (e.g., syntax trees, DNFs, CNFs): random generation, parsing, minimization;
- [Interpretations](https://en.wikipedia.org/wiki/Interpretation_(logic)) (e.g., propositional assignments, [Kripke structures](https://en.wikipedia.org/wiki/Kripke_structure_(model_checking)));
- Algorithms for evaluating the: validity/satisfiability of a formula, and truth of a formula on an interpretation ([model checking](https://en.wikipedia.org/wiki/Model_checking)).

## About

The package is developed by the [ACLAI Lab](https://aclai.unife.it/en/) @ University of Ferrara.

*SoleLogics.jl* lays the logical foundations for [*Sole.jl*](https://github.com/aclai-lab/Sole.jl), an open-source framework for *symbolic machine learning*, originally designed for machine learning based on modal logics (see [Eduard I. Stan](https://eduardstan.github.io/)'s PhD thesis *'Foundations of Modal Symbolic Learning'* [here](https://www.repository.unipr.it/bitstream/1889/5219/5/main.pdf)).

Thanks to Jakob Peters ([PAndQ.jl](https://github.com/jakobjpeters/PAndQ.jl/)) for the interesting discussions and ideas.

## More on Sole
- [SoleBase.jl](https://github.com/aclai-lab/SoleBase.jl)
- [SoleData.jl](https://github.com/aclai-lab/SoleData.jl)
- [SoleFeatures.jl](https://github.com/aclai-lab/SoleFeatures.jl) 
- [SoleModels.jl](https://github.com/aclai-lab/SoleModels.jl)
- [SolePostHoc.jl](https://github.com/aclai-lab/SolePostHoc.jl)