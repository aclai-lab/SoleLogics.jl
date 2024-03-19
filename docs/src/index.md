```@meta
CurrentModule = SoleLogics
```

# SoleLogics

Welcome to the documentation for [SoleLogics.jl](https://github.com/aclai-lab/SoleLogics.jl), a Julia package for [computational logic](https://en.wikipedia.org/wiki/Computational_logic).
<!-- SoleLogics.jl lays the logical foundations for [Sole.jl](https://github.com/aclai-lab/Sole.jl), an open-source framework for *symbolic machine learning*. -->
To let you better orient yourself while understanding SoleLogics' structure, each chapter will begin with a little summary of what you are going to learn, in the form of small type-hierarchy trees that will grow during your reading journey. To see a complete map of SoleLogics.jl types and structures, please refer to [Complete Exports Map](@ref complete-exports-map).

## Feature Summary

SoleLogics.jl allows manipulation of:

- Syntax tokens (e.g., atoms, logical constants (e.g., operators and truth values), etc.);
- Alphabets & context-free logical grammars (e.g., [normal forms](https://en.m.wikipedia.org/wiki/Conjunctive_normal_form));
- Algebras (e.g., crisp, fuzzy, many-valued);
- Logics (e.g., propositional, (multi)modal);
- [Formulas](https://en.wikipedia.org/wiki/Well-formed_formula) (e.g., syntax trees, DNFs, CNFs): random generation, parsing, minimization;
- [Interpretations](https://en.wikipedia.org/wiki/Interpretation_(logic)) (e.g., propositional assignments, [Kripke structures](https://en.wikipedia.org/wiki/Kripke_structure_(model_checking)));
- Algorithms for evaluating the truth of a formula on an interpretation ([model checking](https://en.wikipedia.org/wiki/Model_checking));
- Interfaces to Z3, for evaluating the validity/satisfiability of a propositional formula.

## Installation

To install SoleLogics.jl, use the Julia package manager:
```julia
using Pkg
Pkg.add("SoleLogics")
```

## About

*SoleLogics.jl* lays the logical foundations for [*Sole.jl*](https://github.com/aclai-lab/Sole.jl), an open-source framework for *symbolic machine learning*, originally designed for machine learning based on modal logics (see [Eduard I. Stan](https://eduardstan.github.io/)'s PhD thesis *'Foundations of Modal Symbolic Learning'* [here](https://www.repository.unipr.it/bitstream/1889/5219/5/main.pdf)).

More on Sole:
- [SoleData.jl](https://github.com/aclai-lab/SoleData.jl)
- [SoleFeatures.jl](https://github.com/aclai-lab/SoleFeatures.jl) 
- [SoleModels.jl](https://github.com/aclai-lab/SoleModels.jl)
- [SolePostHoc.jl](https://github.com/aclai-lab/SolePostHoc.jl)

The package is developed by the [ACLAI Lab](https://aclai.unife.it/en/) @ University of Ferrara.

## [Complete Types Hierarchy Map](@id complete-exports-map)

Here is a map of SoleLogics' most important types and structures. Feels overwhelming? Don't worry, if you are not practical with SoleLogics, this is useful to just know what definitions exist and hints at a glance how types are related to each other.

- [`Syntactical`](@ref)
    - [`Connective`](@ref) ([`¬`](@ref), [`∧`](@ref), [`∨`](@ref), [`→`](@ref), [`◊`](@ref), [`□`](@ref))
    - [`AbstractRelationalConnective`](@ref)
        - [`DiamondRelationalConnective`](@ref)
        - [`BoxRelationalConnective`](@ref)         
    - [`Formula`](@ref)
        - [`AnchoredFormula`](@ref) 
        - [`AbstractSyntaxStructure`](@ref)
            - [`SyntaxTree`](@ref)              
                - [`SyntaxLeaf`](@ref)
                    - [`Atom`](@ref)            
                    - [`Truth`](@ref)    
                        - [`BooleanTruth`](@ref) 
                        - [`FiniteTruth`](@ref) 
                - [`SyntaxBranch`](@ref)        
            - [`LeftmostLinearForm`](@ref)
            - [`Literal`](@ref)         
  - [`Operator`](@ref) 
  - [`SyntaxToken`](@ref)
- [`AbstractInterpretation`](@ref)
    - [`AbstractAssignment`](@ref)
        - [`TruthDict`](@ref)
        - [`DefaultedTruthDict`](@ref)
    - [`AbstractKripkeStructure`](@ref)
        - [`KripkeStructure`](@ref)
    - [`LogicalInstance`](@ref) <!-- - [`TruthTable{A,T<:Truth}`](@ref) -->
- [`AbstractInterpretationSet`](@ref)
    - [`InterpretationVector`](@ref)
- [`AbstractAlphabet`](@ref)
    - [`ExplicitAlphabet`](@ref)
    - [`AlphabetOfAny`](@ref)
- [`AbstractGrammar`](@ref) 
    - [`CompleteFlatGrammar`](@ref)
- [`AbstractAlgebra`](@ref)
    - [`BooleanAlgebra`](@ref)
- [`AbstractLogic`](@ref)
    - [`BaseLogic`](@ref)
- [`AbstractWorld`](@ref)
    - [`World`](@ref)
    - [`Piont`](@ref)
    - [`Interval`](@ref)
    - [`Interval2D`](@ref)
- [`AbstractFrame`](@ref)
    - [`AbstractUniModalFrame`](@ref)
    - [`AbstractMultiModalFrame`](@ref)
    - [`WrapperMultiModalFrame`](@ref)
- [`AbstractRelation`](@ref)
