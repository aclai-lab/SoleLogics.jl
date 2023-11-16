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

To let you better orient yourself while understanding SoleLogics structure, each chapter will begin with a little summary of what you are going to learn, in the form of small type-hierarchy trees that will grow during your reading journey. To see a complete map of SoleLogics.jl types and structures, please refer to [Complete Exports Map](@ref complete-exports-map).

## About

The package is developed by the [ACLAI Lab](https://aclai.unife.it/en/) @ University of Ferrara.

*SoleLogics.jl* lays the logical foundations for [*Sole.jl*](https://github.com/aclai-lab/Sole.jl), an open-source framework for *symbolic machine learning*, originally designed for machine learning based on modal logics (see [Eduard I. Stan](https://eduardstan.github.io/)'s PhD thesis *'Foundations of Modal Symbolic Learning'* [here](https://www.repository.unipr.it/bitstream/1889/5219/5/main.pdf)).

Thanks to Jakob Peters ([PAndQ.jl](https://github.com/jakobjpeters/PAndQ.jl/)) for the interesting discussions and ideas.

## More on Sole
- [SoleData.jl](https://github.com/aclai-lab/SoleData.jl)
- [SoleFeatures.jl](https://github.com/aclai-lab/SoleFeatures.jl) 
- [SoleModels.jl](https://github.com/aclai-lab/SoleModels.jl)
- [SolePostHoc.jl](https://github.com/aclai-lab/SolePostHoc.jl)

## [Complete Exports Map](@id complete-exports-map)
Here is a (almost) complete map of SoleLogics available exports. Feels overwhelming? Don't worry, if you are not practical with this package this is useful to just know what definitions does exists and guessing at a glance how types are relationed with each other.

- [`Syntactical`](@ref)
    - [`Connective`](@ref)        
        - [`NEGATION`](@ref)
        - [`CONJUNCTION`](@ref) 
        - [`DISJUNCTION`](@ref)
        - [`IMPLICATION`](@ref)    
        - [`DIAMOND`](@ref)
        - [`BOX`](@ref)   
    - [`AbstractRelationalConnective`](@ref)
        - [`DiamondRelationalConnective`](@ref)
        - [`BoxRelationalConnective`](@ref)         
    - [`Formula`](@ref)
        - [`AnchoredFormula`](@ref) 
        - [`AbstractSyntaxStructure`](@ref)
            - [`SyntaxTree`](@ref)              
                - [`SyntaxLeaf`](@ref)
                    - [`Atom`](@ref)            
                    - [`Literal`](@ref)         
                    - [`Truth`](@ref)    
                        - [`BooleanTruth`](@ref)
                            - [`Top`](@ref)
                            - [`Bot`](@ref)  
                - [`SyntaxBranch`](@ref)        
            - [`LeftmostLinearForm{C<:Connective,SS<:AbstractSyntaxStructure}`](@ref)

- [`Operator`](@ref) 
- [`SyntaxToken`](@ref)
---

- [`AbstractInterpretation`](@ref)
    - [`AbstractAssignment`](@ref) 
        - [`TruthDict{D<:AbstractDict{A where A<:Atom,T where T<:Truth}}`](@ref)
        - [`DefaultedTruthDict{D<:AbstractDict{A where A<:Atom,T where T<:Truth}, T<:Truth}`](@ref)
    - [`AbstractInterpretationSet{M<:AbstractInterpretation}`](@ref)
        - [`InterpretationVector{M<:AbstractInterpretation}`](@ref)
    - [`LogicalInstance{S<:AbstractInterpretationSet}`](@ref) 

- [`TruthTable{A,T<:Truth}`](@ref)
- [`LogicalInstance{S<:AbstractInterpretationSet}`](@ref)
---

- [`AbstractAlphabet{V}`](@ref)
    - [`ExplicitAlphabet{V}`](@ref)
    - [`AlphabetOfAny{V}`](@ref)
---

- [`AbstractGrammar{V<:AbstractAlphabet,O<:Operator}`](@ref) 
    - [`CompleteFlatGrammar{V<:AbstractAlphabet,O<:Operator}`](@ref)

---

- [`AbstractAlgebra{T<:Truth}`](@ref)
    - [`BooleanAlgebra`](@ref)
---

- [`AbstractLogic{G<:AbstractGrammar,V<:AbstractAlgebra}`](@ref)
    - [`BaseLogic{G<:AbstractGrammar,A<:AbstractAlgebra}`](@ref)
---

- [`AbstractWorld`](@ref)
    - [`World{T}`](@ref)

- [`AbstractFrame{W<:AbstractWorld}`](@ref)
    - [`AbstractUniModalFrame{W<:AbstractWorld}`](@ref)
    - [`AbstractMultiModalFrame{W<:AbstractWorld}`](@ref)
    - [`WrapperMultiModalFrame{W<:AbstractWorld, D<:AbstractDict{<:AbstractRelation,<:AbstractUniModalFrame{W}}}`](@ref)

- [`AbstractRelation`](@ref)

- [`AbstractInterpretation`](@ref)
    - [`AbstractKripkeStructure`](@ref)
        - [`KripkeStructure{FR<:AbstractFrame, MAS<:AbstractDict}`](@ref)