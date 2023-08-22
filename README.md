<div align="center"><a href="https://github.com/aclai-lab/Sole.jl"><img src="logo.png" alt="" title="This package is part of Sole.jl" width="200"></a></div>

# SoleLogics.jl – Computational logic in Julia

[![Stable](https://img.shields.io/badge/docs-stable-blue.svg)](https://aclai-lab.github.io/SoleLogics.jl/stable)
[![Dev](https://img.shields.io/badge/docs-dev-blue.svg)](https://aclai-lab.github.io/SoleLogics.jl/dev)
[![Build Status](https://api.cirrus-ci.com/github/aclai-lab/SoleLogics.jl.svg?branch=main)](https://cirrus-ci.com/github/aclai-lab/SoleLogics.jl)
[![codecov](https://codecov.io/gh/aclai-lab/SoleLogics.jl/branch/main/graph/badge.svg?token=LT9IYIYNFI)](https://codecov.io/gh/aclai-lab/SoleLogics.jl)
[![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/aclai-lab/SoleLogics.jl/HEAD?labpath=pluto-demo.jl)
<!-- [![Code Style: Blue](https://img.shields.io/badge/code%20style-blue-4495d1.svg)](https://github.com/invenia/BlueStyle) -->
<!-- [![Coverage](https://coveralls.io/repos/github/aclai-lab/SoleLogics.jl/badge.svg?branch=main)](https://coveralls.io/github/aclai-lab/SoleLogics.jl?branch=main) -->

## In a nutshell

*SoleLogics.jl* provides a fresh codebase for computational logic, featuring easy manipulation of:
- Propositional and (multi)modal logics (propositions, logical constants, alphabet, grammars, crisp/fuzzy algebras);
- Logical formulas (parsing, random generation, minimization);
- Logical interpretations (e.g., propositional valuations, Kripke structures);
- Algorithms for *finite [model checking](https://en.wikipedia.org/wiki/Model_checking)*, that is, checking that a formula is satisfied by an interpretation.

<!-- However, it can be used for other purposes by computational logicians. -->

## Usage

```julia
using Pkg; Pkg.add("SoleLogics")
using SoleLogics
```

### Parsing and manipulating Formulas

```julia
julia> φ1 = parseformula("¬p∧q∧(¬s∧¬z)");

julia> φ1 isa SyntaxTree
true

julia> syntaxstring(φ1)
"¬p ∧ q ∧ ¬s ∧ ¬z"

julia> φ2 = ⊥ ∨ Proposition("t") → φ1;

julia> φ2 isa SyntaxTree
true

julia> syntaxstring(φ2)
"(⊥ ∨ t) → (¬p ∧ q ∧ ¬s ∧ ¬z)"
```

### Generating random formulas

```julia
julia> using Random

julia> height = 2

julia> alphabet = Proposition.(["p", "q"])

# Propositional case 
julia> SoleLogics.BASE_PROPOSITIONAL_OPERATORS
6-element Vector{SoleLogics.AbstractOperator}:
 ⊤
 ⊥
 ¬
 ∧
 ∨
 →

julia> randformula(Random.MersenneTwister(507), height, alphabet, SoleLogics.BASE_PROPOSITIONAL_OPERATORS)
SyntaxTree: ¬(q → p)

# Modal case
julia> SoleLogics.BASE_MODAL_OPERATORS
8-element Vector{SoleLogics.AbstractOperator}:
 ⊤
 ⊥
 ¬
 ∧
 ∨
 →
 ◊
 □

julia> randformula(Random.MersenneTwister(14), height, alphabet, SoleLogics.BASE_MODAL_OPERATORS)
SyntaxTree: ¬□p
```

### Model checking

```julia
# Propositional case
julia> phi = parseformula("¬(p ∧ q)")
SyntaxTree: ¬(p ∧ q)

julia> I = TruthDict(["p" => true, "q" => false])
┌────────┬────────┐
│      q │      p │
│ String │ String │
├────────┼────────┤
│  false │   true │
└────────┴────────┘

julia> check(phi, I)
true

# Modal case
julia> using Graphs

# Instantiate a Kripke frame with 5 worlds and 5 edges
julia> worlds = SoleLogics.World.(1:5)

julia> edges = Edge.([ (1, 2), (1, 3), (2, 4), (3, 4), (3, 5)])

julia> fr = SoleLogics.ExplicitCrispUniModalFrame(worlds, Graphs.SimpleDiGraph(edges))
SoleLogics.ExplicitCrispUniModalFrame{SoleLogics.World{Int64}, SimpleDiGraph{Int64}} with
- worlds = ["1", "2", "3", "4", "5"]
- accessibles = 
        1 -> [2, 3]
        2 -> [4]
        3 -> [4, 5]
        4 -> []
        5 -> []

# Enumerate the world that are accessible from the first world
julia> accessibles(fr, first(worlds))
2-element Vector{SoleLogics.World{Int64}}:
 SoleLogics.World{Int64}(2)
 SoleLogics.World{Int64}(3)

julia> p,q = Proposition.(["p", "q"])

 # Assign each world a propositional interpretation
julia> valuation = Dict([
	        worlds[1] => TruthDict([p => true, q => false]),
	        worlds[2] => TruthDict([p => true, q => true]),
	        worlds[3] => TruthDict([p => true, q => false]),
	        worlds[4] => TruthDict([p => false, q => false]),
	        worlds[5] => TruthDict([p => false, q => true]),
	     ])

# Instantiate a Kripke structure by combining a Kripke frame and the propositional interpretations over each world
julia> K = KripkeStructure(fr, valuation)

# Generate a modal formula
julia> modphi = parseformula("◊(p ∧ q)")

# Check the just generated formula on each world of the Kripke structure
julia> [w => check(modphi, K, w) for w in worlds]
5-element Vector{Pair{SoleLogics.World{Int64}, Bool}}:
 SoleLogics.World{Int64}(1) => 1
 SoleLogics.World{Int64}(2) => 0
 SoleLogics.World{Int64}(3) => 0
 SoleLogics.World{Int64}(4) => 0
 SoleLogics.World{Int64}(5) => 0
```

<!--
### Interpretation sets
-->

## About

The package is developed by the [ACLAI Lab](https://aclai.unife.it/en/) @ University of Ferrara.

*SoleLogics.jl* lays the logical foundations for [*Sole.jl*](https://github.com/aclai-lab/Sole.jl), an open-source framework for *symbolic machine learning* and [*modal symbolic machine learning*](https://www.repository.unipr.it/bitstream/1889/5219/5/main.pdf).

## More on Sole
- [SoleData.jl](https://github.com/aclai-lab/SoleData.jl)
- [SoleFeatures.jl](https://github.com/aclai-lab/SoleFeatures.jl) 
- [SoleModels.jl](https://github.com/aclai-lab/SoleModels.jl)
- [SolePostHoc.jl](https://github.com/aclai-lab/SolePostHoc.jl)

## Related Projects

- [PAndQ.jl](https://raw.githubusercontent.com/jakobjpeters/PAndQ.jl/main/README.md)
- [Julog.jl](https://github.com/ztangent/Julog.jl)
- [LogicCircuits.jl](https://github.com/Juice-jl/LogicCircuits.jl)
- [FirstOrderLogic.jl](https://github.com/roberthoenig/FirstOrderLogic.jl)