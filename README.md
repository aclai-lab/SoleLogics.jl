# *SoleLogics.jl* – Computational logic in Julia

[![Stable](https://img.shields.io/badge/docs-stable-blue.svg)](https://aclai-lab.github.io/SoleLogics.jl/stable)
[![Dev](https://img.shields.io/badge/docs-dev-blue.svg)](https://aclai-lab.github.io/SoleLogics.jl/dev)
[![Build Status](https://api.cirrus-ci.com/github/aclai-lab/SoleLogics.jl.svg)](https://cirrus-ci.com/github/aclai-lab/SoleLogics.jl)
[![Coverage](https://codecov.io/gh/aclai-lab/SoleLogics.jl/branch/master/graph/badge.svg)](https://codecov.io/gh/aclai-lab/SoleLogics.jl)
<!-- [![Coverage](https://coveralls.io/repos/github/aclai-lab/SoleLogics.jl/badge.svg?branch=master)](https://coveralls.io/github/aclai-lab/SoleLogics.jl?branch=master) -->
[![Code Style: Blue](https://img.shields.io/badge/code%20style-blue-4495d1.svg)](https://github.com/invenia/BlueStyle)

## In a nutshell

*SoleLogics.jl* provides a fresh codebase for computational logic, featuring easy manipulation of:
- Propositional and (multi)modal logics (propositions, logical constants, alphabet, grammars, crisp/fuzzy algebras);
- Logical formulas (random generation, parsing, minimization);
- Logical interpretations (e.g., propositional valuations, Kripke structures);
- Algorithms for *finite [model checking](https://en.wikipedia.org/wiki/Model_checking)*, that is, checking that a formula is satisfied by an interpretation.

# Usage

<!-- However, it can be used for other purposes by computational logicians. -->

## Generating random formulas
Random formulas generation is provided by the following methods:
- randformula (which returns a SyntaxTree);
- randformulatree (which returns a SyntaxTree anchored to a Logic, thus, a Formula).

Both allow customizing the generation process by setting an alphabet (that is, which propositional letters are in play), an operators vector, the maximum height of the SyntaxTree and the maximum modal depth (that is, the maximum number of modal operators, if any, in each SyntaxTree path).

Notes for Giovanni; 
1) many important definitions are introduced here: is this the correct place to explain them or it's better to just link the documentation? 
2) I'm still working on putting a modal_depth parameter in both randformula and randformulatree 
3) As stated in AbstractSyntaxStructure docstring, classically a logical formula is implemented using a tree structure (an AST) but other representations may exist: I guess we could add a "randnormalform" method

## Parsing Formulas
Consider a logical formula written as a string in infix notation (e.g. "p∧q") or in functional notation (e.g. "∧(p,q)"); such expressions can easily be converted into SyntaxTrees using the method parseformulatree. As you can see from the documentation it is highly customizable, allowing parsing custom-defined operators and changing how recognized propositions must be interpreted (e.g. in "true∧false" propositions are booleans, while in "1∧0" they are integers).

The SyntaxTree returned by parseformulatree can be paired with a logic (a grammar and an algebra) using parseformula, thus returning a Formula: the latter method disposes of the same flexibility of parseformulatree.

## Generating random interpretations
Kripke models generation is provided by gen_kmodel interface. As you can see from the documentation, internally it builds up a random directed graph structure (using some, possibly custom algorithm). The adjacency list obtained is enriched with informations to obtain a so called Kripke frame; this is done by taking each vertex in the list and

- converting it into a World structure;
- defining which worlds are accessible from this following a specific relation (I'm trying to summarize both the concept of binary accessiblity relation R and the accessibles method...).

Finally, pairing each world in the Kripke frame with a list of propositional letters (this goes undeer the name of world valuation function), a Kripke model is returned by gen_kmodel.

## Model checking

## Interpretation sets


## About

The package is developed by the [ACLAI Lab](https://aclai.unife.it/en/) @ University of Ferrara.

*SoleLogics.jl* lays the logical foundations for [*Sole.jl*](https://github.com/aclai-lab/Sole.jl), an open-source framework for *symbolic machine learning*.
