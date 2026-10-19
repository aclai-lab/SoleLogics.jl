```@meta
CurrentModule = SoleLogics.FirstOrderLogics
```

```@contents
Pages = ["first-order-logic.md"]
```

# [Introduction](@id first-order-logic-introduction)
Propositional logic reasons about atomic facts that are either true or false, and [modal logic](@ref modal-logic-introduction) adds a controlled form of quantification over the worlds of a Kripke structure.
[First-order logic](https://en.wikipedia.org/wiki/First-order_logic) (FOL) goes further, talking about the elements of a domain, about the relations among them and about functions over them, and quantifying over those elements with the quantifiers ∀ ("for all") and ∃ ("there exists").

For example, the sentence "every element has a strictly greater element" is written

```math
\forall x \, \exists y \, Less(x, y)
```

and is true on the natural numbers, but false on any finite linear order.

```@docs
FirstOrderLogics
```

SoleLogics provides first-order logic in the `FirstOrderLogics` submodule:
```julia
using SoleLogics
using SoleLogics.FirstOrderLogics
```

The design reuses the existing SoleLogics machinery rather than duplicating it:
- first-order atomic formulas, such as `Less(x, y)`, are [`PredicateAtom`](@ref)s, which are `AbstractAtom`s;
- the quantifiers [`ForAll`](@ref) and [`Exists`](@ref) are unary `Connective`s carrying the variable they bind.

As a consequence, first-order formulas are ordinary `SyntaxTree`s.
They combine with `¬`, `∧`, `∨`, `→`, `⊤` and `⊥`, and generic utilities such as `syntaxstring`, `height`, `subformulas`, `natoms` or `leaves` work on them unchanged.

# [Syntax](@id first-order-logic-syntax)

## Terms
Terms denote elements of the domain.
They are built from variables and constants by applying function symbols.

```julia
julia> @variables x y z;           # Variable(:x), Variable(:y), Variable(:z)

julia> c = Constant(:c);

julia> succ = FunctionSymbol(:succ, 1); plus = FunctionSymbol(:plus, 2);

julia> syntaxstring(plus(succ(x), c))
"plus(succ(x), c)"
```

Applying a function symbol to the wrong number of terms raises an `ArgumentError`.

```@docs
Term
Variable
Constant
FunctionSymbol
FunctionTerm
functionsymbol
arguments
@variables
@constants
```

## Atomic formulas
A [`Predicate`](@ref) is a relation symbol with a fixed arity; applying it to terms gives an atomic formula.
Nullary predicates play the role of propositional letters.
The equality predicate [`EQUALITY`](@ref) can be written infix with `≐` (`\doteq<tab>`).

```julia
julia> Less = Predicate(:Less, 2); Rain = Predicate(:Rain, 0);

julia> syntaxstring(Less(x, succ(c)))
"Less(x, succ(c))"

julia> syntaxstring(x ≐ c), syntaxstring(Rain())
("x = c", "Rain")
```

```@docs
Predicate
PredicateAtom
predicate
EQUALITY
≐
```

## Quantifiers
Quantified formulas can be built with the quantifier connectives themselves (`ForAll(x)(φ)`), with [`forall`](@ref) and [`exists`](@ref), or with their aliases `∀` and `∃`.
Passing several variables nests the quantifiers.

```julia
julia> φ = ∀(x, ∃(y, Less(x, y)));

julia> syntaxstring(φ)
"∀x ∃y Less(x, y)"

julia> syntaxstring(∀(x, y, Less(x, y) → ¬(x ≐ y)))
"∀x ∀y (Less(x, y) → ¬x = y)"

julia> dual(ForAll(x)) == Exists(x)     # ∀ and ∃ are dual, like □ and ◊
true
```

```@docs
AbstractQuantifier
ForAll
Exists
forall
exists
∀
∃
variable
isquantifier
```

## Variables, sentences and substitution
A variable occurrence is *free* when no quantifier binds it.
A formula with no free variables is a *sentence*: its truth on a structure does not depend on any assignment.

[`substitute`](@ref) replaces the free occurrences of a variable with a term, and is capture-avoiding: when a quantifier would capture a variable of the substituted term, the bound variable is renamed first.

```julia
julia> freevariables(∃(y, Less(x, y)))
Set{Variable} with 1 element:
  Variable: x

julia> issentence(∀(x, ∃(y, Less(x, y))))
true

julia> syntaxstring(substitute(∃(y, Less(x, y)), x => y))
"∃y1 Less(y, y1)"
```

```@docs
variables
freevariables
boundvariables
issentence
substitute
predicates
constants
functionsymbols
```

# [Semantics](@id first-order-logic-semantics)
First-order formulas are interpreted on a [`FOStructure`](@ref), that is, tuples composed of a finite domain, together with an interpretation of every constant, function symbol and predicate.
Functions and predicates can be interpreted with any Julia callable, or extensionally (with a dictionary for functions, or a collection of tuples for predicates).

Free variables get their value from an *assignment*, passed via the `assignment` keyword argument.
Quantifiers range over the domain and short-circuit: `∀` stops at the first counterexample and `∃` at the first witness.

The usual SoleLogics entry points apply: [`interpret`](@ref) returns `⊤` or `⊥`, `check` returns a `Bool`, and `M[φ]` is a shorthand for `interpret(φ, M)`.

```julia
julia> zero = Constant(:zero); Even = Predicate(:Even, 1);

julia> M = FOStructure(0:3;
           constants = [zero => 0],
           functions = [succ => (n -> min(n + 1, 3))],
           predicates = [Less => <, Even => iseven]);

julia> check(∃(x, Even(x)), M), check(∀(x, Even(x)), M)
(true, false)

julia> check(∀(x, ∃(y, Less(x, y))), M)          # 3 has no greater element
false

julia> check(∀(x, ∃(y, Less(x, y) ∨ (x ≐ y))), M)
true

julia> check(Less(zero, x), M; assignment = Dict(x => 2))
true

julia> N = FOStructure([:a, :b, :c];
           predicates = [Less => Set([(:a, :b), (:b, :c), (:a, :c)])]);

julia> check(∀(x, y, z, Less(x, y) ∧ Less(y, z) → Less(x, z)), N)    # transitivity
true
```

Evaluating a formula with an unassigned free variable, or with a symbol that the structure does not interpret, raises an error.
Empty domains are accepted: there, `∀x φ` is vacuously true and `∃x φ` is false.

```@docs
FOStructure
domain
evaluate
interpret(::PredicateAtom, ::FOStructure, ::Vararg)
```

# [Limitations and future work](@id first-order-logic-limitations)
This first version covers syntax, syntactic utilities and model checking on finite structures.
The following are not covered yet:
- **Parsing**: `parseformula` does not read first-order formulas; extending it means teaching the tokenizer about terms and quantifier prefixes.
- **`atoms`**: `SoleLogics.atoms` collects into a `Vector{Atom}`, which cannot hold `PredicateAtom`s; use `leaves(φ)` or `appendatoms!(SoleLogics.AbstractAtom[], φ)` instead (widening the container to `AbstractAtom` in the core would fix it).
- **Normal forms**: `normalize`, `cnf` and `dnf` do not push negations through quantifiers, and there is no prenex form or Skolemisation yet.
- **Many-valued first-order logic**: quantifiers currently compute Boolean truth values; a many-valued version would replace them with the meet and join of the algebra over the domain.
- **Random generation**: `randformula` and `randmodel` do not generate first-order formulas or structures.
- **Logic object**: there is no `firstorderlogic()` constructor analogous to `propositionallogic()` and `modallogic()`, because the set of quantifier connectives depends on the variables in use.

