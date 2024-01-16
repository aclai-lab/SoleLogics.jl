```@meta
CurrentModule = SoleLogics
```

```@contents
Pages = ["fuzzy.md"]
```

# [Introduction](@id fuzzy-introduction)
SoleLogic also provides tools to works with [many-valued logics](https://en.wikipedia.org/wiki/Many-valued_logic), i.e., containing truth values other than the classic boolean ones `true` and `false`, characterised by a partial order.

The main reference for this part is [Many-Valued Modal Logics](https://melvinfitting.org/bookspapers/pdf/papers/ManyValMod.pdf) by Melvin Fitting.

# [HeytingTruth](@id heytingtruth)
```@docs
HeytingTruth
heytingtruths
```

# [HeytingAlgebra](@id heytingalgebra)
```@docs
HeytingAlgebra
heytingalgebra
```

# [A simple example](@id fuzzy-example)

Let's take as an example the simplest meaningful Heyting Algebra, characterised by 4 values ⊥ α β ⊤, where α and β are both greater than ⊥ but lesser than ⊤.

This could be represented by the following bounded lattice:

```
   ⊤
 /   \
α     β
 \   /
   ⊥
```

To declare a new Heyting algebra in SoleLogic,  we first have to declare the new truth values different from ⊥ and ⊤ as variable of type HeytingTruth. To do that, we can use the [`heytingtruths`](@ref) macro, which takes as input the symbols of the new truths and returns a vector containing the HeytingTruths representing such values, which have been declared as constants in the global scope.

```julia-repl
julia> SoleLogics.@heytingtruths α β
2-element Vector{HeytingTruth}:
 HeytingTruth: α
 HeytingTruth: β
```

Then, we can declare our new Heyting algebra using the [`heytingalgebra`](@ref) macro, which takes as input the name of the new algebra, the a tuple containing the new values of the algebra other than ⊥ and ⊤, and the direct relations between these values, with each relation being a tuple (t1, t2) asserting that t1 < t2.

!!!info
    The order of the symbols passed as an argument to the macro in the tuple MUST be consistent with the order in which they have been declared, e.g., in our care, (α, β) and NOT (β, α)!

```julia-repl
julia> SoleLogics.@heytingalgebra heytingalgebra4 (α, β) (⊥, α) (⊥, β) (α, ⊤) (β, ⊤)
HeytingTruth[HeytingTruth: ⊤, HeytingTruth: ⊥, HeytingTruth: α, HeytingTruth: β]
Vector{HeytingTruth}
HeytingAlgebra(HeytingTruth[HeytingTruth: ⊤, HeytingTruth: ⊥, HeytingTruth: α, HeytingTruth: β], SimpleDiGraph{Int64}(4, [Int64[], [3, 4], [1], [1]], [[3, 4], Int64[], [2], [2]]))
```

Meet (lower greatest bound), join (greater lowest bound) and heyting implication are implemented as [`Connective`](@ref)s and can be computed using the [`collatetruth`](@ref) method passing the algebra as an additional argument.

!!!info
    Note how the truth values have no meaning when taken by themselves, but they a must always be associated with an algebra instead!

```julia-repl
julia> collatetruth(∧, (α, β), heytingalgebra4)
HeytingTruth: ⊥

julia> collatetruth(∨, (α, β), heytingalgebra4)
HeytingTruth: ⊤

julia>  collatetruth(→, (α, β), heytingalgebra4)
HeytingTruth: β
```

# [About boolean algebra backward compatibility ](@id fuzzy-boolean)

It is also possible to get the classic boolean algebra, e.g., using the [`heytingalgebra`](@ref) macro leaving the tuple of new values empty and using a single relations `(⊥, ⊤)`.

```julia-repl
@heytingalgebra booleanalgebra () (⊥, ⊤)
```

This way it is also possible, given a [`TruthDict`](@ref), to [`check`](@ref) a formula on a logical interpretation (or model):

```julia-repl
julia> @atoms a b
2-element Vector{Atom{String}}:
 Atom{String}: a
 Atom{String}: b

julia> td = TruthDict([a => ⊤, b => ⊥])
TruthDict with values:
┌────────┬────────┐
│      b │      a │
│ String │ String │
├────────┼────────┤
│      ⊥ │      ⊤ │
└────────┴────────┘

julia> rf = randformula(Random.MersenneTwister(1234), 3, [a, b], SoleLogics.BASE_PROPOSITIONAL_CONNECTIVES)
SyntaxBranch{NamedConnective{:∨}}: (b ∨ b → b ∧ b) ∨ ((b → a) → ¬b)

julia> check(rf, td)
true
```
