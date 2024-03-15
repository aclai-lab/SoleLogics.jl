```@meta
CurrentModule = SoleLogics
```

```@contents
Pages = ["fuzzy.md"]
```

# [Introduction](@id fuzzy-introduction)
SoleLogics also provides tools to work with [many-valued logics](https://en.wikipedia.org/wiki/Many-valued_logic) (e.g., fuzzy logics), that is, logics with more truth values other than the classical Boolean ones `⊤` and `⊥`. With many-valued logics, the truth values are part of a bounded lattice encoding a partial order between them, encoding a *truer than* relation.

The main reference, here, is [Many-Valued Modal Logics](https://melvinfitting.org/bookspapers/pdf/papers/ManyValMod.pdf) by Melvin Fitting.

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

Let's take as an example the simplest meaningful Heyting Algebra, characterised by the 4 values ⊥, α, β, and ⊤, where α and β are both greater than ⊥ but lesser than ⊤.

This could be represented by the following bounded lattice:

```
   ⊤
 /   \
α     β
 \   /
   ⊥
```

To declare a new Heyting algebra in SoleLogics, we can use the [`heytingalgebra`](@ref) macro, which takes as input a tuple containing the symbols representing the new values of the algebra other than ⊥ and ⊤, and the direct relations between these values, with each relation being a tuple (t1, t2) asserting that t1 < t2. The macro will take care of the declaration of [`HeytingTruths`](@ref).

!!!info
    Please note how both the HeytingTruths and the HeytingAlgebra defined in this way are declared as constants in the global scope.

```julia-repl
julia> SoleLogics.@heytingalgebra myalgebra (α, β) (⊥, α) (⊥, β) (α, ⊤) (β, ⊤)
HeytingAlgebra(HeytingTruth[⊤, ⊥, α, β], SimpleDiGraph{Int64}(4, [Int64[], [3, 4], [1], [1]], [[3, 4], Int64[], [2], [2]]))
```

The classical Boolean connectives are extended to the *meet* (lower greatest bound), *join* (greater lowest bound) and *Heyting implication* operations. These are computed via the [`collatetruth`](@ref) method, which requires the algebra as an additional argument.

!!!info
    Note how the truth values have no meaning by themselves, and they must always be associated with an algebra!

```julia-repl
julia> collatetruth(∧, (α, β), myalgebra)
HeytingTruth: ⊥

julia> collatetruth(∨, (α, β), myalgebra)
HeytingTruth: ⊤

julia>  collatetruth(→, (α, β), myalgebra)
HeytingTruth: β
```
```
