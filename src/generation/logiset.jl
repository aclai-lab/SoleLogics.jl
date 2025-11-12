using Random

"""
    @__rng_dispatch function randlogiset(
        rng::Union{Integer,AbstractRNG},
        _formulas::Tuple{SyntaxBranch},
        nkripkestructures::Int;
        maxiterations::Int=100,
        nworlds::Int=10,
        nedges::Int=15,
        truthvalues::Union{AbstractVector{<:Truth}}=SoleLogics.inittruthvalues(
            SoleLogics.BooleanAlgebra()),
        kwargs...
    )

TODO
"""
@__rng_dispatch function randlogiset(
    rng::Union{Integer,AbstractRNG},
    _formulas::Tuple{SyntaxBranch},
    nkripkestructures::Int;
    maxiterations::Int=100,
    nworlds::Int=10,
    nedges::Int=15,
    truthvalues::Union{AbstractVector{<:Truth}}=SoleLogics.inittruthvalues(
        SoleLogics.BooleanAlgebra()),
    kwargs...
)
    # collect all the atoms appearing in every formula (this is the alpahbet of the logiset)
    _atoms = [atoms(formula) for formula in _formulas] |> Iterators.flatten |> unique

    # keep track of the future modal instances:
    # ∀ instance ∈ _instances[i], instance satisfies _formulas[i]
    _instances = [KripkeStructure[] for _ in 1:length(_formulas)]
    _failed_instances = [[] for _ in 1:length(_formulas)]

    # we cannot overcome this limit
    _niterations = 0

    for (i,formula) in enumerate(_formulas)
        model = randmodel(rng, nworlds, nedges, _atoms, truthvalues; kwargs...)

        # if the random model satisfied the `formula`, we consider it in the final logiset;
        # otherwise, we keep the model for the future class formulas.
        check(model, formula) ?
            push!(_instances[i], model) :
            push!(_failed_instances[i], model)

        if i > maxiterations
            println("The maximum number of iterations ($maxiterations) has been reached.")
        end

        _niterations = _niterations + 1
    end
end
