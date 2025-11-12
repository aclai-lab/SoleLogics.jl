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

# Examples
```julia
julia> using SoleLogics

julia> _myrng = 42
julia> _alphabet = ExplicitAlphabet(Atom.('p':'z'))
julia> _operators = [CONJUNCTION, DISJUNCTION, NEGATION, DIAMOND, BOX]

julia> _nformulas = 10

julia> _earlystoppingthreshold = 0.2
julia> _formulas = [
    randformula(
        _myrng,
        _alphabet,
        _operators;
        earlystoppingthreshold =_earlystoppingthreshold
    )
    for _ in 1:_nformulas
]

julia> _conjunction = CONJUNCTION(_formulas...)


julia> randlogiset(_myrng, ((_conjunction,)), 5; silent=false)

```
"""
@__rng_dispatch function randlogiset(
    rng::Union{Integer,AbstractRNG},
    _formulas::Tuple{SyntaxBranch},
    nkripkestructures::Int;
    maxiterations::Int=100,
    maxistances::Int=10,
    nworlds::Int=10,
    nedges::Int=15,
    truthvalues::Union{AbstractVector{<:Truth}}=SoleLogics.inittruthvalues(
        SoleLogics.BooleanAlgebra()),
    silent::Bool=true,
    kwargs...
)
    # collect all the atoms appearing in every formula (this is the alpahbet of the logiset)
    _atoms = [atoms(formula) for formula in _formulas] |> Iterators.flatten |> unique

    # keep track of the future modal instances:
    # ∀ instance ∈ _instances[i], it will be true that instance satisfies _formulas[i]
    _instances = [KripkeStructure[] for _ in 1:length(_formulas)]

    # these are the leftovers of _instances, that is, the models that are not pushed to it
    _failed_instances = [[] for _ in 1:length(_formulas)]

    for (i,formula) in enumerate(_formulas)
        # we cannot overcome `maxiterations` number of iterations
        _niterations = 0

        # ideally, we want maxistances number of instances;
        # this is the counter keeping track of the instances that are OK.
        _okiterations = 0

        while (_niterations <= maxiterations) || (_okiterations <= maxistances)
            model = randmodel(rng, nworlds, nedges, _atoms, truthvalues; kwargs...)

            # if the random model satisfied the `formula`, we consider it in the final logiset;
            # otherwise, we keep the model for the future class formulas.
            check(formula, model, World(1)) == TOP ?
                # the generated model is ok, so we push it and update its related counter
                begin push!(_instances[i], model); _okiterations = _okiterations + 1; end :
                # unfortunately, the model is not ok
                push!(_failed_instances[i], model)

            _niterations = _niterations + 1
        end

        if _niterations > maxiterations
            silent || println(
                "The maximum number of iterations ($maxiterations) has been reached.")
        end
    end

    return _instances
end
