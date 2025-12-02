using Random

"""
    _check_sat_with_spartacus(f::SyntaxBranch)

Return `nothing` if the given formula `f` is unsatisfiable.
"""
function _check_sat_with_spartacus(f::SyntaxBranch)
    _spartan_f = soletospartacus(f)

    _command = `$(SPARTACUS_DIR)/spartacus --showModel --formula=$(_spartan_f)`

    _buffer = IOBuffer()
    run(pipeline(_command, stdout=_buffer))
    _result = String(take!(_buffer))

    return !isnothing(spartacustomodel(_result))
end

"""
Return the indexes of `_formulas` that are not satisfiable considering the provided
`satsolver`.
"""
function _get_unsat_indexes(
    _formulas::Tuple{SyntaxBranch},
    satsolver::Base.Callable,
)
    ans = []

    for (i,f) in enumerate(_formulas)
        # the ith formula is not satisfiable;
        # we want to notify the user
        if isnothing(satsolver(f))
            push!(ans, i)
        end
    end

    return ans
end

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
        checksat::Bool=false,
        satsolver::Union{Nothing,Base.Callable}=_check_sat_with_spartacus
        kwargs...
    )

# Arguments
TODO

# Keyword Arguments
- `checksat::Bool=false`: set to `true` if you want a preliminary check over the
    satisfiability of the `_formulas` provided;
- `satsolver::Union{Nothing,Base.Callable}`: if `checksat` is `true`, this callback is
    exploited to check whether each formula is satisfiable.

# Examples
```julia
julia> using SoleLogics

julia> _myrng = 42
julia> _alphabet = ExplicitAlphabet(Atom.('p':'z'))
julia> _operators = [CONJUNCTION, DISJUNCTION, NEGATION, DIAMOND, BOX]

julia> _nformulas = 10

julia> _earlystoppingthreshold = 0.2
julia> _formulas = [
    randformula(Keyword Arguments
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
    istancesperclass::Int;
    maxiterations::Int=10000,
    nworlds::Int=5,
    nedges::Int=15,
    truthvalues::Union{AbstractVector{<:Truth}}=SoleLogics.inittruthvalues(
        SoleLogics.BooleanAlgebra()),
    silent::Bool=true,
    checksat::Bool=false,
    satsolver::Union{Nothing,Base.Callable}=_check_sat_with_spartacus,
    kwargs...
)
    if checksat && !isnothing(satsolver)
        if !isempty(_get_unsat_indexes(_formulas, satsolver))
            throw(ErrorException("Not all the given formulas are SAT. The indexes are " *
                "$(_unsatisfiable_formulas_indexes)"))
        end
    end

    # collect all the atoms appearing in every formula (this is the alpahbet of the logiset)
    _atoms = [atoms(formula) for formula in _formulas] |> Iterators.flatten |> unique

    # keep track of the future modal instances:
    # ∀ instance ∈ _instances[i], it will be true that instance satisfies _formulas[i]
    _instances = [KripkeStructure[] for _ in 1:length(_formulas)]

    # these are the leftovers of _instances, that is, the models that are not pushed to it
    _failed_instances = [KripkeStructure[] for _ in 1:length(_formulas)]

    # the "previously seen" formulas;
    # the first conjunction, c1, is taken as-is;
    # the second one, c2, will be transformed into CONJUNCTION(NEGATION(c1), c2) and so on.
    previous_formulas = SyntaxBranch[]

    for (i,formula) in enumerate(_formulas)
        # we cannot overcome `maxiterations` number of iterations
        _niterations = 0

        # ideally, we want `istancesperclass` number of instances;
        # this is the counter keeping track of the instances that are OK.
        _okinstances = 0

        # this formula is the one we are reading in this cycle, and must be completely
        # disjoint from all the previously seen formulas
        current_formula = CONJUNCTION(formula, NEGATION.(previous_formulas)...)

        # the current formula is saved for properly generate the next labels
        push!(previous_formulas, formula)

        # for each instance generated in the previous cycles,
        # try to see if `current_formula` is satisfied by the instance.
        for failed_instance in _failed_instances
            if check(current_formula, failed_instance, World(1))
                push!(_instances[i], failed_instance)
                _okinstances = _okinstances + 1
            end
        end

        # this is the standard, core logic
        while (_niterations <= maxiterations) && (_okinstances < istancesperclass)
            model = randmodel(rng, nworlds, nedges, _atoms, truthvalues; kwargs...)

            # if the random model satisfied the `current_formula`, we consider it in the final logiset;
            # otherwise, we keep the model for the future class formulas.
            if check(current_formula, model, World(1))
                # the generated model is ok, so we push it and update its related counter
                push!(_instances[i], model)
                _okinstances = _okinstances + 1
            else
                # unfortunately, the model is not ok
                push!(_failed_instances[i], model)
            end

            _niterations = _niterations + 1
        end

        if _niterations > maxiterations
            silent || println(
                "The maximum number of iterations ($maxiterations) has been reached.")
        end
    end

    return _instances, _failed_instances
end
