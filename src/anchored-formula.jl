# TODO: see check_tree optional parameter
"""
    struct AnchoredFormula{L<:AbstractLogic} <: Formula
        _logic::Base.RefValue{L}
        synstruct::AbstractSyntaxStructure
    end

A formula anchored to a logic of type `L`, and wrapping a syntax structure.
The structure encodes a formula belonging to the grammar of the logic, and the truth of the
formula can be evaluated on interpretations of the same logic. Note that, here, the logic is
represented by a reference.

Upon construction, the logic can be passed either directly, or via a RefValue.
Additionally, the following keyword arguments may be specified:
- `check_atoms::Bool = false`: whether to perform or not a check that the atoms
    belong to the alphabet of the logic;
- `check_tree::Bool = false`: whether to perform or not a check that the formula's
    syntactic structure honors the grammar
    (includes the check performed with `check_atoms = true`);

*Cool feature*: a `AnchoredFormula` can be used for instating other formulas of the same logic.
See the examples.

# Examples
```jldoctest
julia> f = parsebaseformula("◊(p→q)");

julia> f2 = f(parseformula("p"));

julia> syntaxstring(f)
"◊(→(p, q))"

julia> syntaxstring(f2)
"p"

julia> @assert logic(f) == logic(f2)

julia> @assert ◊ in operators(logic(f2))

julia> @assert ◊ isa operatorstype(logic(f2))

```

See also [`AbstractLogic`](@ref), [`logic`](@ref), [`SyntaxToken`](@ref),
[`SyntaxBranch`](@ref), [`tree`](@ref).
"""
struct AnchoredFormula{L<:AbstractLogic} <: Formula
    _logic::Base.RefValue{L}
    synstruct::AbstractSyntaxStructure

    _l(l::AbstractLogic) = Base.RefValue(l)
    _l(l::Base.RefValue) = l

    function AnchoredFormula{L}(
        l::Union{L,Base.RefValue{L}},
        synstruct::AbstractSyntaxStructure;
        check_atoms::Bool = false,
        check_tree::Bool = false,
    ) where {L<:AbstractLogic}
        _logic = _l(l)

        if check_tree
            return error("TODO implement check_tree parameter when constructing AnchoredFormula's!")
        end
        # Check that the atoms belong to the alphabet of the logic
        if !check_tree && check_atoms
            @assert all([p in alphabet(_logic[])
                         for p in unique(atoms(synstruct))]) "Cannot " *
                           "instantiate AnchoredFormula{$(L)} with illegal atoms: " *
                           "$(filter((p)->!(p in alphabet(_logic[])), unique(atoms(synstruct))))"
        end

        # Check that the token types of the tree are a subset of the tokens
        #  allowed by the logic
        if !(tokenstype(synstruct) <: tokenstype(_logic[]))
            return error("Out of grammar! Cannot " *
                 "instantiate AnchoredFormula{$(L)} with illegal token types `$(tokenstype(synstruct))`. " *
                 "Token types should be <: $(tokenstype(_logic[])).")
        end

        return new{L}(_logic, synstruct)
    end

    # function AnchoredFormula{L}(
    #     l::Union{L,Base.RefValue{L}},
    #     synstruct::AbstractSyntaxStructure;
    #     kwargs...
    # ) where {L<:AbstractLogic}
    #     t = convert(SyntaxTree, synstruct)
    #     return AnchoredFormula{L,typeof(t)}(l, t; kwargs...)
    # end

    function AnchoredFormula(
        l::Union{L,Base.RefValue{L}},
        synstruct;
        kwargs...
    ) where {L<:AbstractLogic}
        return AnchoredFormula{L}(l, synstruct; kwargs...)
    end
end

_logic(f::AnchoredFormula) = f._logic
logic(f::AnchoredFormula) = f._logic[]
synstruct(f::AnchoredFormula) = f.synstruct
tree(f::AnchoredFormula) = tree(f.synstruct)

function Base.show(io::IO, f::AnchoredFormula)
    println(io, "AnchoredFormula: $(syntaxstring(f))")
    print(io, "Anchored to logic: ")
    Base.show(io, logic(f))
end

# Note that, since `c` might not be in the logic of the child formulas,
#  the resulting formula may be of a different logic.
function composeformulas(c::Connective, children::NTuple{N,AnchoredFormula}) where {N}
    ls = unique(logic.(children)) # Uses Base.isequal
    @assert length(ls) == 1 "Cannot " *
                "build formula by combination of formulas with different logics: $(ls)."
    l = first(ls)
    # "TODO expand logic's set of operators (c is not in it: $(typeof(c)) ∉ $(operatorstype(l)))."
    @assert typeof(c) <: operatorstype(l) "Cannot join $(N) formulas via operator $(c): " *
        "this operator does not belong to the logic. $(typeof(c)) <: $(operatorstype(l)) should hold!"
    return AnchoredFormula(l, composeformulas(c, synstruct.(children)))
end

# When constructing a new formula from a syntax tree, the logic is passed by reference.
(f::AnchoredFormula)(t::AbstractSyntaxStructure, args...) = AnchoredFormula(_logic(f), t, args...)

# A logic can be used to instantiate `AnchoredFormula`s out of syntax trees.
(l::AbstractLogic)(t::AbstractSyntaxStructure, args...) = AnchoredFormula(Base.RefValue(l), t; args...)

# Adapted from https://github.com/JuliaLang/julia/blob/master/base/promotion.jl
function Base._promote(x::AnchoredFormula, y::AbstractSyntaxStructure)
    @inline
    return (x, x(y))
end
Base._promote(x::AbstractSyntaxStructure, y::AnchoredFormula) = reverse(Base._promote(y, x))

iscrisp(f::AnchoredFormula) = iscrisp(logic(f))
grammar(f::AnchoredFormula) = grammar(logic(f))
algebra(f::AnchoredFormula) = algebra(logic(f))

syntaxstring(f::AnchoredFormula; kwargs...) = syntaxstring(f.synstruct; kwargs...)

############################################################################################


subformulas(f::AnchoredFormula; kwargs...) = f.(subformulas(tree(f); kwargs...))
normalize(f::AnchoredFormula; kwargs...) = f(normalize(tree(f); kwargs...))

"""
    function baseformula(
        φ::Formula;
        infer_logic = true,
        additional_operators::Union{Nothing,Vector{<:Operator}} = nothing,
        kwargs...,
    )

Attempt at instantiating a `AnchoredFormula` from a syntax token/formula,
by inferring the logic it belongs to. If `infer_logic` is true, then
a canonical logic (e.g., propositional logic with all the `BASE_PROPOSITIONAL_OPERATORS`) is
inferred; if it's false, then a logic with exactly the operators appearing in the syntax tree,
plus the `additional_operators` is instantiated.

# Examples
```julia-repl
julia> t = parseformula("◊((p∧q)→r)");

julia> unique(operators(logic(SoleLogics.baseformula(t))))
3-element Vector{Union{SoleLogics.NamedConnective{:→}, SoleLogics.NamedConnective{:◊}, SoleLogics.NamedConnective{:∧}}}:
 ∧
 ◊
 →

julia> unique(operators(logic(SoleLogics.baseformula(t; additional_operators = SoleLogics.BASE_MODAL_OPERATORS))))
8-element Vector{Union{SoleLogics.BottomOperator, SoleLogics.NamedConnective{:¬}, SoleLogics.NamedConnective{:∧}, SoleLogics.NamedConnective{:∨}, SoleLogics.NamedConnective{:→}, SoleLogics.NamedConnective{:◊}, SoleLogics.NamedConnective{:□}, SoleLogics.TopOperator}}:
 ¬
 ∧
 ∨
 →
 ◊
 □
```
"""
function baseformula(
    φ::Formula;
    infer_logic = true,
    additional_operators::Union{Nothing,Vector{<:Operator}} = nothing,
    kwargs...,
)
    t = convert(SyntaxTree, φ)

    ops = isnothing(additional_operators) ? SoleLogics.operators(t) : additional_operators
    ops = unique(ops)
    conns = filter(o->o isa Connective, ops)
    # operators = unique([additional_operators..., ops...])
    # props = atoms(t)

    logic = begin
        if issubset(conns, BASE_PROPOSITIONAL_OPERATORS)
            propositionallogic(;
                operators = (infer_logic ? BASE_PROPOSITIONAL_OPERATORS : ops),
                kwargs...,
            )
        elseif issubset(conns, BASE_MODAL_OPERATORS)
            modallogic(;
                operators = (infer_logic ? BASE_MODAL_OPERATORS : ops),
                default_operators = BASE_MODAL_OPERATORS,
                kwargs...,
            )
        elseif issubset(conns, BASE_MULTIMODAL_OPERATORS)
            modallogic(;
                operators = (infer_logic ? BASE_MULTIMODAL_OPERATORS : ops),
                default_operators = BASE_MULTIMODAL_OPERATORS,
                kwargs...,
            )
        else
            unknown_ops = setdiff(conns, BASE_PROPOSITIONAL_OPERATORS, BASE_MODAL_OPERATORS, BASE_MULTIMODAL_OPERATORS)
            error("Could not infer logic from object of type $(typeof(φ)): $(t). Unknown operators: $(unknown_ops).")
        end
    end
    AnchoredFormula(logic, t)
end

############################################################################################



"""
    parsebaseformula(
        expression::String,
        additional_operators::Union{Nothing,Vector{<:Operator}} = nothing;
        operators::Union{Nothing,Vector{<:Operator}},
        grammar::Union{Nothing,AbstractGrammar} = nothing,
        algebra::Union{Nothing,AbstractAlgebra} = nothing,
        kwargs...
    )::AnchoredFormula

Return a `AnchoredFormula` which is the result of parsing `expression`
 via the [Shunting yard](https://en.wikipedia.org/wiki/Shunting_yard_algorithm)
 algorithm.
By default, this function is only able to parse operators in
`SoleLogics.BASE_PARSABLE_OPERATORS`; additional operators may be provided as
a second argument.

The `grammar` and `algebra` of the associated logic is inferred using
the `baseformula` function from the operators encountered
in the expression, and those in `additional_operators`.

See [`parseformula`](@ref), [`baseformula`](@ref).
"""
parsebaseformula(expr::String, args...; kwargs...) = parseformula(AnchoredFormula, expr, args...; kwargs...)

function parseformula(
    ::Type{AnchoredFormula},
    expression::String,
    additional_operators::Union{Nothing,Vector{<:Operator}} = nothing;
    # TODO add alphabet parameter add custom parser for atoms
    # alphabet::Union{Nothing,Vector,AbstractAlphabet} = nothing,
    grammar::Union{Nothing,AbstractGrammar} = nothing,
    algebra::Union{Nothing,AbstractAlgebra} = nothing,
    kwargs...
)
    additional_operators =
        (isnothing(additional_operators) ? Operator[] : additional_operators)

    t = parseformula(SyntaxTree, expression, additional_operators; kwargs...)
    baseformula(t;
        # additional_operators = unique(Operator[operators..., SoleLogics.operators(t)...]),
        additional_operators = length(additional_operators) == 0 ? nothing :
            unique(Operator[additional_operators..., SoleLogics.operators(t)...]),
        # alphabet = alphabet,
        alphabet = AlphabetOfAny{String}(),
        grammar = grammar,
        algebra = algebra
    )
end

function parseformula(
    ::Type{AnchoredFormula},
    expression::String,
    logic::AbstractLogic;
    kwargs...,
)
    AnchoredFormula(logic, parseformula(SyntaxTree, expression, operators(logic); kwargs...))
end



"""$(doc_randformula)"""
function randbaseformula(
    height::Integer,
    g::AbstractGrammar;
    picker::Function=rand,
    kwargs...
)::AnchoredFormula
    _alphabet = alphabet(g)
    _operators = operators(g)
    baseformula(
        randformula(height, _alphabet, _operators; picker=picker, kwargs...);
        alphabet = _alphabet,
        additional_operators = _operators
    )
end

function randbaseformula(
    height::Integer,
    alphabet,
    operators::AbstractVector{<:Operator};
    picker::Function=rand,
    kwargs...
)::AnchoredFormula
    alphabet = convert(AbstractAlphabet, alphabet)
    baseformula(
        randformula(height, alphabet, operators; picker=picker, kwargs...);
        alphabet = alphabet,
        additional_operators = operators,
    )
end

function randbaseformula(
    height::Integer,
    g::AbstractGrammar,
    args...;
    rng::Union{Integer,AbstractRNG} = Random.GLOBAL_RNG,
    kwargs...
)::AnchoredFormula
    randbaseformula(height, alphabet(g), operator(g), args...; rng=rng, kwargs...)
end
