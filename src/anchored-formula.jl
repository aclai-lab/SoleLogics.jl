# TODO: see check_tree optional parameter
"""
    struct AnchoredFormula{L<:AbstractLogic} <: Formula
        _logic::Base.RefValue{L}
        synstruct::SyntaxStructure
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
```julia-repl
julia> φ = parsebaseformula("◊(p→q)");

julia> f2 = φ(parseformula("p"));

julia> syntaxstring(φ)
"◊(→(p, q))"

julia> syntaxstring(f2)
"p"

julia> @assert logic(φ) == logic(f2)

julia> @assert ◊ in operators(logic(f2))

julia> @assert ◊ isa operatorstype(logic(f2))

```

See also [`AbstractLogic`](@ref), [`logic`](@ref), [`SyntaxToken`](@ref),
[`SyntaxBranch`](@ref), [`tree`](@ref).
"""
struct AnchoredFormula{L<:AbstractLogic} <: Formula
    _logic::Base.RefValue{L}
    synstruct::SyntaxStructure

    _l(l::AbstractLogic) = Base.RefValue(l)
    _l(l::Base.RefValue) = l

    function AnchoredFormula{L}(
        l::Union{L,Base.RefValue{L}},
        synstruct::SyntaxStructure;
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
    #     synstruct::SyntaxStructure;
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

_logic(φ::AnchoredFormula) = φ._logic

"""
    logic(φ::AnchoredFormula)::AbstractLogic

Return the logic of an anchored formula

See [`AnchoredFormula`](@ref).
"""
logic(φ::AnchoredFormula) = φ._logic[]

"""
    synstruct(φ::AnchoredFormula)::SyntaxStructure

Return the syntactic component of an anchored formula.

See [`AnchoredFormula`](@ref).
"""
synstruct(φ::AnchoredFormula) = φ.synstruct
tree(φ::AnchoredFormula) = tree(φ.synstruct)

function Base.show(io::IO, φ::AnchoredFormula)
    println(io, "AnchoredFormula: $(syntaxstring(φ))")
    print(io, "Anchored to logic: ")
    Base.show(io, logic(φ))
end

# Note that, since `c` might not be in the logic of the child formulas,
#  the resulting formula may be of a different logic.
function composeformulas(c::Connective, φs::NTuple{N,AnchoredFormula}) where {N}
    ls = unique(logic.(φs)) # Uses Base.isequal
    @assert length(ls) == 1 "Cannot " *
                "compose an anchored formula from formulas anchored to different logics: $(ls)."
    l = first(ls)
    # "TODO expand logic's set of operators (c is not in it: $(typeof(c)) ∉ $(operatorstype(l)))."
    @assert typeof(c) <: operatorstype(l) "Cannot join $(N) formulas via operator $(c): " *
        "this operator does not belong to the logic. $(typeof(c)) <: $(operatorstype(l)) should hold!"
    return AnchoredFormula(l, composeformulas(c, synstruct.(φs)))
end

# When constructing a new formula from a syntax tree, the logic is passed by reference.
(φ::AnchoredFormula)(t::SyntaxStructure, args...) = AnchoredFormula(_logic(φ), t, args...)

# A logic can be used to instantiate `AnchoredFormula`s out of syntax trees.
(l::AbstractLogic)(t::SyntaxStructure, args...) = AnchoredFormula(Base.RefValue(l), t; args...)

# Adapted from https://github.com/JuliaLang/julia/blob/master/base/promotion.jl
function Base._promote(x::AnchoredFormula, y::SyntaxStructure)
    @inline
    return (x, x(y))
end
Base._promote(x::SyntaxStructure, y::AnchoredFormula) = reverse(Base._promote(y, x))

iscrisp(φ::AnchoredFormula) = iscrisp(logic(φ))
grammar(φ::AnchoredFormula) = grammar(logic(φ))
algebra(φ::AnchoredFormula) = algebra(logic(φ))

syntaxstring(φ::AnchoredFormula; kwargs...) = syntaxstring(φ.synstruct; kwargs...)

############################################################################################

subformulas(φ::AnchoredFormula; kwargs...) = φ.(subformulas(tree(φ); kwargs...))
normalize(φ::AnchoredFormula; kwargs...) = φ(normalize(tree(φ); kwargs...))

"""
    function baseformula(
        φ::Formula;
        infer_logic = true,
        additional_operators::Union{Nothing,Vector{<:Operator}} = nothing,
        kwargs...
    )

Attempt at instantiating a `AnchoredFormula` from a syntax token/formula,
by inferring the logic it belongs to. If `infer_logic` is true, then
a canonical logic (e.g., propositional logic with all the `BASE_PROPOSITIONAL_CONNECTIVES`) is
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

julia> unique(operators(logic(SoleLogics.baseformula(t; additional_operators = SoleLogics.BASE_MODAL_CONNECTIVES))))
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
        if issubset(conns, BASE_PROPOSITIONAL_CONNECTIVES)
            propositionallogic(;
                operators = (infer_logic ? BASE_PROPOSITIONAL_CONNECTIVES : ops),
                kwargs...,
            )
        elseif issubset(conns, BASE_MODAL_CONNECTIVES)
            modallogic(;
                operators = (infer_logic ? BASE_MODAL_CONNECTIVES : ops),
                default_operators = BASE_MODAL_CONNECTIVES,
                kwargs...,
            )
        elseif issubset(conns, BASE_MULTIMODAL_CONNECTIVES)
            modallogic(;
                operators = (infer_logic ? BASE_MULTIMODAL_CONNECTIVES : ops),
                default_operators = BASE_MULTIMODAL_CONNECTIVES,
                kwargs...,
            )
        else
            unknown_ops = setdiff(conns, BASE_PROPOSITIONAL_CONNECTIVES, BASE_MODAL_CONNECTIVES, BASE_MULTIMODAL_CONNECTIVES)
            error("Could not infer logic from object of type $(typeof(φ)): $(t). Unknown operators: $(unknown_ops).")
        end
    end
    AnchoredFormula(logic, t)
end

############################################################################################

"""
    parsebaseformula(
        expr::String,
        additional_operators::Union{Nothing,Vector{<:Operator}} = nothing;
        operators::Union{Nothing,Vector{<:Operator}},
        grammar::Union{Nothing,AbstractGrammar} = nothing,
        algebra::Union{Nothing,AbstractAlgebra} = nothing,
        kwargs...
    )::AnchoredFormula

Return a `AnchoredFormula` which is the result of parsing an expression
via the [Shunting yard](https://en.wikipedia.org/wiki/Shunting_yard_algorithm) algorithm.
By default, this function is only able to parse operators in
`SoleLogics.BASE_PARSABLE_CONNECTIVES`; additional operators may be provided as
a second argument.

The `grammar` and `algebra` of the associated logic is inferred using
the `baseformula` function from the operators encountered
in the expression, and those in `additional_operators`.

See [`parseformula`](@ref), [`baseformula`](@ref), [`BASE_PARSABLE_CONNECTIVES`](@ref).
"""
parsebaseformula(expr::String, args...; kwargs...) = parseformula(AnchoredFormula, expr, args...; kwargs...)

function parseformula(
    ::Type{AnchoredFormula},
    expr::String,
    additional_operators::Union{Nothing,Vector{<:Operator}} = nothing;
    # TODO add alphabet parameter add custom parser for atoms
    # alphabet::Union{Nothing,Vector,AbstractAlphabet} = nothing,
    grammar::Union{Nothing,AbstractGrammar} = nothing,
    algebra::Union{Nothing,AbstractAlgebra} = nothing,
    kwargs...
)
    additional_operators =
        (isnothing(additional_operators) ? Operator[] : additional_operators)

    t = parseformula(SyntaxTree, expr, additional_operators; kwargs...)
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
    expr::String,
    logic::AbstractLogic;
    kwargs...,
)
    AnchoredFormula(logic, parseformula(SyntaxTree, expr, operators(logic); kwargs...))
end

"""$(doc_randformula)"""
function randbaseformula(
    height::Integer,
    g::AbstractGrammar;
    kwargs...
)::AnchoredFormula
    _alphabet = alphabet(g)
    _operators = operators(g)
    baseformula(
        randformula(height, _alphabet, _operators; kwargs...);
        alphabet = _alphabet,
        additional_operators = _operators
    )
end

function randbaseformula(
    height::Integer,
    alphabet,
    operators::AbstractVector{<:Operator};
    kwargs...
)::AnchoredFormula
    alphabet = convert(AbstractAlphabet, alphabet)
    baseformula(
        randformula(height, alphabet, operators; kwargs...);
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
    randbaseformula(height, alphabet(g), operators(g), args...; rng=rng, kwargs...)
end
