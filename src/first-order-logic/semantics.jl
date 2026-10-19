############################################################################################
#### First-order structures ################################################################
############################################################################################

"""
    struct FOStructure{D} <: Interpretation
        domain::Vector{D}
        constants::Dict{Constant,D}
        functions::Dict{FunctionSymbol,Any}
        predicates::Dict{Predicate,Any}
    end

A finite first-order structure (or model), consisting of a `domain` of
elements of type `D`, and of an interpretation for constant, function and predicate
symbols.

- A `Constant` is mapped to a domain element.
- A `FunctionSymbol` of arity `n` is mapped either to a Julia callable taking `n` domain
  elements, or to a dictionary whose keys are `n`-tuples of domain elements (for unary
  functions, keys can also be plain elements).
- A `Predicate` of arity `n` is mapped either to a Julia callable taking `n` domain
  elements and returning a `Bool`, or to a collection (e.g., a `Set`) of `n`-tuples
  (for unary predicates, the collection can also contain plain elements), or to a `Bool`
  for nullary predicates.

[`EQUALITY`](@ref) is interpreted as `isequal`, unless the structure interprets it
explicitly.

Formulas are interpreted (or checked) on a structure with respect to an *assignment*
for their free variables, which is passed via the keyword argument `assignment` (an
`AbstractDict` mapping `Variable`s to domain elements).

# Examples
```julia-repl
julia> @variables x y; zero = Constant(:zero); succ = FunctionSymbol(:succ, 1);

julia> Less = Predicate(:Less, 2);

julia> M = FOStructure(0:3;
           constants = [zero => 0],
           functions = [succ => (n -> min(n + 1, 3))],
           predicates = [Less => <]);

# For every variable assignment, a less-or-equal variable value exists
julia> check(∀(x, ∃(y, Less(x, y) ∨ (x ≐ y))), M)
true

julia> check(Less(zero, x), M; assignment = Dict(x => 2))
true
```

See also [`evaluate`](@ref), [`interpret`](@ref), [`check`](@ref), [`domain`](@ref).
"""
struct FOStructure{D} <: Interpretation
    domain::Vector{D}
    constants::Dict{Constant, D}
    functions::Dict{FunctionSymbol, Any}
    predicates::Dict{Predicate, Any}

    function FOStructure{D}(
            domain::AbstractVector,
            constants::AbstractDict,
            functions::AbstractDict,
            predicates::AbstractDict,
    ) where {D}
        for (c, d) in constants
            d in domain || throw(ArgumentError(
                "Constant $(syntaxstring(c)) is mapped to $(d), which is not in the domain."))
        end
        new{D}(domain, constants, functions, predicates)
    end
end

function FOStructure(
        domain;
        constants = (),
        functions = (),
        predicates = (),
)
    dom = collect(domain)
    D = eltype(dom)
    return FOStructure{D}(
        dom,
        Dict{Constant, D}(constants...),
        Dict{FunctionSymbol, Any}(functions...),
        Dict{Predicate, Any}(predicates...),
    )
end

"""
    domain(M::FOStructure)::Vector

Return the domain of a first-order structure.
"""
domain(M::FOStructure) = M.domain

function Base.show(io::IO, M::FOStructure{D}) where {D}
    print(io,
        "FOStructure{$D} with $(length(domain(M))) elements, " *
        "$(length(M.constants)) constants, $(length(M.functions)) functions, " *
        "$(length(M.predicates)) predicates",)
end

############################################################################################
#### Assignments ###########################################################################
############################################################################################

# This is used internally and should not be exported
const Assignment = Base.ImmutableDict{Variable, Any}

_assignment(σ::Assignment) = σ
function _assignment(σ)
    foldl((acc, p) -> Base.ImmutableDict(acc, p.first => p.second), σ;
        init = Assignment(),)
end

# Binding a variable is O(1), and shadows a previous binding of the same variable
_bind(σ, x::Variable, d) = Base.ImmutableDict(_assignment(σ), x => d)

############################################################################################
#### Term evaluation #######################################################################
############################################################################################

"""
    evaluate(t::Term, M::FOStructure, assignment = Dict())

Return the domain element denoted by the term `t` on the structure `M`, given an
assignment for the variables in `t`.
An error is raised if `t` contains an unassigned variable, or a symbol which is not
interpreted by `M`.

# Examples
```julia-repl
julia> using SoleLogics, SoleLogics.FirstOrderLogics

julia> @variables x y;
julia> zero = Constant(:zero);
julia> succ = FunctionSymbol(:succ, 1);
julia> Less = Predicate(:Less, 2);

julia> M = FOStructure(0:3;
    constants  = [zero => 0],
    functions  = [succ => (n -> min(n + 1, 3))],
    predicates = [Less => <])

# a constant evaluates to the element the structure assigns it
julia> evaluate(zero, M)                        
0

# a function term applies the function's Julia implementation, inside out
julia> evaluate(succ(succ(zero)), M)            
2

# a term with a variable needs an assignment, given as the third argument
julia> evaluate(succ(x), M, Dict(x => 1))       
2

# the domain of M is limited to 3
julia> evaluate(succ(succ(succ(succ(x)))), M, Dict(x => 1)) 
3

# of course, zero is always less than its successor
julia> check(Less(zero, succ(x)), M; assignment = Dict(x => 0)) 
true

# error! the variable x is free and has no value in the assignment.
julia> try
        evaluate(succ(x), M)
    catch e
        println("Error: ", sprint(showerror, e))
    end

# error! the constant :one is not defined in the structure M
try
    evaluate(Constant(:one), M)
catch e
    println("Error: ", sprint(showerror, e))
end
```

See also [`FOStructure`](@ref), [`Term`](@ref).
"""
function evaluate(x::Variable, ::FOStructure, assignment = Assignment())
    haskey(assignment, x) ||
        error("Variable $(syntaxstring(x)) is free and has no value in the assignment.")
    return assignment[x]
end

function evaluate(c::Constant, M::FOStructure, assignment = Assignment())
    haskey(M.constants, c) ||
        error("Constant $(syntaxstring(c)) is not interpreted by the structure.")
    return M.constants[c]
end

function evaluate(t::FunctionTerm, M::FOStructure, assignment = Assignment())
    f = functionsymbol(t)
    haskey(M.functions, f) ||
        error("Function symbol $(syntaxstring(f)) is not interpreted by the structure.")
    args = map(a -> evaluate(a, M, assignment), arguments(t))
    return _apply(M.functions[f], args)
end

function _apply(f::AbstractDict, args::Tuple)
    (length(args) == 1 && !haskey(f, args)) ? f[only(args)] : f[args]
end
_apply(f, args::Tuple) = f(args...)

############################################################################################
#### Formula interpretation ################################################################
############################################################################################

function _holds(I::Bool, args::Tuple)
    isempty(args) ? I :
    error("A Bool can only interpret a nullary predicate.")
end
function _holds(I::Union{AbstractSet, AbstractVector}, args::Tuple)
    (length(args) == 1 && only(args) in I) || args in I
end
_holds(I, args::Tuple) = I(args...)::Bool

"""
    interpret(φ::Formula, M::FOStructure; assignment = Dict())::BooleanTruth

Interpret a first-order formula on a structure, under an assignment of its free variables,
returning `⊤` or `⊥`.
Quantifiers range over the domain of `M`, and short-circuit as soon as the result is known.
Note that `check(φ, M; assignment)` returns the corresponding `Bool`.

See also [`FOStructure`](@ref), [`evaluate`](@ref), [`issentence`](@ref).
"""
function interpret(
        a::PredicateAtom,
        M::FOStructure,
        args...;
        assignment = Assignment(),
        kwargs...,
)
    p = predicate(a)
    vals = map(t -> evaluate(t, M, assignment), arguments(a))
    holds = begin
        if haskey(M.predicates, p)
            _holds(M.predicates[p], vals)
        elseif p == EQUALITY
            isequal(vals...)
        else
            error("Predicate $(syntaxstring(p)) is not interpreted by the structure.")
        end
    end
    return holds ? TOP : BOT
end

function interpret(
        φ::AbstractSyntaxBranch,
        M::FOStructure,
        args...;
        assignment = Assignment(),
        kwargs...,
)
    tok = token(φ)
    if isquantifier(tok)
        return _quantify(tok, children(φ)[1], M, assignment, args...; kwargs...)
    else
        ts = map(ch -> interpret(ch, M, args...; assignment = assignment, kwargs...),
            children(φ),)
        return simplify(tok, ts)
    end
end

function _quantify(q::ForAll, body, M::FOStructure, assignment, args...; kwargs...)
    for d in domain(M)
        σ = _bind(assignment, variable(q), d)
        isbot(interpret(body, M, args...; assignment = σ, kwargs...)) && return BOT
    end
    return TOP
end

function _quantify(q::Exists, body, M::FOStructure, assignment, args...; kwargs...)
    for d in domain(M)
        σ = _bind(assignment, variable(q), d)
        istop(interpret(body, M, args...; assignment = σ, kwargs...)) && return TOP
    end
    return BOT
end

function _quantify(q::AbstractQuantifier, args...; kwargs...)
    error("Please, provide method " *
          "FirstOrderLogics._quantify(::$(typeof(q)), body, M, assignment, args...).")
end
