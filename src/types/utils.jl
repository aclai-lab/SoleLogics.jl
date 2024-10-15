# ---------------------------------------------------------------------------------------- #
#           definition of AbstractConnective, taken from syntactical, line 120             #
# ---------------------------------------------------------------------------------------- #
# abstract type AbstractConnective <: Syntactical end

# ---------------------------------------------------------------------------------------- #
#                                        Functions                                         #
# ---------------------------------------------------------------------------------------- #
function collatetruth end
function simplify end

function precedence end
function associativity end

function randatom end
function formulas end
function _baselogic end

# ---------------------------------------------------------------------------------------- #
#                                       collatetruth                                       #
# ---------------------------------------------------------------------------------------- #
"""
    collatetruth(c::AbstractConnective, ts::NTuple{N,T where T<:Truth})::Truth where {N}

Return the truth value for a composed formula `c(t1, ..., tN)`, given the `N`
with t1, ..., tN being `Truth` values.

See also [`simplify`](@ref), [`AbstractConnective`](@ref), [`Truth`](@ref).
"""
function collatetruth(c::AbstractConnective, ts::NTuple{N, T where T <: Truth})::Truth where {N}
    if arity(c) != length(ts)
        return error("Cannot collate $(length(ts)) truth values for " *
                     "connective $(typeof(c)) with arity $(arity(c))).")
    else
        return error("Please, provide method collatetruth(::$(typeof(c)), " *
                     "::NTuple{$(arity(c)),$(T)}).")
    end
end

# Helper (so that collatetruth work for all operators)
collatetruth(t::Truth, ::Tuple{}) = t

# ---------------------------------------------------------------------------------------- #
#                                         connectives                                      #
# ---------------------------------------------------------------------------------------- #
"""
    connectives(g::AbstractGrammar)

List all connectives appearing in a grammar.

See also [`AbstractConnective`](@ref), [`nconnectives`](@ref).
"""
function connectives(g::AbstractGrammar)::AbstractVector{AbstractConnective}
    return filter(!isnullary, operators(g))
end

# ---------------------------------------------------------------------------------------- #
#                                           leaves                                         #
# ---------------------------------------------------------------------------------------- #
"""
    leaves(g::AbstractGrammar)

List all leaves appearing in a grammar.

See also [`SyntaxLeaf`](@ref), [`nleaves`](@ref).
"""
function leaves(g::AbstractGrammar)
    return [atoms(alphabet(g))..., filter(isnullary, operators(g))...]
end

# ---------------------------------------------------------------------------------------- #
#                                         simplify                                         #
# ---------------------------------------------------------------------------------------- #
"""
    simplify(c::AbstractConnective, ts::NTuple{N,F where F<:Formula})::Truth where {N}

Return a formula with the same semantics of a composed formula `c(φ1, ..., φN)`,
given the `N`
immediate sub-formulas.

See also [`collatetruth`](@ref), [`AbstractConnective`](@ref), [`Formula`](@ref).
"""
function simplify(c::AbstractConnective, φs::NTuple{N, T where T <: Formula}) where {N}
    c(φs)
end

function simplify(c::AbstractConnective, φs::NTuple{N, T where T <: Truth}) where {N}
    collatetruth(c, φs)
end

