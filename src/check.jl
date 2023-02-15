
"""
    subformulas(f::Formula)

Return each `Formula` in a tree, sorted by size
"""
subformulas(f::Formula; kwargs...) = f.(subformulas(tree(f); kwargs...))
function subformulas(t::SyntaxTree; sorted=true)
    # function _subformulas(_t::SyntaxTree)
    #     SyntaxTree{tokenstype(t)}[
    #         (map(SyntaxTree{tokenstype(t)}, Iterators.flatten(subformulas.(children(_t)))))...,
    #         SyntaxTree{tokenstype(t)}(_t)
    #     ]
    # end
    function _subformulas(_t::SyntaxTree)
        SyntaxTree[
            (Iterators.flatten(subformulas.(children(_t))))...,
            _t
        ]
    end
    ts = _subformulas(t)
    if sorted
        sort!(ts, by = t -> SoleLogics.height(t))
    end
    ts
end

"""
TODO
"""
normalize(f::Formula; kwargs...) = f(normalize(tree(f); kwargs...))
function normalize(t::SyntaxTree; remove_boxes=true, reduce_negations=true, allow_inverse_propositions=true)
    tok, ch = token(t), children(t)
    if remove_boxes && tok isa BoxRelationalOperator && arity(tok) == 1
        # remove_boxes -> substitute every [X]φ with ¬⟨X⟩¬φ
        childtok = ch[1]
        dual_op = dual(tok)
        ¬(dual_op(normalize(¬childtok)))
        # TODO remove
        # if relation(tok) == RelationGlob
        #     # Special case: [G]φ -> ⟨G⟩φ
        #     dual_op(normalize(childtok))
        # else
        #     ¬(dual_op(normalize(¬childtok)))
        # end
    elseif reduce_negations && (tok == ¬) && arity(tok) == 1
        # reduce_negations
        childtok = ch[1]
        grandchildren = children(childtok)
        if (token(childtok) == ∨) && arity(token(childtok)) == 2
            ∧(normalize(¬(grandchildren[1])), normalize(¬(grandchildren[2])))
            # TODO use implication, maybe it's more interpretable?
        elseif (token(childtok) == ∧) && arity(token(childtok)) == 2
            ∨(normalize(¬(grandchildren[1])), normalize(¬(grandchildren[2])))
        elseif (token(childtok) == →) && arity(token(childtok)) == 2
            # normalize(∨(¬(grandchildren[1]), grandchildren[2]))
            ∨(normalize(grandchildren[1]), normalize(¬(grandchildren[2])))
        elseif (token(childtok) == ¬) && arity(token(childtok)) == 1
            normalize(grandchildren[1])
        elseif token(childtok) isa Proposition
            if allow_inverse_propositions
                SyntaxTree(inverse(token(childtok)))
            else
                ¬(normalize(childtok))
            end
        elseif token(childtok) isa SoleLogics.AbstractRelationalOperator && arity(token(childtok)) == 1
            dual_op = dual(token(childtok))
            if remove_boxes && dual_op isa SoleLogics.BoxRelationalOperator
                ¬(normalize(childtok))
            else
                dual_op(normalize(¬(grandchildren[1])))
            end
        else
            error("Unknown token(childtok) when removing negations: $(token(childtok)) (type = $(typeof(token(childtok))))")
        end
    else
        SyntaxTree(tok, normalize.(ch))
    end
end

isglobal(f::Formula) = isglobal(tree(f))
isglobal(t::SyntaxTree) =
    (token(t) isa SoleLogics.AbstractRelationalOperator && relation(token(t)) == RelationGlob) ||
    (token(t) isa Union{typeof(∧),typeof(∨),typeof(¬),typeof(→)} && all(isglobal, children(t)))

"""
    collate_worlds(
        fr::AbstractFrame,
        op::AbstractOperator,
        t::NTuple{N, T},
    )::T where {N, T<:TruthValue}

TODO fix
Returns the truth value of a composed formula op(φ1, ..., φN), given the `N`
truth values of its immediate sub-formulas.
A frame must provide a `collate_worlds` method for each operator that can be
interpreted on it.

See also [`AbstractAlgebra`](@ref) [`AbstractOperator`](@ref), [`TruthValue`](@ref).
"""
function collate_worlds(
    fr::AbstractFrame{T},
    op::AbstractOperator,
    t::NTuple{N,T},
)::T where {N,T<:TruthValue}
    if truthtype(a) != T
        return error("Cannot collate $(length(t)) truth values of type $(T)" *
                     " with frame $(typeof(a)) with truth type $(truthtype(a))).")
    elseif arity(op) != length(t)
        return error("Cannot collate $(length(t)) truth values for" *
                     " operator $(typeof(op)) with arity $(arity(op))).")
    else
        return error("Please, provide method collate_worlds(::$(typeof(a)), ::$(typeof(op))," *
                     " ::NTuple{$(arity(op)), $(truthtype(a))}.")
    end
end

# Note: `collate_worlds` for TOP and BOTTOM relies on the `top` and `bottom` methods.
collate_worlds(fr::AbstractFrame{W,Bool}, ::typeof(⊤), ::NTuple{0}) where {W<:AbstractWorld} = allworlds(fr)
collate_worlds(::AbstractFrame{W,Bool}, ::typeof(⊥), ::NTuple{0}) where {W<:AbstractWorld} = W[]

# Standard semantics for NOT, AND, OR, IMPLIES
collate_worlds(fr::AbstractFrame{W,Bool}, ::typeof(¬), (ws,)::NTuple{1}) where {W<:AbstractWorld} = setdiff(allworlds(fr), ws)
collate_worlds(::AbstractFrame{W,Bool}, ::typeof(∧), (ws1, ws2)::NTuple{2}) where {W<:AbstractWorld} = intersect(ws1, ws2)
collate_worlds(::AbstractFrame{W,Bool}, ::typeof(∨), (ws1, ws2)::NTuple{2}) where {W<:AbstractWorld} = union(ws1, ws2)
collate_worlds(fr::AbstractFrame{W,Bool}, ::typeof(→), (ws1, ws2)::NTuple{2}) where {W<:AbstractWorld} = union(setdiff(allworlds(fr), ws1), ws2)

function collate_worlds(fr::AbstractMultiModalFrame{W,Bool}, op::DiamondRelationalOperator, (ws,)::NTuple{1}) where {W<:AbstractWorld}
    r = relation(op)
    if r == RelationGlob
        if length(ws) > 0
            collect(allworlds(fr))
        else
            W[]
        end
    else
        if hasconverse(r)
            union(W[], [accessibles(fr, w, converse(r)) for w in ws]...) # DIAMOND STRATEGY 1
        else
            filter(w1->intersects(ws, accessibles(fr, w1, r)), collect(allworlds(fr))) # DIAMOND STRATEGY 2
        end
    end
end

function collate_worlds(fr::AbstractMultiModalFrame{W,Bool}, op::BoxRelationalOperator, (ws,)::NTuple{1}) where {W<:AbstractWorld}
    r = relation(op)
    if r == RelationGlob
        if length(ws) == nworlds(fr) # Assuming no duplicates
            collect(allworlds(fr))
        else
            W[]
        end
    else
        if hasconverse(r)
            negws = setdiff(collect(allworlds(fr)), ws)
            negboxws = union(W[], [accessibles(fr, w, converse(r)) for w in negws]...)
            setdiff(collect(allworlds(fr)), negboxws) # BOX STRATEGY 1
            # filter(w1->all((w2)->w1 in accessibles(fr, w2, converse(r)), ws), collect(allworlds(fr))) # BOX STRATEGY 3
        else
            filter(w1->issubset(accessibles(fr, w1, r), ws), collect(allworlds(fr))) # BOX STRATEGY 2
        end
        # union(intersect(W[], [accessibles(fr, w, converse(r)) for w in ws]...)) # 10.957 s (158394802 allocations: 6.22 GiB), but probably wrong: does not include worlds for which it's trivially true
    end
end

