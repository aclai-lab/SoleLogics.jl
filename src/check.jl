
"""
    subformulas(f::AbstractFormula; sorted=true)

Return all sub-formulas (sorted by size when `sorted=true`)
of a given formula.

# Examples
```julia-repl
julia> syntaxstring.(SoleLogics.subformulas(parseformula("◊((p∧q)→r)")))
6-element Vector{String}:
 "p"
 "q"
 "r"
 "p ∧ q"
 "◊(p ∧ q)"
 "(◊(p ∧ q)) → r"
```

See also
[`SyntaxTree`](@ref), [`Formula`](@ref), [`AbstractFormula`](@ref).
"""
subformulas(f::AbstractSyntaxStructure; kwargs...) = subformulas(tree(f); kwargs...)
subformulas(f::Formula; kwargs...) = f.(subformulas(tree(f); kwargs...))
function subformulas(t::SyntaxTree; sorted=true)
    # function _subformulas(_t::SyntaxTree)
    #     SyntaxTree[
    #         (map(SyntaxTree, Iterators.flatten(subformulas.(children(_t)))))...,
    #         SyntaxTree(_t)
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

# TODO move to utils and rename "normalize" -> "transform"/"reshape"/"simplify"
# TODO \to diventano \lor
# TODO explain profile's and other parameters
"""
    normalize(
        f::AbstractFormula;
        remove_boxes = true,
        reduce_negations = true,
        allow_proposition_flipping = true,
    )

Return a modified version of a given formula, that has the same semantics
but different syntax. This is useful when dealing with the truth of many
(possibly similar) formulas; for example, when performing
[model checking](https://en.wikipedia.org/wiki/Model_checking).
BEWARE: it currently assumes the underlying algebra is Boolean!

# Arguments
- `f::AbstractFormula`: when set to `true`,
    the formula;
- `remove_boxes::Bool`: converts all uni-modal and multi-modal box operators by using the
    equivalence ◊φ ≡ ¬□¬φ. Note: this assumes an underlying Boolean algebra.
- `reduce_negations::Bool`: when set to `true`,
    attempts at reducing the number of negations by appling
    some transformation rules
    (e.g., [De Morgan's laws](https://en.wikipedia.org/wiki/De_Morgan%27s_laws)).
    Note: this assumes an underlying Boolean algebra.
- `allow_proposition_flipping::Bool`: when set to `true`,
    together with `reduce_negations=true`, this may cause the negation of a proposition
    to be replaced with the proposition with its [`negation`](@ref).

# Examples
```julia-repl
julia> f = parseformula("□¬((p∧¬q)→r)∧⊤");

julia> syntaxstring(f)
"(□(¬((p ∧ (¬(q))) → r))) ∧ ⊤"

julia> syntaxstring(SoleLogics.normalize(f; profile = :modelchecking, allow_proposition_flipping = false))
"¬(◊(((¬(p)) ∨ q) ∧ r))"

julia> syntaxstring(SoleLogics.normalize(f; profile = :readability, allow_proposition_flipping = false))
"□((p ∧ (¬(q))) ∨ (¬(r)))"
```

See also
[`SyntaxTree`](@ref), [`Formula`](@ref), [`AbstractFormula`](@ref).
"""
normalize(f::AbstractSyntaxStructure; kwargs...) = normalize(tree(f); kwargs...)
normalize(f::Formula; kwargs...) = f(normalize(tree(f); kwargs...))
function normalize(
    t::SyntaxTree;
    profile = :readability,
    remove_boxes = nothing,
    reduce_negations = nothing,
    simplify_constants = nothing,
    allow_proposition_flipping = nothing,
    forced_negation_removal = nothing,
)
    if profile == :readability
        if isnothing(remove_boxes)               remove_boxes = false end
        if isnothing(reduce_negations)           reduce_negations = true end
        if isnothing(simplify_constants)         simplify_constants = true end
        if isnothing(allow_proposition_flipping) allow_proposition_flipping = true end
        # TODO leave \to's instead of replacing them with \lor's...
    elseif profile == :modelchecking
        if isnothing(remove_boxes)               remove_boxes = true end
        if isnothing(reduce_negations)           reduce_negations = true end
        if isnothing(simplify_constants)         simplify_constants = true end
        if isnothing(allow_proposition_flipping) allow_proposition_flipping = true end
    else
        error("Unknown normalization profile: $(repr(profile))")
    end

    if isnothing(forced_negation_removal)
        if isnothing(allow_proposition_flipping)
            forced_negation_removal = true
        else
            forced_negation_removal = false
        end
    end

    # TODO we're currently assuming Boolean algebra!!! Very wrong.

    _normalize = t->normalize(t;
        profile = profile,
        remove_boxes = remove_boxes,
        reduce_negations = reduce_negations,
        simplify_constants = simplify_constants,
        allow_proposition_flipping = allow_proposition_flipping,
        forced_negation_removal = forced_negation_removal,
    )

    # TODO @Mauro introduce rotate_commutatives parameter and use rotate_commutatives && iscommutative for normalizing the structure of commutative operators.
    tok, ch = token(t), children(t)
    newt = begin
        if tok isa AbstractOperator && (
            tok in [∨, ∧, →]
            || SoleLogics.isbox(tok)
            || SoleLogics.isdiamond(tok)
        )
            normch = _normalize.(ch)
            if simplify_constants
                if (tok == ∨) && arity(tok) == 2
                    if     token(normch[1]) == ⊥  normch[2]
                    elseif token(normch[2]) == ⊥  normch[1]
                    elseif token(normch[1]) == ⊤  SyntaxTree(⊤)
                    elseif token(normch[2]) == ⊤  SyntaxTree(⊤)
                    else                          SyntaxTree(tok, normch)
                    end
                elseif (tok == ∧) && arity(tok) == 2
                    if     token(normch[1]) == ⊥  SyntaxTree(⊥)
                    elseif token(normch[2]) == ⊥  SyntaxTree(⊥)
                    elseif token(normch[1]) == ⊤  normch[2]
                    elseif token(normch[2]) == ⊤  normch[1]
                    else                          SyntaxTree(tok, normch)
                    end
                elseif (tok == →) && arity(tok) == 2
                    if     token(normch[1]) == ⊥  SyntaxTree(⊥)
                    elseif token(normch[2]) == ⊥  SyntaxTree(⊥)
                    elseif token(normch[1]) == ⊤  normch[2]
                    elseif token(normch[2]) == ⊤  normch[1]
                    else                          SyntaxTree(∨, _normalize(¬normch[1]), normch[2])
                    end
                elseif SoleLogics.isbox(tok) && arity(tok) == 1
                    if     token(normch[1]) == ⊤  SyntaxTree(⊤)
                    else                          SyntaxTree(tok, normch)
                    end
                elseif SoleLogics.isdiamond(tok) && arity(tok) == 1
                    if     token(normch[1]) == ⊥  SyntaxTree(⊥)
                    else                          SyntaxTree(tok, normch)
                    end
                else
                    error("Internal error in normalize(..., simplify_constants = $(simplify_constants))")
                end
            else
                SyntaxTree(tok, normch)
            end
        elseif (tok == ¬) && arity(tok) == 1
            child = ch[1]
            chtok, grandchildren = token(child), children(child)
            if reduce_negations && (chtok == ¬) && arity(chtok) == 1
                _normalize(grandchildren[1])
            elseif reduce_negations && (chtok == ∨) && arity(chtok) == 2
                ∧(_normalize(¬(grandchildren[1])), _normalize(¬(grandchildren[2])))
                # TODO use implication, maybe it's more interpretable?
            elseif reduce_negations && (chtok == ∧) && arity(chtok) == 2
                ∨(_normalize(¬(grandchildren[1])), _normalize(¬(grandchildren[2])))
            elseif reduce_negations && (chtok == →) && arity(chtok) == 2
                # _normalize(∨(¬(grandchildren[1]), grandchildren[2]))
                ∨(_normalize(grandchildren[1]), _normalize(¬(grandchildren[2])))
            elseif reduce_negations && chtok isa Proposition
                if allow_proposition_flipping
                    SyntaxTree(negation(chtok))
                else
                    ¬(_normalize(child))
                end
            # elseif reduce_negations && chtok isa SoleLogics.AbstractRelationalOperator && arity(chtok) == 1
            #     dual_op = dual(chtok)
            #     if remove_boxes && dual_op isa SoleLogics.BoxRelationalOperator
            #         ¬(_normalize(child))
            #     else
            #         dual_op(_normalize(¬(grandchildren[1])))
            #     end
            elseif reduce_negations && ismodal(chtok) && arity(chtok) == 1
                dual_op = dual(chtok)
                # if remove_boxes && SoleLogics.isbox(dual_op)
                #     ¬(_normalize(child))
                # else
                dual_op(_normalize(¬(grandchildren[1])))
                # end
            elseif (reduce_negations || simplify_constants) && chtok == ⊤ && arity(chtok) == 1
                SyntaxTree(⊥)
            elseif (reduce_negations || simplify_constants) && chtok == ⊥ && arity(chtok) == 1
                SyntaxTree(⊤)
            elseif !forced_negation_removal
                SyntaxTree(tok, _normalize.(ch))
            else
                error("Unknown chtok when removing negations: $(chtok) (type = $(typeof(chtok)))")
            end
        else
            SyntaxTree(tok, _normalize.(ch))
        end
    end

    tok, ch = token(newt), children(newt)
    if remove_boxes && tok isa AbstractOperator && SoleLogics.isbox(tok) && arity(tok) == 1
        # remove_boxes -> substitute every [X]φ with ¬⟨X⟩¬φ
        child = ch[1]
        dual_op = dual(tok)
        ¬(dual_op(_normalize(¬child)))
        # TODO remove
        # if relation(tok) == globalrel
        #     # Special case: [G]φ -> ⟨G⟩φ
        #     dual_op(_normalize(child))
        # else
        #     ¬(dual_op(_normalize(¬child)))
        # end
    else
        newt
    end

end

"""
    isglobal(f::AbstractFormula)::Bool

Return `true` if the formula is global, that is, if it can be inferred from its syntactic
structure that, given any frame-based model, the truth value of the formula is the same
on every world.

# Examples
```julia-repl
julia> f = parseformula("⟨G⟩p → [G]q");

julia> syntaxstring(f)
"([G](p)) → (⟨G⟩(q))"

julia> SoleLogics.isglobal(f)
true
```

See also
[`SyntaxTree`](@ref), [`Formula`](@ref), [`AbstractFormula`](@ref).
"""
isglobal(f::AbstractFormula)::Bool = isglobal(tree(f))
isglobal(t::SyntaxTree)::Bool =
    # (println(token(t)); println(children(t)); true) &&
    (token(t) isa SoleLogics.AbstractRelationalOperator && relation(token(t)) == globalrel) ||
    # (token(t) in [◊,□]) ||
    (token(t) isa AbstractOperator && all(c->isglobal(c), children(t)))

"""
    function collateworlds(
        fr::AbstractFrame{W,Bool},
        op::AbstractOperator,
        t::NTuple{N,WorldSetType},
    )::AbstractWorldSet{<:W} where {N,W<:AbstractWorld,WorldSetType<:AbstractWorldSet}

For a given crisp frame (`truthtype == Bool`),
return the set of worlds where a composed formula op(φ1, ..., φN) is true, given the `N`
sets of worlds where the each immediate sub-formula is true.

See also [`check`](@ref), [`iscrisp`](@ref),
[`AbstractOperator`](@ref), [`AbstractFrame`](@ref).
"""
function collateworlds(
    fr::AbstractFrame{W,Bool},
    op::AbstractOperator,
    t::NTuple{N,<:AbstractWorldSet},
)::AbstractWorldSet{<:W} where {N,W<:AbstractWorld}
    if arity(op) != length(t)
        return error("Cannot collate $(length(t)) truth values for" *
                     " operator $(typeof(op)) with arity $(arity(op))).")
    else
        return error("Please, provide method collateworlds(::$(typeof(fr))," *
                     " ::$(typeof(op)), ::NTuple{$(arity(op)), $(AbstractWorldSet{W})}.")
    end
end

# I know, these exceed 92 characters. But they look nicer like this!! :D
collateworlds(fr::AbstractFrame{W,Bool}, ::typeof(⊤), ::NTuple{0,<:AbstractWorldSet}) where {W<:AbstractWorld} = allworlds(fr)
collateworlds(::AbstractFrame{W,Bool}, ::typeof(⊥), ::NTuple{0,<:AbstractWorldSet}) where {W<:AbstractWorld} = W[]

collateworlds(fr::AbstractFrame{W,Bool}, ::typeof(¬), (ws,)::NTuple{1,<:AbstractWorldSet}) where {W<:AbstractWorld} = setdiff(allworlds(fr), ws)
collateworlds(::AbstractFrame{W,Bool}, ::typeof(∧), (ws1, ws2)::NTuple{2,<:AbstractWorldSet}) where {W<:AbstractWorld} = intersect(ws1, ws2)
collateworlds(::AbstractFrame{W,Bool}, ::typeof(∨), (ws1, ws2)::NTuple{2,<:AbstractWorldSet}) where {W<:AbstractWorld} = union(ws1, ws2)
collateworlds(fr::AbstractFrame{W,Bool}, ::typeof(→), (ws1, ws2)::NTuple{2,<:AbstractWorldSet}) where {W<:AbstractWorld} = union(setdiff(allworlds(fr), ws1), ws2)

function collateworlds(
    fr::AbstractMultiModalFrame{W,Bool},
    op::DiamondRelationalOperator,
    (ws,)::NTuple{1,<:AbstractWorldSet},
) where {W<:AbstractWorld}
    r = relation(op)
    if r == globalrel
        if length(ws) > 0
            collect(allworlds(fr))
        else
            W[]
        end
    else
        if hasconverse(r)
            # DIAMOND STRATEGY 1
            union(W[], [accessibles(fr, w, converse(r)) for w in ws]...)
        else
            # DIAMOND STRATEGY 2
            filter(w1->intersects(ws, accessibles(fr, w1, r)), collect(allworlds(fr)))
        end
    end
end

function collateworlds(
    fr::AbstractMultiModalFrame{W,Bool},
    op::BoxRelationalOperator,
    (ws,)::NTuple{1,<:AbstractWorldSet},
) where {W<:AbstractWorld}
    r = relation(op)
    if r == globalrel
        if length(ws) == nworlds(fr) # Assuming no duplicates
            collect(allworlds(fr))
        else
            W[]
        end
    else
        if hasconverse(r)
            # BOX STRATEGY 1
            negws = setdiff(collect(allworlds(fr)), ws)
            negboxws = union(W[], [accessibles(fr, w, converse(r)) for w in negws]...)
            setdiff(collect(allworlds(fr)), negboxws)
            # BOX STRATEGY 3
            # filter(w1->all((w2)->w1 in accessibles(fr, w2, converse(r)), ws), collect(allworlds(fr)))
        else
            # BOX STRATEGY 2
            filter(w1->issubset(accessibles(fr, w1, r), ws), collect(allworlds(fr)))
        end
        # Note: this is wrong, as it does not include worlds for which φ is trivially true.
        # union(intersect(W[], [accessibles(fr, w, converse(r)) for w in ws]...))
    end
end
