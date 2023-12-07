module experimentals

using SoleLogics

export formula2natlang

rel2natlang(r::SoleLogics.AbstractRelation)             = syntaxstring(r)
rel2natlang(::SoleLogics._IA_A)             = "succeding"
rel2natlang(::SoleLogics._IA_Ai)             = "preceding"
rel2natlang(::SoleLogics._IA_L)             = "later"
rel2natlang(::SoleLogics._IA_Li)             = "earlier"
rel2natlang(::SoleLogics._IA_B)             = "prefix"
rel2natlang(::SoleLogics._IA_E)             = "suffix"
rel2natlang(::SoleLogics._IA_D)             = "inner"
rel2natlang(::SoleLogics._IA_Di)             = "outer"
rel2natlang(::SoleLogics._IA_O)             = "partially overlapping"
rel2natlang(::SoleLogics._IA_Oi)             = "preceding, partially overlapping"
rel2natlang(::SoleLogics._IA_AorO)             = "partially overlapping"
rel2natlang(::SoleLogics._IA_AiorOi)             = "preceding, partially overlapping"
rel2natlang(::SoleLogics._IA_I)             = "intersecting"

function rel2natlang(r::RectangleRelation; kwargs...)
    "$(rel2natlang(r.x; kwargs...)),$(rel2natlang(r.y; kwargs...))"
end

rel2natlang(::SoleLogics._Topo_DC)    = "disconnected"
rel2natlang(::SoleLogics._Topo_EC)    = "externally touching"
rel2natlang(::SoleLogics._Topo_PO)    = "partially overlapping"
rel2natlang(::SoleLogics._Topo_TPP)   = "internally touching"
rel2natlang(::SoleLogics._Topo_TPPi)  = "internally-touched"
rel2natlang(::SoleLogics._Topo_NTPP)  = "inner"
rel2natlang(::SoleLogics._Topo_NTPPi) = "outer"

# Note: assuming interval frame
function formula2natlang(φ::Formula; kwargs...)
    formula2natlang(tree(φ); kwargs...)
end
function formula2natlang(φ::SyntaxTree; depth = 0, kwargs...)
    f2nl = (ch)->formula2natlang(ch; depth = depth+1, kwargs...)
    if token(φ) == ¬
        "¬($(f2nl(first(children(φ)))))"
    elseif token(φ) == globalbox
        "∀ intervals ($(f2nl(first(children(φ)))))"
    elseif token(φ) == globaldiamond
        "∃ interval where ($(f2nl(first(children(φ)))))"
    elseif SoleLogics.isbox(token(φ)) && token(φ) isa SoleLogics.AbstractRelationalConnective
        r = SoleLogics.relation(token(φ))
        "∀ $(rel2natlang(r)) intervals ($(f2nl(first(children(φ)))))"
    elseif SoleLogics.isdiamond(token(φ)) && token(φ) isa SoleLogics.AbstractRelationalConnective
        r = SoleLogics.relation(token(φ))
        "∃ $(rel2natlang(r)) interval where ($(f2nl(first(children(φ)))))"
    elseif token(φ) == ∧
        "($(f2nl(first(children(φ))))) and ($(f2nl(last(children(φ)))))"
    elseif token(φ) == ∨
        "($(f2nl(first(children(φ))))) or ($(f2nl(last(children(φ)))))"
    elseif token(φ) == →
        "whenever $(f2nl(first(children(φ)))) holds, also $(f2nl(last(children(φ))))"
    else
        syntaxstring(φ; kwargs...)
    end
end
function formula2natlang(φ::SyntaxLeaf; depth = 0, kwargs...)
    syntaxstring(φ; kwargs...)
end

end
