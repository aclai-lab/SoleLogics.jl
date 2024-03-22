# Fast isempty(intersect(u, v))
function intersects(u, v)
    for x in u
        if x in v
            return true
        end
    end
    false
end

inittruthvalues(truthvalues::Union{Vector{<:Truth},AbstractAlgebra}) =
    return (truthvalues isa AbstractAlgebra) ? domain(truthvalues) : truthvalues


function displaysyntaxvector(a, maxnum = 8; quotes = true)
    q = e->(quotes ? "\"$(e)\"" : "$(e)")
    els = begin
        if length(a) > maxnum
            [(q.(syntaxstring.(a)[1:div(maxnum, 2)]))..., "...", (q.(syntaxstring.(a)[end-div(maxnum, 2):end]))...]
        else
            q.(syntaxstring.(a))
        end
    end
    "$(eltype(a))[$(join(els, ", "))]"
end
