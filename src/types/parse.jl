import Base: parse

function Base.parse(
    F::Type{<:Formula},
    expr::String,
    args...;
    kwargs...
)
    return parseformula(F, expr, args...; kwargs...)
end


function parseformula(F::Type{<:Formula}, expr::String, args...; kwargs...)
    return error("Please, provide method parseformula(::Type{$(F)}, expr::String, ::$(typeof(args))...; ::$(typeof(kwargs))...).")
end

parseformula(expr::String, args...; kwargs...) = parseformula(SyntaxTree, expr, args...; kwargs...)

