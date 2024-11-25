import Base: parse

function Base.parse(
    F::Type{<:Formula},
    expr::AbstractString,
    args...;
    kwargs...
)
    return parseformula(F, expr, args...; kwargs...)
end


function parseformula(F::Type{<:Formula}, expr::AbstractString, args...; kwargs...)
    return error("Please, provide method parseformula(::Type{$(F)}, expr::AbstractString, ::$(typeof(args))...; ::$(typeof(kwargs))...).")
end

parseformula(expr::AbstractString, args...; kwargs...) = parseformula(SyntaxTree, expr, args...; kwargs...)

