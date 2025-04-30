################################################################################
#### Cayley Tables #############################################################
################################################################################

function evaluate(ct::Vector{Int8}, a::Int8, b::Int8, n::Int8)
    a == Int8(0) && return Int8(0)
    b == Int8(0) && return Int8(0)
    a == n-Int8(1) && return b
    b == n-Int8(1) && return a
    if a < b
        return ct[(n-Int8(2))*(a-Int8(1))+b-(a*(a-Int8(1)))÷Int8(2)]
    else
        return ct[(n-Int8(2))*(b-Int8(1))+a-(b*(b-Int8(1)))÷Int8(2)]
    end
end

function evaluate(
    ct::Vector{Int8},
    a::Int8,
    b::Int8,
    indexes::Matrix{Union{Nothing,Int8}}
)
    n = Int8(size(indexes)[1])
    a == Int8(0) && return Int8(0)
    b == Int8(0) && return Int8(0)
    a == n-Int8(1) && return b
    b == n-Int8(1) && return a
    if a < b
        return ct[indexes[a+Int8(1), b+Int8(1)]]
    else
        return ct[indexes[b+Int8(1), a+Int8(1)]]
    end
end

function isassociative(ct::Vector{Int8}, n::Int8)
    for a in Int8(1):n-Int8(2)
        for b in Int8(1):n-Int8(2)
            for c in Int8(1):n-Int8(2)
                if evaluate(
                    ct,
                    evaluate(ct, a, b, n),
                    c,
                    n
                ) != evaluate(
                    ct,
                    a,
                    evaluate(ct, b, c, n),
                    n
                )
                    return false
                end
            end
        end
    end
    return true
end

function isassociative(ct::Vector{Int8}, indexes::Matrix{Union{Nothing,Int8}})
    n = Int8(size(indexes)[1])
    for a in Int8(1):n-Int8(2)
        for b in Int8(1):n-Int8(2)
            for c in Int8(1):n-Int8(2)
                if evaluate(
                    ct,
                    evaluate(ct, a, b, indexes),
                    c,
                    indexes
                ) != evaluate(
                    ct,
                    a,
                    evaluate(ct, b, c, indexes),
                    indexes
                )
                    return false
                end
            end
        end
    end
    return true
end

################################################################################
#### Finite FLew-chains ########################################################
################################################################################

"""
    struct FiniteFLewChain{N}
        cayleytable::Vector{Int8}
    end

A structure representing a finite FLew-chain over a set of N linearly ordered
elements.

Given a set A of N linearly ordered elements A = {0, ..., N-1}, 
C_N = {0, 1/(N-1), 2/(N-1), ..., (N-2)/(N-1), (N-1)/(N-1) = 1}.

We could represent the Cayley table for (C_N, ⋅) with an N × N matrix, but:
 - since the magma is abelian, we just need to represent its upper triangular
   using an (N*(N+1))÷2 vector;
 - since 1 is an identity element and 0 an absorbing element, we don't need
   to represent the first and last rows/columns, so we just need a (N-2)^2
   matrix.
Taking both points into consideration, we just need an ((N-2)(N-1))÷2 vector.

E.g., with N=5:

   ⋅  |  0  1/4 2/4 3/4  1
 ===========================
   0  |  0   0   0   0   0
  1/4 |  0   A   B   C  1/4
  2/4 |  0   B   D   E  2/4
  3/4 |  0   C   E   F  3/4
   1  |  0  1/4 2/4 3/4  1

We just need {A, B, C, D, E, F} (6 elements instead of 25!!)

Also, since the values of N we can evaluate are very small (way less than 127)
we can use Int8 to save up space.
"""
struct FiniteFLewChain{N}
    cayleytable::Vector{Int8}
    
    function FiniteFLewChain{N}(cayleytable::Vector{Int8}; safe=false) where {N}
        if length(cayleytable) != ((N-Int8(2))*(N-Int8(1)))÷Int8(2)
            error("Wrong number of elements provided")
        end
        if !safe && !isassociative(cayleytable, N)
            error("Cayley table is not associative")
        end
        return new{N}(cayleytable)
    end

    function FiniteFLewChain{N}() where {N}
        if N < Int8(1)
            error("N must be greater than 0")
        elseif N == Int8(1) || N == Int8(2)
            return FiniteFLewChain{N}(Vector{Int8}())
        else
            return N > Int8(2) && error("Please provide elements for N > 2")
        end
    end
end

function evaluate(
    ffc::FiniteFLewChain{N},
    a::Int8,
    b::Int8;
    indexes::Union{Nothing, Matrix{Union{Nothing,Int8}}} = nothing
) where {
    N
}
    if isnothing(indexes)
        return evaluate(ffc.cayleytable, a, b, N)
    else
        return evaluate(ffc.cayleytable, a, b, indexes)
    end
end

import Base.show

function Base.show(io::IO, ffc::FiniteFLewChain{N}) where {N}
    ct = Matrix{Union{Nothing, Int8}}(nothing, N, N)
    for i in Int8(1):N
        ct[Int8(1), i] = ct[i, Int8(1)] = Int8(0)
        ct[N, i] = ct[i,N] = i-Int8(1)
    end
    for i in Int8(2):N-Int8(1)
        for j in i:N-Int8(1)
            ct[i,j] = ct[j,i] = evaluate(ffc, i-Int8(1), j-Int8(1))
        end
    end
    print(io, "\n")
    print(io, " ⋅ |")
    for i in Int8(1):N print(io, " $(Int8(i-Int8(1)))") end
    print(io, "\n")
    for _ in Int8(1):N+Int8(3) print(io, "==") end
    print(io, "\n")
    for i in Int8(1):N
        print(io, " $(Int8(i-Int8(1))) |")
        for j in Int8(1):N print(io, " $(Int8(ct[i,j]))") end
        print(io, "\n")
    end
end

################################################################################
#### Generating FLew-chains ####################################################
################################################################################

function weaklyincreasing(
    seqs::Vector{Vector{Int8}},
    seq::Vector{Int8},
    min::Int8,
    max::Int8,
    l::Int8
)
    if l == Int8(0)
        push!(seqs, copy(seq))  # deep copy necessary here
    else
        for i in min:max
            push!(seq, i)
            weaklyincreasing(seqs, seq, i, max, l-Int8(1))
            pop!(seq)
        end
        return seqs
    end
end

function weaklyincreasing(min::Int8, max::Int8, l::Int8)
    return weaklyincreasing(Vector{Vector{Int8}}(), Vector{Int8}(), min, max, l)
end

function isweaklyincreasingbycolumns(ct::Vector{Int8}, wiseq::Vector{Int8})
    for j in 1:length(wiseq)
        if reverse(wiseq)[j] < reverse(ct)[j]
            return false
        end
    end
    return true
end

function generateflewchains(
    cts::Vector{Vector{Int8}},
    ct::Vector{Int8},
    min::Int8,
    max::Int8,
    l::Int8,
    n::Int8,
    u::Base.Threads.SpinLock,
    indexes::Matrix{Union{Nothing,Int8}}
)
    wiseqs = weaklyincreasing(min, max, l) 
    if l == Int8(1)
        for wiseq in wiseqs
            if isassociative(vcat(ct, wiseq), indexes)
                Threads.lock(u) do
                    push!(cts, vcat(ct, wiseq))
                end
            end
        end
    else
        Threads.@threads for wiseq in wiseqs
            if isempty(ct) || isweaklyincreasingbycolumns(ct, wiseq)
                generateflewchains(
                    cts,
                    vcat(ct, wiseq),
                    Int8(wiseq[Int8(2)]),
                    max+Int8(1),
                    l-Int8(1),
                    n,
                    u,
                    indexes
                )
            end
        end
    end
    return cts
end

function generateflewchains(n, indexes)
    cts = Vector{Vector{Int8}}()
    n < Int8(3) && return cts
    return generateflewchains(
        cts,
        Vector{Int8}(),
        Int8(0),
        Int8(1),
        n-Int8(2),
        n,
        Threads.SpinLock(),
        indexes
    )
end

function generateflewchains(n::Int8)
    if n < Int8(3)
        return [FiniteFLewChain{n}()]
    else
        indexes = Matrix{Union{Nothing, Int8}}(nothing, n, n)
        for i in Int8(0):n-Int8(1)
            for j in Int8(0):n-Int8(1)
                indexes[
                    i+Int8(1),
                    j+Int8(1)
                ] = (n-Int8(2))*(i-Int8(1))+j-(i*(i-Int8(1)))÷Int8(2)
            end
        end
        return FiniteFLewChain{n}.(
            [generateflewchains(n, indexes)...];
            safe=true
        )
    end
end

generateflewchains(n::Int) = generateflewchains(Int8(n))
