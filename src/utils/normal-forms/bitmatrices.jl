############################################################################################
# Efficient BitMatrix-backed CNF/DNF SyntaxStructures for Atom{Int} interpretations
############################################################################################

"""
An efficient, `BitMatrix`-backed [`SyntaxStructure`](@ref) for representing a DNF
or CNF where every clause has the same atoms.

**Assumptions**:
- The alphabet is fixed to a set `Atom{Int}.(1:N)`.
- In each clause, every atom appears either negated or positive (i.e.,
one bit for each, so all possible literal positions filled, no "absent" atoms).

This enables efficient evaluation for interpretations as sets or dicts over the fixed alphabet.

# Fields
- `isdnf::Bool` - Whether this is a DNF (`true`) or CNF (`false`)
- `literals::BitMatrix` - (nclauses, natoms), where each entry is `true` if the atom is positive in the clause, false if negative.
"""
struct BitMatrixNormalForm{V} <: SyntaxStructure
    isdnf::Bool
    # size: (clauses, atoms)
    literals::BitMatrix
    function BitMatrixNormalForm{T}(isdnf::Bool, literals) where T
        @assert T == isdnf "Cannot instantiate BitMatrixNormalFor with $T != $isdnf"
        new{isdnf}(isdnf, BitMatrix(literals))
    end
    function BitMatrixNormalForm{T}(literals) where T
        new{T}(T, BitMatrix(literals))
    end
    function BitMatrixNormalForm(isdnf::Bool, literals)
        new{isdnf}(isdnf, BitMatrix(literals))
    end
end

"A DNF formula defined by a `BitMatrix` of shape `nclauses` × `natoms`."
const BitMatrixDNF = BitMatrixNormalForm{true}

"A CNF formula defined by a `BitMatrix` of shape `nclauses` × `natoms`."
const BitMatrixCNF = BitMatrixNormalForm{false}

natomsperclause(f::BitMatrixNormalForm) = size(f.literals, 2)
isdnf(::BitMatrixNormalForm{T}) where T = T
ndisjuncts(f::BitMatrixNormalForm{true}) = size(f.literals, 1)
nconjuncts(f::BitMatrixNormalForm{false}) = size(f.literals, 1)

atomsperclause(f::BitMatrixNormalForm) = Atom{Int}.(1:natomsperclause(f))
natoms(f::BitMatrixNormalForm) = prod(size(f.literals))

function syntaxstring(f::BitMatrixNormalForm)
    join(
        map(s -> "($s)", [join([(val ? string(i) : "¬" * string(i)) for (i, val) in enumerate(row)], (isdnf(f) ? " ∧ " : " ∨ "))
    for row in eachrow(f.literals)])
    , (isdnf(f) ? " ∨ " : " ∧ "))
end
function Base.show(io::IO, f::BitMatrixNormalForm)
    println(io, "$(typeof(f)) with $(size(f.literals, 1)) " *
        (isdnf(f) ? "disjuncts" : "conjuncts") * " over $(natomsperclause(f)) atoms:")
    show_assignment_pretty_table(
        io,
        atomsperclause(f),
        f.literals;
        show_row_number_column = true,
        row_number_column_label = (isdnf(f) ? "Disjunct" : "Conjunct")
    )
end
function tree(f::BitMatrixNormalForm; silent = false)
    !silent && @warn "Converting $(typeof(f)) to `tree`. This operation is slow, " *
        "and may lead to downstream inefficiencies. You might want to prevent this from happening.\nStacktrace: $(stacktrace()[1:5])"
    term_mat = Matrix{SyntaxTree}(repeat(reshape(SoleLogics.atomsperclause(f), 1, size(f.literals, 2)), size(f.literals, 1)))
    foreach(i -> if !f.literals[i] term_mat[i] = ¬(term_mat[i]) end, eachindex(term_mat))
    if isdnf(f)
      mat = (x -> CONJUNCTION(Tuple(x))).(eachslice(term_mat; dims = 1))
      DISJUNCTION(Tuple(mat))
    else
      mat = (x -> DISJUNCTION(Tuple(x))).(eachslice(term_mat; dims = 1))
      CONJUNCTION(Tuple(mat))
    end
end

"""
    check(f::BitMatrixNormalForm, i::Interpretation)

Efficiently evaluates the normal form for an interpretation over `Atom{Int}`.
"""
function check(f::BitMatrixNormalForm, i::Interpretation)
    # For each clause:
    #   For DNF: if any clause is true, return true (short-circuit-OR)
    #   For CNF: if any clause is false, return false (short-circuit-AND)
    ISDNF = isdnf(f)
    atms = atomsperclause(f)
    # For each clause
    for r in axes(f.literals, 1)
        ok = ISDNF ? true : false
        for (c, a) in enumerate(atms)
            if haskey(i, a)
                t = istop(i[a])
                truthval = f.literals[r, c] ? t : !t
                if ISDNF
                    ok &= truthval
                    if !ok
                        break # falsified clause
                    end
                else
                    ok |= truthval
                    if ok
                        break # satisfied clause
                    end
                end
            end
        end
        if ISDNF && ok
            return true # Some satisfied clause
        elseif !ISDNF && !ok
            return false # Some unsatisfied clause
        end
    end
    ISDNF
end

