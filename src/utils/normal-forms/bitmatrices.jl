export BitMatrixNormalForm, BitMatrixCNF, BitMatrixDNF

############################################################################################
# Efficient BitMatrix-backed CNF/DNF SyntaxStructures for Atom{Int} interpretations
############################################################################################

"""
An efficient, BitMatrix-backed [`SyntaxStructure`](@ref) for representing a CNF
or DNF where every clause has the same atoms.

**Assumptions**:
- The alphabet is fixed to a set `Atom{Int}.(1:N)`.
- In each clause, every atom appears either negated or positive (i.e.,
one bit for each, so all possible literal positions filled, no "absent" atoms).

This enables extremely efficient evaluation for interpretations as sets or dicts over the fixed alphabet.

# Fields
- `isdnf::Bool` - Whether this is a CNF (`true`) or DNF (`false`)
- `literals::BitMatrix` - (nclauses, nalphabet), where each entry is `true` if the atom is positive in the clause, false if negative
"""
struct BitMatrixNormalForm{V} <: SyntaxStructure
    isdnf::Bool
    # size: (atoms, clauses)
    literals::BitMatrix
    function BitMatrixNormalForm(isdnf::Bool, literals)
        return new{isdnf}(isdnf, BitMatrix(literals))
    end
end

const BitMatrixDNF = BitMatrixNormalForm{true}
const BitMatrixCNF = BitMatrixNormalForm{false}

natomsperclause(f::BitMatrixNormalForm) = size(f.literals, 1)
isdnf(f::BitMatrixNormalForm{T}) where T = T
nconjuncts(f::BitMatrixNormalForm{true}) = size(f.literals, 2)
ndisjuncts(f::BitMatrixNormalForm{false}) = size(f.literals, 2)

atomsperclause(f::BitMatrixNormalForm) = Atom{Int}.(1:natomsperclause(f))
natoms(f::BitMatrixNormalForm) = prod(size(f.literals))

function Base.show(io::IO, f::BitMatrixNormalForm)
    println(io, "$(typeof(f)) with $(size(f.literals, 2)) $(isdnf(f) ? "disjuncts" : "conjuncts") over $(natomsperclause(f)) atoms:")
    show_assignment_pretty_table(
        io,
        atomsperclause(f),
        f.literals';
        show_row_number_column = true,
        row_number_column_label = (isdnf(f) ? "Disjunct" : "Conjunct")
    )
end
function tree(f::BitMatrixNormalForm; silent = false)
    !silent && @warn "Converting $(typeof(f)) to `tree`. This is not recommended."
    # Usa le strutture SyntaxTree (∧, ∨, NotTree, Atom) da types/syntactical.jl
    atomvec = 
    terms = SyntaxTree[]
    # DNF: ∨ di ∧
    for i in axes(f.literals, 2)
        clause = SyntaxTree[]
        for (j, a) in enumerate(atomsperclause(f))
            if f.literals[j, i]
                push!(clause, Atom(a))
            else
                push!(clause, ¬(Atom(a)))
            end
        end
        push!(terms, isdnf(f) ? ∧(Tuple(clause)) : ∨(Tuple(clause)))
    end
    return isdnf(f) ? ∨(Tuple(terms)) : ∧(Tuple(terms))
end

function tree2(f::BitMatrixNormalForm; silent = false)
    !silent && @warn "Converting $(typeof(f)) to `tree`. This is not recommended."
    # Usa le strutture SyntaxTree (∧, ∨, NotTree, Atom) da types/syntactical.jl
    atomvec = 
    terms = SyntaxTree[]

    # mat = Matrix{SyntaxTree}(repeat(reshape(SoleLogics.atomsperclause(f), 1, 30), size(f.literals, 1)))
    mat = Matrix{SyntaxTree}(repeat(reshape(SoleLogics.atomsperclause(f), 1, 30), size(f.literals, 2)))
    foreach(i -> if f.literals[i] mat[i] = ¬(mat[i]) end, eachindex(mat))
    if isdnf(f)
      mat = (x -> CONJUNCTION(Tuple(x))).(eachslice(mat; dims = 2))
      DISJUNCTION(Tuple(mat))
    else
      mat = (x -> DISJUNCTION(Tuple(x))).(eachslice(mat; dims = 2))
      CONJUNCTION(Tuple(mat))
    end
end

"""
    interpret(f::BitMatrixNormalForm, i)

Efficiently evaluates the normal form for an interpretation of Atom{Int}, 
where the interpretation can be e.g. a Set/Vector of Int, or a Dict{Atom{Int},Bool}.
"""
function interpret(f::BitMatrixNormalForm, i)
    # For each clause:
    #   For DNF: if any clause is true, return true (short-circuit-OR)
    #   For CNF: if any clause is false, return false (short-circuit-AND)
    ISDNF = isdnf(f)
    # For each clause
    for r in axes(f.literals, 2)
        ok = ISDNF ? true : false
        for (c, a) in enumerate(atoms(f))
            # v = value of atom a in i (true/false)
            v = value(a) in i
            pos = f.literals[c, r]
            litval = pos ? v : !v
            if ISDNF
                ok &= litval
                if !ok
                    break # clause falsified
                end
            else
                ok |= litval
                if ok
                    break # clause satisfied
                end
            end
        end
        if ISDNF
            if ok
                return true # Some clause satisfied
            end
        else
            if !ok
                return false # Clause unsatisfied
            end
        end
    end
    return ISDNF
end

