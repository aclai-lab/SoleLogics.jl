# we want to generate a set of Kripke frames, starting from a set of modal formulas;
# in particular, we want to leverage Spartacus (already installed by SoleReasoners)
# to propose models (if there are any).

using Graphs

# we exploit SoleReasoners (the embedding branch, at the moment of reading)
using SoleLogics

"""
Install spartacus solver in a subfolder at the location of this file.

See install-spartacus.sh script.

You need mlton; you can get it from https://sourceforge.net/projects/mlton/files/mlton/20210117/mlton-20210117-1.amd64-linux-glibc2.31.tgz
(adjust for your architecture and OS) and tar xfv the resulting folder;
also, you need to move `bin` and `lib` folders to, respectively, `usr/bin` and `usr/lib`.
"""
function installspartacus()
    _installation_instructions = joinpath(
        pkgdir(SoleLogics), "src", "utils", "spartacus-installation")

    if !isfile( joinpath(_installation_instructions, "spartacus") )
        println("Installing Spartacus")
        run(`sh $(joinpath(_installation_instructions, "install_spartacus.sh")) $(_installation_instructions)`)
    end
end


"""
Convert a sole formula to spartacus format.
"""
function soletospartacus(φ::Union{Atom, BooleanTruth, SyntaxBranch})
    s = syntaxstring(φ)
    r = Vector{Char}()
    for c in s
        if c == '¬'
            push!(r, '~')
        elseif c == '∧'
            push!(r, '&')
        elseif c == '∨'
            push!(r, '|')
        elseif c == '→'
            push!(r, '-', '>')
        elseif c == '◊'
            push!(r, '<', 'a', '>')
        elseif c == '□'
            push!(r, '[', 'a', ']')
        elseif c isa DiamondRelationalConnective    # beware, this is not K modal logic...
            push!(r, '<', syntaxstring(SoleLogics.relation(c)), '>')
        elseif c isa BoxRelationalConnective
            push!(r, '[', syntaxstring(SoleLogics.relation(c)), ']')
        elseif c == '⊤'
            push!(r, '1')
        elseif c == '⊥'
            push!(r, '0')
        else
            push!(r, c)
        end
    end
    return String(r)
end


"""
    function spartacustomodel(
        spartacuslog::String;
        atomsconversion::Function=identity,
        silent::Bool=true
    )::Union{Nothing,KripkeStructure}

If the given spartacus model (`spartacuslog`) is satisfiable, return a
[`KripkeStructure`](@ref) encoding it; otherwise, return `nothing`.

# Arguments
- `spartacuslog::String`: string containing the output of a spartacus run.

# Keyword Arguments
- `atomsconversion::Function`: callable used to build every [`Atom`](@ref) within the
[`KripkeStructure`](@ref);
- `silent::Bool=true`: suppress printings.

# Examples
```julia
using Graphs
using SoleLogics

_myrng = 42
_alphabet = ExplicitAlphabet(Atom.('p':'z'))
_operators = [CONJUNCTION, DISJUNCTION, NEGATION, DIAMOND, BOX]

_earlystoppingthreshold = 0.2
_nformulas = 10

_formulas = [
    randformula(
        _myrng,
        _alphabet,
        _operators;
        earlystoppingthreshold =_earlystoppingthreshold
    )
    for _ in 1:_nformulas
]

_conjunction = CONJUNCTION(_formulas...)

# we want a model that satisfies all the generated formulas;
# we will leverage soletospartacus;
# because of that, this is clearly a "spartan" formula
_spartan_conjunction = soletospartacus(_conjunction)

# invoke spartacus executable
_command = `€(SPARTACUS_DIR)/spartacus --showModel --formula=€(_spartan_conjunction)`

_buffer = IOBuffer()
_raw_result = run(pipeline(_command, stdout=_buffer))
_result = String(take!(_buffer))
```

!!! note
    Spartacus solver is required to execute this.
    See [`installspartacus`](@ref).

By @perro2110 and @mauro_milella.
"""
function spartacustomodel(
    spartacuslog::String;
    atomsconversion::Function=identity,
    silent::Bool=true
)::Union{Nothing,KripkeStructure}
    if !isnothing(findfirst("unsatisfiable", spartacuslog))
        silent || println("Error: the provided spartacus model is unsatisfiable.")
        return nothing
    end

    # example after splitting:
    # -----------------------------------------------------
    # 3 nodes
    # ------------------------------------------------------------
    # Node 0
    # propositions: p, q, r, s, t, u, w, y, z
    # successors:   a:1
    # ------------------------------------------------------------
    # Node 1
    # propositions: r, u
    # successors:   a:2
    # ------------------------------------------------------------
    # Node 2
    # propositions: p
    # successors:
    # ------------------------------------------------------------
    spartacuslog = split(spartacuslog, "MODEL")[2]

    # now, we split by \n
    spartacuslog = split(spartacuslog, "\n")
    nworlds = parse(Int, split(spartacuslog[2], " ")[1])

    # we can discard the first 3 lines
    spartacuslog = spartacuslog[3:end]

    # the graph is a directed graph; at the moment, we only know the numberf of its vertices
    graph = Graphs.SimpleDiGraph(nworlds)

    # keep track of the atoms that are true on the ith world/vertex
    ithworld_to_atoms = Dict{Int,Vector{Atom}}()
    # also, we want to keep track of all the existent atoms
    allatoms = Atom[]

    for i in 1:nworlds
        # "Node 1" becomes 1 (as integer);
        # beware that indexing is zero based.
        currentworld = parse(Int, split(spartacuslog[i+1], " ")[end]) + 1

        # "propositions: r, u" becomes [r,u] (whose type is defined by atomsconversion)
        atoms_on_currentworld = split(spartacuslog[i+2], " ")[2:end] .|> String
        atoms_on_currentworld = filter.(
            x -> x != ',', atoms_on_currentworld) .|> atomsconversion .|> Atom

        # keep track of the linking between a specific world and its atoms
        ithworld_to_atoms[currentworld] = atoms_on_currentworld

        # later, by applying unique to this collection, we will isolate the entire alphabet
        push!(allatoms, atoms_on_currentworld...)

        successors = split(spartacuslog[i+3], "successors:")[2] .|> strip
        # when there are no successors, then we just want to skip
        if !isempty(successors)
            for (_,targetworld) in split.(successors, ":")
                # WARNING: beware, as the specific type of relations is currently ignored
                push!(graph, currentworld, targetworld)
            end
        end
    end

    # allatoms now contains the full alphabet
    unique!(allatoms)

    # translate from Graphs to Kripke frames
    worlds = World.(1:nworlds)
    _frame = SoleLogics.ExplicitCrispUniModalFrame(worlds, graph)

    function _assign_truthvalues(allatoms::Vector{Atom}, ithworld_to_atoms::Vector{Atom})
        atom_to_top = [_atom => TOP for _atom in ithworld_to_atoms]
        atom_to_bot = [_atom => BOT for _atom in setdiff(allatoms, ithworld_to_atoms)]

        return TruthDict(Iterators.flatten([atom_to_top, atom_to_bot]) |> collect)
    end

    # for each world w that we collected,
    # assign to it the atoms that are true (TOP) or false (BOT) on them
    valuation = Dict([
        w => _assign_truthvalues(allatoms, ithworld_to_atoms[wid])
        for (wid,w) in enumerate(worlds)
    ])

    return KripkeStructure(_frame, valuation)
end


# directory where spartacus solver is installed
SPARTACUS_DIR = joinpath(
    joinpath(pkgdir(SoleLogics), "src", "utils"),
    "spartacus-installation",
    "spartacus"
)


# Example of usage
#=
_myrng = 42
_alphabet = ExplicitAlphabet(Atom.('p':'z'))
_operators = [CONJUNCTION, DISJUNCTION, NEGATION, DIAMOND, BOX]
_earlystoppingthreshold = 0.2
_nformulas = 10
_formulas = [
    randformula(
        _myrng,
        _alphabet,
        _operators;
        earlystoppingthreshold =_earlystoppingthreshold
    )
    for _ in 1:_nformulas
]
_conjunction = CONJUNCTION(_formulas...)
# we want a model that satisfies all the generated formulas;
# we will leverage soletospartacus;
# because of that, this is clearly a "spartan" formula
_spartan_conjunction = soletospartacus(_conjunction)
# invoke executable (beware of how te executable is called!)
_command = `$(SPARTACUS_DIR)/spartacus --showModel --formula=$(_spartan_conjunction)`
_buffer = IOBuffer()
_raw_result = run(pipeline(_command, stdout=_buffer))
_result = String(take!(_buffer))
# now, we need to parse the _result
spartacustomodel(_result)
=#
