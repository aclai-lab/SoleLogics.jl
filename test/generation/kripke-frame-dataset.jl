# we want to generate a set of Kripke frames, starting from a set of modal formulas;
# in particular, we want to leverage Spartacus (already installed by SoleReasoners)
# to propose models (if there are any).

# we exploit SoleReasoners (the embedding branch, at the moment of reading)
using SoleLogics

"""
Install spartacus solver in a subfolder at the location of this file.

The installer is:
```
cd $1
curl https://www.ps.uni-saarland.de/spartacus/spartacus-1.1.3.tar.bz2 -o spartacus-1.1.2.tar.bz2
tar -x -f spartacus-1.1.2.tar.bz2
cd spartacus
make spartacus
```

You need mlton; you can get it from https://sourceforge.net/projects/mlton/files/mlton/20210117/mlton-20210117-1.amd64-linux-glibc2.31.tgz
(adjust for your architecture and OS) and tar xfv the resulting folder;
also, you need to move `bin` and `lib` folders to, respectively, `usr/bin` and `usr/lib`.
"""
function installspartacus()
    if !isspartacusinstalled()
        println("Installing Spartacus")
        run(`sh $(joinpath(@__DIR__, "install_spartacus.sh")) $(@__DIR__)`)
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
            push!(r, '<', syntaxstring(c)[4], '>')
        elseif c isa BoxRelationalConnective
            push!(r, '[', syntaxstring(c)[4], ']')
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


# directory in which this script lives
WORKING_DIR = joinpath(@__DIR__, "test", "generation")

# directory where spartacus solver is installed
SPARTACUS_DIR = joinpath(WORKING_DIR, "spartacus-installation", "spartacus")


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
_raw_result = run(pipeline(cmd, stdout=_buffer))
_result = String(take!(_buffer))

# now, we need to parse the _result
