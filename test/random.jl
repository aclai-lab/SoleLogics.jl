
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ random ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

@testset "Random" begin

_alphabet = ExplicitAlphabet(Proposition.(["p", "q"]))
_operators = [NEGATION, CONJUNCTION, IMPLICATION]
@test_nowarn randformulatree(10, _alphabet, _operators)
@test_nowarn randformulatree(2, _alphabet, _operators)

end

@testset "Random+Parsing" begin

#=
    TODO: fix
    LoadError: syntax: unsupported `const` declaration on local variable around
    /home/mauro/Desktop/UNI/Laboratorio/Sole/SoleLogics.jl/test/random.jl:15

    const TERN = SoleLogics.NamedOperator{:TERN}()
    import SoleLogics: arity
    SoleLogics.arity(::Type{typeof(TERN)}) = 3
=#

_alphabet = ExplicitAlphabet(Proposition.(["p", "q"]))
_operators = [NEGATION, CONJUNCTION, IMPLICATION,
    DiamondRelationalOperator(globalrel), BoxRelationalOperator(globalrel)]

@test all([begin
    f = randformula(4, _alphabet, _operators)
    s = syntaxstring(f)
    s == syntaxstring(parseformulatree(s))
end
 for i in 1:10])

@test all([begin
    f = randformula(4, _alphabet, _operators)
    s = syntaxstring(f; function_notation = true)
    s == syntaxstring(parseformulatree(s; function_notation = true);
        function_notation = true)
end for i in 1:10])

end
