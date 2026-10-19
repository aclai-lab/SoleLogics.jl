module TestFirstOrderLogics

using Test
using SoleLogics
using SoleLogics.FirstOrderLogics

################################################################################
#### Syntax ####################################################################
################################################################################

# definition of a variable, both via macro and manually
@variables x y
z = Variable(:z)

# like above, with constants
@constants a b
zero = Constant(:zero)

# the other term we want to define are functions
succ = FunctionSymbol(:succ, 1)
plus = FunctionSymbol(:plus, 2)

# finally, predicate names with their arity
Even = Predicate(:Even, 1)
Less = Predicate(:Less, 2)
Rain = Predicate(:Rain, 0)

@test x isa Variable && x == Variable(:x) && x == Variable("x")
@test zero isa Constant
@test succ(x) isa FunctionTerm
@test arguments(plus(x, zero)) == (x, zero)
@test functionsymbol(succ(x)) == succ
@test_throws ArgumentError succ(x, y)
@test_throws ArgumentError FunctionSymbol(:bad, -1)

@test Less(x, y) isa PredicateAtom
@test Less(x, y) isa SoleLogics.AbstractAtom
@test predicate(Less(x, y)) == Less
@test_throws ArgumentError Less(x)
@test Less(x, y) == Less(x, y)
@test isequal(Less(x, succ(y)), Less(x, succ(y)))
@test hash(Less(x, y)) == hash(Less(x, y))
@test length(Set([Less(x, y), Less(x, y), Less(y, x)])) == 2

@test syntaxstring(x) == "x"
@test syntaxstring(plus(succ(x), zero)) == "plus(succ(x), zero)"
@test syntaxstring(Less(x, succ(y))) == "Less(x, succ(y))"
@test syntaxstring(Rain()) == "Rain"
@test syntaxstring(x ≐ zero) == "x = zero"

# quantifiers are unary connectives
φ = ForAll(x)(Exists(y)(Less(x, y)))
φ = ∀(x, ∃(y, Less(x, y))) # alternative definition
@test φ == forall(x, exists(y, Less(x, y))) # another alternative definition

@test φ isa SyntaxBranch
@test token(φ) isa ForAll && variable(token(φ)) == x
@test isquantifier(token(φ)) && !isquantifier(∧)
@test arity(ForAll(x)) == 1

@test ∀(x, y, Less(x, y)) == ∀(x, ∀(y, Less(x, y)))
@test dual(ForAll(x)) == Exists(x) && dual(Exists(x)) == ForAll(x)
@test hasdual(Exists(x))
@test height(φ) == 2
@test natoms(φ) == 1

@test syntaxstring(φ) == "∀x ∃y Less(x, y)"
@test syntaxstring(∀(x, Even(x) → Less(zero, x))) == "∀x (Even(x) → Less(zero, x))"
@test syntaxstring(¬∃(x, Even(x)) ∧ Rain()) == "¬∃x Even(x) ∧ Rain"

# mixing with propositional connectives and atoms
@atoms p
ψ = p ∧ ∀(x, Even(x))
@test syntaxstring(ψ) == "p ∧ ∀x Even(x)"
@test length(subformulas(ψ)) == 4

################################################################################
#### Syntactic utilities #######################################################
################################################################################

@test variables(plus(x, succ(y))) == Set([x, y])
@test variables(∃(z, Less(x, y))) == Set([x, y, z])
@test freevariables(∃(y, Less(x, y))) == Set([x])
@test boundvariables(∃(y, Less(x, y))) == Set([y])
@test freevariables(Even(x) ∧ ∃(x, Less(x, y))) == Set([x, y])
@test issentence(φ)
@test !issentence(∃(y, Less(x, y)))
@test issentence(p ∧ Rain())

@test predicates(∀(x, Even(x) ∨ Less(x, zero))) == [Even, Less]
@test constants(Less(zero, plus(x, zero))) == [zero]
@test functionsymbols(Less(succ(x), plus(x, zero))) == [succ, plus]

# non bound occurrences of a variable are substituted
@test substitute(plus(x, y), x, zero) == plus(zero, y) # FunctionTerm{2}
@test substitute(Less(x, y), x => zero) == Less(zero, y) # ::PredicateAtom{2}
@test substitute(Less(x, y), x => y, y => z) == Less(z, z)

# bound occurrences are left untouched
@test substitute(∃(x, Less(x, y)), x => zero) == ∃(x, Less(x, y))
@test substitute(Even(x) ∧ ∃(x, Even(x)), x => zero) == Even(zero) ∧ ∃(x, Even(x))

# capture is avoided by renaming the bound variable
captured = substitute(∃(y, Less(x, y)), x => y)
@test syntaxstring(captured) == "∃y1 Less(y, y1)"
@test freevariables(captured) == Set([y])

################################################################################
#### Semantics #################################################################
################################################################################

M = FOStructure(0:3;
    constants = [zero => 0],
    functions = [succ => (n -> min(n + 1, 3)),
        plus => Dict((a, b) => min(a + b, 3) for a in 0:3 for b in 0:3),],
    predicates = [Even => iseven, Less => <, Rain => false],
)

@test domain(M) == [0, 1, 2, 3]
@test_throws ArgumentError FOStructure(1:2; constants = [zero => 0])

@test evaluate(zero, M) == 0
@test evaluate(succ(succ(zero)), M) == 2
@test evaluate(plus(x, succ(zero)), M, Dict(x => 2)) == 3
@test_throws ErrorException evaluate(x, M)

@test check(Even(zero), M)
@test !check(Rain(), M)
@test check(Less(zero, x), M; assignment = Dict(x => 2))
@test !check(Less(zero, x), M; assignment = Dict(x => 0))
@test interpret(Even(zero), M) == ⊤
@test M[Even(succ(zero))] == ⊥
@test_throws ErrorException check(Less(zero, x), M)  # x is free and unassigned

@test check(∃(x, Even(x)), M)
@test !check(∀(x, Even(x)), M)
@test check(∀(x, ∃(y, Less(x, y) ∨ (x ≐ y))), M)
@test !check(∀(x, ∃(y, Less(x, y))), M)    # 3 has no successor in the domain
@test check(∃(x, ∀(y, Less(y, x) ∨ (y ≐ x))), M)
@test check(∀(x, Less(x, zero) → Even(x)), M)  # vacuously true
@test check(¬∃(x, Less(x, zero)), M)
@test check(∀(x, x ≐ x), M)
@test !check(∀(x, y, x ≐ y), M)

# inner quantifiers shadow outer bindings and free variables
@test check(∃(x, Even(x)), M; assignment = Dict(x => 1))
@test check(∀(x, ∃(x, Even(x))), M)

# duality: ∀x φ ≡ ¬∃x ¬φ
for χ in [Even(x), Less(x, succ(x)), Less(zero, x) ∨ (x ≐ zero)]
    @test check(ForAll(x)(χ), M) == check(¬Exists(x)(¬χ), M)
    @test check(Exists(x)(χ), M) == check(¬ForAll(x)(¬χ), M)
end

# predicates interpreted as collections of tuples or elements
N = FOStructure([:a, :b, :c]; constants = [a => :a],
    predicates = [Less => Set([(:a, :b), (:b, :c), (:a, :c)]), Even => [:b]],)
@test check(∃(x, Even(x)), N)
@test check(∀(x, Less(a, x) ∨ (a ≐ x)), N)
@test check(∀(x, y, z, Less(x, y) ∧ Less(y, z) → Less(x, z)), N)
@test !check(∀(x, ∃(y, Less(x, y))), N)

# empty domains: ∀ is vacuously true, ∃ is false
E = FOStructure(Int[]; predicates = [Even => iseven])
@test check(∀(x, Even(x)), E)
@test !check(∃(x, Even(x)), E)

# truth constants are interpreted as themselves
@test check(∀(x, Even(x) ∨ ⊤), M)
@test !check(∃(x, ⊥), M)

end
