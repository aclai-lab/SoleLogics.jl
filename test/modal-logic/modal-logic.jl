using SoleLogics

using Graphs
using Random
using Test

# you can use Debugger.jl to investigate the stacktrace of a function call;
# this is super useful to learn the exact dispatches that are invoked by a function call.
# using Debugger
# https://discourse.julialang.org/t/how-to-trace-the-functions-called-by-my-script/106975/6

@atoms String p q

# create a kripke frame
worlds = SoleLogics.World.(1:5)
edges = Edge.([(1,2), (1,3), (2,4), (3,4), (3,5)])
kframe = SoleLogics.ExplicitCrispUniModalFrame(worlds, Graphs.SimpleDiGraph(edges))

# create a kripke structure, by enriching the frame with a valuation function
valuation = Dict([
    worlds[1] => TruthDict([p => true, q => false]),
    worlds[2] => TruthDict([p => true, q => true]),
    worlds[3] => TruthDict([p => true, q => false]),
    worlds[4] => TruthDict([p => false, q => false]),
    worlds[5] => TruthDict([p => false, q => true]),
 ])
kstruct = KripkeStructure(kframe, valuation)


##### frame-related behaviour of graded connectives ########################################
##### (mainly, collateworlds) ##############################################################

# in these tests, the behaviour of ◊₂ and □₂ emerges naturally from how the frame is shaped

# ¬□(p ∨ q)
f = DISJUNCTION(p, q) |> BOX |> NEGATION

@test collateworlds(kframe, DIAMOND, ([World(4)], ) ) == World{Int64}.([2,3])
@test collateworlds(kframe, DIAMOND2, ([World(2), World(3)],)) == World{Int64}.([1])
@test collateworlds(kframe, DIAMOND3, ([World(2), World(3)],)) == World{Int64}[]

@test collateworlds(kframe, BOX, ([World(1)],)) == World{Int64}.([4,5])
@test collateworlds(kframe, BOX2, ([World(1)],)) == World{Int64}.([1,2,3,4,5])
@test collateworlds(kframe, BOX3, ([World(1)],)) == World{Int64}.([1,2,3,4,5])

# for example, the result here is [2,4,5] because, given that something is true on 4,
# then BOX(something) is certainly true on 2; 4 and 5 are considered too, as they
# no neighbors.
@test collateworlds(kframe, BOX, ([World(4)],)) == World{Int64}.([2,4,5])

# BOX2 holds, since every world has <=2 neighbors on which an imaginary property (which is
# true on w4) is bot... of course! This is trivial since there are at most 2 neighbors.
@test collateworlds(kframe, BOX2, ([World(4)],)) == World{Int64}.([1,2,3,4,5])
@test collateworlds(kframe, BOX3, ([World(4)],)) == World{Int64}.([1,2,3,4,5])

@test check(f, kstruct, worlds[1]) == false
@test check(f, kstruct, worlds[2]) == true
@test check(f, kstruct, worlds[3]) == true
@test check(f, kstruct, worlds[4]) == false
@test check(f, kstruct, worlds[5]) == false


# ◊2(p ∨ q)
gradedf1 = DISJUNCTION(p, q) |> DIAMOND2

@test collateworlds(kframe, DIAMOND2, ([SoleLogics.World(4)],)) == World{Int64}[]

@test check(gradedf1, kstruct, worlds[1]) == true
@test check(gradedf1, kstruct, worlds[2]) == false
@test check(gradedf1, kstruct, worlds[3]) == false
@test check(gradedf1, kstruct, worlds[4]) == false
@test check(gradedf1, kstruct, worlds[5]) == false

for i in 1:5
    # diamonds
    @test collateworlds(kframe, DIAMOND2, ([World(i)], )) == World.([])
    @test collateworlds(kframe, DIAMOND3, ([World(i)], )) == World.([])

    # boxes
    @test collateworlds(kframe, BOX2, ([SoleLogics.World(i)],)) == World{Int64}.([
        1,2,3,4,5
    ])
    @test collateworlds(kframe, BOX3, ([SoleLogics.World(i)],)) == World{Int64}.([
        1,2,3,4,5
    ])

    # dual form (threshold is 2)
    @test collateworlds(kframe, BOX2, ([World(i)], )) == collateworlds(
        kframe, dual(DIAMOND2), ([World(i)], ))
    @test collateworlds(kframe, DIAMOND2, ([World(i)], )) == collateworlds(
        kframe, dual(BOX2), ([World(i)], ))

    # dual forms (threshold is 3)
    @test collateworlds(kframe, BOX3, ([World(i)], )) == collateworlds(
        kframe, dual(DIAMOND3), ([World(i)], ))
    @test collateworlds(kframe, DIAMOND3, ([World(i)], )) == collateworlds(
        kframe, dual(BOX3), ([World(i)], ))
end


##### model checking algorithm #############################################################

worlds2 = SoleLogics.World.(1:5)
edges2 = Edge.([(1,2), (1,3), (1,4), (5,5)])
kframe2 = SoleLogics.ExplicitCrispUniModalFrame(worlds2, Graphs.SimpleDiGraph(edges2))

# create a kripke structure, by enriching the frame with a valuation function
valuation2 = Dict([
    worlds2[1] => TruthDict([p => false]),
    worlds2[2] => TruthDict([p => true]),
    worlds2[3] => TruthDict([p => true]),
    worlds2[4] => TruthDict([p => false]),
    worlds2[5] => TruthDict([p => true]),
 ])
kstruct2 = KripkeStructure(kframe2, valuation2)

# note how the truth value changes when regulating the threshold of each connective
@test check(DIAMOND(p), kstruct2, worlds[1]) == true
@test check(DIAMOND2(p), kstruct2, worlds[1]) == true
@test check(DIAMOND3(p), kstruct2, worlds[1]) == false

@test check(BOX(p), kstruct2, worlds[1]) == false
@test check(BOX2(p), kstruct2, worlds[1]) == true
@test check(BOX3(p), kstruct2, worlds[1]) == true

# beware that the dual form of a ConstrainedConnective is called by the model checker
# to encode ¬dualop¬φ;
# here, dual(DIAMONDn) = BOXn, and dual(BOXn) = DIAMONDn

@test check(dual(BOX)(p), kstruct2, worlds[1]) == true
@test check(dual(BOX2)(p), kstruct2, worlds[1]) == true
@test check(dual(BOX3)(p), kstruct2, worlds[1]) == false

@test check(dual(DIAMOND)(p), kstruct2, worlds[1]) == false
@test check(dual(DIAMOND2)(p),  kstruct2, worlds[1]) == true
@test check(dual(DIAMOND3)(p),  kstruct2, worlds[1]) == true


############################################################################################


worlds3 = SoleLogics.World.(1:7)
edges3 = Edge.([(1,2), (1,3), (1,4), (1,5), (1,6), (1,7)])
kframe3 = SoleLogics.ExplicitCrispUniModalFrame(worlds3, Graphs.SimpleDiGraph(edges3))

# create a kripke structure, by enriching the frame with a valuation function
valuation3 = Dict([
    worlds3[1] => TruthDict([p => false]),
    worlds3[2] => TruthDict([p => true]),
    worlds3[3] => TruthDict([p => true]),
    worlds3[4] => TruthDict([p => true]),
    worlds3[5] => TruthDict([p => false]),
    worlds3[6] => TruthDict([p => false]),
    worlds3[7] => TruthDict([p => true])
 ])
kstruct3 = KripkeStructure(kframe3, valuation3)

@test check(DIAMOND(p), kstruct3, worlds[1]) == true
@test check(DIAMOND2(p), kstruct3, worlds[1]) == true
@test check(DIAMOND3(p), kstruct3, worlds[1]) == true

@test check(BOX(p), kstruct3, worlds[1]) == false
@test check(BOX2(p), kstruct3, worlds[1]) == false
@test check(BOX3(p), kstruct3, worlds[1]) == true

@test check(dual(BOX)(p), kstruct3, worlds[1]) == true
@test check(dual(BOX2)(p), kstruct3, worlds[1]) == true
@test check(dual(BOX3)(p), kstruct3, worlds[1]) == true

@test check(dual(DIAMOND)(p), kstruct3, worlds[1]) == false
@test check(dual(DIAMOND2)(p),  kstruct3, worlds[1]) == false
@test check(dual(DIAMOND3)(p),  kstruct3, worlds[1]) == true



##### more tests for double check and code coverage ########################################

@test_nowarn mygradedconnective = ConstrainedConnective{:🌞, 2}(==)
@test_nowarn ConstrainedConnective{:🌞}(==, 2)

@test SoleLogics.name(DIAMOND2) == :◊
@test condition(DIAMOND2) == >=
@test condition(DIAMOND2, 1) == false
@test threshold(DIAMOND2) == 2

@test_nowarn syntaxstring(DIAMOND2);

@test_nowarn DIAMOND2; # to trigger code coverage for Base.show

@test ismodal(◊ₙ) == true

@test ismodal(◊ₙ) == true
@test isbox(◊ₙ) == isbox(◊)
@test arity(◊ₙ) == 1
@test precedence(◊ₙ) == precedence(◊)
@test associativity(◊ₙ) == associativity(◊)

@test ismodal(□ₙ) == ismodal(□)
@test isbox(□ₙ) == isbox(□)
@test arity(□ₙ) == arity(□)
@test precedence(□ₙ) == precedence(□)
@test associativity(□ₙ) == associativity(□)

@test hasdual(DIAMOND2)
@test hasdual(DIAMOND3)
@test hasdual(BOX2)
@test hasdual(BOX3)

@test dual(DIAMOND2) == BOX2
@test dual(DIAMOND3) == BOX3
@test dual(BOX2) == DIAMOND2
@test dual(BOX3) == DIAMOND3
