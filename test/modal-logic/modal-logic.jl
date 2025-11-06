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
@test collateworlds(kframe, BOX2, ([World(1)],)) == World{Int64}.([2,4,5])
@test collateworlds(kframe, BOX3, ([World(1)],)) == World{Int64}.([1,2,3,4,5])

# for example, the result here is [2,4,5] because, given that something is true on 4,
# then BOX(something) is certainly true on 2; 4 and 5 are considered too, as they
# no neighbors.
@test collateworlds(kframe, BOX, ([World(4)],)) == World{Int64}.([2,4,5])

# BOX2 holds on every world that has <2 neighbors that are not 4
@test collateworlds(kframe, BOX2, ([World(4)],)) == World{Int64}.([2,3,4,5])
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
    if i != 2
        # in this case, BOX2 has a different outcome
        @test collateworlds(kframe, BOX2, ([SoleLogics.World(1)],)) == World{Int64}.([
            2,4,5
        ])
    end
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


@test collateworlds(kframe, BOX2, ([SoleLogics.World(2)],)) == World{Int64}.([1,2,4,5])

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

@test check(DIAMOND(p), kstruct3, worlds3[1]) == true
@test check(DIAMOND2(p), kstruct3, worlds3[1]) == true
@test check(DIAMOND3(p), kstruct3, worlds3[1]) == true

@test check(BOX(p), kstruct3, worlds3[1]) == false
@test check(BOX2(p), kstruct3, worlds3[1]) == false
@test check(BOX3(p), kstruct3, worlds3[1]) == true

@test check(dual(BOX)(p), kstruct3, worlds3[1]) == true
@test check(dual(BOX2)(p), kstruct3, worlds3[1]) == true
@test check(dual(BOX3)(p), kstruct3, worlds3[1]) == true

@test check(dual(DIAMOND)(p), kstruct3, worlds3[1]) == false
@test check(dual(DIAMOND2)(p),  kstruct3, worlds3[1]) == false
@test check(dual(DIAMOND3)(p),  kstruct3, worlds3[1]) == true


############################################################################################

worlds4 = SoleLogics.World.(1:7)
edges4 = Edge.([(1,2), (1,3), (1,4), (1,5), (1,6), (1,7)])
kframe4 = SoleLogics.ExplicitCrispUniModalFrame(worlds4, Graphs.SimpleDiGraph(edges4))

# create a kripke structure, by enriching the frame with a valuation function
valuation4 = Dict([
    worlds4[1] => TruthDict([p => false]),
    worlds4[2] => TruthDict([p => true]),
    worlds4[3] => TruthDict([p => true]),
    worlds4[4] => TruthDict([p => true]),
    worlds4[5] => TruthDict([p => false]),
    worlds4[6] => TruthDict([p => false]),
    worlds4[7] => TruthDict([p => false])
 ])
kstruct4 = KripkeStructure(kframe4, valuation4)

@test check(DIAMOND(p), kstruct4, worlds4[1]) == true
@test check(DIAMOND2(p), kstruct4, worlds4[1]) == true
@test check(DIAMOND3(p), kstruct4, worlds4[1]) == true

@test check(BOX(p), kstruct4, worlds4[1]) == false
@test check(BOX2(p), kstruct4, worlds4[1]) == false
@test_broken check(BOX3(p), kstruct4, worlds4[1]) == true

# maybe the dual of BOX2 is DIAMOND2 but with strict relation!

@test check(¬(DIAMOND3(¬p)), kstruct4, worlds4[1]) == true

@test check(dual(BOX)(p), kstruct4, worlds4[1]) == true
@test check(dual(BOX2)(p), kstruct4, worlds4[1]) == true
@test check(dual(BOX3)(p), kstruct4, worlds4[1]) == false

@test check(dual(DIAMOND)(p), kstruct4, worlds4[1]) == false
@test check(dual(DIAMOND2)(p),  kstruct4, worlds4[1]) == false
@test_broken check(dual(DIAMOND3)(p),  kstruct4, worlds4[1]) == true



##### more tests for double check and code coverage ########################################

@test_nowarn mygradedconnective = ConstrainedConnective{:🌞, 2}(==)
@test_nowarn ConstrainedConnective{:🌞}(2, ==)
@test_nowarn println(DIAMOND2)
@test_nowarn @show(BOX2)

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


##### Benchmarking check: ConstrainedConnectives vs NamedConnectives #######################
###
### using BenchmarkTools
###
### # little parameterization for random formulas generation
### nformulas = 1000
###
### _rng = Xoshiro(3278)
### _height = 5
### _letters = 'a':1:'z' |> collect
### _alphabet = ExplicitAlphabet(_letters)
###
### base_connectives = [NEGATION, CONJUNCTION, IMPLICATION]
### named_connectives = [DIAMOND, BOX]
### constrained_connectives = [DIAMOND2, DIAMOND3, BOX2, BOX3]
###
###
### _generate_formulas = c -> begin
### randformula(
###     _rng,
###     _height,
###     _alphabet,
###     vcat(base_connectives, c),
###     mode=:exactheight)
### end
###
### # random formulas
### named_connective_formulas = [_generate_formulas(named_connectives) for _ in 1:nformulas]
### constrained_connective_formulas = [
###     _generate_formulas(constrained_connectives) for _ in 1:nformulas]
###
### # little parameterization for random models generation
### nworlds = 10
### nedges = 30
###
### kstruct = randmodel(_rng, nworlds, nedges, Atom.(_letters), BooleanAlgebra())
###
### @benchmark map(f -> check(f, kstruct, World(1)), named_connective_formulas)
### # julia> @benchmark map(f -> check(f, kstruct, World(1)), named_connective_formulas)
### # BenchmarkTools.Trial: 21 samples with 1 evaluation per sample.
### #  Range (min … max):  207.603 ms … 336.039 ms  ┊ GC (min … max): 0.00% … 0.00%
### #  Time  (median):     227.827 ms               ┊ GC (median):    0.00%
### #  Time  (mean ± σ):   243.313 ms ±  44.320 ms  ┊ GC (mean ± σ):  2.94% ± 4.25%
### #
### #   █ ▁ ▁     ▄                                                 ▁
### #   █▆█▁█▁▁▆▁▆█▆▁▁▁▁▁▁▁▁▆▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▆▁▁▁▆▁▁▁▁▁▁▁▁▁▁▁▁▆▁▁▁▁█ ▁
### #   208 ms           Histogram: frequency by time          336 ms <
### #
### #  Memory estimate: 56.55 MiB, allocs estimate: 1688439.
###
### @benchmark map(f -> check(f, kstruct, World(1)), constrained_connective_formulas)
### # julia> @benchmark map(f -> check(f, kstruct, World(1)), constrained_connective_formulas)
### # BenchmarkTools.Trial: 21 samples with 1 evaluation per sample.
### #  Range (min … max):  218.614 ms … 293.397 ms  ┊ GC (min … max): 0.00% … 14.14%
### #  Time  (median):     233.706 ms               ┊ GC (median):    5.16%
### #  Time  (mean ± σ):   239.592 ms ±  19.281 ms  ┊ GC (mean ± σ):  3.57% ±  3.82%
### #
### #   █         ▁                      ▁
### #   █▁▁▁▁▆▆▁▆▁█▆▆▆▆▁▁▁▆▆▁▁▁▁▁▁▁▆▁▆▁▁▁█▁▁▁▁▁▁▁▆▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▆ ▁
### #   219 ms           Histogram: frequency by time          293 ms <
### #
### #  Memory estimate: 76.64 MiB, allocs estimate: 1932369.
###
