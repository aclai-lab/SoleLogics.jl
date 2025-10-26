using SoleLogics

using Graphs
using Random
using Test

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

# in these tests, the behaviour of ◊₂ and □₂ emerges naturally from how the frame is shaped

# ¬□(p ∨ q)
f = DISJUNCTION(p, q) |> BOX |> NEGATION

@test collateworlds(kframe, DIAMOND, ([SoleLogics.World(4)], ) ) == World{Int64}.([2,3])

@test check(f, kstruct, worlds[1]) == false
@test check(f, kstruct, worlds[2]) == true
@test check(f, kstruct, worlds[3]) == false
@test check(f, kstruct, worlds[4]) == false
@test check(f, kstruct, worlds[5]) == false


# ◊2(p ∨ q)
gradedf1 = DISJUNCTION(p, q) |> DIAMOND2

@test collateworlds(kframe, DIAMOND2, ([SoleLogics.World(4)],)) == World{Int64}.([3])

# note how 2, 4 and 5 are trivially false... as they have less than 2 neighbors,
# but `gradedf1` formula is headed by a DIAMOND2 operator!
@test check(gradedf1, kstruct, worlds[1]) == true
@test check(gradedf1, kstruct, worlds[2]) == false
@test check(gradedf1, kstruct, worlds[3]) == true
@test check(gradedf1, kstruct, worlds[4]) == false
@test check(gradedf1, kstruct, worlds[5]) == false

for i in 1:5
    @test collateworlds(kframe, BOX2, ([SoleLogics.World(i)],)) ==
        collateworlds(kframe, BOX, ([SoleLogics.World(i)],))
end


# differently from the tests above, these needs to adjust the model checking algorithm
