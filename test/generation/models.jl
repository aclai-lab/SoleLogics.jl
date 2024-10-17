using Random

@testset "randframe + randmodel" begin

@test_nowarn randframe(42, 10, 20)
@test_nowarn randframe(Random.MersenneTwister(42), 10, 20)

@test_nowarn randmodel(42, 5, 10, [Atom("s"), Atom("p")], BooleanAlgebra())
@test_nowarn randmodel(42, 5, 10, [Atom("s"), Atom("p")], BooleanAlgebra())

end
