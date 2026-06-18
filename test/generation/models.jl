using Random

@testset "randframe + randmodel" begin

    # Check whether two randframes are equal
    # TODO: create and export this dispatch
    function _isequal_frame(f1, f2)
        return f1.worlds == f2.worlds && f1.graph == f2.graph
    end

    # TODO: this is an important todo;
    # right now, this function is the same as _isequal_frame, since assignments
    # are not considered.
    #
    # The problem is that checking hte equality of two TruthDicts is deceptive;
    # try to create a randmodel `m` and then run:
    # m == deepcopy(m)
    # or 
    # m.assignments == deepcopy(m.assignments)
    function _isequal_model(m1, m2)
        return _isequal_frame(m1.frame, m2.frame)
    end

    # randframe and randmodel seed
    _rfrmseed = 42
    _atoms = [Atom("s"), Atom("p")]
    _ba = BooleanAlgebra()

    @test_nowarn randframe(_rfrmseed, 10, 20)
    @test_nowarn randframe(Random.MersenneTwister(_rfrmseed), 10, 20)

    randframes1 = [randframe(_rfrmseed + i, 10, 20) for i in 1:10]
    randframes2 = [randframe(_rfrmseed + i, 10, 20) for i in 1:10]


    @test all(t -> t == 1,
        [_isequal_frame(f1, f2) for (f1, f2) in zip(randframes1, randframes2)],)


    @test_nowarn randmodel(_rfrmseed, 5, 10, _atoms, _ba)

    randmodels1 = [randmodel(_rfrmseed + i, 5, 10, _atoms, _ba) for i in 1:10]
    randmodels2 = [randmodel(_rfrmseed + i, 5, 10, _atoms, _ba) for i in 1:10]

    @test all(t -> t == 1,
        [_isequal_model(m1, m2) for (m1, m2) in zip(randmodels1, randmodels2)],)
end
