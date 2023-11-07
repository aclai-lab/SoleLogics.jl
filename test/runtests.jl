using SoleLogics
using Test
using Random

function run_tests(list)
    println("\n" * ("#"^50))
    for test in list
        println("TEST: $test")
        include(test)
    end
end

println("Julia version: ", VERSION)

test_suites = [
    ("Core", ["core.jl",]),
    ("Parse", ["parse.jl",]),
    ("Random", ["random.jl",]),
    ("Normalize", ["normalize.jl",]),

    ("Formula Generation", ["formulas/generation.jl",]),
    ("Formulas I/O", ["formulas/input.jl",]),

    ("Operators", ["logics/operators.jl"]),
    # ("Logics", ["logics/logics.jl"]),

    ("Algebras: worlds", ["algebras/worlds.jl",]),
    ("Algebras: frames", ["algebras/frames.jl",]),

    # ("Demos", [
    #     "pluto-demo.jl",
    # ]),
]

@testset "SoleLogics.jl" begin
    for ts in eachindex(test_suites)
        name = test_suites[ts][1]
        list = test_suites[ts][2]
        let
            @testset "$name" begin
                run_tests(list)
            end
        end
    end
    println()
end
