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
    ("Base", [
        "general.jl",
        "parse.jl",
        "random.jl",
    ]),

    ("Formulas", [
        "formulas/generation.jl",
        "formulas/input.jl"
    ]),

   ("Logics", ["logics/operators.jl", "logics/logics.jl"]),

    ("Algebras", [
        "algebras/worlds.jl",
    ]),
]

@testset "Test Suites" begin
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
