using SoleLogics
using Test
using Random

function run_tests(list)
    println("\n" * ("#"^50))
    for test in list
        println("TEST: $test")
        @time include(test)
    end
end

println("Julia version: ", VERSION)

test_suites = [
    ("Core", ["core.jl",]),
    ("Parse", ["parse.jl",]),
    ("Normalize", ["normalize.jl",]),
    ("Syntax Utils", ["syntax-utils.jl",]),

    ("Formula Generation", ["formulas/generation.jl",]),
    ("Formula I/O", ["formulas/input.jl",]),
    ("Normal Forms", ["formulas/normal-forms.jl",]),

    ("Operators", ["logics/operators.jl"]),
    # ("Logics", ["logics/logics.jl"]),

    ("Interpretation Sets", ["interpretation-sets.jl"]),

    ("Propositional Logic", ["propositional-logic.jl"]),

    ("Algebras: worlds", ["frames/worlds.jl",]),
    ("Algebras: frames", ["frames/frames.jl",]),
    ("Algebras: relations", ["frames/relations.jl",]),

    ("MultiModalLogic", ["multi-modal-logic.jl"]),

    ("Generation: formula", ["generation/formula.jl",]),
    ("Generation: models", ["generation/models.jl",]),

    ("Kripke word", ["kripke-word.jl",]),
    ("Kripke image", ["kripke-image.jl",]),

    ("Pluto Demo", ["$(dirname(dirname(pathof(SoleLogics))))/pluto-demo.jl", ]),

    ("ManyValuedLogics", ["many-valued-logics.jl"]),

    ("Miscellaneous", ["misc.jl", "util.jl"]),
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
