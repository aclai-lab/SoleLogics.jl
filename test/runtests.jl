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
    ("Syntactical", ["syntactical.jl",]),
    ("Parse", ["parse.jl",]),
    ("Normalize", ["normalize.jl",]),
    ("Syntax Utils", ["syntax-utils.jl",]),

    ("Propositional logic", ["check/propositional.jl"]),
    
    ("Check performance", ["check-performance.jl"]),

    ("Formula Generation", ["formulas/generation.jl",]),
    ("Formula I/O", ["formulas/input.jl",]),
    ("Normal Forms", ["formulas/normal-forms.jl",]),

    ("Operators", ["logics/operators.jl"]),
    # ("Logics", ["logics/logics.jl"]),

    ("Interpretation Sets", ["interpretation-sets.jl"]),

    ("Modal logic: worlds", ["modal-logic/frames/worlds.jl",]),
    ("Modal logic: frames", ["modal-logic/frames/frames.jl",]),
    ("Modal logic: relations", ["modal-logic/frames/relations.jl",]),

    ("Modal logic: unimodal", ["modal-logic/modal-logic.jl"]),
    ("Modal logic: multimodal", ["modal-logic/multi-modal-logic.jl"]),
    ("Modal logic: Kripke word", ["modal-logic/kripke-word.jl",]),
    ("Modal logic: Kripke image", ["modal-logic/kripke-image.jl",]),


    ("Generation: formula", ["generation/formula.jl",]),
    ("Generation: models", ["generation/models.jl",]),

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
