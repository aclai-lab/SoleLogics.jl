@testset "Formulas fundamental checks" begin
    function fxtest_general(h::Integer)
        formula = gen_formula(h)

        # Height check
        @test SoleLogics.height(tree(formula)) >= 0
        @test SoleLogics.height(tree(formula)) <= h
        # Size check
        @test SoleLogics.size(tree(formula)) > SoleLogics.height(tree(formula))
        @test SoleLogics.size(tree(formula)) <= 2^(SoleLogics.height(tree(formula)) + 1) - 1

        for node in subformulas(tree(formula), sorted = false)
            lsize = isdefined(node, :leftchild) ? SoleLogics.size(leftchild(node)) : 0
            rsize = isdefined(node, :rightchild) ? SoleLogics.size(rightchild(node)) : 0
            @test SoleLogics.size(node) == lsize + rsize + 1
        end
    end

    function fxtest_modal(height::Integer, max_modepth::Integer)
        root = gen_formula(height, max_modepth = max_modepth).tree
        @test SoleLogics.modal_depth(root) <= max_modepth
    end

    for i = 1:20
        fxtest_general(i)
    end
end

@testset "Modal depth regulation" begin
    function fxtest_modal(height::Integer, max_modepth::Integer)
        root = tree(gen_formula(height, max_modepth = max_modepth))
        @test SoleLogics.modal_depth(root) <= max_modepth
    end

    for i = 1:25
        fxtest_modal(i, i - rand(1:i))
    end
end
