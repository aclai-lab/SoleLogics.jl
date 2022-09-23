@testset "Formula construction from input" begin

    @testset "General suite" begin
        n1 = FNode(CONJUNCTION)

        @test string(n1.token) == "∧"
        @test isdefined(n1, :parent) == false
        @test isdefined(n1, :leftchild) == false
        @test isdefined(n1, :rightchild) == false
        @test isleaf(n1) == true

        #    n1
        #    │
        # n1l┴n1r

        n1l = FNode("p")
        n1r = FNode("q")
        parent!(n1l, n1)
        parent!(n1r, n1)
        leftchild!(n1, n1l)
        rightchild!(n1, n1r)
        size!(n1)

        @test leftchild(n1) == n1l
        @test rightchild(n1) == n1r
        @test SoleLogics.parent(n1l) == n1
        @test SoleLogics.parent(n1r) == n1

        @test SoleLogics.size(n1) == 3
        @test SoleLogics.size(n1l) == 1
        @test SoleLogics.size(n1r) == 1

        @test isleaf(n1) == false
        @test isleaf(n1l) == true
        @test isleaf(n1r) == true

        #    n3
        #    ┌┴──────┐
        #    n1      n2
        #    │       │
        # n1l┴n1r    ┴n2r

        n2 = FNode(NEGATION)
        n2r = FNode("t")
        n3 = FNode(DISJUNCTION)

        parent!(n2r, n2)
        rightchild!(n2, n2r)
        parent!(n1, n3)
        leftchild!(n3, n1)
        rightchild!(n3, n2)
        size!(n3)

        @test SoleLogics.size(n3) == 6
        @test SoleLogics.size(n1) == 3
        @test SoleLogics.size(n2) == 2
        @test SoleLogics.size(n2r) == 1
        @test SoleLogics.size(n1l) == 1
        @test SoleLogics.size(n1r) == 1

        #          ∨
        #    ┌─────┴─────┐
        #    ◊           ◊
        #    ┴─┐         ┴─┐
        #      p           ◊
        #                  ┴┐
        #                   s

        a = FNode(DISJUNCTION)
        b = FNode(DIAMOND)
        c = FNode(DIAMOND)
        d = FNode(DIAMOND)
        e = FNode(Letter("p"))
        f = FNode(Letter("s"))

        leftchild!(a, b)
        rightchild!(b, e)
        rightchild!(a, c)
        rightchild!(c, d)
        rightchild!(d, f)

        @test SoleLogics.modal_depth(a) == 2
    end


    @testset "Shunting yard and formula tree" begin
        # Formula tree testing is further explained in "Formula tree generation" testset

        #          ∨
        #    ┌─────┴─────┐
        #    ¬           ∧
        #    │           │
        #    ┴─┐       ┌─┴─┐
        #      ∧       □   ◊
        #      │       │   │
        #     ┌┴┐      ┴┐  ┴┐
        #     p q       r   s

        exp1 = "(¬(p∧q)∨(□r∧◊s))"
        sh1 = shunting_yard(exp1)
        f1 = build_tree(sh1)
        @test sh1 == [
            "p",
            "q",
            CONJUNCTION,
            NEGATION,
            "r",
            BOX,
            "s",
            DIAMOND,
            CONJUNCTION,
            DISJUNCTION,
        ]
        @test inorder(tree(f1)) == "((¬((p)∧(q)))∨((□(r))∧(◊(s))))"

        #     ∧
        # ┌───┴────┐
        # p        ∧
        #    ┌─────┴─────┐
        #    q           ∧
        #                │
        #              ┌─┴─┐
        #              r   ∧
        #                  │
        #                 ┌┴┐
        #                 s t
        exp2 = "(p∧q∧r∧s∧t)"
        sh2 = shunting_yard(exp2)
        f2 = build_tree(sh2)
        @test sh2 == ["p", "q", "r", "s", "t", fill(CONJUNCTION, (1, 4))...]
        @test inorder(tree(f2)) == "((p)∧((q)∧((r)∧((s)∧(t)))))"

        #             ∧
        #     ┌───────┴─────────────┐
        #     │                     ∧
        #     ∧                 ┌───┴───┐
        #     │                 ∧       ◊
        # ┌───┴───┐         ┌───┴───┐   ┴┐
        # p       q         r       s    t

        exp3 = "(p∧q)∧(r∧s)∧(◊t)"
        sh3 = shunting_yard(exp3)
        f3 = build_tree(sh3)
        @test sh3 == [
            "p",
            "q",
            CONJUNCTION,
            "r",
            "s",
            CONJUNCTION,
            "t",
            DIAMOND,
            CONJUNCTION,
            CONJUNCTION,
        ]
        @test inorder(tree(f3)) == "(((p)∧(q))∧(((r)∧(s))∧(◊(t))))"
    end

end
