using SoleLogics
using Test

@testset "Formula tree" begin
    n1 = Node(CONJUNCTION)

    @test string(n1.token) == "∧"
    @test isdefined(n1, :parent) == false
    @test isdefined(n1, :leftchild) == false
    @test isdefined(n1, :rightchild) == false
    @test isleaf(n1) == true

    #    n1
    #    │
    # n1l┴n1r

    n1l = Node("p")
    n1r = Node("q")
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

    n2 = Node(NEGATION)
    n2r = Node("t")
    n3 = Node(DISJUNCTION)

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
end

@testset "Operators" begin
    @test is_unary_operator(NEGATION) == true
    @test is_unary_operator(IMPLICATION) == false
    @test is_unary_operator(CONJUNCTION) == false
    @test is_unary_operator(DISJUNCTION) == false
    @test is_unary_operator(DIAMOND) == true
    @test is_unary_operator(BOX) == true

    @test is_binary_operator(NEGATION) == false
    @test is_binary_operator(IMPLICATION) == true
    @test is_binary_operator(CONJUNCTION) == true
    @test is_binary_operator(DISJUNCTION) == true
    @test is_binary_operator(DIAMOND) == false
    @test is_binary_operator(BOX) == false

    @test typeof(DIAMOND) <: AbstractUnaryOperator
    @test typeof(DIAMOND) <: AbstractModalOperator
    @test typeof(DIAMOND) <: AbstractExistentialModalOperator

    @test typeof(BOX) <: AbstractUnaryOperator
    @test typeof(BOX) <: AbstractModalOperator
    @test typeof(BOX) <: AbstractUniversalModalOperator

    op1234 = EXMODOP(("OP1", "OP2", "OP3", "OP4"))
    @test EXMODOP(("OP1", "OP2", "OP3", "OP4")) isa AbstractExistentialModalOperator
    @test is_unary_operator(op1234) == true
    @test is_binary_operator(op1234) == false
end

@testset "HS modal logic" begin
    d = 3
    rels = vec(collect(Iterators.product([HSRELATIONS for _ in 1:d]...)))
    @test ("L̅", "L̅", "B̅") in rels
    @test ("=", "=", "O̅") in rels
    @test ("B", "B", "E") in rels
    @test ("L", "A", "=") in rels
    @test ("D", "D", "D") in rels
    @test length(rels) == length(HSRELATIONS) ^ d

    d = 4
    rels = vec(collect(Iterators.product([HS₃RELATIONS for _ in 1:d]...)))
    @test ("L̅", "L̅", "I", "L̅") in rels
    @test ("I", "I", "L", "L") in rels
    @test (("L", "O", "L", "O") in rels) == false
    @test (("L̅", "L̅", "L̅", "DBE") in rels) == false
    @test (("L", "L", "L", "A") in rels) == false
    @test length(rels) == length(HS₃RELATIONS) ^ d

    d = 2
    rels = vec(collect(Iterators.product([HS₇RELATIONS for _ in 1:d]...)))
    @test ("AO", "=") in rels
    @test ("DBE", "L") in rels
    @test ("A̅O̅", "D̅B̅E̅") in rels
    @test ("=", "=") in rels
    @test ("L̅", "L") in rels
    @test length(rels) == length(HS₇RELATIONS) ^ d

    modops = @modaloperators HSRELATIONS 2
    @test EXMODOP("L,L") in modops
    @test (EXMODOP("L, L") in modops) == false
    @test EXMODOP(("L","L")) in modops
end
