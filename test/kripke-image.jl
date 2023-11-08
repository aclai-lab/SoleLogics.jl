using SoleLogics
import SoleLogics: AbstractKripkeStructure, frame, interpret
import SoleLogics: Point, FullDimensionalFrame


struct BWImageKripkeStructure <: AbstractKripkeStructure
	image::Matrix{Float64}
end

function interpret(a::Atom{Int}, i::BWImageKripkeStructure, w::Point{2,Int})
	integervalue = value(a)
	valueatpoint = i.image[w[1], w[2]]
	return (valueatpoint >= integervalue) ? TOP : BOT
end

frame(i::AbstractKripkeStructure) = FullDimensionalFrame(size(i.image), Point{2,Int})

using SoleLogics: Point2DRelations

ops = [SoleLogics.diamond.(Point2DRelations)..., SoleLogics.box.(Point2DRelations)...]
φ = @test_nowarn parseformula("10 ∧ ⟨N⟩ 2 ∧ [NE] -1", ops; atom_parser = x->(Atom{Int}(parse(Int, x))))
φ = @test_nowarn parseformula(SyntaxTree, "10 ∧ ⟨N⟩ 2 ∧ [NE] -1", ops; atom_parser = x->(Atom{Int}(parse(Int, x))))

@test_nowarn check(φ, BWImageKripkeStructure(rand(1:10,10,10)), Point(1,2))
@test_nowarn check(φ, BWImageKripkeStructure(rand(1:10,10,10)), Point(1,2))
@test_nowarn check(φ, BWImageKripkeStructure(rand(1:10,10,10)), Point(1,2))
@test_nowarn check(φ, BWImageKripkeStructure(rand(1:10,10,10)), Point(1,2))
@test_nowarn check(φ, BWImageKripkeStructure(rand(1:10,10,10)), Point(1,2))
@test_nowarn check(φ, BWImageKripkeStructure(rand(1:10,10,10)), Point(1,2))


@test eltype([collect(accessibles(BWImageKripkeStructure(rand(1:10,10,10)), Point(1,1), R)) for R in SoleLogics.Point2DRelations]) <: Array{<:Point}
@test eltype([collect(accessibles(BWImageKripkeStructure(rand(1:10,10,10)), Point(2,1), R)) for R in SoleLogics.Point2DRelations]) <: Array{<:Point}
@test eltype([collect(accessibles(BWImageKripkeStructure(rand(1:10,10,10)), Point(1,2), R)) for R in SoleLogics.Point2DRelations]) <: Array{<:Point}
@test eltype([collect(accessibles(BWImageKripkeStructure(rand(1:10,10,10)), Point(2,2), R)) for R in SoleLogics.Point2DRelations]) <: Array{<:Point}
@test eltype([collect(accessibles(BWImageKripkeStructure(rand(1:10,10,10)), Point(10,2), R)) for R in SoleLogics.Point2DRelations]) <: Array{<:Point}
@test eltype([collect(accessibles(BWImageKripkeStructure(rand(1:10,10,10)), Point(10,-1), R)) for R in SoleLogics.Point2DRelations]) <: Array{<:Point}
@test eltype([collect(accessibles(BWImageKripkeStructure(rand(1:10,10,10)), Point(9,9), R)) for R in SoleLogics.Point2DRelations]) <: Array{<:Point}
@test eltype([collect(accessibles(BWImageKripkeStructure(rand(1:10,10,10)), Point(9,8), R)) for R in SoleLogics.Point2DRelations]) <: Array{<:Point}
@test eltype([collect(accessibles(BWImageKripkeStructure(rand(1:10,10,10)), Point(8,9), R)) for R in SoleLogics.Point2DRelations]) <: Array{<:Point}
@test eltype([collect(accessibles(BWImageKripkeStructure(rand(1:10,10,10)), Point(8,8), R)) for R in SoleLogics.Point2DRelations]) <: Array{<:Point}

