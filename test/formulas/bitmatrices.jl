using Test
using SoleLogics

@test_nowarn BitMatrixNormalForm(true, (rand(Bool, 3,3)))

conj_nf = @test_nowarn BitMatrixNormalForm(true, [
 0  1  1
 1  1  0
 1  1  1
])

disj_nf = @test_nowarn BitMatrixNormalForm(true, [
 0  1  1
 1  1  0
 1  1  1
])

