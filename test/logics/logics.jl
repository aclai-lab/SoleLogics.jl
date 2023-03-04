# TODO adjust
# d = 3
# rels = vec(collect(Iterators.product([HSRELATIONS for _ = 1:d]...)))
# @test ("L̅", "L̅", "B̅") in rels
# @test ("=", "=", "O̅") in rels
# @test ("B", "B", "E") in rels
# @test ("L", "A", "=") in rels
# @test ("D", "D", "D") in rels
# @test length(rels) == length(HSRELATIONS)^d

# d = 4
# rels = vec(collect(Iterators.product([HS₃RELATIONS for _ = 1:d]...)))
# @test ("L̅", "L̅", "I", "L̅") in rels
# @test ("I", "I", "L", "L") in rels
# @test (("L", "O", "L", "O") in rels) == false
# @test (("L̅", "L̅", "L̅", "DBE") in rels) == false
# @test (("L", "L", "L", "A") in rels) == false
# @test length(rels) == length(HS₃RELATIONS)^d

# d = 2
# rels = vec(collect(Iterators.product([HS₇RELATIONS for _ = 1:d]...)))
# @test ("AO", "=") in rels
# @test ("DBE", "L") in rels
# @test ("A̅O̅", "D̅B̅E̅") in rels
# @test ("=", "=") in rels
# @test ("L̅", "L") in rels
# @test length(rels) == length(HS₇RELATIONS)^d

# modops = @modaloperators HSRELATIONS 2
# @test EXMODOP("L,L") in modops
# @test (EXMODOP("L, L") in modops) == false
# @test EXMODOP(("L", "L")) in modops
