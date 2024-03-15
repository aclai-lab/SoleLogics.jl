using SoleLogics
using Test

@test_nowarn SoleLogics.displaysyntaxvector([Atom(1)])
# Atom{Int64}["1"]

@test_nowarn SoleLogics.displaysyntaxvector([Atom(1)]; quotes = false)
# Atom{Int64}[1]

@test_nowarn SoleLogics.displaysyntaxvector(Atom.(1:20); quotes = false)
# Atom{Int64}[1, 2, 3, 4, ..., 16, 17, 18, 19, 20]

@test_nowarn SoleLogics.displaysyntaxvector(Atom.(1:20))
# Atom{Int64}["1", "2", "3", "4", ..., "16", "17", "18", "19", "20"]
