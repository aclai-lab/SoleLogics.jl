using SoleLogics
using Graphs

@test_throws AssertionError(
    "Cannot instantiate `HeytingAlgebra` with domain of length 0. Need to specify at " *
    "least a top and a bottom element (to be placed at positions 1 and 2, respectively)."
) HeytingAlgebra(HeytingTruth[], Edge{Int64}[])

A = HeytingTruth("A", 3)
B = HeytingTruth("B", 4)
C = HeytingTruth("C", 5)
D = HeytingTruth("D", 6)
E = HeytingTruth("E", 7)
F = HeytingTruth("F", 8)
G = HeytingTruth("G", 9)
domain = [HeytingTruth(⊤), HeytingTruth(⊥), A, B, C, D, E, F, G]

# Missing the relation (F, ⊤), the lattice is not bounded (both F and ⊤ are top candidates)
relations = Edge.([
    (⊥, A),
    (⊥, B),
    (A, C),
    (A, D),
    (B, D),
    (B, E),
    (C, F),
    (D, F),
    (D, G),
    (E, G),
    (G, ⊤)
])

@test_throws AssertionError(
    "Tried to define an HeytingAlgebra with a graph which is not a bounded lattice."
) HeytingAlgebra(domain, relations)

# Missing the relation (⊥, B), the lattice is not bounded (both ⊥ and B are bot candidates)
relations = Edge.([
    (⊥, A),
    (A, C),
    (A, D),
    (B, D),
    (B, E),
    (C, F),
    (D, F),
    (D, G),
    (E, G),
    (F, ⊤),
    (G, ⊤)
])

@test_throws AssertionError(
    "Tried to define an HeytingAlgebra with a graph which is not a bounded lattice."
) HeytingAlgebra(domain, relations)

# C∧D is not defined, as upper bounds F and G are not comparable and both less than ⊤
relations = Edge.([
    (⊥, A),
    (⊥, B),
    (A, C),
    (A, D),
    (B, D),
    (B, E),
    (C, G),
    (C, F),
    (D, F),
    (D, G),
    (E, G),
    (F, ⊤),
    (G, ⊤)
])

@test_throws AssertionError(
    "Tried to define an HeytingAlgebra with a graph which is not a complete lattice."
) HeytingAlgebra(domain, relations)

# C∨D is not defined, as lower bounds A and B are not comparable and both greater than ⊥
relations = Edge.([
    (⊥, A),
    (⊥, B),
    (A, C),
    (A, D),
    (B, C),
    (B, D),
    (B, E),
    (C, F),
    (D, F),
    (D, G),
    (E, G),
    (F, ⊤),
    (G, ⊤)
])

@test_throws AssertionError(
    "Tried to define an HeytingAlgebra with a graph which is not a complete lattice."
) HeytingAlgebra(domain, relations)

relations = Edge.([
    (⊥, A),
    (⊥, B),
    (A, C),
    (A, D),
    (B, D),
    (B, E),
    (C, F),
    (D, F),
    (D, G),
    (E, G),
    (F, ⊤),
    (G, ⊤)
])
myalgebra = HeytingAlgebra(domain, relations, evaluate=true)

@test precedes(myalgebra, ⊥, ⊥) == false
@test precedes(myalgebra, ⊥, A) == true
@test precedes(myalgebra, ⊥, B) == true
@test precedes(myalgebra, ⊥, C) == true
@test precedes(myalgebra, ⊥, D) == true
@test precedes(myalgebra, ⊥, E) == true
@test precedes(myalgebra, ⊥, F) == true
@test precedes(myalgebra, ⊥, G) == true
@test precedes(myalgebra, ⊥, ⊤) == true
@test precedes(myalgebra, A, ⊥) == false
@test precedes(myalgebra, A, A) == false
@test precedes(myalgebra, A, B) == false
@test precedes(myalgebra, A, C) == true
@test precedes(myalgebra, A, D) == true
@test precedes(myalgebra, A, E) == false
@test precedes(myalgebra, A, F) == true
@test precedes(myalgebra, A, G) == true
@test precedes(myalgebra, A, ⊤) == true
@test precedes(myalgebra, B, ⊥) == false
@test precedes(myalgebra, B, A) == false
@test precedes(myalgebra, B, B) == false
@test precedes(myalgebra, B, C) == false
@test precedes(myalgebra, B, D) == true
@test precedes(myalgebra, B, E) == true
@test precedes(myalgebra, B, F) == true
@test precedes(myalgebra, B, G) == true
@test precedes(myalgebra, B, ⊤) == true
@test precedes(myalgebra, C, ⊥) == false
@test precedes(myalgebra, C, A) == false
@test precedes(myalgebra, C, B) == false
@test precedes(myalgebra, C, C) == false
@test precedes(myalgebra, C, D) == false
@test precedes(myalgebra, C, E) == false
@test precedes(myalgebra, C, F) == true
@test precedes(myalgebra, C, G) == false
@test precedes(myalgebra, C, ⊤) == true
@test precedes(myalgebra, D, ⊥) == false
@test precedes(myalgebra, D, A) == false
@test precedes(myalgebra, D, B) == false
@test precedes(myalgebra, D, C) == false
@test precedes(myalgebra, D, D) == false
@test precedes(myalgebra, D, E) == false
@test precedes(myalgebra, D, F) == true
@test precedes(myalgebra, D, G) == true
@test precedes(myalgebra, D, ⊤) == true
@test precedes(myalgebra, E, ⊥) == false
@test precedes(myalgebra, E, A) == false
@test precedes(myalgebra, E, B) == false
@test precedes(myalgebra, E, C) == false
@test precedes(myalgebra, E, D) == false
@test precedes(myalgebra, E, E) == false
@test precedes(myalgebra, E, F) == false
@test precedes(myalgebra, E, G) == true
@test precedes(myalgebra, E, ⊤) == true
@test precedes(myalgebra, F, ⊥) == false
@test precedes(myalgebra, F, A) == false
@test precedes(myalgebra, F, B) == false
@test precedes(myalgebra, F, C) == false
@test precedes(myalgebra, F, D) == false
@test precedes(myalgebra, F, E) == false
@test precedes(myalgebra, F, F) == false
@test precedes(myalgebra, F, G) == false
@test precedes(myalgebra, F, ⊤) == true
@test precedes(myalgebra, G, ⊥) == false
@test precedes(myalgebra, G, A) == false
@test precedes(myalgebra, G, B) == false
@test precedes(myalgebra, G, C) == false
@test precedes(myalgebra, G, D) == false
@test precedes(myalgebra, G, E) == false
@test precedes(myalgebra, G, F) == false
@test precedes(myalgebra, G, G) == false
@test precedes(myalgebra, G, ⊤) == true
@test precedes(myalgebra, ⊤, ⊥) == false
@test precedes(myalgebra, ⊤, A) == false
@test precedes(myalgebra, ⊤, B) == false
@test precedes(myalgebra, ⊤, C) == false
@test precedes(myalgebra, ⊤, D) == false
@test precedes(myalgebra, ⊤, E) == false
@test precedes(myalgebra, ⊤, F) == false
@test precedes(myalgebra, ⊤, G) == false
@test precedes(myalgebra, ⊤, ⊤) == false

@test precedeq(myalgebra, ⊥, ⊥) == true
@test precedeq(myalgebra, ⊥, A) == true
@test precedeq(myalgebra, ⊥, B) == true
@test precedeq(myalgebra, ⊥, C) == true
@test precedeq(myalgebra, ⊥, D) == true
@test precedeq(myalgebra, ⊥, E) == true
@test precedeq(myalgebra, ⊥, F) == true
@test precedeq(myalgebra, ⊥, G) == true
@test precedeq(myalgebra, ⊥, ⊤) == true
@test precedeq(myalgebra, A, ⊥) == false
@test precedeq(myalgebra, A, A) == true
@test precedeq(myalgebra, A, B) == false
@test precedeq(myalgebra, A, C) == true
@test precedeq(myalgebra, A, D) == true
@test precedeq(myalgebra, A, E) == false
@test precedeq(myalgebra, A, F) == true
@test precedeq(myalgebra, A, G) == true
@test precedeq(myalgebra, A, ⊤) == true
@test precedeq(myalgebra, B, ⊥) == false
@test precedeq(myalgebra, B, A) == false
@test precedeq(myalgebra, B, B) == true
@test precedeq(myalgebra, B, C) == false
@test precedeq(myalgebra, B, D) == true
@test precedeq(myalgebra, B, E) == true
@test precedeq(myalgebra, B, F) == true
@test precedeq(myalgebra, B, G) == true
@test precedeq(myalgebra, B, ⊤) == true
@test precedeq(myalgebra, C, ⊥) == false
@test precedeq(myalgebra, C, A) == false
@test precedeq(myalgebra, C, B) == false
@test precedeq(myalgebra, C, C) == true
@test precedeq(myalgebra, C, D) == false
@test precedeq(myalgebra, C, E) == false
@test precedeq(myalgebra, C, F) == true
@test precedeq(myalgebra, C, G) == false
@test precedeq(myalgebra, C, ⊤) == true
@test precedeq(myalgebra, D, ⊥) == false
@test precedeq(myalgebra, D, A) == false
@test precedeq(myalgebra, D, B) == false
@test precedeq(myalgebra, D, C) == false
@test precedeq(myalgebra, D, D) == true
@test precedeq(myalgebra, D, E) == false
@test precedeq(myalgebra, D, F) == true
@test precedeq(myalgebra, D, G) == true
@test precedeq(myalgebra, D, ⊤) == true
@test precedeq(myalgebra, E, ⊥) == false
@test precedeq(myalgebra, E, A) == false
@test precedeq(myalgebra, E, B) == false
@test precedeq(myalgebra, E, C) == false
@test precedeq(myalgebra, E, D) == false
@test precedeq(myalgebra, E, E) == true
@test precedeq(myalgebra, E, F) == false
@test precedeq(myalgebra, E, G) == true
@test precedeq(myalgebra, E, ⊤) == true
@test precedeq(myalgebra, F, ⊥) == false
@test precedeq(myalgebra, F, A) == false
@test precedeq(myalgebra, F, B) == false
@test precedeq(myalgebra, F, C) == false
@test precedeq(myalgebra, F, D) == false
@test precedeq(myalgebra, F, E) == false
@test precedeq(myalgebra, F, F) == true
@test precedeq(myalgebra, F, G) == false
@test precedeq(myalgebra, F, ⊤) == true
@test precedeq(myalgebra, G, ⊥) == false
@test precedeq(myalgebra, G, A) == false
@test precedeq(myalgebra, G, B) == false
@test precedeq(myalgebra, G, C) == false
@test precedeq(myalgebra, G, D) == false
@test precedeq(myalgebra, G, E) == false
@test precedeq(myalgebra, G, F) == false
@test precedeq(myalgebra, G, G) == true
@test precedeq(myalgebra, G, ⊤) == true
@test precedeq(myalgebra, ⊤, ⊥) == false
@test precedeq(myalgebra, ⊤, A) == false
@test precedeq(myalgebra, ⊤, B) == false
@test precedeq(myalgebra, ⊤, C) == false
@test precedeq(myalgebra, ⊤, D) == false
@test precedeq(myalgebra, ⊤, E) == false
@test precedeq(myalgebra, ⊤, F) == false
@test precedeq(myalgebra, ⊤, G) == false
@test precedeq(myalgebra, ⊤, ⊤) == true

@test succeedes(myalgebra, ⊥, ⊥) == false
@test succeedes(myalgebra, ⊥, A) == false
@test succeedes(myalgebra, ⊥, B) == false
@test succeedes(myalgebra, ⊥, C) == false
@test succeedes(myalgebra, ⊥, D) == false
@test succeedes(myalgebra, ⊥, E) == false
@test succeedes(myalgebra, ⊥, F) == false
@test succeedes(myalgebra, ⊥, G) == false
@test succeedes(myalgebra, ⊥, ⊤) == false
@test succeedes(myalgebra, A, ⊥) == true
@test succeedes(myalgebra, A, A) == false
@test succeedes(myalgebra, A, B) == false
@test succeedes(myalgebra, A, C) == false
@test succeedes(myalgebra, A, D) == false
@test succeedes(myalgebra, A, E) == false
@test succeedes(myalgebra, A, F) == false
@test succeedes(myalgebra, A, G) == false
@test succeedes(myalgebra, A, ⊤) == false
@test succeedes(myalgebra, B, ⊥) == true
@test succeedes(myalgebra, B, A) == false
@test succeedes(myalgebra, B, B) == false
@test succeedes(myalgebra, B, C) == false
@test succeedes(myalgebra, B, D) == false
@test succeedes(myalgebra, B, E) == false
@test succeedes(myalgebra, B, F) == false
@test succeedes(myalgebra, B, G) == false
@test succeedes(myalgebra, B, ⊤) == false
@test succeedes(myalgebra, C, ⊥) == true
@test succeedes(myalgebra, C, A) == true
@test succeedes(myalgebra, C, B) == false
@test succeedes(myalgebra, C, C) == false
@test succeedes(myalgebra, C, D) == false
@test succeedes(myalgebra, C, E) == false
@test succeedes(myalgebra, C, F) == false
@test succeedes(myalgebra, C, G) == false
@test succeedes(myalgebra, C, ⊤) == false
@test succeedes(myalgebra, D, ⊥) == true
@test succeedes(myalgebra, D, A) == true
@test succeedes(myalgebra, D, B) == true
@test succeedes(myalgebra, D, C) == false
@test succeedes(myalgebra, D, D) == false
@test succeedes(myalgebra, D, E) == false
@test succeedes(myalgebra, D, F) == false
@test succeedes(myalgebra, D, G) == false
@test succeedes(myalgebra, D, ⊤) == false
@test succeedes(myalgebra, E, ⊥) == true
@test succeedes(myalgebra, E, A) == false
@test succeedes(myalgebra, E, B) == true
@test succeedes(myalgebra, E, C) == false
@test succeedes(myalgebra, E, D) == false
@test succeedes(myalgebra, E, E) == false
@test succeedes(myalgebra, E, F) == false
@test succeedes(myalgebra, E, G) == false
@test succeedes(myalgebra, E, ⊤) == false
@test succeedes(myalgebra, F, ⊥) == true
@test succeedes(myalgebra, F, A) == true
@test succeedes(myalgebra, F, B) == true
@test succeedes(myalgebra, F, C) == true
@test succeedes(myalgebra, F, D) == true
@test succeedes(myalgebra, F, E) == false
@test succeedes(myalgebra, F, F) == false
@test succeedes(myalgebra, F, G) == false
@test succeedes(myalgebra, F, ⊤) == false
@test succeedes(myalgebra, G, ⊥) == true
@test succeedes(myalgebra, G, A) == true
@test succeedes(myalgebra, G, B) == true
@test succeedes(myalgebra, G, C) == false
@test succeedes(myalgebra, G, D) == true
@test succeedes(myalgebra, G, E) == true
@test succeedes(myalgebra, G, F) == false
@test succeedes(myalgebra, G, G) == false
@test succeedes(myalgebra, G, ⊤) == false
@test succeedes(myalgebra, ⊤, ⊥) == true
@test succeedes(myalgebra, ⊤, A) == true
@test succeedes(myalgebra, ⊤, B) == true
@test succeedes(myalgebra, ⊤, C) == true
@test succeedes(myalgebra, ⊤, D) == true
@test succeedes(myalgebra, ⊤, E) == true
@test succeedes(myalgebra, ⊤, F) == true
@test succeedes(myalgebra, ⊤, G) == true
@test succeedes(myalgebra, ⊤, ⊤) == false

@test succeedeq(myalgebra, ⊥, ⊥) == true
@test succeedeq(myalgebra, ⊥, A) == false
@test succeedeq(myalgebra, ⊥, B) == false
@test succeedeq(myalgebra, ⊥, C) == false
@test succeedeq(myalgebra, ⊥, D) == false
@test succeedeq(myalgebra, ⊥, E) == false
@test succeedeq(myalgebra, ⊥, F) == false
@test succeedeq(myalgebra, ⊥, G) == false
@test succeedeq(myalgebra, ⊥, ⊤) == false
@test succeedeq(myalgebra, A, ⊥) == true
@test succeedeq(myalgebra, A, A) == true
@test succeedeq(myalgebra, A, B) == false
@test succeedeq(myalgebra, A, C) == false
@test succeedeq(myalgebra, A, D) == false
@test succeedeq(myalgebra, A, E) == false
@test succeedeq(myalgebra, A, F) == false
@test succeedeq(myalgebra, A, G) == false
@test succeedeq(myalgebra, A, ⊤) == false
@test succeedeq(myalgebra, B, ⊥) == true
@test succeedeq(myalgebra, B, A) == false
@test succeedeq(myalgebra, B, B) == true
@test succeedeq(myalgebra, B, C) == false
@test succeedeq(myalgebra, B, D) == false
@test succeedeq(myalgebra, B, E) == false
@test succeedeq(myalgebra, B, F) == false
@test succeedeq(myalgebra, B, G) == false
@test succeedeq(myalgebra, B, ⊤) == false
@test succeedeq(myalgebra, C, ⊥) == true
@test succeedeq(myalgebra, C, A) == true
@test succeedeq(myalgebra, C, B) == false
@test succeedeq(myalgebra, C, C) == true
@test succeedeq(myalgebra, C, D) == false
@test succeedeq(myalgebra, C, E) == false
@test succeedeq(myalgebra, C, F) == false
@test succeedeq(myalgebra, C, G) == false
@test succeedeq(myalgebra, C, ⊤) == false
@test succeedeq(myalgebra, D, ⊥) == true
@test succeedeq(myalgebra, D, A) == true
@test succeedeq(myalgebra, D, B) == true
@test succeedeq(myalgebra, D, C) == false
@test succeedeq(myalgebra, D, D) == true
@test succeedeq(myalgebra, D, E) == false
@test succeedeq(myalgebra, D, F) == false
@test succeedeq(myalgebra, D, G) == false
@test succeedeq(myalgebra, D, ⊤) == false
@test succeedeq(myalgebra, E, ⊥) == true
@test succeedeq(myalgebra, E, A) == false
@test succeedeq(myalgebra, E, B) == true
@test succeedeq(myalgebra, E, C) == false
@test succeedeq(myalgebra, E, D) == false
@test succeedeq(myalgebra, E, E) == true
@test succeedeq(myalgebra, E, F) == false
@test succeedeq(myalgebra, E, G) == false
@test succeedeq(myalgebra, E, ⊤) == false
@test succeedeq(myalgebra, F, ⊥) == true
@test succeedeq(myalgebra, F, A) == true
@test succeedeq(myalgebra, F, B) == true
@test succeedeq(myalgebra, F, C) == true
@test succeedeq(myalgebra, F, D) == true
@test succeedeq(myalgebra, F, E) == false
@test succeedeq(myalgebra, F, F) == true
@test succeedeq(myalgebra, F, G) == false
@test succeedeq(myalgebra, F, ⊤) == false
@test succeedeq(myalgebra, G, ⊥) == true
@test succeedeq(myalgebra, G, A) == true
@test succeedeq(myalgebra, G, B) == true
@test succeedeq(myalgebra, G, C) == false
@test succeedeq(myalgebra, G, D) == true
@test succeedeq(myalgebra, G, E) == true
@test succeedeq(myalgebra, G, F) == false
@test succeedeq(myalgebra, G, G) == true
@test succeedeq(myalgebra, G, ⊤) == false
@test succeedeq(myalgebra, ⊤, ⊥) == true
@test succeedeq(myalgebra, ⊤, A) == true
@test succeedeq(myalgebra, ⊤, B) == true
@test succeedeq(myalgebra, ⊤, C) == true
@test succeedeq(myalgebra, ⊤, D) == true
@test succeedeq(myalgebra, ⊤, E) == true
@test succeedeq(myalgebra, ⊤, F) == true
@test succeedeq(myalgebra, ⊤, G) == true
@test succeedeq(myalgebra, ⊤, ⊤) == true

using SoleLogics: maximalmembers

@test Set{HeytingTruth}([maximalmembers(myalgebra, ⊥)...]) == Set{HeytingTruth}([HeytingTruth[]...])
@test Set{HeytingTruth}([maximalmembers(myalgebra, A)...]) == Set{HeytingTruth}([HeytingTruth[E]...])
@test Set{HeytingTruth}([maximalmembers(myalgebra, B)...]) == Set{HeytingTruth}([HeytingTruth[C]...])
@test Set{HeytingTruth}([maximalmembers(myalgebra, C)...]) == Set{HeytingTruth}([HeytingTruth[G]...])
@test Set{HeytingTruth}([maximalmembers(myalgebra, D)...]) == Set{HeytingTruth}([HeytingTruth[E, C]...])
@test Set{HeytingTruth}([maximalmembers(myalgebra, E)...]) == Set{HeytingTruth}([HeytingTruth[F]...])
@test Set{HeytingTruth}([maximalmembers(myalgebra, F)...]) == Set{HeytingTruth}([HeytingTruth[C, G]...])
@test Set{HeytingTruth}([maximalmembers(myalgebra, G)...]) == Set{HeytingTruth}([HeytingTruth[F, E]...])
@test Set{HeytingTruth}([maximalmembers(myalgebra, ⊤)...]) == Set{HeytingTruth}([HeytingTruth[F, G]...])

using SoleLogics: minimalmembers

@test Set{HeytingTruth}([minimalmembers(myalgebra, ⊥)...]) ==  Set{HeytingTruth}([HeytingTruth[B, A]...])
@test Set{HeytingTruth}([minimalmembers(myalgebra, A)...]) ==  Set{HeytingTruth}([HeytingTruth[C, B]...])
@test Set{HeytingTruth}([minimalmembers(myalgebra, B)...]) ==  Set{HeytingTruth}([HeytingTruth[E, A]...])
@test Set{HeytingTruth}([minimalmembers(myalgebra, C)...]) ==  Set{HeytingTruth}([HeytingTruth[B]...])
@test Set{HeytingTruth}([minimalmembers(myalgebra, D)...]) ==  Set{HeytingTruth}([HeytingTruth[E, C]...])
@test Set{HeytingTruth}([minimalmembers(myalgebra, E)...]) ==  Set{HeytingTruth}([HeytingTruth[A]...])
@test Set{HeytingTruth}([minimalmembers(myalgebra, F)...]) ==  Set{HeytingTruth}([HeytingTruth[E]...])
@test Set{HeytingTruth}([minimalmembers(myalgebra, G)...]) ==  Set{HeytingTruth}([HeytingTruth[C]...])
@test Set{HeytingTruth}([minimalmembers(myalgebra, ⊤)...]) ==  Set{HeytingTruth}([HeytingTruth[]...])

using SoleLogics: collatetruth

@test collatetruth(∧, (⊥, ⊥), myalgebra) == HeytingTruth(⊥)
@test collatetruth(∧, (⊥, A), myalgebra) == HeytingTruth(⊥)
@test collatetruth(∧, (⊥, B), myalgebra) == HeytingTruth(⊥)
@test collatetruth(∧, (⊥, C), myalgebra) == HeytingTruth(⊥)
@test collatetruth(∧, (⊥, D), myalgebra) == HeytingTruth(⊥)
@test collatetruth(∧, (⊥, E), myalgebra) == HeytingTruth(⊥)
@test collatetruth(∧, (⊥, F), myalgebra) == HeytingTruth(⊥)
@test collatetruth(∧, (⊥, G), myalgebra) == HeytingTruth(⊥)
@test collatetruth(∧, (⊥, ⊤), myalgebra) == HeytingTruth(⊥)
@test collatetruth(∧, (A, ⊥), myalgebra) == HeytingTruth(⊥)
@test collatetruth(∧, (A, A), myalgebra) == A
@test collatetruth(∧, (A, B), myalgebra) == HeytingTruth(⊥)
@test collatetruth(∧, (A, C), myalgebra) == A
@test collatetruth(∧, (A, D), myalgebra) == A
@test collatetruth(∧, (A, E), myalgebra) == HeytingTruth(⊥)
@test collatetruth(∧, (A, F), myalgebra) == A
@test collatetruth(∧, (A, G), myalgebra) == A
@test collatetruth(∧, (A, ⊤), myalgebra) == A
@test collatetruth(∧, (B, ⊥), myalgebra) == HeytingTruth(⊥)
@test collatetruth(∧, (B, A), myalgebra) == HeytingTruth(⊥)
@test collatetruth(∧, (B, B), myalgebra) == B
@test collatetruth(∧, (B, C), myalgebra) == HeytingTruth(⊥)
@test collatetruth(∧, (B, D), myalgebra) == B
@test collatetruth(∧, (B, E), myalgebra) == B
@test collatetruth(∧, (B, F), myalgebra) == B
@test collatetruth(∧, (B, G), myalgebra) == B
@test collatetruth(∧, (B, ⊤), myalgebra) == B
@test collatetruth(∧, (C, ⊥), myalgebra) == HeytingTruth(⊥)
@test collatetruth(∧, (C, A), myalgebra) == A
@test collatetruth(∧, (C, B), myalgebra) == HeytingTruth(⊥)
@test collatetruth(∧, (C, C), myalgebra) == C
@test collatetruth(∧, (C, D), myalgebra) == A
@test collatetruth(∧, (C, E), myalgebra) == HeytingTruth(⊥)
@test collatetruth(∧, (C, F), myalgebra) == C
@test collatetruth(∧, (C, G), myalgebra) == A
@test collatetruth(∧, (C, ⊤), myalgebra) == C
@test collatetruth(∧, (D, ⊥), myalgebra) == HeytingTruth(⊥)
@test collatetruth(∧, (D, A), myalgebra) == A
@test collatetruth(∧, (D, B), myalgebra) == B
@test collatetruth(∧, (D, C), myalgebra) == A
@test collatetruth(∧, (D, D), myalgebra) == D
@test collatetruth(∧, (D, E), myalgebra) == B
@test collatetruth(∧, (D, F), myalgebra) == D
@test collatetruth(∧, (D, G), myalgebra) == D
@test collatetruth(∧, (D, ⊤), myalgebra) == D
@test collatetruth(∧, (E, ⊥), myalgebra) == HeytingTruth(⊥)
@test collatetruth(∧, (E, A), myalgebra) == HeytingTruth(⊥)
@test collatetruth(∧, (E, B), myalgebra) == B
@test collatetruth(∧, (E, C), myalgebra) == HeytingTruth(⊥)
@test collatetruth(∧, (E, D), myalgebra) == B
@test collatetruth(∧, (E, E), myalgebra) == E
@test collatetruth(∧, (E, F), myalgebra) == B
@test collatetruth(∧, (E, G), myalgebra) == E
@test collatetruth(∧, (E, ⊤), myalgebra) == E
@test collatetruth(∧, (F, ⊥), myalgebra) == HeytingTruth(⊥)
@test collatetruth(∧, (F, A), myalgebra) == A
@test collatetruth(∧, (F, B), myalgebra) == B
@test collatetruth(∧, (F, C), myalgebra) == C
@test collatetruth(∧, (F, D), myalgebra) == D
@test collatetruth(∧, (F, E), myalgebra) == B
@test collatetruth(∧, (F, F), myalgebra) == F
@test collatetruth(∧, (F, G), myalgebra) == D
@test collatetruth(∧, (F, ⊤), myalgebra) == F
@test collatetruth(∧, (G, ⊥), myalgebra) == HeytingTruth(⊥)
@test collatetruth(∧, (G, A), myalgebra) == A
@test collatetruth(∧, (G, B), myalgebra) == B
@test collatetruth(∧, (G, C), myalgebra) == A
@test collatetruth(∧, (G, D), myalgebra) == D
@test collatetruth(∧, (G, E), myalgebra) == E
@test collatetruth(∧, (G, F), myalgebra) == D
@test collatetruth(∧, (G, G), myalgebra) == G
@test collatetruth(∧, (G, ⊤), myalgebra) == G
@test collatetruth(∧, (⊤, ⊥), myalgebra) == HeytingTruth(⊥)
@test collatetruth(∧, (⊤, A), myalgebra) == A
@test collatetruth(∧, (⊤, B), myalgebra) == B
@test collatetruth(∧, (⊤, C), myalgebra) == C
@test collatetruth(∧, (⊤, D), myalgebra) == D
@test collatetruth(∧, (⊤, E), myalgebra) == E
@test collatetruth(∧, (⊤, F), myalgebra) == F
@test collatetruth(∧, (⊤, G), myalgebra) == G
@test collatetruth(∧, (⊤, ⊤), myalgebra) == HeytingTruth(⊤)

@test collatetruth(∨, (⊥, ⊥), myalgebra) == HeytingTruth(⊥)
@test collatetruth(∨, (⊥, A), myalgebra) == A
@test collatetruth(∨, (⊥, B), myalgebra) == B
@test collatetruth(∨, (⊥, C), myalgebra) == C
@test collatetruth(∨, (⊥, D), myalgebra) == D
@test collatetruth(∨, (⊥, E), myalgebra) == E
@test collatetruth(∨, (⊥, F), myalgebra) == F
@test collatetruth(∨, (⊥, G), myalgebra) == G
@test collatetruth(∨, (⊥, ⊤), myalgebra) == HeytingTruth(⊤)
@test collatetruth(∨, (A, ⊥), myalgebra) == A
@test collatetruth(∨, (A, A), myalgebra) == A
@test collatetruth(∨, (A, B), myalgebra) == D
@test collatetruth(∨, (A, C), myalgebra) == C
@test collatetruth(∨, (A, D), myalgebra) == D
@test collatetruth(∨, (A, E), myalgebra) == G
@test collatetruth(∨, (A, F), myalgebra) == F
@test collatetruth(∨, (A, G), myalgebra) == G
@test collatetruth(∨, (A, ⊤), myalgebra) == HeytingTruth(⊤)
@test collatetruth(∨, (B, ⊥), myalgebra) == B
@test collatetruth(∨, (B, A), myalgebra) == D
@test collatetruth(∨, (B, B), myalgebra) == B
@test collatetruth(∨, (B, C), myalgebra) == F
@test collatetruth(∨, (B, D), myalgebra) == D
@test collatetruth(∨, (B, E), myalgebra) == E
@test collatetruth(∨, (B, F), myalgebra) == F
@test collatetruth(∨, (B, G), myalgebra) == G
@test collatetruth(∨, (B, ⊤), myalgebra) == HeytingTruth(⊤)
@test collatetruth(∨, (C, ⊥), myalgebra) == C
@test collatetruth(∨, (C, A), myalgebra) == C
@test collatetruth(∨, (C, B), myalgebra) == F
@test collatetruth(∨, (C, C), myalgebra) == C
@test collatetruth(∨, (C, D), myalgebra) == F
@test collatetruth(∨, (C, E), myalgebra) == HeytingTruth(⊤)
@test collatetruth(∨, (C, F), myalgebra) == F
@test collatetruth(∨, (C, G), myalgebra) == HeytingTruth(⊤)
@test collatetruth(∨, (C, ⊤), myalgebra) == HeytingTruth(⊤)
@test collatetruth(∨, (D, ⊥), myalgebra) == D
@test collatetruth(∨, (D, A), myalgebra) == D
@test collatetruth(∨, (D, B), myalgebra) == D
@test collatetruth(∨, (D, C), myalgebra) == F
@test collatetruth(∨, (D, D), myalgebra) == D
@test collatetruth(∨, (D, E), myalgebra) == G
@test collatetruth(∨, (D, F), myalgebra) == F
@test collatetruth(∨, (D, G), myalgebra) == G
@test collatetruth(∨, (D, ⊤), myalgebra) == HeytingTruth(⊤)
@test collatetruth(∨, (E, ⊥), myalgebra) == E
@test collatetruth(∨, (E, A), myalgebra) == G
@test collatetruth(∨, (E, B), myalgebra) == E
@test collatetruth(∨, (E, C), myalgebra) == HeytingTruth(⊤)
@test collatetruth(∨, (E, D), myalgebra) == G
@test collatetruth(∨, (E, E), myalgebra) == E
@test collatetruth(∨, (E, F), myalgebra) == HeytingTruth(⊤)
@test collatetruth(∨, (E, G), myalgebra) == G
@test collatetruth(∨, (E, ⊤), myalgebra) == HeytingTruth(⊤)
@test collatetruth(∨, (F, ⊥), myalgebra) == F
@test collatetruth(∨, (F, A), myalgebra) == F
@test collatetruth(∨, (F, B), myalgebra) == F
@test collatetruth(∨, (F, C), myalgebra) == F
@test collatetruth(∨, (F, D), myalgebra) == F
@test collatetruth(∨, (F, E), myalgebra) == HeytingTruth(⊤)
@test collatetruth(∨, (F, F), myalgebra) == F
@test collatetruth(∨, (F, G), myalgebra) == HeytingTruth(⊤)
@test collatetruth(∨, (F, ⊤), myalgebra) == HeytingTruth(⊤)
@test collatetruth(∨, (G, ⊥), myalgebra) == G
@test collatetruth(∨, (G, A), myalgebra) == G
@test collatetruth(∨, (G, B), myalgebra) == G
@test collatetruth(∨, (G, C), myalgebra) == HeytingTruth(⊤)
@test collatetruth(∨, (G, D), myalgebra) == G
@test collatetruth(∨, (G, E), myalgebra) == G
@test collatetruth(∨, (G, F), myalgebra) == HeytingTruth(⊤)
@test collatetruth(∨, (G, G), myalgebra) == G
@test collatetruth(∨, (G, ⊤), myalgebra) == HeytingTruth(⊤)
@test collatetruth(∨, (⊤, ⊥), myalgebra) == HeytingTruth(⊤)
@test collatetruth(∨, (⊤, A), myalgebra) == HeytingTruth(⊤)
@test collatetruth(∨, (⊤, B), myalgebra) == HeytingTruth(⊤)
@test collatetruth(∨, (⊤, C), myalgebra) == HeytingTruth(⊤)
@test collatetruth(∨, (⊤, D), myalgebra) == HeytingTruth(⊤)
@test collatetruth(∨, (⊤, E), myalgebra) == HeytingTruth(⊤)
@test collatetruth(∨, (⊤, F), myalgebra) == HeytingTruth(⊤)
@test collatetruth(∨, (⊤, G), myalgebra) == HeytingTruth(⊤)
@test collatetruth(∨, (⊤, ⊤), myalgebra) == HeytingTruth(⊤)

@test collatetruth(→, (⊥, ⊥), myalgebra) == HeytingTruth(⊤)
@test collatetruth(→, (⊥, A), myalgebra) == HeytingTruth(⊤)
@test collatetruth(→, (⊥, B), myalgebra) == HeytingTruth(⊤)
@test collatetruth(→, (⊥, C), myalgebra) == HeytingTruth(⊤)
@test collatetruth(→, (⊥, D), myalgebra) == HeytingTruth(⊤)
@test collatetruth(→, (⊥, E), myalgebra) == HeytingTruth(⊤)
@test collatetruth(→, (⊥, F), myalgebra) == HeytingTruth(⊤)
@test collatetruth(→, (⊥, G), myalgebra) == HeytingTruth(⊤)
@test collatetruth(→, (⊥, ⊤), myalgebra) == HeytingTruth(⊤)
@test collatetruth(→, (A, ⊥), myalgebra) == E
@test collatetruth(→, (A, A), myalgebra) == HeytingTruth(⊤)
@test collatetruth(→, (A, B), myalgebra) == E
@test collatetruth(→, (A, C), myalgebra) == HeytingTruth(⊤)
@test collatetruth(→, (A, D), myalgebra) == HeytingTruth(⊤)
@test collatetruth(→, (A, E), myalgebra) == E
@test collatetruth(→, (A, F), myalgebra) == HeytingTruth(⊤)
@test collatetruth(→, (A, G), myalgebra) == HeytingTruth(⊤)
@test collatetruth(→, (A, ⊤), myalgebra) == HeytingTruth(⊤)
@test collatetruth(→, (B, ⊥), myalgebra) == C
@test collatetruth(→, (B, A), myalgebra) == C
@test collatetruth(→, (B, B), myalgebra) == HeytingTruth(⊤)
@test collatetruth(→, (B, C), myalgebra) == C
@test collatetruth(→, (B, D), myalgebra) == HeytingTruth(⊤)
@test collatetruth(→, (B, E), myalgebra) == HeytingTruth(⊤)
@test collatetruth(→, (B, F), myalgebra) == HeytingTruth(⊤)
@test collatetruth(→, (B, G), myalgebra) == HeytingTruth(⊤)
@test collatetruth(→, (B, ⊤), myalgebra) == HeytingTruth(⊤)
@test collatetruth(→, (C, ⊥), myalgebra) == E
@test collatetruth(→, (C, A), myalgebra) == G
@test collatetruth(→, (C, B), myalgebra) == E
@test collatetruth(→, (C, C), myalgebra) == HeytingTruth(⊤)
@test collatetruth(→, (C, D), myalgebra) == G
@test collatetruth(→, (C, E), myalgebra) == E
@test collatetruth(→, (C, F), myalgebra) == HeytingTruth(⊤)
@test collatetruth(→, (C, G), myalgebra) == G
@test collatetruth(→, (C, ⊤), myalgebra) == HeytingTruth(⊤)
@test collatetruth(→, (D, ⊥), myalgebra) == HeytingTruth(⊥)
@test collatetruth(→, (D, A), myalgebra) == C
@test collatetruth(→, (D, B), myalgebra) == E
@test collatetruth(→, (D, C), myalgebra) == C
@test collatetruth(→, (D, D), myalgebra) == HeytingTruth(⊤)
@test collatetruth(→, (D, E), myalgebra) == E
@test collatetruth(→, (D, F), myalgebra) == HeytingTruth(⊤)
@test collatetruth(→, (D, G), myalgebra) == HeytingTruth(⊤)
@test collatetruth(→, (D, ⊤), myalgebra) == HeytingTruth(⊤)
@test collatetruth(→, (E, ⊥), myalgebra) == C
@test collatetruth(→, (E, A), myalgebra) == C
@test collatetruth(→, (E, B), myalgebra) == F
@test collatetruth(→, (E, C), myalgebra) == C
@test collatetruth(→, (E, D), myalgebra) == F
@test collatetruth(→, (E, E), myalgebra) == HeytingTruth(⊤)
@test collatetruth(→, (E, F), myalgebra) == F
@test collatetruth(→, (E, G), myalgebra) == HeytingTruth(⊤)
@test collatetruth(→, (E, ⊤), myalgebra) == HeytingTruth(⊤)
@test collatetruth(→, (F, ⊥), myalgebra) == HeytingTruth(⊥)
@test collatetruth(→, (F, A), myalgebra) == A
@test collatetruth(→, (F, B), myalgebra) == E
@test collatetruth(→, (F, C), myalgebra) == C
@test collatetruth(→, (F, D), myalgebra) == G
@test collatetruth(→, (F, E), myalgebra) == E
@test collatetruth(→, (F, F), myalgebra) == HeytingTruth(⊤)
@test collatetruth(→, (F, G), myalgebra) == G
@test collatetruth(→, (F, ⊤), myalgebra) == HeytingTruth(⊤)
@test collatetruth(→, (G, ⊥), myalgebra) == HeytingTruth(⊥)
@test collatetruth(→, (G, A), myalgebra) == C
@test collatetruth(→, (G, B), myalgebra) == B
@test collatetruth(→, (G, C), myalgebra) == C
@test collatetruth(→, (G, D), myalgebra) == F
@test collatetruth(→, (G, E), myalgebra) == E
@test collatetruth(→, (G, F), myalgebra) == F
@test collatetruth(→, (G, G), myalgebra) == HeytingTruth(⊤)
@test collatetruth(→, (G, ⊤), myalgebra) == HeytingTruth(⊤)
@test collatetruth(→, (⊤, ⊥), myalgebra) == HeytingTruth(⊥)
@test collatetruth(→, (⊤, A), myalgebra) == A
@test collatetruth(→, (⊤, B), myalgebra) == B
@test collatetruth(→, (⊤, C), myalgebra) == C
@test collatetruth(→, (⊤, D), myalgebra) == D
@test collatetruth(→, (⊤, E), myalgebra) == E
@test collatetruth(→, (⊤, F), myalgebra) == F
@test collatetruth(→, (⊤, G), myalgebra) == G
@test collatetruth(→, (⊤, ⊤), myalgebra) == HeytingTruth(⊤)

### Testing if check works on random propositional formulas and it gives the same result ###

using Random

booleanalgebra = @heytingalgebra () (⊥, ⊤)
myalphabet = Atom.(["a", "b", "c", "d", "e", "f", "g"])

for i ∈ 20:200
    height = div(i, 10)
    rf = randformula(Random.MersenneTwister(i), height, myalphabet, SoleLogics.BASE_PROPOSITIONAL_CONNECTIVES)
    td = TruthDict(Dict([p => rand([true, false]) for p in unique(myalphabet)]))
    @test check(rf, td) == check(rf, td, booleanalgebra)
end
