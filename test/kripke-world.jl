using SoleLogics
import SoleLogics: AbstractKripkeStructure, frame, interpret
import SoleLogics: Point, FullDimensionalFrame

struct KripkeWorld <: AbstractKripkeStructure
    word::String
end

frame(i::KripkeWorld) = FullDimensionalFrame((length(i.word),), Point{1,Int})

function interpret(a::Atom{Char}, i::KripkeWorld, w::Point)
    ch = value(a)
    wordch = i.word[w[1]]
    return (wordch == ch) ? TOP : BOT
end

accessibles(KripkeWorld("01001"), Point(3), globalrel) |> collect
accessibles(KripkeWorld("01001"), Point(3), SoleLogics.SuccessorRel) |> collect
accessibles(KripkeWorld("01001"), Point(3), SoleLogics.PredecessorRel) |> collect
accessibles(KripkeWorld("01001"), Point(3), SoleLogics.MinRel) |> collect
accessibles(KripkeWorld("01001"), Point(3), SoleLogics.MaxRel) |> collect

# Note: using checking algorithm that is not optimal in this case
check(SoleLogics.diamond(globalrel)(Atom('0')), KripkeWorld("01001"), Point(3))
check(SoleLogics.diamond(globalrel)(Atom('0')), KripkeWorld("01001"))
check(SoleLogics.diamond(globalrel)(Atom('0')), KripkeWorld("1111"), Point(3))
check(SoleLogics.diamond(globalrel)(Atom('0')), KripkeWorld("1111"))
check(SoleLogics.diamond(SoleLogics.PredecessorRel)(Atom('0')), KripkeWorld("01001"), Point(3))
check(SoleLogics.diamond(SoleLogics.PredecessorRel)(Atom('1')), KripkeWorld("01001"), Point(3))

φ000 = globaldiamond(SoleLogics.diamond(SoleLogics.SuccessorRel)(Atom('1') ∧ SoleLogics.diamond(SoleLogics.SuccessorRel)(Atom('0') ∧ SoleLogics.diamond(SoleLogics.SuccessorRel)(Atom('0')))))

check(φ000, KripkeWorld("01001"))
check(φ000, KripkeWorld("0101"))
