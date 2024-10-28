using SoleLogics
import SoleLogics: AbstractKripkeStructure, frame, interpret
import SoleLogics: Point, FullDimensionalFrame

struct KripkeWord <: AbstractKripkeStructure
    word::String
end

frame(i::KripkeWord) = FullDimensionalFrame((length(i.word),), Point{1,Int})

function interpret(a::Atom{Char}, i::KripkeWord, w::Point)
    ch = SoleLogics.value(a)
    wordch = i.word[w[1]]
    return (wordch == ch) ? TOP : BOT
end

accessibles(KripkeWord("01001"), Point(3), globalrel) |> collect
accessibles(KripkeWord("01001"), Point(3), SoleLogics.SuccessorRel) |> collect
accessibles(KripkeWord("01001"), Point(3), SoleLogics.PredecessorRel) |> collect
accessibles(KripkeWord("01001"), Point(3), SoleLogics.MinRel) |> collect
accessibles(KripkeWord("01001"), Point(3), SoleLogics.MaxRel) |> collect

# Note: using checking algorithm that is not optimal in this case
check(SoleLogics.diamond(globalrel)(Atom('0')), KripkeWord("01001"), Point(3))
check(SoleLogics.diamond(globalrel)(Atom('0')), KripkeWord("01001"))
check(SoleLogics.diamond(globalrel)(Atom('0')), KripkeWord("1111"), Point(3))
check(SoleLogics.diamond(globalrel)(Atom('0')), KripkeWord("1111"))
check(SoleLogics.diamond(SoleLogics.PredecessorRel)(Atom('0')), KripkeWord("01001"), Point(3))
check(SoleLogics.diamond(SoleLogics.PredecessorRel)(Atom('1')), KripkeWord("01001"), Point(3))

φ000 = globaldiamond(SoleLogics.diamond(SoleLogics.SuccessorRel)(Atom('1') ∧ SoleLogics.diamond(SoleLogics.SuccessorRel)(Atom('0') ∧ SoleLogics.diamond(SoleLogics.SuccessorRel)(Atom('0')))))

check(φ000, KripkeWord("01001"))
check(φ000, KripkeWord("0101"))



# check(SoleLogics.diamond(globalrel)(Atom("11") ∧ diamond(IA_A)(Atom("11") ∧ diamond(IA_A)(Atom("11")))), KripkeWord("101100011"))
