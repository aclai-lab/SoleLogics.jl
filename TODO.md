# MAIN TODOS


Utils:
  ☐ iscnf(::Formula)/isdnf(::Formula)

Modal Logic:
  ☐ recheck/double-check/add tests for accessibles Point1D

Connectives:
  ☐ Change DiamondRelationalConnective and BoxRelationalConnective from singletons to structs wrapping a relation.
    In theory, not all relations are singletons; some might be parametric (e.g., a relation leading to the world w). Therefore, the Diamond and Box relational connectives (e.g., DiamondRelationalConnective) should be structs containing a relation, rather than singletons with the relation encoded only as a type parameter.

Interpretations:
  ☐ Create a SupportedInterpretation
  ☐ Algorithm to generate a truth table

Normalization:
  ☐ Add custom normalization rules to normalize
  ☐ Assuming boolean logic, "((¬q v p) ∧ ¬q)" could be simplified to "¬q"

Random:
  ☐ Generator for formulas with specific contraints:
    ✔ Fixed height formulas @done(23-12-06 12:04)
    ☐ CNF/DNF formulas
    ☐ UnSAT formulas
    ☐ Fuzzy formulas (algebra is an argument)
    ☐ Logger for offline formulas (in a log file, i can load and retrieve already generated formulas)
    ☐ Generator for complete collection of formulas generator (all possible combinations of formulas are considered)

  ☐ Add the following utility dispatches:
    ☐ rand(connectives, atom leaves array, algebra from which infer truth values)
    ☐ rand(connectives, atom leaves array, truth values with common supertype)
    ☐ rand(connectives, atom leaves array, true/false (use truth values as leaf or not. If true, default to boolean))
    ☐ sample(..., probability distribution)
    
  ✔ Write random models generation (see pluto-demo.jl and exploit Graphs.SimpleGraphs generation) @done(23-12-20 17:52)
  ☐ Expand the model generator with the possibility of using a generic SimpleGraphs method to shape a kripke frame 

Readability:
  ☐ Add parameters to syntaxstring:
    ☐ syntaxstring.parenthesize_unary_modals = false, but force parentheses in cases like <G>φ -> <G>(φ).
    ☐ syntaxstring.parenthesize_operators = [list of unary/binary operators for which you want to force parentheses...]/function that decides whether to parenthesize or not

  ☐ meaningful names for relations: - IA_L -> After/Right - IA_A -> RightAfter

Tests:
  ☐ Increase code coverage

PAndQ:
  ☐ Recognize if a formula is satisfiable, is a contingency, a tautology or a contraddiction.

# OTHER SPARSE NOTES


- That is, the user provides a callback, or a set of callbacks to cover additional cases beyond the defaults. To test it, try simplifying normalize(parseformula("((¬q v p) ∧ ¬q)")) to parseformula("¬q") by providing a function that applies a specific rule. First, study how normalize works: it's divided into steps, and it's not obvious in which step these callbacks should be applied.

- normalize: again, (always assuming a Boolean algebra) cases like "((¬q v p) ∧ ¬q)" could be simplified to "¬q", but... even without providing callbacks: can we add simple logic that understands what to remove?

- Increase the package "coverage" by checking the results from codecov via GitHub Actions, and adding tests.

- Check everywhere we have assumed that ismodal(x) implies isunary(x), because, in general, it's not true. Soon we will have binary modal operators and I don't want to be embarrassed :P i.e., I don't want something to break or, worse, silently not break.

Three cases I noted where there might be redundant parentheses. They might be obsolete checks, dating back at least 3 months. Check:
- f = parseformula("⟨G⟩p → [G]q"); syntaxstring(f) == "(⟨G⟩p) → ([G]q)" != "⟨G⟩p → [G]q"
- syntaxstring(parseformula("¬1→0"; atom_parser = (x -> Atom{Float64}(parse(Float64, x))))) == "(¬1.0) → 0.0"
- syntaxstring(parseformula("¬a → b ∧ c")) == "(¬a) → (b ∧ c)"

- Relation -> Relation{A}
- composeformulas(::Connective, ::Tuple{}) = TODO to resolve ambiguities
- SoleLogics: TODO note that IA_A for time series (and not for words) does not reflect what it should. Everything is shifted by one.
- SoleLogics add tests for `simplify`
- simple minimize


    function collatetruth(c::typeof(∧), (φ1, φ2)::NTuple{N,Formula})
        if     isbot(φ1)  ⊥           # ⊥ ∧ φ ≡ ⊥
        elseif isbot(φ2)  ⊥           # φ ∧ ⊥ ≡ ⊥
        elseif istop(φ1)  φ2          # ⊤ ∧ φ ≡ φ
        elseif istop(φ2)  φ1          # φ ∧ ⊤ ≡ φ
        else   c((φ1, φ2))
        end
    end

    function collatetruth(c::typeof(∧), chs::NTuple{N,Formula})
        if     any(isbot, chs) ⊥
        elseif all(istop, chs) ⊤
        else
            chs = filter(!istop, chs)
            if length(...)
                chs
            else
                c(chs)
            end
        end
    end

- TODO: some types like Union{AbstractString,Number,AbstractChar} can be passively used as atoms? e.g. "a" in \varphi ?

- (PAndQ.jl) Algorithm that produces truth tables of propositional logic formulas, and that determines whether a formula is satisfiable, contingent, a tautology, or a contradiction.
- function normalize: simplify the function using the two new traits dual/hasdual.
- Adjust normalize rules
- SoleLogics documentation: sections, examples of random generation, parsing, formula enumeration,
- separate dual/negation returns an abstractformula representing the negated one.
- LeftmostDisjunctiveForm -> Disjunction
- default value for allow_atom_flipping?
- @Mauro expand and improve the definition of dual_op into booleandual. ⊤/\bottom \land/\lor
- @Mauro OneWorldFrame where accessibles and representatives are defined on globalrel/identityrel only: _frame(X::Union{UniformDimensionalDataset{T,2},AbstractArray{T,2}}) where {T} = OneWorldFrame()
- frame-specific formula normalization!!!
- Examples with the definition of xor: composing formulas also with infix notation if: https://stackoverflow.com/a/60321302/5646732
- parts of formulas that are nested <G> can probably come out.
- Modal formulas simplification examples:
    ⟨G⟩(p ∧ ⟨G⟩(q ∧ ⟨A̅∨O̅⟩p))
    ⟨G⟩p ∧ ⟨G⟩(q ∧ ⟨A̅∨O̅⟩p)
    ⟨G⟩p ∧ ⟨G⟩(q ∧ ⟨A̅∨O̅⟩p) ∧ ⟨G⟩⟨A̅∨O̅⟩p

    ⟨G⟩(p ∧ ⟨G⟩(q ∧ ⟨A̅∨O̅⟩p)) -> ⟨G⟩p
    ⟨G⟩(p ∧ ⟨G⟩(q ∧ ⟨A̅∨O̅⟩p)) -> ⟨G⟩(q ∧ ⟨A̅∨O̅⟩p)

    ⟨G⟩p ∧ ⟨G⟩(q ∧ ⟨A̅∨O̅⟩p) -> ⟨G⟩⟨A̅∨O̅⟩p
    ⟨G⟩⟨A̅∨O̅⟩p -> ⟨G⟩p
    
- In README.md add:
1) Checks of propositional and modal formulas (maybe take from NOTE or pluto-demo.jl?)
2) Next to the list of similar packages, explain what SoleLogics offers and doesn't offer.


# DONE

Documentation:
  ✔ In the documentation, show how to create a custom operator (maybe a Xor/⊻/⊕) starting from scratch. @done(23-12-06 12:05)

Normalization:
  ✔ Simplify code when possible, using dual/hasdual traits @done(23-12-06 12:05)
  
Random:
  ✔ Clean rand and sample dispatches

README:
  ✔ Check examples (see NOTE or pluto-demo.jl) @done(23-09-05 19:34)
  ✔ List of similar packages @done(23-09-05 19:30)
  ✔ Highlight SoleLogics use cases

Readability: 
  ✔ Write check's docstring @done(23-09-05 19:33)
  ✔ Rename things to increase readability, using @deprecate in a deprecate.jl file. @done(23-12-06 12:05)

Tests:
  ✔ Fix SatsBase.sample errors @done(23-09-05 19:36)
  ✔ Add the following tests: @done(23-12-20 18:07)
    ✔ syntaxstring tests: get "◊¬p ∧ ¬q" from "(◊¬p) ∧ (¬q)" @done(23-12-20 18:06)
    ✔ syntaxstring tests: get "(q) → ((p) → (¬(q)))" from "q → p → ¬q" @done(23-12-20 18:07)
    ✔ (parseformula("p ∧ q ∧ r ∨ s ∧ t")) == @synexpr p ∧ q ∧ r ∨ s ∧ t @done(23-12-19 10:29)

Utilities:
  ✔ Differentiate Base.rand and StatsBase.sample (randformula should have a picker::Function argument, which can be rand or sample) @done(23-12-06 12:05)
  ✔ @atoms defaulted to String @done(23-09-12 15:48)

PAndQ:
  ✔ Write the algorithm that produces truth tables of propositional formulae @done(23-09-05 19:36)
  ✔ Macro to easily create atoms and parse expressions related to syntax (See PAndQ.jl) @done(23-09-05 19:36)
