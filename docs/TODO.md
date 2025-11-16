(A) Outline documentation structure
    Establish SoleLogics fundamental entities
    Write down the documentation hierarchy
        # An idea might be 
        # - Core concepts
        # - Propositional logic
        # - Modal logic 
    Write down real functioning examples using ```julia-repl etc.```
        Also, think about making a functioning notebook using Pluto.jl
    Improve docstrings to make them more clear
    Fill up docstrings with considerations

(C) Improve this general description about SoleLogics.jl, in order to make the first page in SoleLogics documentation
#     # Usage
#     ## Parsing Formulas
#     Consider the string representation of a logical formula (in infix notation, e.g., "p∧q" or in function notation, e.g. "∧(p,q)". Such expressions can easily be parsed to a `SyntaxTree` representation using the method `parseformula`. As you can see from the documentation, it is highly customizable, allowing parsing custom-defined operators and changing how recognized atoms must be interpreted (e.g. in "true∧false" atoms are booleans, while in "1∧0" they are integers).
#     ## Generating random formulas
#     Random formulas generation is provided by the following methods:
#     - randformula (which returns a SyntaxBranch);
#     Both allow customizing the generation process by setting an alphabet (that is, which propositional letters are in play), an operators vector, the maximum height of the SyntaxBranch and the maximum modal depth (that is, the maximum number of modal connectives, if any, in each SyntaxBranch path).
#     Notes for Giovanni; 
#     1) many important definitions are introduced here: is this the correct place to explain them or it's better to just link the documentation? 
#     2) I'm still working on putting a modal_depth parameter in both randbaseformula and randformula 
#     3) As stated in SyntaxStructure docstring, classically a logical formula is implemented using a tree structure (an AST) but other representations may exist: I guess we could add a "randnormalform" method
#     ## Generating random interpretations
#     Kripke structures generation is provided by gen_kstructure interface. As you can see from the documentation, internally it builds up a random directed graph structure (using some, possibly custom algorithm). The adjacency list obtained is enriched with informations to obtain a so called Kripke frame; this is done by taking each vertex in the list and
#     - converting it into a World structure;
#     - defining which worlds are accessible from this following a specific relation (I'm trying to summarize both the concept of binary accessiblity relation R and the accessibles method...).
#     Finally, pairing each world in the Kripke frame with a list of propositional letters (this goes undeer the name of world valuation function), a Kripke structure is returned by gen_kstructure.
#     ## Model checking
#     ## Interpretation sets

