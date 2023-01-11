module SoleLogics

"""
GENERAL TODOs:
    * Check the links in the documentation, see AbstractOperator information about "logical constant"
        and see also AbstractAlphabet information about "countable" <-- when I'm trying to see the docstring
        I'm puzzled (we can talk about this face2face)
        -> I'm not sure I understand: is there something wrong with url links?
    * I've seen that some docstrings of functions are out of bounds due to the first definition. I propose to use
        # Parameters in docstrings so that we can avoid to use the explicit definition of functions
        that is, instead of using check(a::T1, b::T2, c::T3) we can use check(a,b,c) and then put
        # Parameters
        * a::T1 this is something
        * b::T2 this is something else
        * c::T3 this is something else else
        -> I went for a similar approach, which I believe is enough.
    * Why some functions have returned types and others do not have it? Is there an internal rule or something?
        -> Yes: returned types are specified when defining interfaces, and the reason is to convey to the user
            the expected return type of the method that they should write.
"""

# TODO: We should implement show methods for objects, such as logics
#  (try propositional_logic() to see an example of what I'm saying)

import Base: show
using DataStructures
using Dictionaries
using Random
using Reexport

@reexport using SoleBase

# TODO: Include exports

include("general.jl")
include("base-logic.jl")
include("propositional-logic.jl")
include("modal-logic.jl")

include("utils.jl")

include("parsing.jl")

include("multi-modal-logic-base/worlds.jl")
include("multi-modal-logic-base/relations.jl")

include("random.jl")

end
