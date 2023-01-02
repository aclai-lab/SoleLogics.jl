module SoleLogics

"""
GENERAL TODOs:
    * Check the links in the documentation, see AbstractOperator information about "logical constant"
        and see also AbstractAlphabet information about "countable" <-- when I'm trying to see the docstring
        I'm puzzled (we can talk about this face2face)
    * I've seen that some docstrings of functions are out of bounds due to the first definition. I propose to use
        # Parameters in docstrings so that we can avoid to use the explicit definition of functions
        that is, instead of using check(a::T1, b::T2, c::T3) we can use check(a,b,c) and then put
        # Parameters
        * a::T1 this is something
        * b::T2 this is something else
        * c::T3 this is something else else
    * Why some functions have returned types and others do not have it? Is there an internal rule or something?
    * Why some constructors return new() and others return NameOfStruct()?
    * We should implement show methods for objects, such as logics (try propositional_logic() to see an example of what I'm saying)
"""

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
include("multimodal-logic.jl")

# TODO: to see with Giovanni
include("UsefulOldIdeas/Worlds/worlds.jl")
include("UsefulOldIdeas/Relations/relations.jl")


# TODO: to see with Giovanni
include("random.jl")

end
