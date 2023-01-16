module SoleLogics

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
# include("multimodal-logic.jl")

# TODO: to see with Giovanni
include("UsefulOldIdeas/Worlds/worlds.jl")
include("UsefulOldIdeas/Relations/relations.jl")


# TODO: to see with Giovanni
include("random.jl")

end
