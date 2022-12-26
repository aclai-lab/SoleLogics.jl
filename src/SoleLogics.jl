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

include("parsing.jl")

include("multi-modal-logic-base/worlds.jl")
include("multi-modal-logic-base/relations.jl")


include("random.jl")

end
