module Worlds

    include("general.jl")
    include("intervals.jl")

    export World
    export EmptyWorld, CenteredWorld
    export AbstractWorldSet, WorldSet

    export Interval, Interval2D, OneWorld
    export dimensionality

end
