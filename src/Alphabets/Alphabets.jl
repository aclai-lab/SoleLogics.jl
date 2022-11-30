module Alphabets
    import Base: show, string

    # TODO: think about importing only the needed resources
    # for example, import SoleLogics.Relations: AbstractRelation
    using SoleLogics.Relations
    using ..SoleLogics

    include("letters.jl")

    # NOTE: Currently, we don't expect this submodule to export something.
    # In fact, this module is intrinsically tied to SoleLogics operators
    # and relations definitions.
    # In other words, this module can't exist without SoleLogics.

    export AbstractPropositionalLetter, MetaLetter
    export Letter
    export is_proposition
end
