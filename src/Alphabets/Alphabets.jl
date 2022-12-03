import Base: show, string

include("letters.jl")

# NOTE: Currently, we don't expect this submodule to export something.
# In fact, this module is intrinsically tied to SoleLogics operators
# and relations definitions.
# In other words, this module can't exist without SoleLogics.

export AbstractPropositionalLetter, MetaLetter
export Letter
export is_proposition
