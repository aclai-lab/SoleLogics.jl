# """
#     combinedimensionalrelations(R, d::Integer)
# Collect all the valid modal operators -both existential and universal- from a collection
# of strings or symbols.

# # Example
# ```jldoctest
# julia> @combinedimensionalrelations HSRELATIONS 1
# ⟨L⟩
# ⟨A⟩
# ⟨O⟩
# ⟨E⟩
# ⋮
# [E̅]
# [D̅]
# [B̅]
# julia> @combinedimensionalrelations HS₃RELATIONS 2
# ⟨L,L⟩
# ⟨L̅,L⟩
# ⟨I,L⟩
# ⟨L,L̅⟩
# ⋮
# [L,I]
# [L̅,I]
# [I,I]
# ```
# """
# macro combinedimensionalrelations(R, d::Integer)
#     quote
#         rels = vec(collect(Iterators.product([$(R) for _ = 1:$(d)]...)))
#         if "=" in $(R)
#             # TODO add "=" to rels, and remove the combination between rels.
#         end
#         rels
#     end
# end
