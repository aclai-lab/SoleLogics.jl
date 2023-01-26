goeswith(::Type{<:Full2DFrame}, ::RCCRelation) = true

############################################################################################
# Methods for RCC8 relations and Interval2D's can be obtained by combining their 1D versions.
# Consider the following table:
#
#                      .-------------------------------------------------------.
#                      |         DC   EC   PO   TPP   T̅P̅P̅   NTPP   N̅T̅P̅P̅    Id  |
#                      |-------------------------------------------------------|
#                      | DC   |  DC | DC | DC | DC  | DC  |  DC  |  DC  |  DC  |
#                      | EC   |  DC | EC | EC | EC  | EC  |  EC  |  EC  |  EC  |
#                      | PO   |  DC | EC | PO | PO  | PO  |  PO  |  PO  |  PO  |
#                      | TPP  |  DC | EC | PO | TPP | PO  |  TPP |  PO  |  TPP |
#                      | T̅P̅P̅  |  DC | EC | PO | PO  | T̅P̅P̅ |  PO  |  T̅P̅P̅ |  T̅P̅P̅ |
#                      | NTPP |  DC | EC | PO | TPP | PO  | NTPP |  PO  |  TPP |
#                      | N̅T̅P̅P̅ |  DC | EC | PO | PO  | T̅P̅P̅ |  PO  | N̅T̅P̅P̅ |  T̅P̅P̅ |
#                      |  Id  |  DC | EC | PO | TPP | T̅P̅P̅ |  TPP |  T̅P̅P̅ |  Id  |
#                      '-------------------------------------------------------'
#
############################################################################################

_accessibles(fr::Full2DFrame, w::Interval2D, ::_Topo_DC) =
	IterTools.distinct(
		Iterators.flatten((
			Iterators.product(_accessibles(Full1DFrame(X(fr)), w.x,      Topo_DC), _accessibles__(Full1DFrame(Y(fr)), w.y,     RelationGlob)),
			Iterators.product(_accessibles__(Full1DFrame(X(fr)), w.x,  RelationGlob), _accessibles(Full1DFrame(Y(fr)), w.y, Topo_DC)),
			# TODO try avoiding the distinct, replacing the second line (RelationGlob,_accessibles) with 7 combinations of RelationGlob with Topo_EC, Topo_PO, Topo_TPP, Topo_TPPi, Topo_NTPP, Topo_NTPPi
		))
	)
_accessibles(fr::Full2DFrame, w::Interval2D, ::_Topo_EC) =
	Iterators.flatten((
		Iterators.product(_accessibles(Full1DFrame(X(fr)), w.x,      Topo_EC), _accessibles(Full1DFrame(Y(fr)), w.y,     Topo_EC)),
		#
		Iterators.product(_accessibles(Full1DFrame(X(fr)), w.x,      Topo_EC), _accessibles(Full1DFrame(Y(fr)), w.y,     Topo_PO)),
		Iterators.product(_accessibles(Full1DFrame(X(fr)), w.x,      Topo_EC), _accessibles(Full1DFrame(Y(fr)), w.y,     Topo_TPP)),
		Iterators.product(_accessibles(Full1DFrame(X(fr)), w.x,      Topo_EC), _accessibles(Full1DFrame(Y(fr)), w.y,     Topo_TPPi)),
		Iterators.product(_accessibles(Full1DFrame(X(fr)), w.x,      Topo_EC), _accessibles(Full1DFrame(Y(fr)), w.y,     Topo_NTPP)),
		Iterators.product(_accessibles(Full1DFrame(X(fr)), w.x,      Topo_EC), _accessibles(Full1DFrame(Y(fr)), w.y,     Topo_NTPPi)),
		Iterators.product(_accessibles(Full1DFrame(X(fr)), w.x,      Topo_EC), _accessibles(Full1DFrame(Y(fr)), w.y,     RelationId)),
		#
		Iterators.product(_accessibles(Full1DFrame(X(fr)), w.x,      Topo_PO), _accessibles(Full1DFrame(Y(fr)), w.y,     Topo_EC)),
		Iterators.product(_accessibles(Full1DFrame(X(fr)), w.x,     Topo_TPP), _accessibles(Full1DFrame(Y(fr)), w.y,    Topo_EC)),
		Iterators.product(_accessibles(Full1DFrame(X(fr)), w.x,    Topo_TPPi), _accessibles(Full1DFrame(Y(fr)), w.y,   Topo_EC)),
		Iterators.product(_accessibles(Full1DFrame(X(fr)), w.x,    Topo_NTPP), _accessibles(Full1DFrame(Y(fr)), w.y,   Topo_EC)),
		Iterators.product(_accessibles(Full1DFrame(X(fr)), w.x,   Topo_NTPPi), _accessibles(Full1DFrame(Y(fr)), w.y,  Topo_EC)),
		Iterators.product(_accessibles(Full1DFrame(X(fr)), w.x,   RelationId), _accessibles(Full1DFrame(Y(fr)), w.y,  Topo_EC)),
	))
_accessibles(fr::Full2DFrame, w::Interval2D, ::_Topo_PO) =
	Iterators.flatten((
		Iterators.product(_accessibles(Full1DFrame(X(fr)), w.x,      Topo_PO), _accessibles(Full1DFrame(Y(fr)), w.y,     Topo_PO)),
		#
		Iterators.product(_accessibles(Full1DFrame(X(fr)), w.x,     Topo_TPP), _accessibles(Full1DFrame(Y(fr)), w.y,    Topo_PO)),
		Iterators.product(_accessibles(Full1DFrame(X(fr)), w.x,    Topo_TPPi), _accessibles(Full1DFrame(Y(fr)), w.y,   Topo_PO)),
		Iterators.product(_accessibles(Full1DFrame(X(fr)), w.x,    Topo_NTPP), _accessibles(Full1DFrame(Y(fr)), w.y,   Topo_PO)),
		Iterators.product(_accessibles(Full1DFrame(X(fr)), w.x,   Topo_NTPPi), _accessibles(Full1DFrame(Y(fr)), w.y,  Topo_PO)),
		Iterators.product(_accessibles(Full1DFrame(X(fr)), w.x,   RelationId), _accessibles(Full1DFrame(Y(fr)), w.y,  Topo_PO)),
		#
		Iterators.product(_accessibles(Full1DFrame(X(fr)), w.x,      Topo_PO), _accessibles(Full1DFrame(Y(fr)), w.y,     Topo_TPP)),
		Iterators.product(_accessibles(Full1DFrame(X(fr)), w.x,      Topo_PO), _accessibles(Full1DFrame(Y(fr)), w.y,     Topo_TPPi)),
		Iterators.product(_accessibles(Full1DFrame(X(fr)), w.x,      Topo_PO), _accessibles(Full1DFrame(Y(fr)), w.y,     Topo_NTPP)),
		Iterators.product(_accessibles(Full1DFrame(X(fr)), w.x,      Topo_PO), _accessibles(Full1DFrame(Y(fr)), w.y,     Topo_NTPPi)),
		Iterators.product(_accessibles(Full1DFrame(X(fr)), w.x,      Topo_PO), _accessibles(Full1DFrame(Y(fr)), w.y,     RelationId)),
		#
		Iterators.product(_accessibles(Full1DFrame(X(fr)), w.x,     Topo_TPP), _accessibles(Full1DFrame(Y(fr)), w.y,    Topo_TPPi)),
		Iterators.product(_accessibles(Full1DFrame(X(fr)), w.x,    Topo_TPPi), _accessibles(Full1DFrame(Y(fr)), w.y,   Topo_TPP)),
		#
		Iterators.product(_accessibles(Full1DFrame(X(fr)), w.x,    Topo_TPPi), _accessibles(Full1DFrame(Y(fr)), w.y,   Topo_NTPP)),
		Iterators.product(_accessibles(Full1DFrame(X(fr)), w.x,    Topo_NTPP), _accessibles(Full1DFrame(Y(fr)), w.y,   Topo_TPPi)),
		#
		Iterators.product(_accessibles(Full1DFrame(X(fr)), w.x,     Topo_TPP), _accessibles(Full1DFrame(Y(fr)), w.y,    Topo_NTPPi)),
		Iterators.product(_accessibles(Full1DFrame(X(fr)), w.x,    Topo_NTPP), _accessibles(Full1DFrame(Y(fr)), w.y,   Topo_NTPPi)),
		Iterators.product(_accessibles(Full1DFrame(X(fr)), w.x,   Topo_NTPPi), _accessibles(Full1DFrame(Y(fr)), w.y,  Topo_TPP)),
		Iterators.product(_accessibles(Full1DFrame(X(fr)), w.x,   Topo_NTPPi), _accessibles(Full1DFrame(Y(fr)), w.y,  Topo_NTPP)),
	))
_accessibles(fr::Full2DFrame, w::Interval2D, ::_Topo_TPP) =
	Iterators.flatten((
		Iterators.product(_accessibles(Full1DFrame(X(fr)), w.x,     Topo_TPP), _accessibles(Full1DFrame(Y(fr)), w.y,    Topo_TPP)),
		#
		Iterators.product(_accessibles(Full1DFrame(X(fr)), w.x,     Topo_TPP), _accessibles(Full1DFrame(Y(fr)), w.y,    Topo_NTPP)),
		Iterators.product(_accessibles(Full1DFrame(X(fr)), w.x,    Topo_NTPP), _accessibles(Full1DFrame(Y(fr)), w.y,   Topo_TPP)),
		#
		Iterators.product(_accessibles(Full1DFrame(X(fr)), w.x,     Topo_TPP), _accessibles(Full1DFrame(Y(fr)), w.y,    RelationId)),
		Iterators.product(_accessibles(Full1DFrame(X(fr)), w.x,   RelationId), _accessibles(Full1DFrame(Y(fr)), w.y,  Topo_TPP)),
		#
		Iterators.product(_accessibles(Full1DFrame(X(fr)), w.x,    Topo_NTPP), _accessibles(Full1DFrame(Y(fr)), w.y,   RelationId)),
		Iterators.product(_accessibles(Full1DFrame(X(fr)), w.x,   RelationId), _accessibles(Full1DFrame(Y(fr)), w.y,  Topo_NTPP)),
	))

_accessibles(fr::Full2DFrame, w::Interval2D, ::_Topo_TPPi) =
	Iterators.flatten((
		Iterators.product(_accessibles(Full1DFrame(X(fr)), w.x,    Topo_TPPi), _accessibles(Full1DFrame(Y(fr)), w.y,   Topo_TPPi)),
		#
		Iterators.product(_accessibles(Full1DFrame(X(fr)), w.x,    Topo_TPPi), _accessibles(Full1DFrame(Y(fr)), w.y,   Topo_NTPPi)),
		Iterators.product(_accessibles(Full1DFrame(X(fr)), w.x,   Topo_NTPPi), _accessibles(Full1DFrame(Y(fr)), w.y,  Topo_TPPi)),
		#
		Iterators.product(_accessibles(Full1DFrame(X(fr)), w.x,    Topo_TPPi), _accessibles(Full1DFrame(Y(fr)), w.y,   RelationId)),
		Iterators.product(_accessibles(Full1DFrame(X(fr)), w.x,   RelationId), _accessibles(Full1DFrame(Y(fr)), w.y,  Topo_TPPi)),
		#
		Iterators.product(_accessibles(Full1DFrame(X(fr)), w.x,   Topo_NTPPi), _accessibles(Full1DFrame(Y(fr)), w.y,  RelationId)),
		Iterators.product(_accessibles(Full1DFrame(X(fr)), w.x,   RelationId), _accessibles(Full1DFrame(Y(fr)), w.y,  Topo_NTPPi)),
	))

_accessibles(fr::Full2DFrame, w::Interval2D, ::_Topo_NTPP) =
	# Iterators.flatten((
		Iterators.product(_accessibles(Full1DFrame(X(fr)), w.x,    Topo_NTPP), _accessibles(Full1DFrame(Y(fr)), w.y,   Topo_NTPP))
		# , ))
_accessibles(fr::Full2DFrame, w::Interval2D, ::_Topo_NTPPi) =
	# Iterators.flatten((
		Iterators.product(_accessibles(Full1DFrame(X(fr)), w.x,   Topo_NTPPi), _accessibles(Full1DFrame(Y(fr)), w.y,  Topo_NTPPi))
	# , ))

############################################################################################

_accessibles(fr::Full2DFrame, w::Interval2D, r::RCC5Relation) =
    Iterators.flatten((_accessibles(fr, w, IA_r) for IA_r in RCC52IARelations(r)))
    # Iterators.flatten((_accessibles(fr, w, RCC8_r) for RCC8_r in RCC52RCC8Relations(r)))
    # Iterators.flatten((_accessibles(fr, w, IA_r) for RCC8_r in RCC52RCC8Relations(r) for IA_r in topo2IARelations(RCC8_r)))

############################################################################################
