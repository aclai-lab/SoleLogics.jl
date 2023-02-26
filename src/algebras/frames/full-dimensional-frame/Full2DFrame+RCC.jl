# goeswith(::Type{<:Full2DFrame}, ::RCCRelation) = true

_accessibles(::AbstractMultiModalFrame, w::Interval,    ::IdentityRel) = [(w.x, w.y),] # TODO try IterTools.imap(identity, [w])

# Refer to [`RCCRelation`](@ref).

_accessibles(fr::Full2DFrame, w::Interval2D, ::_Topo_DC) =
	IterTools.distinct(
		Iterators.flatten((
			Iterators.product(_accessibles(Full1DFrame(X(fr)), w.x,      Topo_DC), _accessibles__(Full1DFrame(Y(fr)), w.y,     globalrel)),
			Iterators.product(_accessibles__(Full1DFrame(X(fr)), w.x,  globalrel), _accessibles(Full1DFrame(Y(fr)), w.y, Topo_DC)),
			# TODO try avoiding the distinct, replacing the second line (globalrel,_accessibles) with 7 combinations of globalrel with Topo_EC, Topo_PO, Topo_TPP, Topo_TPPi, Topo_NTPP, Topo_NTPPi
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
		Iterators.product(_accessibles(Full1DFrame(X(fr)), w.x,      Topo_EC), _accessibles(Full1DFrame(Y(fr)), w.y,     identityrel)),
		#
		Iterators.product(_accessibles(Full1DFrame(X(fr)), w.x,      Topo_PO), _accessibles(Full1DFrame(Y(fr)), w.y,     Topo_EC)),
		Iterators.product(_accessibles(Full1DFrame(X(fr)), w.x,     Topo_TPP), _accessibles(Full1DFrame(Y(fr)), w.y,    Topo_EC)),
		Iterators.product(_accessibles(Full1DFrame(X(fr)), w.x,    Topo_TPPi), _accessibles(Full1DFrame(Y(fr)), w.y,   Topo_EC)),
		Iterators.product(_accessibles(Full1DFrame(X(fr)), w.x,    Topo_NTPP), _accessibles(Full1DFrame(Y(fr)), w.y,   Topo_EC)),
		Iterators.product(_accessibles(Full1DFrame(X(fr)), w.x,   Topo_NTPPi), _accessibles(Full1DFrame(Y(fr)), w.y,  Topo_EC)),
		Iterators.product(_accessibles(Full1DFrame(X(fr)), w.x,   identityrel), _accessibles(Full1DFrame(Y(fr)), w.y,  Topo_EC)),
	))
_accessibles(fr::Full2DFrame, w::Interval2D, ::_Topo_PO) =
	Iterators.flatten((
		Iterators.product(_accessibles(Full1DFrame(X(fr)), w.x,      Topo_PO), _accessibles(Full1DFrame(Y(fr)), w.y,     Topo_PO)),
		#
		Iterators.product(_accessibles(Full1DFrame(X(fr)), w.x,     Topo_TPP), _accessibles(Full1DFrame(Y(fr)), w.y,    Topo_PO)),
		Iterators.product(_accessibles(Full1DFrame(X(fr)), w.x,    Topo_TPPi), _accessibles(Full1DFrame(Y(fr)), w.y,   Topo_PO)),
		Iterators.product(_accessibles(Full1DFrame(X(fr)), w.x,    Topo_NTPP), _accessibles(Full1DFrame(Y(fr)), w.y,   Topo_PO)),
		Iterators.product(_accessibles(Full1DFrame(X(fr)), w.x,   Topo_NTPPi), _accessibles(Full1DFrame(Y(fr)), w.y,  Topo_PO)),
		Iterators.product(_accessibles(Full1DFrame(X(fr)), w.x,   identityrel), _accessibles(Full1DFrame(Y(fr)), w.y,  Topo_PO)),
		#
		Iterators.product(_accessibles(Full1DFrame(X(fr)), w.x,      Topo_PO), _accessibles(Full1DFrame(Y(fr)), w.y,     Topo_TPP)),
		Iterators.product(_accessibles(Full1DFrame(X(fr)), w.x,      Topo_PO), _accessibles(Full1DFrame(Y(fr)), w.y,     Topo_TPPi)),
		Iterators.product(_accessibles(Full1DFrame(X(fr)), w.x,      Topo_PO), _accessibles(Full1DFrame(Y(fr)), w.y,     Topo_NTPP)),
		Iterators.product(_accessibles(Full1DFrame(X(fr)), w.x,      Topo_PO), _accessibles(Full1DFrame(Y(fr)), w.y,     Topo_NTPPi)),
		Iterators.product(_accessibles(Full1DFrame(X(fr)), w.x,      Topo_PO), _accessibles(Full1DFrame(Y(fr)), w.y,     identityrel)),
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
		Iterators.product(_accessibles(Full1DFrame(X(fr)), w.x,     Topo_TPP), _accessibles(Full1DFrame(Y(fr)), w.y,    identityrel)),
		Iterators.product(_accessibles(Full1DFrame(X(fr)), w.x,   identityrel), _accessibles(Full1DFrame(Y(fr)), w.y,  Topo_TPP)),
		#
		Iterators.product(_accessibles(Full1DFrame(X(fr)), w.x,    Topo_NTPP), _accessibles(Full1DFrame(Y(fr)), w.y,   identityrel)),
		Iterators.product(_accessibles(Full1DFrame(X(fr)), w.x,   identityrel), _accessibles(Full1DFrame(Y(fr)), w.y,  Topo_NTPP)),
	))

_accessibles(fr::Full2DFrame, w::Interval2D, ::_Topo_TPPi) =
	Iterators.flatten((
		Iterators.product(_accessibles(Full1DFrame(X(fr)), w.x,    Topo_TPPi), _accessibles(Full1DFrame(Y(fr)), w.y,   Topo_TPPi)),
		#
		Iterators.product(_accessibles(Full1DFrame(X(fr)), w.x,    Topo_TPPi), _accessibles(Full1DFrame(Y(fr)), w.y,   Topo_NTPPi)),
		Iterators.product(_accessibles(Full1DFrame(X(fr)), w.x,   Topo_NTPPi), _accessibles(Full1DFrame(Y(fr)), w.y,  Topo_TPPi)),
		#
		Iterators.product(_accessibles(Full1DFrame(X(fr)), w.x,    Topo_TPPi), _accessibles(Full1DFrame(Y(fr)), w.y,   identityrel)),
		Iterators.product(_accessibles(Full1DFrame(X(fr)), w.x,   identityrel), _accessibles(Full1DFrame(Y(fr)), w.y,  Topo_TPPi)),
		#
		Iterators.product(_accessibles(Full1DFrame(X(fr)), w.x,   Topo_NTPPi), _accessibles(Full1DFrame(Y(fr)), w.y,  identityrel)),
		Iterators.product(_accessibles(Full1DFrame(X(fr)), w.x,   identityrel), _accessibles(Full1DFrame(Y(fr)), w.y,  Topo_NTPPi)),
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

_accessibles(fr::Full2DFrame, w::Interval2D, r::RCC5RelationFromRCC8) =
    Iterators.flatten((_accessibles(fr, w, IA_r) for IA_r in RCC52IARelations(r)))
    # Iterators.flatten((_accessibles(fr, w, RCC8_r) for RCC8_r in RCC52RCC8Relations(r)))
    # Iterators.flatten((_accessibles(fr, w, IA_r) for RCC8_r in RCC52RCC8Relations(r) for IA_r in topo2IARelations(RCC8_r)))

############################################################################################
