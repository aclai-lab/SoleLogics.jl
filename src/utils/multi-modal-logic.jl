################################################################################
# This file provides useful aliases for the most common multi-modal logics:
# - Linear Temporal Logic with Future and Past (LTL[F,P])
# - Compass Logic (CL)
# - Halpern and Shoham's Modal Logic of Time Intervals (HS)
# - Lutz and Wolter's Modal Logic of Topological Relations (LRCC8)
#
# Relation names are given in an uniform way as follows:
#   `NAMEOFTHELOGIC`_`NAMEOFTHERELATION`
#
# For example, the relation Future (F) for LTL[F,P] will be named:
#   `LTLFP_F`
################################################################################

################################################################################
# Linear Temporal Logic with Future and Past (LTL[F,P])
################################################################################
const LTLFP_F = GreaterRel
const LTLFP_P = LesserRel

const LTLFP = [LTLFP_F, LTLFP_P]

################################################################################
# Compass Logic (CL)
################################################################################
const CL = [CL_N, CL_S, CL_E, CL_W]

################################################################################
# Halpern and Shoham's Modal Logic of Time Intervals (HS)
################################################################################
const HS_A  = IA_A
const HS_L  = IA_L
const HS_B  = IA_B
const HS_E  = IA_E
const HS_D  = IA_D
const HS_O  = IA_O
const HS_Ai = IA_Ai
const HS_Li = IA_Li
const HS_Bi = IA_Bi
const HS_Ei = IA_Ei
const HS_Di = IA_Di
const HS_Oi = IA_Oi

const HS = [
    HS_A,
    HS_L,
    HS_B,
    HS_E,
    HS_D,
    HS_O,
    HS_Ai,
    HS_Li,
    HS_Bi,
    HS_Ei,
    HS_Di,
    HS_Oi
]

################################################################################
# Lutz and Wolter's Modal Logic of Topological Relations (LRCC8)
################################################################################
const LRCC8_DC    = Topo_DC
const LRCC8_EC    = Topo_EC
const LRCC8_PO    = Topo_PO
const LRCC8_TPP   = Topo_TPP
const LRCC8_TPPi  = Topo_TPPi
const LRCC8_NTPP  = Topo_NTPP
const LRCC8_NTPPi = Topo_NTPPi

const LRCC8 = [
    LRCC8_DC,
    LRCC8_EC,
    LRCC8_PO,
    LRCC8_TPP,
    LRCC8_TPPi,
    LRCC8_NTPP,
    LRCC8_NTPPi
]
