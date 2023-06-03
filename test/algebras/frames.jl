using Test
using StatsBase
using SoleLogics
using SoleModels
using Graphs
using Random
worlds = SoleLogics.World.(1:10)
fr = SoleLogics.ExplicitCrispUniModalFrame(worlds, SimpleDiGraph(length(worlds), 4))
