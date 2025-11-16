using Test
using StatsBase
using SoleLogics
using Graphs
using Random
worlds = SoleLogics.World.(1:10)
fr = SimpleModalFrame(worlds, SimpleDiGraph(length(worlds), 4))
