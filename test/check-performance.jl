using Dictionaries
using Chairmarks, Graphs, Random, SoleLogics, StatsBase, ThreadSafeDicts
using SoleLogics: AbstractKripkeStructure, AbstractSyntaxBranch, AnyWorld, World
using SoleLogics: allworlds, collateworlds, frame, isgrounded

function generateformulas(n::Int, h::Int; seed::Int=42, natoms = 5, type = :propositional_dict, truths::Bool = true, kwargs...)
    rng = Xoshiro(seed)
    Σ = Atom.(1:natoms)
    os = if startswith(string(type), "propositional")
        Vector{Connective}([∧, ∨, →, ¬, ])
    elseif type == :modal
        Vector{Connective}([∧, ∨, →, ¬, ◊, □])
    else
        error("$type")
    end

    leaves = truths ? vcat(Σ, [⊤, ⊥]) : Σ
    T = truths ? Union{Atom, BooleanTruth, SyntaxBranch} : Union{Atom, SyntaxBranch}
    aotpicker = (rng)->StatsBase.sample(rng, leaves, uweights(length(leaves)))

    Γ = Vector{T}()
    for _ in 1:n
        push!(
            Γ, 
            randformula(
                rng,
                h,
                Σ,
                os;
                opweights = StatsBase.uweights(length(os)),
                basecase = aotpicker,
                kwargs...
            )
        )
    end
    return Γ
end

function generatemodalmodels(n::Int, nw::Int, ne::Int; seed::Int=42, natoms = 5, type = :propositional_dict, truths::Bool = true, kwargs...)
    rng = Xoshiro(seed)
    Σ = Atom.(1:natoms)
    if type == :propositional_array
        [Σ[rand(Bool, length(Σ))] for i in 1:n]
    elseif type == :propositional_dict
        [Dict([p => rand(Bool) for p in Σ]) for i in 1:n]
    elseif type == :propositional_t1d
        [TruthDict(Dict([p => rand(Bool) for p in Σ])) for i in 1:n]
    elseif type == :propositional_t1y
        [TruthDict(dictionary([p => rand(Bool) for p in Σ])) for i in 1:n]
    elseif type == :propositional_t2d
        [TruthDict2(Dict([p => rand(Bool) for p in Σ])) for i in 1:n]
    elseif type == :propositional_t2y
        [TruthDict2(dictionary([p => rand(Bool) for p in Σ])) for i in 1:n]
    elseif type == :modal
        e = Vector{KripkeStructure}()
        for _ in 1:n
            g = SimpleDiGraph(nw,ne)
            rem_vertices!(g, vertices(g)[map(v->!has_path(g,1,v),vertices(g))])
            f = SimpleModalFrame(SoleLogics.World.(1:nv(g)), g)
            v = Dict{
                SoleLogics.World{Int64},
                TruthDict{Dict{Atom{String}, BooleanTruth}}
            }()
            ws = f.worlds
            for i in 1:length(f.worlds)
                v[ws[i]] = TruthDict{Dict{Atom{String}, BooleanTruth}}()
                values = bitrand(rng, 5)
                for j in 1:5
                    v[ws[i]][Σ[j]] = values[j]
                end
            end
            push!(e, KripkeStructure(f, v))
        end
        return e
    else
        error("$type")
    end
end

function checkall(algo, Γ::Vector, e::Vector)
    # [check(algo, φ, m, AnyWorld()) for φ ∈ Γ, m ∈ e]
    [check(algo, φ, m) for φ ∈ Γ, m ∈ e]
end

Γ = nothing
e = nothing

algos = [
    DefaultCheckAlgorithm()
]
for type in [
    :propositional_array,
    :propositional_t1d,
    # :propositional_t1y,
    :propositional_t2d,
    :propositional_t2y,
    # :modal,
]
    println("type: $(type)")
    for setting in [
        (; h = 7, mode = :maxheight, truths = true, natoms = 5, ),
        (; h = 7, mode = :maxheight, truths = false, natoms = 5, ),
        (; h = 7, mode = :maxheight, truths = true, natoms = 30, ),
        (; h = 7, mode = :maxheight, truths = false, natoms = 30, ),
        (; h = 30, mode = :height, truths = true, natoms = 5, ),
        (; h = 30, mode = :height, truths = false, natoms = 5, ),
        (; h = 30, mode = :height, truths = true, natoms = 30, ),
        (; h = 30, mode = :height, truths = false, natoms = 30, ),
        (; h = 10, mode = :full, truths = true, natoms = 5, ),
        (; h = 10, mode = :full, truths = false, natoms = 5, ),
        (; h = 10, mode = :full, truths = true, natoms = 30, ),
        (; h = 10, mode = :full, truths = false, natoms = 30, ),
    ]
        # h = 7
        ne = 18
        global Γ, e
        Γ = generateformulas(50, setting.h; setting.mode, type, setting...);
        e = generatemodalmodels(50, setting.h, ne; type, setting...);
        # println("  setting: $(setting)")
        for algo in algos
            # println("    ", algo, "\t->\t", @b checkall(algo, Γ, e))
            println("    ", @b checkall(algo, Γ, e))
            # println()
        end
    end
end
