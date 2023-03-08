using SoleLogics
using Documenter

DocMeta.setdocmeta!(SoleLogics, :DocTestSetup, :(using SoleLogics); recursive = true)

makedocs(;
    modules = [SoleLogics],
    authors = "Mauro MILELLA, Giovanni PAGLIARINI, Eduard I. STAN",
    repo = "https://github.com/aclai-lab/SoleLogics.jl/blob/{commit}{path}#{line}",
    sitename = "SoleLogics.jl",
    format = Documenter.HTML(;
        prettyurls = get(ENV, "CI", "false") == "true",
        canonical = "https://aclai-lab.github.io/SoleLogics.jl",
        assets = String[],
    ),
    pages = [
        "Home" => "index.md",
        "About SoleLogics" => "temp.md",
        "Getting started" => "temp.md",
        "Manual" => [
            "Basics" => "temp.md",
            "Propositional logic" => "temp.md"
        ]
    ],
)

deploydocs(; repo = "github.com/aclai-lab/SoleLogics.jl")
