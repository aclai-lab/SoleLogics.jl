using SoleLogics
using Documenter

DocMeta.setdocmeta!(SoleLogics, :DocTestSetup, :(using SoleLogics); recursive = true)

Documenter.HTMLWriter.HTML(
    size_threshold = 4000000,
)

makedocs(;
    modules = [SoleLogics],
    authors = "Mauro Milella, Giovanni Pagliarini, Eduard I. Stan",
    repo = "https://github.com/aclai-lab/SoleLogics.jl/blob/{commit}{path}#{line}",
    sitename = "SoleLogics.jl",
    format = Documenter.HTML(;
        prettyurls = get(ENV, "CI", "false") == "true",
        canonical = "https://aclai-lab.github.io/SoleLogics.jl",
        assets = String[],
    ),
    pages = [
        "Home" => "index.md",
        "Getting started" => "getting-started.md",
        "Propositional logic" => "propositional-logic.md",
        "Modal logic" => "modal-logic.md",
        "Full reference" => "autodocs.md"
    ]
)

deploydocs(;
    repo = "github.com/aclai-lab/SoleLogics.jl",
    target = "build",
    branch = "gh-pages",
    versions = ["main" => "main", "stable" => "v^", "v#.#", "dev" => "dev"],
)
