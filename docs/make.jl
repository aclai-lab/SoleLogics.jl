using SoleLogics
using Documenter

DocMeta.setdocmeta!(SoleLogics, :DocTestSetup, :(using SoleLogics); recursive = true)

BaseDocs = [
    "base/syntax.md",
    "base/operators.md",
    "base/formulas.md",
    "base/alphabets.md"
]

const PAGES = [
    "Home" => "index.md",
    "Base" => BaseDocs,
]

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
    pages = PAGES
)

deploydocs(;
    repo = "github.com/aclai-lab/SoleLogics.jl",
    target = "build",
    branch = "gh-pages",
    versions = ["main" => "main", "stable" => "v^", "v#.#", "dev" => "dev"],
)
