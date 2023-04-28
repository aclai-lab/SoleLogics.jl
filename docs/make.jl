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
    "Julia Documentation" => "index.md",
    "Base" => BaseDocs,
]

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
    pages = PAGES
)

deploydocs(; repo = "github.com/aclai-lab/SoleLogics.jl")
