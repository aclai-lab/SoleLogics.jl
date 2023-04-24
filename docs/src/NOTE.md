## Quick start

make.jl structure is inspired by the official julia language repo: 
https://github.com/JuliaLang/julia/blob/master/doc/make.jl

Move inside this folder (doc) and run `julia --project=. make.jl` to build documentation;
a new private "build" folder will be created if no errors occur.

This is Documenter.jl documentation: https://documenter.juliadocs.org/stable/man/guide/

## Troubleshooting

The command `make` can generate very large warning logs.
Here it is a list of common mistakes. 

### Missing docstring
If the documentation is generated properly but some yellow frames "Missing docstring" appears,
check if the definition is exported from your Package entry (e.g. SoleLogics.jl).
If it is not exported, the following block

\`\`\`@doc
AbstractFormula
\`\`\`

has to be rewritten like this, for example

\`\`\`@doc
SoleLogics.AbstractFormula
\`\`\`

Note that if we want to specify a specific dispatch docstring, the same rule applies, for example

\`\`\`@doc
SoleLogics.propositions(f::SoleLogics.AbstractFormula)
\`\`\`

instead of 

\`\`\`@doc
SoleLogics.propositions(f::AbstractFormula)
\`\`\`
