# This file can be used to automatically resolve dependencies
# involving unregistered packages.
# To do so, simply call `update_dependency()` one time for each package
# respecting the correct dependency order.

using Pkg

# Remove the specified package (do not abort if it is already removed) and reinstall it.
function install(package::String, url::String, rev::String)
    printstyled(stdout, "\nRemoving: $package\n", color=:green)
    try
        Pkg.rm(package)
    catch error
        println(); showerror(stdout, error); println()
    end

    printstyled(stdout, "\nFetching: $url at branch $rev\n", color=:green)
    try
        Pkg.add(url=url, rev=rev)
        printstyled(stdout, "\nPackage $package instantiated correctly\n", color=:green)
    catch error
        println(); showerror(stdout, error); println()
    end
end

install("SoleBase", "https://github.com/aclai-lab/SoleBase.jl", "dev")
install("SoleLogics", "https://github.com/aclai-lab/SoleLogics.jl", "algebras/giopaglia")

Pkg.instantiate()
