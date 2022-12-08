# This file can be used to automatically resolve dependencies
# involving unregistered packages.
# To do so, simply call `update_dependency()` one time for each package
# respecting the correct dependency order.

using Pkg

# Remove the specified package (do not abort if it is already removed) and reinstall it.
function update_dependency(
    package::String;
    branch::String="dev",
    repository="https://github.com/aclai-lab/"
)
    println("Removing: ", package)
    try
        Pkg.rm(package)
    catch
        println(package, " not found in project or manifest")
    end

    url = repository * package * '#' * branch
    println("Fetching: ", url)
    Pkg.add(path=url)
end

update_dependency("SoleBase.jl")
