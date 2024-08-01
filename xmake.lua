-- mode
set_allowedmodes("releasedbg", "release", "debug", "profile")
add_rules("mode.releasedbg", "mode.release", "mode.debug", "mode.profile")

-- plat
set_allowedplats("linux", "macosx", "windows")

set_project("Goldfish Scheme")

add_repositories("goldfish-repo xmake")

S7_VERSION = "20240702"
add_requires("s7 "..S7_VERSION, {system=false})

target ("goldfish") do
    set_languages("c++17")
    add_files ("src/goldfish.cpp")
    add_packages("s7")
end
