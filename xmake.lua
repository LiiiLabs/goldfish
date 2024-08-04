-- mode
set_allowedmodes("releasedbg", "release", "debug", "profile")
add_rules("mode.releasedbg", "mode.release", "mode.debug", "mode.profile")

-- plat
set_allowedplats("linux", "macosx", "windows")

-- proj
set_project("Goldfish Scheme")

-- repo
add_repositories("goldfish-repo xmake")

local S7_VERSION = "20240702"
add_requires("s7 "..S7_VERSION, {system=false})

local TBOX_VERSION = "1.7.5"
tbox_configs = {["force-utf8"]=true}
add_requires("tbox " .. TBOX_VERSION, {system=false, configs=tbox_configs})

target ("goldfish") do
    set_languages("c++17")
    set_targetdir("$(projectdir)/bin/")
    add_files ("src/goldfish.cpp")
    add_packages("s7")
    add_packages("tbox")

    add_installfiles("goldfish/(scheme/*.scm)", {prefixdir = "goldfish"})
    add_installfiles("goldfish/(srfi/*.scm)", {prefixdir = "goldfish"})
end

