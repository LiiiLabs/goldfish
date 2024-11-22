-- mode
set_allowedmodes("releasedbg", "release", "debug", "profile")
add_rules("mode.releasedbg", "mode.release", "mode.debug", "mode.profile")

-- plat
set_allowedplats("linux", "macosx", "windows")

-- proj
set_project("Goldfish Scheme")

-- repo
add_repositories("goldfish-repo xmake")

option("tbox")
    set_description("Use tbox installed via apt")
    set_default(false)
    set_values(false, true)
option_end()

local S7_VERSION = "20241122"
add_requires("s7 "..S7_VERSION, {system=false})

local TBOX_VERSION = "1.7.6"
if has_config("tbox") then
    add_requires("apt::libtbox-dev", {alias="tbox"})
else
    tbox_configs = {hash=true, ["force-utf8"]=true}
    add_requires("tbox " .. TBOX_VERSION, {system=false, configs=tbox_configs})
end

target ("goldfish") do
    set_languages("c++98")
    if is_plat("linux") then
        -- for Ubuntu 20.04
        add_syslinks("stdc++")
    end
    set_targetdir("$(projectdir)/bin/")
    add_files ("src/goldfish.cpp")
    add_packages("s7")
    add_packages("tbox")

    add_installfiles("goldfish/(scheme/*.scm)", {prefixdir = "goldfish"})
    add_installfiles("goldfish/(srfi/*.scm)", {prefixdir = "goldfish"})
end

