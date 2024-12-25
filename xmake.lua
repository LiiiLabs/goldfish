set_version ("17.11.0")

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

option("http")
    set_description("Enable http")
    set_default(false)
    set_values(false, true)
option_end()

if has_config("http") then
    add_requires("cpr 1.10.5")
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

    add_installfiles("$(projectdir)/goldfish/(scheme/*.scm)", {prefixdir = "share/goldfish"})
    add_installfiles("$(projectdir)/goldfish/(srfi/*.scm)", {prefixdir = "share/goldfish"})
    add_installfiles("$(projectdir)/goldfish/(liii/*.scm)", {prefixdir = "share/goldfish"})
end

target ("http") do
    set_languages("c++17")
    if is_plat("linux") then
        -- for Ubuntu 20.04
        add_syslinks("stdc++")
    end
    set_targetdir("$(projectdir)/bin/")
    add_includedirs("src/")
    add_files ("http/src/http.cpp")
    add_packages("s7")
    add_packages("tbox")
    add_packages("cpr")
end

includes("@builtin/xpack")

xpack ("goldfish")
    set_formats("deb", "rpm", "srpm")
    set_author("Da Shen <da@liii.pro>")
    set_license("Apache-2.0")
    set_title("Goldfish Scheme")
    set_description("A Python-like Scheme Interpreter") 
    set_homepage("https://gitee.com/LiiiLabs/goldfish")
    add_targets ("goldfish")
    add_sourcefiles("(xmake/**)")
    add_sourcefiles("xmake.lua")
    add_sourcefiles("(src/**)")
    add_sourcefiles("(goldfish/**)")
    on_load(function (package)
        if package:with_source() then
            package:set("basename", "goldfish-$(plat)-src-v$(version)")
        else
            package:set("basename", "goldfish-$(plat)-$(arch)-v$(version)")
        end
    end)
