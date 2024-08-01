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
    set_targetdir("$(projectdir)/bin/")
    add_files ("src/goldfish.cpp")
    add_packages("s7")
end

function add_scheme_test(filepath)
    local testname = path.basename(filepath)
    target(testname) do
        set_enabled(not is_plat("wasm"))
        set_kind("phony")
        set_group("tests")
        add_deps("goldfish")
        on_run(function (target)
            name = target:name()
            print("------------------------------------------------------")
            print("Executing: " .. name)
            params = {
              file_path
            }
            if is_plat("macosx", "linux") then
                binary = target:deps()["goldfish"]:targetfile()
            else
                print("Unsupported plat $(plat)")
            end
            cmd = binary
            print("cmd: " .. cmd)
            print("filepath: " .. filepath)
            os.execv(cmd, params)
        end)
    end
end


all_tests = os.files("tests/**/*-test.scm")

for _, filepath in ipairs(all_tests) do
    add_scheme_test(filepath)
end
