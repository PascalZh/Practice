add_rules("mode.debug", "mode.release")
set_config("cxx", "g++-10")
set_config("ld", "g++-10")

target("test")
    set_kind("binary")
    add_files("src/*.cpp")
    add_cxxflags("-std=c++20")
    if is_mode("debug") then
        add_defines("DEBUG")
    end
