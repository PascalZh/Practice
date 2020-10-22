add_rules("mode.debug", "mode.release")
set_config("cxx", "g++-10")
set_config("ld", "g++-10")

target("test")
    set_kind("binary")
    add_files("src/*.cpp")
    add_cxxflags("-std=c++20")
    if is_mode("release") then
        add_defines("NDEBUG")
    end

target("blitz")
    set_kind("shared")
    set_filename("blitz.so")
    add_files("src/*.cpp")
    add_files("src/lua/lua_wrapper.cpp")
    add_cxxflags("-std=c++20")
    add_links("lua5.1")
    add_includedirs("/usr/include/lua5.1")
    add_includedirs("src")
    if is_mode("release") then
        add_defines("NDEBUG")
    end
