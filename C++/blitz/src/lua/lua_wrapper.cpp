#include <lua.hpp>
#include "core.hpp"
using namespace std;
using namespace blitz;
unique_ptr<InputMethod> im;

extern "C" {
  static int init(lua_State *L) {
    im = make_unique<InputMethod>();
    return 0;
  }

  static const luaL_Reg mylib [] = {
    {"init", init},
    {NULL, NULL}
  };

  int luaopen_blitz(lua_State *L) {
    luaL_register(L, "blitz", mylib); // deprecated in lua5.2
    return 1;
  }
}
