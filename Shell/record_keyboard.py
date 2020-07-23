#!/usr/bin/env python3
import struct
import sys
from evdev import InputDevice, ecodes

print("\033[32mRun `xinput list` to know which input device is your keyboard, "
      "and change the source code. \nAnd you need to sudo to run this script"
      " or add yourself to the group `input`.\033[0m")
print("e.g. \033[34mVirtual core keyboard    id=3 [master keyboard (2)]\033[0m"
      "  implies  \033[33m/dev/input/event2\033[0m\n")

dev = InputDevice('/dev/input/event2')
print(f"dev = \033[31m{dev}\033[0m")
print(f"ecodes.KEY = \033[35m{ecodes.KEY}\033[0m")

data = []
path = sys.argv[1]


def write_data(data_):
    with open(path, 'ab') as f:
        for d in data_:
            f.write(struct.pack('<H', d[-1]))


i = 0
N = 10
"""save to csv every N times"""
try:
    for event in dev.read_loop():
        if event.type == ecodes.EV_KEY and event.value < 2:
            value = event.value
            """0: push up  1: push down  2: hold"""
            code = event.code
            key = ecodes.KEY[event.code]
            # print(f"value: \033[31m{value}\033[0m\t"
            #       f"code: \033[32m{code}\033[0m\tkey: \033[33m{key}\033[0m")

            i += 1
            data.append([value, code, key, (value << 15) | code])
            if i >= N:
                i = 0
                write_data(data)
                data = []

except KeyboardInterrupt:
    dev.close()
    write_data(data)
