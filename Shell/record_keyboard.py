#!/usr/bin/env python3
import pandas as pd
import struct
from evdev import InputDevice, categorize, ecodes
print("run xinput list to know which input device is your keyboard, and change the source code")
print("e.g. Virtual core keyboard    id=3 [master keyboard (2]  ->  /dev/input/event2")

dev = InputDevice('/dev/input/event2')
print(dev)

df = pd.DataFrame(columns=['code', 'value', 'key'])
data = []
i = 0
# save to csv every N times
N = 10
for event in dev.read_loop():
    if event.type == ecodes.EV_KEY:
        code = event.code
        # 1: push down 0: push up 2: hold
        value = event.value
        key = ecodes.KEY[event.code]
        # print(code, key)
        if value == 0 or value == 1:
            i += 1
            data.append((value << 15) | code)
            if i >= N:
                i = 0
                with open("record.db", 'ab') as f:
                    for d in data:
                        # print("%x" % d)
                        f.write(struct.pack('<H', d))
                data = []
