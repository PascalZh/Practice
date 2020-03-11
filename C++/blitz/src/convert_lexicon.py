#!/usr/bin/env python3
# -*- coding: utf-8 -*-
import os

with open('sogou_lexicon.txt', 'r', encoding='utf-8') as f:
    lines = f.readlines()
os.system('mkdir -p db')

ind = 1
f = open(f"db/{ind}.db", 'w')
endpoints = []
for i, line in enumerate(lines):
    [pinyin, word] = line.split(' ')
    pinyin = pinyin[1:]
    f.write(f"{pinyin} 0 {word}")
    
    if (i + 1) % 8000 == 0:
        endpoints.append(pinyin)
        ind += 1
        f.close()
        f = open(f"db/{ind}.db", 'w')
    if (i + 1) == len(lines):
        endpoints.append(pinyin)
f.close()

with open("db/index.db", 'w') as f:
    f.write(" ".join(endpoints))
