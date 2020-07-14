#!/usr/bin/env python3
# -*- coding: utf-8 -*-
import os

with open('sogou_lexicon.txt', 'r', encoding='utf-8') as f:
    lines = f.readlines()

f = open("lexicon.txt", "w")
for i, line in enumerate(lines):
    [pinyin, word] = line.split()
    pinyin = pinyin[1:]
    f.write(f"{pinyin} {word} 0\n")

f.close()
