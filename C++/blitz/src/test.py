#!/usr/bin/env python3
import blitz

wq = blitz.WordQuery()
wq.query("ni", 10)
input()
print(wq.get_last_query())
