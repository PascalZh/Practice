#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import torch
from ago_nn import AGoNet
from ago_main import parse_state
from ago_main import eprint
import sys

if __name__ == "__main__":
    model = None
    if sys.argv[1] != None:
        model = AGoNet().to(sys.argv[1])
    if model != None:
        while True:
            # eprint(model)
            state = input()
            if state == "EOF":
                break
            state = parse_state(state.split(':'))
            state = torch.Tensor(state).unsqueeze(0).to(sys.argv[1])
            logit_p, v = model(state)
            p = torch.sigmoid(logit_p).view(19*19+1)
            # the last is pass probability
            pv = []
            pv.append(float(v[0][0]))
            for i in range(19*19+1):
                pv.append(float(p[i]))
            pv = [str(e) for e in pv]
            pv = " ".join(pv)
            print(pv)
