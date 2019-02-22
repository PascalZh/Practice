#!/usr/bin/env python3
# -*- coding: utf-8 -*-
import random


def dispatch_cmd(cmd):
    cmds = cmd.split()
    if cmds[0] == "task_id":
        task_id = cmds[1]
        pv = [random.uniform(0, 1) for i in range(362)] # p: policy
        pv[0] = random.uniform(-1, 1)  # v: result of value network
        pv = [str(e) for e in pv]
        return "task_done " + task_id + " " + " ".join(pv)


def main():
    while True:
        recv = input()
        print(dispatch_cmd(recv))
        # print("\033[32m" + recv + "\033[0m")


if __name__ == "__main__":
    main()
