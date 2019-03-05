#!/usr/bin/env python3
# -*- coding: utf-8 -*-
import random
import subprocess
from sys import stderr
import sys
import os
from time import sleep, time

from queue import Queue
from threading import Thread
import multiprocessing as mp
import threading

thread_num = -1
task_to_do = 0


def eprint(value):
    print(value, file=stderr)


def parse_state(l_state):
    to_play = l_state[0]
    cur = None
    if to_play == '1':
        cur = '2'
    elif to_play == '2':
        cur = '1'
    else:
        raise "parse_state error"
    state = [[[0.0 for k in range(19)] for j in range(19)] for i in range(17)]
    for t in range(8):
        for i in range(19*19):
            state[t][i//19][i % 19] = 1.0 \
                if l_state[t+1][i] == cur else 0.0
            state[t+8][i//19][i % 19] = 1.0 \
                if l_state[t+1][i] == cur else 0.0
            state[16][i//19][i % 19] = 1.0 if to_play == '1' else 0.0
    return state


def worker(device, task_q, recv_q):
    global task_to_do
    ago_eval = subprocess.Popen(['python3', os.getcwd()+'/ago_eval.py', device],
            stdout=subprocess.PIPE,
            stdin=subprocess.PIPE,
            universal_newlines=True, bufsize=0)
    in_ = ago_eval.stdin
    out_ = ago_eval.stdout
    while True:
        # content = task_q.get(blocked=True)
        content = task_q.get()
        if content == "EOF":
            task_q.put(content)
            in_.write('EOF')
            return
        thread_id, state = content

        print(state, file=in_)
        pv = out_.readline()
        pv = pv.strip('\n')
        # eprint("pv:<")
        # eprint(pv[0:12])
        # eprint(">")

        recv_q.put("task_data " + thread_id + " " + pv)


if __name__ == "__main__":

    thds = []
    task_q = mp.Queue()
    recv_q = mp.Queue()
    devices = ['cuda']
    for i in range(len(devices)):
        thd = mp.Process(target=worker, args=(devices[i], task_q, recv_q))
        thds.append(thd)
        thd.start()

    def dispatch_cmd(cmd):
        global task_to_do
        global thread_num
        cmds = cmd.split()
        if cmds[0] == "put_task":
            task_id = cmds[1]
            game_state = cmds[2]
            task_to_do += 1
            task_q.put((task_id, game_state))  # worker will print
        elif cmds[0] == "start":
            thread_num = int(cmds[1])
        elif cmds[0] == "stop":
            task_q.put("EOF")
            return "EOF"
        elif cmds[0] == "genmove_finish":
            print(cmds[0])

    while True:
        if task_to_do > 0:
            recv = recv_q.get()
            task_to_do-=1
            print(recv)
            continue
        recv = input()
        flag = dispatch_cmd(recv)
        if flag is "EOF":
            break
