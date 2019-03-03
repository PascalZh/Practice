#!/usr/bin/env python3
# -*- coding: utf-8 -*-
import random
from sys import stderr
from time import sleep, time

import torch
import torch.nn as nn
import torch.optim as optim
import torch.multiprocessing as mp

task_q = mp.Queue(maxsize=8)
recv_q = mp.Queue(maxsize=8)

def eprint(value):
    print(value, file=stderr)

class ConvLayer(nn.Module):
    def __init__(self):
        super(ConvLayer, self).__init__()
        self.conv = nn.Conv2d(17, 256, 3, padding=1)
        self.bn = nn.BatchNorm2d(256)
        self.relu = nn.ReLU(inplace=True)

    def forward(self, x):
        out = self.conv(x)
        out = self.bn(out)
        out = self.relu(out)
        return out


class ResLayer(nn.Module):
    def __init__(self):
        super(ResLayer, self).__init__()
        self.conv1 = nn.Conv2d(256, 256, 3, padding=1)
        self.bn1 = nn.BatchNorm2d(256)
        self.relu = nn.ReLU(inplace=True)
        self.conv2 = nn.Conv2d(256, 256, 3, padding=1)
        self.bn2 = nn.BatchNorm2d(256)

    def forward(self, x):
        residual = x
        out = self.conv1(x)
        out = self.bn1(out)
        out = self.relu(out)
        out = self.conv2(out)
        out = self.bn2(out)
        out += residual
        out = self.relu(out)
        return out


class PolicyHead(nn.Module):
    def __init__(self):
        super(PolicyHead, self).__init__()
        self.conv = nn.Conv2d(256, 2, 1)
        self.bn = nn.BatchNorm2d(2)
        self.relu = nn.ReLU(inplace=True)
        self.fc = nn.Linear(19*19*2, 19*19+1)

    def forward(self, x):
        out = self.conv(x)
        out = self.bn(out)
        out = self.relu(out)
        out = out.view(out.size(0), -1)
        out = self.fc(out)
        return out


class ValueHead(nn.Module):
    def __init__(self):
        super(ValueHead, self).__init__()
        self.conv = nn.Conv2d(256, 1, 1)
        self.bn = nn.BatchNorm2d(1)
        self.relu = nn.ReLU(inplace=True)
        self.fc1 = nn.Linear(19*19*1, 256)
        self.fc2 = nn.Linear(256, 1)
        self.tanh = nn.Tanh()

    def forward(self, x):
        out = self.conv(x)
        out = self.bn(out)
        out = self.relu(out)
        out = out.view(out.size(0), -1)
        out = self.fc1(out)
        out = self.relu(out)
        out = self.fc2(out)
        out = self.tanh(out)
        return out


class AGoNet(nn.Module):
    def __init__(self):
        super(AGoNet, self).__init__()
        layers = []
        layers.append(ConvLayer())
        for i in range(19):
            layers.append(ResLayer())
        self.body = nn.Sequential(*layers)
        self.policy_head = PolicyHead()
        self.value_head = ValueHead()

    def forward(self, x):
        out = self.body(x)
        logit_policy = self.policy_head(out)
        value = self.value_head(out)
        return logit_policy, value


def get_lr(step):
    step /= 1000
    if step in range(400):
        return 10e-2
    elif step in range(400, 600):
        return 10e-3
    elif step > 600:
        return 10e-4

def train():
    batch_size = 32

    input_state = torch.randn(batch_size, 17, 19, 19)
    input_state.apply_(lambda x: 0 if x < 0 else 1) 
    target_p = torch.randn(batch_size, 19*19+1)
    target_p = torch.sigmoid(target_p)
    target_v = torch.randn(batch_size)
    target_v = torch.tanh(target_v)

    model = AGoNet()

    mse = nn.MSELoss(reduction='sum')

    step = 0
    optimizer = optim.SGD(model.parameters(), lr=get_lr(step) / 2048 * batch_size,
                          momentum=0.9, weight_decay=10e-4)

    for t in range(1000):
        logit_policy, value = model(input_state)

        policy = torch.sigmoid(logit_policy)
        policy = policy.view(policy.size(0), -1, 1)
        target_p = target_p.view(target_p.size(0), 1, -1)
        loss_p = -torch.bmm(target_p, torch.log(policy))
        loss_p = torch.sum(loss_p)

        loss_v = mse(value, target_v)

        loss = loss_p + loss_v
        print(loss)

        optimizer.zero_grad()
        loss.backward()
        optimizer.step()


def worker(device):
    model = AGoNet().to(device)
    while True:
        # content = task_q.get(blocked=True)
        content = task_q.get()
        if content == "EOF":
            return
        thread_id, state = content
        state = torch.Tensor(state).unsqueeze(0).to(device)
        logit_p, v = model(state)
        p = torch.sigmoid(logit_p).view(19*19+1)
        # the last is pass probability
        pv = []
        pv.append(float(v[0][0]))
        for i in range(19*19+1):
            pv.append(float(p[i]))
        pv = [str(e) for e in pv]
        # eprint(len(pv))
        # eprint(pv)
        recv_q.put("task_data " + thread_id + " " + " ".join(pv))


def parse_state(l_state):
    current_player = l_state[0]
    state = [[[0.0 for k in range(19)] for j in range(19)] for i in range(17)]
    for t in range(8):
        for i in range(19*19):
            state[t][i//19][i % 19] = 1.0 \
                if l_state[t+1][i] == current_player else 0.0
            state[t+8][i//19][i % 19] = 1.0 \
                if l_state[t+1][i] == current_player else 0.0
            state[16][i//19][i % 19] = 1.0 if current_player == '1' else 0.0
    # eprint(l_state)
    # eprint(state)
    # sleep(10)
    return state


def test_speed():
    now = time()
    model = AGoNet()
    for i in range(40):
        state = torch.Tensor(1, 17, 19, 19)
        logit_p, v = model(state)
        p = torch.sigmoid(logit_p).view(19*19+1)
        pv = []
        pv.append(float(v[0][0]))
        for i in range(19*19):
            pv.append(float(p[i]))
        pv = [str(e) for e in pv]
        ret = "task_data " + "323224134" + " " + " ".join(pv)
    eprint(time()-now)

mcts_thread_alive = 0
task_to_do = 0
thread_ids = []
thread_num = 0
p_list = []
if __name__ == "__main__":

    devices = ['cpu']
    for i in range(len(devices)):
        p = mp.Process(target=worker, args=(devices[i],))
        p_list.append(p)
        p.start()

    def dispatch_cmd(cmd):
        global task_to_do
        global thread_num
        global mcts_thread_alive
        cmds = cmd.split()
        if cmds[0] == "put_task":
            thread_id = cmds[1]
            game_state = cmds[2].split(":")
            task_q.put((thread_id, parse_state(game_state)))
            task_to_do += 1
            return ""
        elif cmds[0] == "start":
            thread_num = int(cmds[1])
            return ""
        elif cmds[0] == "start_thread":
            thread_ids.append(cmds[1])
            mcts_thread_alive += 1
            return ""
        elif cmds[0] == "exit_thread":
            if cmds[1] not in thread_ids:
                raise "exit unrecorded thread:" + cmds[1]
            mcts_thread_alive -= 1
            if mcts_thread_alive == 0:  # all mcts task thread exited
                for i in range(thread_num):
                    task_q.put("EOF")
                for p in p_list:
                    p.join()
                return "EOF"
            return ""

    while True:
        if task_to_do > 0:
            print(recv_q.get())
            task_to_do -= 1
        recv = input()
        send = dispatch_cmd(recv)
        if send is "EOF":
            print(send)
            break
        print(send)
