#!/usr/bin/env python3
# -*- coding: utf-8 -*-
import random
from sys import stderr
from time import sleep

import torch
import torch.nn as nn
import torch.optim as optim

device = torch.device('cuda' if torch.cuda.is_available() else 'cpu')

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

def start_train():
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


def eval_model(state, model):
    logit_p, v = model(state)
    p = torch.sigmoid(logit_p).view(19*19+1) # TODO pass probability
    ret = []
    ret.append(float(v[0][0]))
    for i in range(19*19):
        ret.append(float(p[i]))
    return ret


def parse_state(l_state):
    current_player = l_state[0]
    opposite_player = '1' if current_player == '2' else '2'
    state = torch.Tensor(17, 19, 19)
    for t in range(8):
        for i in range(19*19):
            state[t][i//19][i % 19] = 1.0 if l_state[t+1][i] == current_player else 0.0
            state[t+8][i//19][i % 19] = 1.0 if l_state[t+1][i] == current_player else 0.0
    state[16][:][:] = 1.0 if current_player == '1' else 0.0
    # eprint(l_state)
    # eprint(state)
    # sleep(10)
    return state.unsqueeze(0)


def main():
    mcts_thread_alive = 0
    thread_ids = []
    model = AGoNet()
    model.to(device)

    # start_train()

    def dispatch_cmd(cmd):
        cmds = cmd.split()
        if cmds[0] == "put_task":
            thread_id = cmds[1]
            game_state = cmds[2].split(":")
            # eprint(game_state)
            pv = eval_model(parse_state(game_state), model)
            pv = [str(e) for e in pv]
            return "task_data " + thread_id + " " + " ".join(pv)
        elif cmds[0] == "start":
            thread_ids.append(cmds[1])
            nonlocal mcts_thread_alive
            mcts_thread_alive += 1
            return ""
        elif cmds[0] == "exit":
            if cmds[1] not in thread_ids:
                raise "exit unrecorded thread:" + cmds[1]
            mcts_thread_alive -= 1
            if mcts_thread_alive == 0:  # all mcts task thread exited
                return "EOF"
            return ""

    while True:
        recv = input()
        send = dispatch_cmd(recv)
        if send is "EOF":
            print(send)
            break
        print(send)


if __name__ == "__main__":
    main()
