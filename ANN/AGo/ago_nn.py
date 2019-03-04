import torch
import torch.nn as nn
import torch.optim as optim

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
