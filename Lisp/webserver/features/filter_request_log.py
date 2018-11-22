#!/usr/bin/env python3
from time import sleep
from random import randint


def filter_request():
    lines = []
    with open("log/request.log", "r") as f:
        lines = f.readlines()
        lines = filter(lambda str_: str_.find("127.0.0.1"), lines)
    with open('log/request.log', 'w') as f:
        f.writelines(lines)


def main():
    while (True):
        filter_request()
        sleep(randint(12, 24) * 60 * 60)


if __name__ == "__main__":
    main()
