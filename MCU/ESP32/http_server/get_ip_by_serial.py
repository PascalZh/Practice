#!/usr/bin/env python3
# -*- coding: utf-8 -*-
from serial import Serial
import re


def soft_reset(port):
    port.setRTS(0)
    port.setDTR(0)
    port.setRTS()
    port.setDTR()


def get_ip():
    port = Serial('/dev/ttyUSB0', 115200)
    ip = ''
    pat = re.compile(r'sta ip: (\d+.\d+.\d+.\d+)')
    soft_reset(port)
    while True:
        data = port.readline()
        line = data.decode('ascii')
        # print(line)
        find_res = re.findall(pat, line)
        if len(find_res) != 0:
            # print("result: %s" % find_res)
            ip = find_res[0]
            break
    return ip

def main():
    print(get_ip())

if __name__ == "__main__":
    main()
