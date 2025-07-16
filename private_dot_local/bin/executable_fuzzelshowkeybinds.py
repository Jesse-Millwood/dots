#!/usr/bin/env python3

import argparse
import subprocess
import socket
from pathlib import Path
import os
import re
import sys
import json

class Binding():
    def __init__(self, mode, binding, command, description):
        self.mode = mode
        self.binding = binding
        self.command = command
        self.description = description

parser = argparse.ArgumentParser()
parser.add_argument('-c', '--sway-config',
                    type=Path,
                    default=Path('~','.config', 'sway', 'config').expanduser(),
                    help="Path to sway config to parse")

args = parser.parse_args()

binds = []

bindsym_re = r"^\s*bindsym (?P<binding>[$a-zA-Z0-9+]*) (?P<command>[a-zA-Z0-9/~. ]*)#: (?P<description>[a-zA-Z0-9 ()]*)$"
mode_re = r"^\s*mode \"(?P<mode>[a-zA-Z_0-9]*)\" {"
closing_re = r".*}"
with open(args.sway_config, 'r') as config:
    in_mode = False
    num_opening_curly = 0
    current_mode = 'default'
    for line in config:
        num_opening_curly += line.count('{')
        num_opening_curly -= line.count('}')
        r = re.search(mode_re, line)
        if r is not None:
            current_mode = r.group('mode')
            continue
        elif num_opening_curly == 0:
            current_mode = 'default'
        r = re.search(bindsym_re, line)
        if r is not None:
            binds.append(Binding(mode=current_mode,
                                 binding=r.group('binding'),
                                 command=r.group('command'),
                                 description=r.group('description')))

fuzzel_str = ""
max_str_len = 0
for binding in binds:
    if 'default' not in binding.mode:
        mode = f"[{binding.mode}]"
    else:
        mode = ""
    bind_str = f"{mode}{binding.binding} | {binding.description}\n"
    if len(bind_str) > max_str_len:
        max_str_len = len(bind_str)
    fuzzel_str+=bind_str

fuzzel_proc = subprocess.run(['fuzzel', '--width', str(max_str_len),
                              '--prompt', ' ',
                              '--dmenu'],
                             input=fuzzel_str,
                             capture_output=True,
                             text=True)
print(f'Return: {fuzzel_proc.returncode}')
print(f'stdout: {fuzzel_proc.stdout}')
chosen_re = r"(?P<mode>\[[a-zA-Z0-9]*\])?(?P<bind>[$a-zA-Z+0-9]*) \| (?P<description>[a-zA-Z ()0-9]*)$"
chosen_m = re.search(chosen_re, fuzzel_proc.stdout)
chosen_binding = None
if chosen_m is not None:
    print(f'Chosen Match: {chosen_m}')
    chosen_bind = chosen_m.group('bind')
    if chosen_m.group('mode'):
        chosen_mode = chosen_m.group('mode')
    else:
        chosen_mode = 'default'

    for binding in binds:
        if chosen_bind == binding.binding and chosen_mode == binding.mode:
            chosen_binding = binding
            print('Chosen binding from cheatsheet')
            print(f'  Binding    : {binding.binding}')
            print(f'  Mode       : {binding.mode}')
            print(f'  Command    : {binding.command}')
            print(f'  Description: {binding.description}')


if chosen_binding is not None:
    # A lot of this is taken from https://github.com/JordanL2/SwayIPC/blob/main/swayipc.py
    sway_sock_path = os.environ['SWAYSOCK']
    magic = 'i3-ipc'
    magic_len = len(magic)

    message_types = {
            'RUN_COMMAND': 0,
            'GET_WORKSPACES': 1,
            'SUBSCRIBE': 2,
            'GET_OUTPUTS': 3,
            'GET_TREE': 4,
            'GET_MARKS': 5,
            'GET_BAR_CONFIG': 6,
            'GET_VERSION': 7,
            'GET_BINDING_MODES': 8,
            'GET_CONFIG': 9,
            'SEND_TICK': 10,
            'SYNC': 11,
            'GET_INPUTS': 100,
            'GET_SEATS': 101,
    }

    sway_msg = magic.encode()
    sway_msg += len(chosen_binding.command).to_bytes(4, sys.byteorder) # payload length
    sway_msg += message_types['RUN_COMMAND'].to_bytes(4, sys.byteorder) # Command type
    sway_msg += chosen_binding.command.encode()
    print(f'Sendnig: {sway_sock_path}: {sway_msg}')
    sway_sock = socket.socket(socket.AF_UNIX)
    sway_sock.connect(sway_sock_path)
    sway_sock.send(sway_msg)
    response_string = sway_sock.recv(1024).decode('utf-8')[magic_len:]
    beg = response_string.index('{')
    end = response_string.index('}') + 1
    response_json_string = response_string[beg:end]
    response_json = json.loads(response_json_string)
    print(response_json)
    sway_sock.close()
