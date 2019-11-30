#!/bin/bash

# Outputs failed logins for ssh

journalctl -u sshd | grep Failed
