#!/bin/bash

emacsclient -s utility -e "1" &>/dev/null || emacs --daemon=utility
