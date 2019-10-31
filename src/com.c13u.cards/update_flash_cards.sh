#!/bin/zsh

# Turn on the Wi-Fi
networksetup -setairportpower en0 on

# Clean the log files
> logs/stdout.txt
> logs/stderr.txt

# Updates my flash cards
python fetch_recent_cards.py
git add --all
git commit -m "Update my flash cards"
git push origin master
