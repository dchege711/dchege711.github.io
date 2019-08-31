#!/bin/bash

# Clean the log files
> logs/stdout.txt
> logs/stderr.txt

# Updates my flash cards
conda activate /Users/dchege711/dchege711.github.io/src/com.c13u.cards/com.c13u.cards.venv
python fetch_recent_cards.py
git add --all
git commit -m "Update my flash cards"
git push origin master
