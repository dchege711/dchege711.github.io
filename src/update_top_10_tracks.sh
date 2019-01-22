#!/bin/bash

# Clean the log files
> logs/stdout.txt
> logs/stderr.txt

# Updates my top 10 songs
python music_chart.py
git add --all
git commit -m "Update top 10 songs"
git push origin master