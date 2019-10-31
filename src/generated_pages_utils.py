"""
Utility functions used by page generators.
"""

import re
from os import path
from datetime import date
import fileinput

def update_date_in_file(abs_filepath: str):
    """
    Write today's date on the `abs_filepath` so that the blog updates. Doesn't 
    change the name of the file as that would change the permalink.
    """
    assert path.isabs(abs_filepath)

    date_regex = re.compile(r"date: \d{4}-\d{2}-\d{2}")
    with fileinput.input(files=[abs_filepath], inplace=True) as fp:
        found_match = False
        for line in fp:
            if not found_match and date_regex.match(line):
                print(f"date: {date.today().strftime('%Y-%m-%d')}\n", end="")
                found_match = True
            else:
                print(line, end="")
