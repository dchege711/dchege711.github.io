"""utils.py"""

"""The list of words in `enable1.txt`"""
word_list = []

_path_to_word_list = (
    "/Users/dchege711/reddit_daily_programmer/challenges/"
    "_easy_challenges/380_easy_smooshed_morse_code/enable1.txt"
)

with open(_path_to_word_list, "r") as fp:
    for line in fp:
        word_list.append(line.strip())
