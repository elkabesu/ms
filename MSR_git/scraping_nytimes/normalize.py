#!/usr/bin/env python

import re

def normalize(s):
    """
    normalizes text, removing unicode and non-word characters
    """

    # remove unicode
    s=filter(lambda c: ord(c) < 128, s)

    # remove non-word characters
    return re.sub(r'[^A-Za-z\s]',' ',s.lower())


if __name__=='__main__':
    sentence = "This sentence has punctuation---lots of it! Let's get rid of it."
    print normalize(sentence)
