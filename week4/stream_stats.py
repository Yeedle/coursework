#!/usr/bin/env python
# memory intensive version.

import sys
import fileinput

def is_even(n):
    return True if n % 2 == 0 else False

def median(lst):
    lst.sort()
    if is_even(len(lst)):
        mid = len(lst) / 2
        return (lst[mid - 1] + lst[mid]) / 2.0
    else:
        return (lst[(len(lst) / 2)])

def mean(lst):
    return float(sum(lst))/len(lst)

if __name__ == '__main__':

    # check for input filename given as first argument
    if len(sys.argv) < 2:
        sys.stderr.write('reading input from stdin\n')
    data = {}
    # read input one line at a time
    for line in fileinput.input():
        # split on tab to get the key and value for each line
        fields = line.rstrip('\n').split('\t')
        data.setdefault(fields[0], []).append(int(fields[1]))
        # update statistics for the group correponding to this key
        # (minimum, median, mean, and maximum)

    # loop over each group and output the group's key and statistics

    for key in data:
        print "%s\t%d\t%.2f\t%.2f\t%d\n" % (key, min(data[key]), median(data[key]), mean(data[key]), max(data[key]))
