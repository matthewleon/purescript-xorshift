from ast import literal_eval
from scipy.stats import chisquare
from sys import stdin

inarr = literal_eval(stdin.read())
print(chisquare(inarr))
