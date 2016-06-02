library("bigmemory")
library("biganalytics")
library("readr")
df = read.big.matrix("~/signal/day23/airline.csv", type="integer", header=TRUE,
                     backingfile="airline.bin",
                     descriptorfile = "airline.desc",
                     extraCols = "Age")

df = read_csv("~/signal/day23/airline.csv")
