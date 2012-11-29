#!/usr/bin/Rscript
monty = function(n, switch) {
    z  = rmultinom(n, 3, replicate(3, 1/3))
    zs = apply(z, MARGIN = 1, sum)
    if (switch) { wins = sum(zs[1:2]) } else { wins = zs[3] }
    print(wins / sum(zs))
}

whenSwitching = monty(10000, T)
whenStaying   = monty(10000, F)

