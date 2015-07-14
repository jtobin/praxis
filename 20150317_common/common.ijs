
test0 =. 1 5 10 20 40 80
test1 =. 6 7 10 20 80 100
test2 =. 3 4 15 20 30 70 80 120

test3 =. 1 5 5 5
test4 =. 3 4 5 5 10
test5 =. 5 5 10 20

NB. for a given array, want keys and counts
NB. should be easy.  vals become keys, counts are reported

histogram =. ({. ; #) /. ~

NB. next need intersection of sorts between these boxed arrays
