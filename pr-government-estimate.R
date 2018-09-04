sep <- floor(mean(c(2478, 2495, 2258, 2368)))
oct <- floor(mean(c(2411, 2894, 2394, 2357)))
nov <- floor(mean(c(2272, 2651, 2268, 2484)))
dic <- floor(mean(c(2541, 2891, 2518, 2854)))

sep7 <- 2928
oct7 <- 3040
nov7 <- 2671
dic7 <- 2820


cat("(", sep7," - ", sep, ") + ",
    "(", oct7," - ", oct, ") + ",
    "(", nov7," - ", nov, ") + " ,
    "(", dic7," - ", dic, ") = ", 
sum(c(sep7-sep, oct7-oct,nov7-nov,dic7-dic)), sep="")
