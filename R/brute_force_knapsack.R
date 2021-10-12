
#'@title Brute Force Knapsack
#'@param W Knapsack size
#'@param x A dataframe containing objects to be put in the knapsack, must contain two variables: v and w
#'@source \url{https://en.wikipedia.org/wiki/Knapsack_problem}
#'@export

brute_force_knapsack <- function(x,W){
        stopifnot("x must be a data frame with columns named v and w" = is.data.frame(x) & colnames(x) == c("w", "v"))
        stopifnot("all values in x must be positive" = (which(x$w < 0) == TRUE) == 0 & (which(x$v < 0) == TRUE) == 0)
        stopifnot("W must be positive integer" = W/1==W & W >= 0)
        best_val <- 0
        best_comb <- vector()
        size <- nrow(x)
        for (i in seq(size)){
                comb <- combinations(size,i,1:size)
                for (j in 1:nrow(comb)){
                        val <- sum(x$v[comb[j, ]])
                        mass <- sum(x$w[comb[j, ]])
                        if (mass <= W && val >= best_val){
                                best_val <- val
                                best_comb <- comb[j, ]
                        }
                }
        }
        return(list("value" = best_val,
                    "elements" = best_comb))

}
# test.function <- function(){
#         start <-Sys.time()
#         dynamic_knapsack(x = test_knapsack[1:16,], W = 3500)
#         end <-Sys.time()
#         return(end-start)
# }
#
# mean(replicate(10, test.function()))

# How much time does it takes to run the algorithm for n = 16 objects?
# > mean(replicate(10, test.function()))
# [1] 0.105761
