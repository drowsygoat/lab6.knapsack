
#'@title Brute Force Knapsack
#'@param W Knapsack size
#'@param x A dataframe containing objects to be put in the knapsack, must contain two variables: v and w
#'@source \url{https://en.wikipedia.org/wiki/Knapsack_problem}
#'@export

brute_force_knapsack <- function(x,W){
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
