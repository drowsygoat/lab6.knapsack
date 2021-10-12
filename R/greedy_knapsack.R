#' Greedy Algorithm Knapsack
#' @param W Knapsack size
#' @param x A dataframe containing objects to be put in the knapsack, must contain two variables: v and w
#' @source \url{https://en.wikipedia.org/wiki/Knapsack_problem}
#' @export
greedy_knapsack <- function(x,W){
        ord <- order(x$w/x$v) # order of irems in sorted knapsack
        ordered_ks <- x[order(x$w/x$v), ] # sorted knapsack
        n <- nrow(ordered_ks)
        best_ks <- 0 # best weight
        best_elements <- vector() # vector of elements of the best knapsack
        cw_1 <- 0 # current weight 1
        cv_1 <- 0 # current value 1
        el_1 <- vector()
        cw_2 <- 0 # current weight 2
        cv_2 <- 0 # current value 2
        el_2 <- vector()
        for (i in 1:n){
                        # print(paste("i..",i))
                if (cw_1 + ordered_ks$w[i] > W){ # if statement to break when knapsack is full
                        # print("Knapsack Full - Solution 1")
                        last <- i # last item that fits in the current knapsack, this item excluded if we want later to include the first item that does not fit, see next for loop
                        break
                }
                cw_1 <- cw_1 + ordered_ks$w[i]
                cv_1 <- cv_1 + ordered_ks$v[i]
                el_1 <- append(el_1,i) # elements for solution 1 (wiki)
        }
        for (j in append(last, (last-2):1)){
                if (cw_2 + ordered_ks$w[j] > W){ # if statement to break when knapsack is full
                        # print("Knapsack Full - Solution 2")
                        break
                        }
                cw_2 <- cw_2 + ordered_ks$w[j]
                cv_2 <- cv_2 + ordered_ks$v[j]
                el_2 <- append(el_2,j) # elements for solution 2 (wiki)
        }

        if (cv_1 > cv_2){ # choose better solution (wiki)
                # print("Solution 1 is better")
                best_ks <- cv_1
                best_elements <- el_1
        }else{
                # print("Solution 2 is better")
                best_ks <- cv_2
                best_elements <- el_2
        }
        return(list("value" = best_ks,
                    "elements" = ord[best_elements]))
}

# probably can be simplified for better code clarity...
#
# RNGversion(min(as.character(getRversion()),"3.5.3"))
# set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
# ##old sampler used for backward compatibility
# ## suppressWarnings() can be used so that the above warning is not displayed set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
# n <- 1000000
# knapsack_1000000 <-
#         data.frame(
#                 w=sample(1:4000, size = n, replace = TRUE),
#                 v=runif(n = n, 0, 100000)
#         )
#
# test.function <- function(){
#         start <-Sys.time()
#         greedy_knapsack(x = knapsack_1000000, W = 3500)
#         end <-Sys.time()
#         return(end-start)
# }
#
# mean(replicate(10, test.function()))

# How much time does it takes to run the algorithm for n = 1000000 objects?
# > mean(replicate(10, test.function()))
# [1] 0.1728671
