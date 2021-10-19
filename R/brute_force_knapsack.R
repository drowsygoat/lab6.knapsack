
#'@title Brute Force Knapsack
#'@param W Knapsack size
#'@param x A dataframe containing objects to be put in the knapsack, must contain two variables: v and w
#'@param parallel If to use multicore
#'@source \url{https://en.wikipedia.org/wiki/Knapsack_problem}
#'@export

brute_force_knapsack <- function(x,W,parallel=FALSE){
        stopifnot("x must be a data frame with columns named v and w" = is.data.frame(x) & colnames(x) == c("w", "v"))
        stopifnot("all values in x must be positive" = (which(x$w < 0) == TRUE) == 0 & (which(x$v < 0) == TRUE) == 0)
        stopifnot("W must be positive integer" = W/1==W & W >= 0)
        size <- nrow(x)
        best_comb <- vector()
        best_val <- 0
        if (isTRUE(parallel)){
                vals <- parallel::mclapply(1:size, function(X,size,x,W){ # this one of the two possibilities to paralelize
                        comb <- combinations(size,X,1:size)
                        for (j in 1:nrow(comb)){
                                val <- sum(x$v[comb[j, ]])
                                mass <- sum(x$w[comb[j, ]])
                                if (mass <= W && val >= best_val){
                                        best_val <- val
                                        best_comb <- comb[j, ]
                                }
                        }
                        return(list(best_val,
                                    best_comb))


                }, size = size, x = x, W = W, # arguments to parallelized function
                   mc.preschedule = TRUE,
                   mc.set.seed = TRUE,
                   mc.silent = FALSE,
                   mc.cores = getOption("mc.cores", parallel::detectCores()),
                   mc.cleanup = TRUE, mc.allow.recursive = TRUE, affinity.list = NULL)

                best_val_index <- which(max(unlist(lapply(vals, "[[", 1))) == unlist(lapply(vals, "[[", 1)))[1]

                return(list("value" = vals[[best_val_index]][[1]],
                            "elements" = vals[[best_val_index]][[2]]))

        }else{
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
}
