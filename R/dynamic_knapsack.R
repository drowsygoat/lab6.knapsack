
#'@title Dynamic Programming Knapsack
#'@param W Knapsack size
#'@param x A dataframe containing objects to be put in the knapsack, must contain two variables: v and w
#'@source \url{https://en.wikipedia.org/wiki/Knapsack_problem}
#'@export

dynamic_knapsack <- function(x,W){
        stopifnot("x must be a data frame with columns named v and w" = is.data.frame(x) & colnames(x) == c("w", "v"))
        stopifnot("all values in x must be positive" = (which(x$w < 0) == TRUE) == 0 & (which(x$v < 0) == TRUE) == 0)
        stopifnot("W must be positive integer" = W/1==W & W >= 0)

        n <- nrow(x)+1
        K <- matrix(0, ncol = W+1, nrow = n)
        for (j in 1:W+1){
                K[1, j] <- 0
                }
        for (i in 1:n){
                K[i, 1] <- 0
        }
        # fill the KS table
        for (i in 2:n){
                for (j in 1:W+1){
                        wi <- x$w[i-1]
                        vi <- x$v[i-1]
                        if (wi <= j){
                                K[i,j] <- max(K[i-1,j-wi]+vi, K[i-1,j])
                        }else{
                                K[i,j] <- K[i-1,j]
                        }
                }
        }
        get_elements <- function(i, j){ # aux function to get ks items
                if (i == 1){
                        return()
                }
                if (K[i,j] > K[i-1,j]){
                        return(append(i, get_elements(i-1,j-x$w[i-1])))
                }else{
                        return(get_elements(i-1,j))
                }
        }
        elements <- sort(get_elements(i=n,j=W))
        return(list("value" = K[n,W+1],
                    "elements" = elements-1))
}

# dynamic_knapsack(x = knapsack_objects[1:800,], W = 3500)
