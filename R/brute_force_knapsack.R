brute_force_knapsack <-
function(x,W){
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
