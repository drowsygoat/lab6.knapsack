#' A data set containing a graph taken from the Wikipedia page of Dijkstra's algorithm.
#'@format A data frame with knapsack elements for testing
#'\describe{
#'   \item{v}{Items values}
#'   \item{w}{Items weights}
#'   }
#'@source Lab 6 Instructions

# put this into vignette
# RNGversion(min(as.character(getRversion()),"3.5.3"))
# set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
# ##old sampler used for backward compatibility
# ## suppressWarnings() can be used so that the above warning is not displayed set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
# n <- 2000
# knapsack_objects <-
#         data.frame(
#                 w=sample(1:4000, size = n, replace = TRUE),
#                 v=runif(n = n, 0, 10000)
#         )
"test_knapsack"
