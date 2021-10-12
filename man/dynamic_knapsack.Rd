\name{dynamic_knapsack}
\alias{dynamic_knapsack}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
dynamic_knapsack(x, W)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{W}{
%%     ~~Describe \code{W} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x, W) 
{
    n <- nrow(x) + 1
    K <- matrix(0, ncol = W + 1, nrow = n)
    for (j in 1:W + 1) {
        K[1, j] <- 0
    }
    for (i in 1:n) {
        K[i, 1] <- 0
    }
    for (i in 2:n) {
        for (j in 1:W + 1) {
            wi <- x$w[i - 1]
            vi <- x$v[i - 1]
            if (wi <= j) {
                K[i, j] <- max(K[i - 1, j - wi] + vi, K[i - 1, 
                  j])
            }
            else {
                K[i, j] <- K[i - 1, j]
            }
        }
    }
    get_elements <- function(i, j) {
        if (i == 1) {
            return()
        }
        if (K[i, j] > K[i - 1, j]) {
            return(append(i, get_elements(i - 1, j - x$w[i - 
                1])))
        }
        else {
            return(get_elements(i - 1, j))
        }
    }
    elements <- sort(get_elements(i = n, j = W))
    return(list(value = K[n, W + 1], elements = elements - 1))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
