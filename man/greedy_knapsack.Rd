\name{greedy_knapsack}
\alias{greedy_knapsack}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
greedy_knapsack(x, W)
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
    ord <- order(x$w/x$v)
    ordered_ks <- x[order(x$w/x$v), ]
    n <- nrow(ordered_ks)
    best_ks <- 0
    best_elements <- vector()
    cw_1 <- 0
    cv_1 <- 0
    el_1 <- vector()
    cw_2 <- 0
    cv_2 <- 0
    el_2 <- vector()
    for (i in 1:n) {
        if (cw_1 + ordered_ks$w[i] > W) {
            print("Knapsack Full - Solution 1")
            last <- i
            break
        }
        cw_1 <- cw_1 + ordered_ks$w[i]
        cv_1 <- cv_1 + ordered_ks$v[i]
        el_1 <- append(el_1, i)
    }
    for (j in append(last, (last - 2):1)) {
        if (cw_2 + ordered_ks$w[j] > W) {
            print("Knapsack Full - Solution 2")
            break
        }
        cw_2 <- cw_2 + ordered_ks$w[j]
        cv_2 <- cv_2 + ordered_ks$v[j]
        el_2 <- append(el_2, j)
    }
    if (cv_1 > cv_2) {
        print("Solution 1 is better")
        best_ks <- cv_1
        best_elements <- el_1
    }
    else {
        print("Solution 2 is better")
        best_ks <- cv_2
        best_elements <- el_2
    }
    return(list(value = best_ks, elements = ord[best_elements]))
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
