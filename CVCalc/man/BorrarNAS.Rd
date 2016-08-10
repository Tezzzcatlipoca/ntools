\name{BorrarNAS}
\alias{BorrarNAS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
BorrarNAS(nombre, Columna)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nombre}{
%%     ~~Describe \code{nombre} here~~
}
  \item{Columna}{
%%     ~~Describe \code{Columna} here~~
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
function (nombre, Columna) 
{
    a <- read.table(nombre, quote = "", sep = "\t", header = TRUE)
    a$borrar <- TRUE
    a$borrar[is.na(a[, Columna])] <- FALSE
    b <- a[a$borrar, 1:5]
    write.table(b, file = nombre, quote = FALSE, sep = "\t", 
        row.names = FALSE)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
