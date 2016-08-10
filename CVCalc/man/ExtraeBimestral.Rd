\name{ExtraeBimestral}
\alias{ExtraeBimestral}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ExtraeBimestral(nombreRaw)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nombreRaw}{
%%     ~~Describe \code{nombreRaw} here~~
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
function (nombreRaw) 
{
    a <- read.table(nombreRaw, quote = "", sep = "\t", header = TRUE)
    unique(a$Period)
    length(unique(a$Period))
    a$borrar <- TRUE
    a$borrar[is.na(a$Sales)] <- FALSE
    a <- a[a$borrar, 1:5]
    rw1513 <- a[a$Period == 2015003, ]
    rw1514 <- a[a$Period == 2015005, ]
    rw1515 <- a[a$Period == 2015007, ]
    rw1516 <- a[a$Period == 2015009, ]
    rw1517 <- a[a$Period == 2015011, ]
    rw1518 <- a[a$Period == 2016001, ]
    write.table(rw1513, file = "RAW_data_PN2015003.txt", quote = FALSE, 
        sep = "\t", row.names = FALSE)
    write.table(rw1514, file = "RAW_data_PN2015005.txt", quote = FALSE, 
        sep = "\t", row.names = FALSE)
    write.table(rw1515, file = "RAW_data_PN2015007.txt", quote = FALSE, 
        sep = "\t", row.names = FALSE)
    write.table(rw1516, file = "RAW_data_PN2015009.txt", quote = FALSE, 
        sep = "\t", row.names = FALSE)
    write.table(rw1517, file = "RAW_data_PN2015011.txt", quote = FALSE, 
        sep = "\t", row.names = FALSE)
    write.table(rw1518, file = "RAW_data_PN2016001.txt", quote = FALSE, 
        sep = "\t", row.names = FALSE)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
