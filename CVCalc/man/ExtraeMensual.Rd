\name{ExtraeMensual}
\alias{ExtraeMensual}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ExtraeMensual(nombreRaw)
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
    rw1513 <- a[a$Period == 2015013, ]
    rw1514 <- a[a$Period == 2015014, ]
    rw1515 <- a[a$Period == 2015015, ]
    rw1516 <- a[a$Period == 2015016, ]
    rw1517 <- a[a$Period == 2015017, ]
    rw1518 <- a[a$Period == 2015018, ]
    rw1519 <- a[a$Period == 2015019, ]
    rw1520 <- a[a$Period == 2015020, ]
    rw1521 <- a[a$Period == 2015021, ]
    rw1522 <- a[a$Period == 2015022, ]
    rw1523 <- a[a$Period == 2015023, ]
    rw1524 <- a[a$Period == 2015024, ]
    rw1613 <- a[a$Period == 2016013, ]
    rw1614 <- a[a$Period == 2016014, ]
    write.table(rw1513, file = "RAW_data_PN2015013.txt", quote = FALSE, 
        sep = "\t", row.names = FALSE)
    write.table(rw1514, file = "RAW_data_PN2015014.txt", quote = FALSE, 
        sep = "\t", row.names = FALSE)
    write.table(rw1515, file = "RAW_data_PN2015015.txt", quote = FALSE, 
        sep = "\t", row.names = FALSE)
    write.table(rw1516, file = "RAW_data_PN2015016.txt", quote = FALSE, 
        sep = "\t", row.names = FALSE)
    write.table(rw1517, file = "RAW_data_PN2015017.txt", quote = FALSE, 
        sep = "\t", row.names = FALSE)
    write.table(rw1518, file = "RAW_data_PN2015018.txt", quote = FALSE, 
        sep = "\t", row.names = FALSE)
    write.table(rw1519, file = "RAW_data_PN2015019.txt", quote = FALSE, 
        sep = "\t", row.names = FALSE)
    write.table(rw1520, file = "RAW_data_PN2015020.txt", quote = FALSE, 
        sep = "\t", row.names = FALSE)
    write.table(rw1521, file = "RAW_data_PN2015021.txt", quote = FALSE, 
        sep = "\t", row.names = FALSE)
    write.table(rw1522, file = "RAW_data_PN2015022.txt", quote = FALSE, 
        sep = "\t", row.names = FALSE)
    write.table(rw1523, file = "RAW_data_PN2015023.txt", quote = FALSE, 
        sep = "\t", row.names = FALSE)
    write.table(rw1524, file = "RAW_data_PN2015024.txt", quote = FALSE, 
        sep = "\t", row.names = FALSE)
    write.table(rw1613, file = "RAW_data_PN2016013.txt", quote = FALSE, 
        sep = "\t", row.names = FALSE)
    write.table(rw1614, file = "RAW_data_PN2016014.txt", quote = FALSE, 
        sep = "\t", row.names = FALSE)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
