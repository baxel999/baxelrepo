\name{box.muller}
\alias{Polar coordinates method}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{box.muller(n)
%%  ~~function to do ... ~~
}
\description{
This function applies Box-Muller technique to generate *n* random numbers from a bivariate Gaussian distribution (mean=0, sd=1, ro=0).
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
box.muller(n)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{number of replications
%%     ~~Describe \code{x} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
\item{N1 } {N1 ~ N(0,1)}
\item{N2 } {N2 ~ N(0,1)}
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
x=box.muller(10^4)
hist(x,freq=F); curve(dnorm(x,0,1), add=T, col='red')
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
