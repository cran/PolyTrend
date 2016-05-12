\name{PolyTrend}
\alias{PolyTrend}
\encoding{UTF-8}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Trend Classification Algorithm
}
\description{
This algorithm can classify the trends as linear, quadratic, cubic, concealed or no-trend. The "concealed trend" are those trends that possess quadratic or cubic forms, but the net change between the start and end of the period hasn't been significant. While, the "no-trend" category includes simple linear trends that are below the significant slope coefficient.
}
\usage{
PolyTrend(Y, alpha)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Y}{
        a vector of values corresponding to the trend or de-seasonalised component of vegetation
        time series data.
}
  \item{alpha}{
        the statistical significance level.
}
}
\details{
An object of the class "PT" is a list with the trend type, slope, direction, and statistical
significance.
}

\value{

\item{TrendType}{the trend type as a number, which can be equal to: -1,0,1,2 and 3. The values correspond to
a concealed trend (-1), no trend (0), linear trend (1), quadratic trend (2) or cubic trend (3).}
\item{Slope}{the linear slope value.}
\item{Direction}{the linear slope direction as number, which could be equal to 1 or -1. These values
correspond to positive or negative direction.}
\item{Significance}{the polynomial significance as a number that can be -1 or 1, which correspond to statistically
insignificant or statistically significant respectively.}
\item{PolynomialDegree}{the polynomial degree as a number. This variable can be: 0, 1, 2 or 3, which corresponds to
no trend, linear, quadratic or cubic trend.}
}

\references{
Jamali, S, Seaquist, J, Eklundh, L, & Ardö, J 2014, 'Automated mapping of vegetation trends with
polynomials using NDVI imagery over the Sahel', Remote Sensing Of Environment, 141, pp. 79-89,
ScienceDirect, EBSCOhost.

Tomov, H. 2016, 'Automated temporal NDVI analysis over the Middle East for the period 1982 - 2010', http://lup.lub.lu.se/student-papers/record/8871893

}
\author{
Sadegh Jamali, Hristo Tomov
}

\examples{

\dontrun{
## Examples of a cubic trend (Site 1 & Site 2, Jamali et al. 2014)
data(ex.a)
data(ex.b)

pt.a <- PolyTrend(ex.a, 0.05)
plot(pt.a, fig.dates = c(1982:2006))

pt.b <- PolyTrend(ex.b, 0.05)
plot(pt.b, fig.dates = c(1982:2006))

## Examples of a concealed trend (Site 3 & Site 4, Jamali et al. 2014)
data(ex.c)
data(ex.e)

pt.c <- PolyTrend(ex.c, 0.05)
plot(pt.c, fig.dates = c(1982:2006))

pt.e <- PolyTrend(ex.e, 0.05)
plot(pt.e, fig.dates = c(1982:2006))

## Examples of a quadratic trend (Site 5 & Site 6, Jamali et al. 2014)
data(ex.d)
data(ex.f)

pt.d <- PolyTrend(ex.d, 0.05)
plot(pt.d, fig.dates = c(1982:2006))

pt.f <- PolyTrend(ex.f, 0.05)
plot(pt.f, fig.dates = c(1982:2006))

## Examples of a concealed trend (Site 7 & Site 8, Jamali et al. 2014)
data(ex.g)
data(ex.h)

pt.g <- PolyTrend(ex.g, 0.05)
plot(pt.g, fig.dates = c(1982:2006))

pt.h <- PolyTrend(ex.h, 0.05)
plot(pt.h, fig.dates = c(1982:2006))

## Examples of a linear trend (Site 9 & Site 10, Jamali et al. 2014)
data(ex.k)
data(ex.m)

pt.k <- PolyTrend(ex.k, 0.05)
plot(pt.k, fig.dates = c(1982:2006))

pt.m <- PolyTrend(ex.m, 0.05)
plot(pt.m, fig.dates = c(1982:2006))

## Example of a no-trend (Site 11, Jamali et al. 2014)
data(ex.n)

pt.n <- PolyTrend(ex.n, 0.05)
plot(pt.n, fig.dates = c(1982:2006))

}


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ts}% use one of  RShowDoc("KEYWORDS")
