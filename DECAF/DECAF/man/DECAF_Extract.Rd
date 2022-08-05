\name{DECAF_Extract}
\alias{DECAF_Extract}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
DECAF_Extract
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
DECAF_Extract loops through all DECAF study .csv files in directory "path". Then
extracts relevant data and organizes into 6 separate data frames for more usable
data formatting for Chelsea's DECAF study.
}
\usage{
DECAF_Extract(path)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{path}{
%%     ~~Describe \code{x} here~~
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
\item{raw_n_back}{Data frame consisting of all raw n-back data separated by participant and prompt time.}
\item{summary_n_back}{Data frame consisting of all scored n-back data (e.g., mean response time, sd response time, hits, correct rejections, false alarms, misses, no responses, number of stimuli presented, hit rate, and false alarm rate) separated by participant and prompt time.}
\item{raw_EMA_SR}{Data frame consisting of all self-reported EMA data separated by participant and prompt time.}
\item{raw_spatial_span}{Data frame consisting of all raw spatial span data, separated by participant and prompt time.}
\item{summary_spatial_span}{Data frame consisting of all scored spatial span data (e.g., total correct, partial correct [i.e., right responses, wrong order], total incorrect, and number of spatial span trials)}
\item{DECAF_all}{Data frame combining raw_EMA_SR, summary_n_back, and summary_spatial_span, all separated by participant and prompt time.}
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Matthew Richard John Vandermeer, PhD, CPsych (supervised practice)
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
%##---- Should be DIRECTLY executable !! ----
%##-- ==>  Define data, use random,
%##--	or do  help(data=index)  for the standard data sets.

%## The function is currently defined as
%function (x)
%{
%  }
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
