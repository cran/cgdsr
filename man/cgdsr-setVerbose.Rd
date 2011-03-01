\name{cgdsr-setVerbose}
\alias{cgdsr-setVerbose}
\alias{setVerbose}
\alias{setVerbose.CGDS}
\title{Set verbose logging level for CGDS function calls}
\description{Sets verbose logging level for CGDS function calls.}
\usage{\method{setVerbose}{CGDS}(x, verbose, ...)}
\arguments{
  \item{x}{A CGDS object (required)}
  \item{verbose}{Activate verbose logging (boolean)}
  \item{...}{Not used.}
}
\author{
<jacobsen@cbio.mskcc.org>
}
\references{
  CGDS public server at MSKCC: http://cbio.mskcc.org/cgds-public/
}
\seealso{
\code{\link{cgdsr}},\code{\link{CGDS}}
}
\examples{
# Create CGDS object
mycgds = CGDS("http://cbio.mskcc.org/cgds-public/")

getCancerTypes(mycgds)

# Activate verbose logging
setVerbose(mycgds, TRUE)

getCancerTypes(mycgds)

}
