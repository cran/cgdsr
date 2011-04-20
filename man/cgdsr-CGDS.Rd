\name{cgdsr-CGDS}
\alias{cgdsr-CGDS}
\alias{CGDS}
\title{Construct a CGDS connection object}
\description{Creates a CGDS connection object from a CGDS endpoint URL. This object
  must be passed on to the methods which query the server.}
\usage{CGDS(url,verbose=FALSE)}
\arguments{
  \item{url}{A CGDS URL (required).}
  \item{verbose}{A boolean variable specifying verbose output (default FALSE)}
}
\value{ A CGDS connection object. This object must be passed on to the methods which query the server. }
\author{
<jacobsen@cbio.mskcc.org>
}
\references{
 cBio Cancer Genomics Portal: \url{http://www.cbioportal.org/cgx/}
}
\seealso{
\code{\link{cgdsr}},\code{\link{getCancerTypes}},\code{\link{getGeneticProfiles}},\code{\link{getCaseLists}},\code{\link{getProfileData}}
}
\examples{
# Create CGDS object
mycgds = CGDS("http://cbio.mskcc.org/cgds-public/")

# Test the CGDS endpoint URL using a few simple API tests
test(mycgds) 

# Get list of cancer types at server
getCancerTypes(mycgds)
}
