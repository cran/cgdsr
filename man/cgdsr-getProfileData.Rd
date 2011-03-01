\name{cgdsr-getProfileData}
\alias{cgdsr-getProfileData}
\alias{getProfileData}
\alias{getProfileData.CGDS}
\title{Retrieves genomic profile data for genes and genetic profiles.}
\description{Queries the CGDS API and returns data based on gene(s),
  genetic profile(s),  and a case list.}
\usage{\method{getProfileData}{CGDS}(x,genes,geneticProfiles,caseList,cases,...)}
\arguments{
  \item{x}{A CGDS object (required)}
  \item{genes}{A vector of gene names or a String specifying a single gene (required)}
  \item{geneticProfiles}{ A vector of genetic profile IDs or String specifying
    a single genetic profile (required)}
  \item{caseList}{A case list ID}
  \item{cases}{A vector of case IDs)}
  \item{...}{Not used.}
}
\value{

When requesting one or multiple genes and a single genetic profile,
the function returns a data.frame with genetic profile data in columns for each gene.

When requesting a single gene and multiple genetic profiles,
the function returns a data.frame containing columns with data for each genetic profile.

Cases can be specified either through a case list ID, or a vector of
case IDs.
}
\details{ Only one list is allowed, specify either a list of genes or
  genetic profiles. The format of the output data.frame depends on if
  a single or a list of genes was specified in the arguments.
}
\author{
<jacobsen@cbio.mskcc.org>
}
\references{
  CGDS public server at MSKCC: http://cbio.mskcc.org/cgds-public/
}
\seealso{
\code{\link{cgdsr}},\code{\link{CGDS}},\code{\link{getCancerTypes}},\code{\link{getGeneticProfiles}},\code{\link{getCaseLists}}
}
\examples{
# Create CGDS object
mycgds = CGDS("http://cbio.mskcc.org/cgds-public/")

# Get list of cancer types at server
getCancerTypes(mycgds)

# Get available case lists (collection of samples) for a given cancer type  
mycancertype = getCancerTypes(mycgds)[1,1]
mycaselist = getCaseLists(mycgds,mycancertype)[1,1]

# Get available genetic profiles
mygeneticprofile = getGeneticProfiles(mycgds,mycancertype)[4,1]

# Get data slices for a specified list of genes, genetic profile and case list
getProfileData(mycgds,c('BRCA1','BRCA2'),mygeneticprofile,mycaselist)

# Get data slice for a single gene
getProfileData(mycgds,'HMGA2',mygeneticprofile,mycaselist)

# Get data slice for multiple genetic profiles and single gene
getProfileData(mycgds,'HMGA2',getGeneticProfiles(mycgds,mycancertype)[c(3,4),1],mycaselist)

# Get the same dataset from a vector of case IDs
cases = unlist(strsplit(getCaseLists(mycgds,mycancertype)[1,'case_ids'],' '))
getProfileData(mycgds,'HMGA2',getGeneticProfiles(mycgds,mycancertype)[c(3,4),1],cases=cases)

}
