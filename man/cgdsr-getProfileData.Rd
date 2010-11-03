\name{cgdsr-getProfileData}
\alias{cgdsr-getProfileData}
\alias{getProfileData}
\alias{getProfileData.CGDS}
\title{Retrieves genomic profile data for genes and genetic profiles.}
\description{Queries the CGDS API and returns data based on gene(s),
  genetic profile(s),  and a case list.}
\usage{\method{getProfileData}{CGDS}(x,genes,geneticProfiles,caseListID,...)}
\arguments{
  \item{x}{A CGDS object (required)}
  \item{genes}{A vector of gene names or a String specifying a single gene (required)}
  \item{geneticProfiles}{ A vector of genetic profile IDs or String specifying
    a single genetic profile (required)}
  \item{caseListID}{A case list ID (required)}
  \item{...}{Not used.}
}
\value{When requesting one or multiple genes and a single genetic profile ID,
you will receive a data.frame with the following columns:
\enumerate{
\item \var{GENE_ID}:  Entrez Gene ID
\item \var{COMMON}:  HUGO Gene Symbol
\item Columns 3 - N:  Data in columns for each case/sample
}

When requesting a single gene and multiple geneic profile IDs, you will
receive a data.frame with the following columns:

\enumerate{
  \item \var{GENETIC_PROFILE_ID}:  The Genetic Profile ID.
  \item \var{ALTERATION_TYPE}:  The Genetic Alteration Type, e.g. MUTATION,
MUTATION_EXTENDED, COPY_NUMBER_ALTERATION, or MRNA_EXPRESSION.
\item \var{GENE_ID}:  Entrez Gene ID.\
\item \var{COMMON}:  HUGO Gene Symbol.
\item Columns 5 - N:  Data in columns for each case/sample.
}
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
}
