\name{cgdsr-getGeneticProfiles}
\alias{cgdsr-getGeneticProfiles}
\alias{getGeneticProfiles}
\alias{getGeneticProfiles.CGDS}
\title{Get available genetic data profiles for a specific cancer type}
\description{Queries the CGDS API and returns the available genetic
  profiles, e.g. mutation or copy number profiles, stored about a
  specific cancer type.
}
\usage{\method{getGeneticProfiles}{CGDS}(x,cancerType,...)}
\arguments{
  \item{x}{A CGDS object (required)}
  \item{cancerType}{Cancer type ID (required)}
  \item{...}{Not used.}
}
\value{A data.frame with six columns:
\enumerate{
\item \var{genetic_profile_id}:  a unique ID used to identify the genetic profile ID
in subsequent interface calls.  This is a human readable ID.  For
example, "gbm_mutations" identifies the TCGA GBM mutation genetic profile.
\item \var{genetic_profile_name}:  short profile name.
\item \var{genetic_profile_description}:  short profile description.
\item \var{cancer_type_id}:  cancer type ID tied to this genetic profile.  Will
match the input cancer_type_id.
\item \var{genetic_alteration_type}:  indicates the profile type.  Will be one of:
MUTATION, MUTATION_EXTENDED, COPY_NUMBER_ALTERATION, MRNA_EXPRESSION.
\item \var{show_profile_in_analysis_tab}:  a boolean flag used for internal purposes
(you can safely ignore it).
}
}
\author{
<jacobsen@cbio.mskcc.org>
}
\references{
  CGDS public server at MSKCC: http://cbio.mskcc.org/cgds-public/
}
\seealso{
\code{\link{cgdsr}},\code{\link{CGDS}},\code{\link{getCancerTypes}},\code{\link{getCaseLists}},\code{\link{getProfileData}}
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
}
