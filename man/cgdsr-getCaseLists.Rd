\name{cgdsr-getCaseLists}
\alias{cgdsr-getCaseLists}
\alias{getCaseLists}
\alias{getCaseLists.CGDS}
\title{Get available case lists for a specific cancer type
}
\description{Queries the CGDS API and returns available case lists for a
  specific cancer type.
}
\usage{\method{getCaseLists}{CGDS}(x,cancerTypeID,...)}
\arguments{
  \item{x}{A CGDS object (required)}
  \item{cancerTypeID}{Cancer type ID (required)}
  \item{...}{Not used.}
}
\value{A data.frame with five columns:
\enumerate{
\item \var{case_list_id}:  a unique ID used to identify the case list ID in
subsequent interface calls.  This is a human readable ID.  For example,
"gbm_all" identifies all cases profiles in the TCGA GBM study.
\item \var{case_list_name}:  short name for the case list.
\item \var{case_list_description}:  short description of the case list.
\item \var{cancer_type_id}:  cancer type ID tied to this genetic profile.  Will
match the input cancer_type_id.
\item \var{case_ids}:  space delimited list of all case IDs that make up this case list.
}
}
\details{Queries the CGDS API and returns available case lists for a
  specific cancer type. For example, a within a particular study, only
  some cases may have sequence data, and another subset of cases may
  have been sequenced and treated with a specific therapeutic protocol.  Multiple 
  case lists may be associated with each cancer type, and this method
  enables you to retrieve meta-data regarding all of these case lists.
}
\author{
<jacobsen@cbio.mskcc.org>
}
\references{
  CGDS public server at MSKCC: http://cbio.mskcc.org/cgds-public/
}
\seealso{
\code{\link{cgdsr}},\code{\link{CGDS}},\code{\link{getCancerTypes}},\code{\link{getGeneticProfiles}},\code{\link{getProfileData}}
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
