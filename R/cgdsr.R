library(R.oo);

setConstructorS3("CGDS", function(url='',verbose=FALSE) {
  extend(Object(), "CGDS",
         .url=url,
         .verbose=verbose)
})

setMethodS3("processURL","CGDS", private=TRUE, function(x, url, ...) {
  if (x$.verbose) cat(url,"\n")
  df = read.table(url, skip=0, header=TRUE, as.is=TRUE, sep="\t") 
})

setMethodS3("getCancerTypes","CGDS", function(x, ...) {
  url = paste(x$.url, "webservice.do?cmd=getCancerTypes&",sep="")
  df = processURL(x,url)
  return (df)
})

setMethodS3("getCaseLists","CGDS", function(x,cancerTypeID, ...) {
  url = paste(x$.url, "webservice.do?cmd=getCaseLists&cancer_type_id=", cancerTypeID, sep="")
  df = processURL(x,url)
  return (df)
})

setMethodS3("getGeneticProfiles","CGDS", function(x,cancerTypeID, ...) {
  url = paste(x$.url, "webservice.do?cmd=getGeneticProfiles&cancer_type_id=", cancerTypeID, sep="")
  df = processURL(x,url)
  return (df)
})

setMethodS3("getProfileData","CGDS", function(x,genes,geneticProfiles,caseListID, ...) {
  url = paste(x$.url, "webservice.do?cmd=getProfileData",
    "&gene_list=", paste(genes,collapse="+"),
    "&genetic_profile_id=", paste(geneticProfiles,collapse="+"),
    "&case_set_id=", caseListID,
    "&id_type=", 'gene_symbol',
    sep="")
  df = processURL(x,url)
  return (df)
})

            
setMethodS3("test","CGDS", function(x, ...) {
  checkEq = function(a,b) { if (identical(a,b)) "OK\n" else "FAILURE!\n" }
  cancertypes = getCancerTypes(x)
  cat('getCancerTypes... ',
      checkEq(colnames(cancertypes),c("cancer_type_id","name","description")))
  ct = cancertypes[1,1]

  cat('getCaseLists (1/2) ... ',
      checkEq(colnames(getCaseLists(x,ct)),
              c("case_list_id","case_list_name",
                "case_list_description","cancer_type_id","case_ids")))
  cat('getCaseLists (2/2) ... ',
      checkEq(colnames(getCaseLists(x,'xxx')),
              'Error...No.case.lists.available.for.cancer_type_id...xxx.'))

  cat('getGeneticProfiles (1/2) ... ',
      checkEq(colnames(getGeneticProfiles(x,ct)),
              c("genetic_profile_id","genetic_profile_name","genetic_profile_description",
                "cancer_type_id","genetic_alteration_type","show_profile_in_analysis_tab")))
  cat('getGeneticProfiles (2/2) ... ',
      checkEq(colnames(getGeneticProfiles(x,'xxx')),
              'Error...No.genetic.profiles.available.for.cancer_type_id...xxx.'))

  # check one gene, one profile
  cat('getProfileData (1/6) ... ',
      checkEq(colnames(getProfileData(x,'NF1','gbm_mrna','gbm_all'))[c(1:2)],
              c("GENE_ID","COMMON")))
  # check many genes, one profile
  cat('getProfileData (2/6) ... ',
      checkEq(colnames(getProfileData(x,c('MDM2','MDM4'),'gbm_mrna','gbm_all'))[c(1:2)],
              c("GENE_ID","COMMON")))
  # check one gene, many profile
  cat('getProfileData (3/6) ... ',
      checkEq(colnames(getProfileData(x,'NF1',c('gbm_mrna','gbm_mutations'),'gbm_all'))[c(1:4)],
              c("GENETIC_PROFILE_ID","ALTERATION_TYPE","GENE_ID","COMMON")))
  # invalid gene names return empty data.frame
  cat('getProfileData (4/6) ... ',
      checkEq(nrow(getProfileData(x,c('NF10','NF11'),'gbm_mrna','gbm_all')),as.integer(0)))
  # invalid case_list_id returns error
  cat('getProfileData (5/6) ... ',
      checkEq(colnames(getProfileData(x,'NF1','gbm_mrna','xxx')),
              'Error...Invalid.case_set_id...xxx.'))
  # invalid genetic_profile_id returns error
  cat('getProfileData (6/6) ... ',
    checkEq(colnames(getProfileData(x,'NF1','xxx','gbm_all')),
            'No.genetic.profile.available.for.genetic_profile_id...xxx.'))
    
})

### non OO

## getCancerTypes=function(cgdsUrl,verbose=F) {
##   url = paste(cgdsUrl, "webservice.do?cmd=getCancerTypes&",sep="")
##   if (verbose) cat(url,"\n")
##   df = read.table(url, skip=1, header=T, as.is=T, sep="\t")
##   return (df)
## }

## getCaseLists=function(cgdsUrl,cancerTypeId,verbose=F) {
##   url = paste(cgdsUrl, "webservice.do?cmd=getCaseLists&cancer_type_id=", cancerTypeId, sep="")
##   if (verbose) cat(url,"\n")
##   df = read.table(url, skip=1, header=T, as.is=T, sep="\t")
##   return (df)
## }

## getGeneticProfiles=function(cgdsUrl,cancerTypeId,verbose=F) {
##   url = paste(cgdsUrl, "webservice.do?cmd=getGeneticProfiles&cancer_type_id=", cancerTypeId, sep="")
##   if (verbose) cat(url,"\n")
##   df = read.table(url, skip=1, header=T, as.is=T, sep="\t")
##   return (df)
## }

## getProfileData=function(cgdsUrl,genes,geneticProfileId,caseListId,idType="gene_symbol",verbose=F) {
##   url = paste(cgdsUrl, "webservice.do?cmd=getProfileData",
##     "&gene_list=", paste(genes,collapse="+"),
##     "&genetic_profile_id=", paste(geneticProfileId,collapse="+"),
##     "&case_set_id=", caseListId,
##     "&id_type=", idType,
##     sep="")
##   if (verbose) cat(url,"\n")
##   df = read.table(url, skip=1, header=T, as.is=T, sep="\t")
##   return (df)
## }

## testCgdsUrl = function(cgdsUrl) {
##   checkEq = function(a,b) { if (identical(a,b)) "ok\n" else "FAILURE!\n" }
##   cancertypes = getCancerTypes(cgdsUrl)
##   cat('getCancerTypes... ',
##       checkEq(colnames(cancertypes),c("cancer_type_id","name","description")))
##   ct = cancertypes[1,1]
##   cat('getCaseLists... ',
##       checkEq(colnames(getCaseLists(cgdsUrl,ct)),
##               c("case_list_id","case_list_name",
##                 "case_list_description","cancer_type_id","case_ids")))
##   cat('getGeneticProfiles... ',
##       checkEq(colnames(getGeneticProfiles(cgdsUrl,ct)),
##               c("genetic_profile_id","genetic_profile_name","genetic_profile_description",
##                 "cancer_type_id","genetic_alteration_type","show_profile_in_analysis_tab")))
## }

