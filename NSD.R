library(solrium)


#######################################################################################
############## functions to get NSD (Normalized Solr Distance)  #######################
#######################################################################################


###############################################################
#
#  description:   returns the Solr results count
#  usage:         getSolrCount(searchTerms, language, ...)
#  arguments:
#                 searchterms   The terms searched for in
#                               vector form e.g. c("wikipedia")
#                               or  c("wikipedia","R")
#                 language      in which lnguage to search.
#                               Either "en" (english) or
#                               "de" (german)          

cli <- SolrClient$new(host = "api.plos.org", path = "search", port = NULL)

getSolrCount <- function(searchTerms=NULL,
                           language="en",
                           ...){
  
  #   # check for arguments
  if(is.null(searchTerms)) stop("Please enter search terms!")
  if(!any(language==c("de","en"))) stop("Please enter correct
                                        language (de, en)!")
  
  #   # uses solrium package function
  # Collapse search terms.
  entry <- paste(searchTerms, collapse="+")
  siteHTML <- solr_stats(cli, params = list(q=entry, stats.field='counter_total_all'),raw=TRUE)
  
  indicatorWord <- "numFound" 
  
  
  #   # start extraction at indicator word position
  posExtractStart <- gregexpr(indicatorWord, siteHTML,
                              fixed = TRUE)[[1]]
  # extract string of 30 chracters length
  stringExtract <- substring(siteHTML, first=posExtractStart,
                             last = posExtractStart + 30)
  #   # search for count of pages in web
  posResults <- gregexpr(':[0-9.,]{1,20},', stringExtract)
  posFirst <- posResults[[1]][1]
  textLength  <- attributes(posResults[[1]])$match.length
  stringExtract <- substring(stringExtract, first=posFirst,
                             last = posFirst + textLength)
  # erase everything but the numbers
  matchCount <- as.numeric(gsub("[^0-9]", "", stringExtract))[1]
  
  return(matchCount)
}

###############################################################


###############################################################
#
#  description:  returns the normalized Solr distance as
#                numeric value
#
#  usage:        NSD(words, language, print, list, ...)
#
#  arguments:
#                words      TWO terms to measure for in
#                           vector form e.g. c("wiki","R")
#                language   in which lnguage to search.
#                           Either "en" (english) or
#                           "de" (german)
#                print      print alls results (NSD, counts)
#                           to console (no default)
#                list       returns list of results (no default)
#                           containing NSD and all counts.
#                ...        at the moment nothing

NSD <- function(words, language="en", print=FALSE,
                list=FALSE, ...){
  
  # check for arguments
  if(!hasArg(words)) stop('NSD needs TWO strings like
                          c("word","word2") as word argument!')
  if(length(words)!=2) stop('word arguments has to be of
                            length two, e.g. c("word","word2")')
  
  # M: total number of web pages searched by Solr (approx.)
  if(hasArg(M)) M <- list(...)$M else M <- 4.5e10    
  
  x <- words[1]
  y <- words[2]
  
  # using getSolrCount() function (see here)
  freq.x  <- getSolrCount(x, language=language)
  freq.y  <- getSolrCount(y, language=language)
  freq.xy <- getSolrCount(c(x,y), language=language)
  
  # apply formula
  NSD = (max(log(freq.x), log(freq.y)) - log(freq.xy)) /
    (log(M) - min( log(freq.x), log(freq.y)) )
  
  # print results to console if requested
  if(print==TRUE){
    cat("\t", x,":", freq.x, "\n",
        "\t", y,":", freq.y, "\n",
        "\t", x,"+", y,":", freq.xy, "\n",
        "\t", "normalized Solr distance (NSD):",
        NSD, "\n", "\n")
  }
  
  
  #   # return list of results if requested (no default)
  #   # containing NSD and all counts. As default only one
  #   # the NSD is returned as numeric value
  
  results <- list(NSD=NSD,
                  x=c(x, freq.x),
                  y=c(y, freq.y),
                  xy=c(paste(x,"+",y), freq.xy)) 
  
  if(list==TRUE) return(results) else  return(NSD)
}

###############################################################


#############################################################################################
############### functions to get NSD (Normalized Solr Distance) - END  ######################
#############################################################################################

#Example with colors
NSD(c("yellow","colors"),print = TRUE)
#Example with colors
res.list <- NSD(c("yellow","colors"),list = TRUE)
res.list
