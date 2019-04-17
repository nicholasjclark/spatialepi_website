#### Plot citation metrics data from GoogleScholar User profiles ####
#'@param user Character string representing Google Scholar user profile ID
#'@return A \code{ggplot2} object showing a graph of citation metrics, with a timestamp
#' 
scholar_summary = function(author_list){
library(scholar)
library(dplyr)
  
  #### Auxillary functions ####
  get_resp <- function(url, attempts_left = 5) {
    
    stopifnot(attempts_left > 0)
    
    resp <- httr::GET(url, handle = getOption("scholar_handle"))
    
    # On a successful GET, return the response
    if (httr::status_code(resp) == 200) {
      resp
    } else if (attempts_left == 1) { # When attempts run out, stop with an error
      stop("Cannot connect to Google Scholar. Is the ID you provided correct?")
    } else { # Otherwise, sleep a second and try again
      Sys.sleep(1)
      get_resp(url, attempts_left - 1)
    }
  }
  
  tidy_id <- function(id) {
    if (length(id)!=1) {
      id <- id[1]
      msg <- sprintf("Only one ID at a time; retrieving %s", id)
      warning(msg)
    }
    
    ## Check with Google to set cookies
    if (getOption("scholar_call_home")) {
      sample_url <- "https://scholar.google.com/citations?user=B7vSqZsAAAAJ"
      sink <- GET(sample_url)      
      options("scholar_call_home"=FALSE, "scholar_handle"=sink)
    }
    
    return(id)
  }
  
  compose_url <- function(id, url_template) {
    if (is.na(id)) return(NA_character_)
    id <- tidy_id(id)
    url <- sprintf(url_template, id)
    
    url
  }
  
  get_cite_history <- function(id) {
    library(rvest)
    url_template <- "http://scholar.google.com/citations?hl=en&user=%s&pagesize=100&view_op=list_works"
    url <- compose_url(id, url_template)
    
    ## A better way would actually be to read out the plot of citations
    page <- get_resp(url) %>% read_html()
    years <- page %>% html_nodes(xpath="//*/span[@class='gsc_g_t']") %>%
      html_text() %>% as.numeric()
    vals <- page %>% html_nodes(xpath="//*/span[@class='gsc_g_al']") %>%
      html_text() %>% as.numeric()
    if(length(years)>length(vals)){
      # Some years don't have citations.
      # We need to match the citation counts and years
      # <a href="javascript:void(0)" class="gsc_g_a" style="left:8px;height:5px;z-index:9">\n  <span class="gsc_g_al">2</span>\n</a>
      style_tags=page %>% html_nodes(css = '.gsc_g_a') %>%
        html_attr('style')
      # these z indices seem to be the indices starting with the last year
      zindices=as.integer(stringr::str_match(style_tags, 'z-index:([0-9]+)')[,2])
      # empty vector of 0s
      allvals=integer(length=length(years))
      # fill in
      allvals[zindices]=vals
      # and then reverse
      vals=rev(allvals)
    }
    df <- data.frame(year=years, cites=vals)
    
    return(df)
  }
  
citations <- do.call(rbind, lapply(seq_len(length(author_list)), function(x){
  cit <- get_cite_history(author_list[x])
})) %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(cites = sum(cites))

tot_articles <- sum(unlist(lapply(seq_len(length(author_list)), function(x){
  tot <- scholar::get_num_articles(author_list[x])
})))

ggplot2:: ggplot(citations, ggplot2::aes(x = year, y = cites)) +
  ggplot2::theme_bw() +
  ggplot2::geom_bar(stat = 'identity', fill = 'darkblue') +
  ggplot2::xlab('Year') +
  ggplot2::ylab('Google Scholar\n citations')+
  ggplot2::annotate('text', label = 'SpatialEpiLab Publications',
                    x = -Inf, y = Inf, hjust = -0.03, vjust = 1.5, size = 4) +
  ggplot2::annotate('text', label = format(Sys.time(), "%Y-%m-%d"),
           x = -Inf, y = Inf, vjust = 4.5, hjust = -0.1, size = 3) +
  ggplot2::annotate("text", x = -Inf, y = Inf, vjust = 6, hjust = -0.05, 
                    label = paste('Total papers = ', tot_articles),
                    size = 3) +
  ggplot2::annotate("text", x = -Inf, y = Inf, vjust = 7.5, hjust = -0.05, 
           label = paste('Total citations = ', sum(citations$cites)),
           size = 3) +
  ggplot2::theme(panel.grid.major = ggplot2::element_blank(), 
                 panel.grid.minor = ggplot2::element_blank(),
                 axis.text.x = ggplot2::element_text(angle = 55, hjust = 1),
                 legend.position = 'none')
}

#### Return citation data from GoogleScholar User profiles for top 10 articles ####
#'@param user Character string representing Google Scholar user profile ID
#'@return A \code{dataframe}
#' 
scholar_top10 = function(author_list){
  library(scholar)
  library(dplyr)
  
  citations <- do.call(rbind, lapply(seq_len(length(author_list)), function(x){
    cit <- scholar::get_publications(author_list[x])
  })) %>%
    dplyr::mutate(Journal = tools::toTitleCase(tolower(as.character(journal))),
                  Title = tools::toTitleCase(tolower(as.character(title))),
                  Year = year,
                  Citations = cites,
                  Authors = author) %>%
    dplyr::arrange(-Citations) %>%
    dplyr::select(Title, Journal, Authors, Year, Citations,
                  -cid, -pubid, -journal, -title, -year, -cites, -author, -number)
    
  citations$Title <- gsub('<I>|< I>|</I>', '', citations$Title)
  citations <- citations[1:10,]
  return(citations)
}
