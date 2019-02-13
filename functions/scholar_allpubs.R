
#### Retreive publication details for a specified list of scholars ####
scholar_allpubs = function(author_list){
  library(dplyr)
  library(scholar)
  
  add_etals = function(x){
    split_authors <- stringr::str_split(x, ',')[[1]]
    if(length(split_authors) > 3){
      output <- paste(trimws(split_authors[1]), 
                      trimws(split_authors[2]), 
                      trimws(split_authors[3]),
                      'et al',
                      sep = ', ')
    } else{
      output <- x
    }
    output
  }
  
  total_authors = length(author_list)
  pub_list = do.call(rbind, lapply(seq_len(total_authors), function(x){
    publications = scholar::get_publications(author_list[x])
  })) %>%
    dplyr::arrange(-year) %>%
    dplyr::mutate(Journal = tools::toTitleCase(tolower(as.character(journal))),
                  Title = tools::toTitleCase(tolower(as.character(title))),
                  Year = year) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(Authors = add_etals(author)) %>%
    dplyr::select(Year, Title, Journal, Authors) %>%
    dplyr::distinct()

}
