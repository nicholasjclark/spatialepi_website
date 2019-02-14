
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

#### Plot top journals ####
scholar_topjournals = function(author_list){
  all_pubs <- scholar_allpubs(author_list)
  
  total_journals = all_pubs %>%
    dplyr::group_by(Journal) %>%
    dplyr::summarise(Total = n()) %>%
    dplyr::filter(!Journal == '') %>% nrow()
  
  top_journals = all_pubs %>%
    dplyr::group_by(Journal) %>%
    dplyr::summarise(Total = n()) %>%
    dplyr::filter(!Journal == '') %>%
    dplyr::filter(Total > 3)
  top_journals$Journal <- factor(top_journals$Journal, 
                                 levels = top_journals$Journal[order(top_journals$Total)])
  if(nrow(top_journals) > 7){
    top_journals <- top_journals[1:7, ]
  }

  ggplot2:: ggplot(top_journals, ggplot2::aes(x = Journal, y = Total)) +
    ggplot2::geom_bar(stat = 'identity') +
    ggplot2::theme_bw() +
    ggplot2::xlab('Journal') +
    ggplot2::ylab('Total Papers\nper Journal')+
    ggplot2::annotate('text', label = 'SpatialEpiLab Top Journals',
                      x = -Inf, y = Inf, hjust = -0.03, vjust = 1.5, size = 4) +
    ggplot2::annotate('text', label = format(Sys.time(), "%Y-%m-%d"),
                      x = -Inf, y = Inf, vjust = 4.5, hjust = -0.1, size = 3) +
    ggplot2::annotate("text", x = -Inf, y = Inf, vjust = 6, hjust = -0.04, 
                      label = paste('Total unique journals = ', total_journals),
                      size = 3) +
    ggplot2::scale_y_continuous(breaks = c(0,3,6,9,12,15,18)) +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(), 
                   panel.grid.minor = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_text(angle = 55, hjust = 1))
}

