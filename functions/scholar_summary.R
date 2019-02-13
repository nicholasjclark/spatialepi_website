#### Plot citation metrics data from GoogleScholar User profiles ####
#'@param user Character string representing Google Scholar user profile ID
#'@return A \code{ggplot2} object showing a graph of citation metrics, with a timestamp
#' 
scholar_summary = function(author_list){
library(scholar)
library(dplyr)
  
citations <- do.call(rbind, lapply(seq_len(length(author_list)), function(x){
  cit <- scholar::get_citation_history(author_list[x])
})) %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(cites = sum(cites))

tot_articles <- sum(unlist(lapply(seq_len(length(author_list)), function(x){
  tot <- scholar::get_num_articles(author_list[x])
})))

ggplot2:: ggplot(citations, ggplot2::aes(x = year, y = cites)) +
  ggplot2::geom_bar(stat = 'identity') +
  ggplot2::theme_bw() +
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
                 axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}