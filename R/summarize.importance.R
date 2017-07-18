#' Summarize importance for the case of optimizing importance across algorithm parameters (draft from old script)
#'
#' @param result
#'
#' @return
#' @export
#'
#' @examples
summarize_importance <- function(result){
  resamples <- do.call(rbind, unlist(lapply(result, function(x) lapply(x, function(y) y$resamples)), recursive = F))
  importance.list <- lapply(result, function(x) do.call(rbind, lapply(x, function(y) y$importances)))
  importance = importance.list %>%
    Reduce(function(dtf1,dtf2) left_join(dtf1,dtf2,by = c("features",colnames(info$loop))), .)

  stability <- importance %>%
    group_by_(.dots = lapply(gsub("^\\.", "", colnames(info$loop)), as.symbol)) %>%
    do(data.frame(Cor = t(cor(.[,-which(names(.) %in% c(gsub("^\\.", "", colnames(info$loop)), 'features'))],
                              .[,-which(names(.) %in% c(gsub("^\\.", "", colnames(info$loop)), 'features'))])),
                  factors = names(.)[-which(names(.) %in% c(gsub("^\\.", "", colnames(info$loop)), 'features'))])) %>%
    melt(id.vars = c(gsub("^\\.", "", colnames(info$loop)),'factors'),
         variable.name = 'factor2',
         value.name = 'correlation') %>%
    transform(factor2 = str_replace(factor2, "^Cor\\.", ""))
  stability = stability %>%
    mutate(dupl = duplicated(t(apply(stability %>%
                                       select(-correlation), 1, sort)))) %>%
    filter(!dupl) %>% select(-dupl) %>%
    filter(factors != factor2)

  stability_out <- stability %>%
    group_by_(.dots = lapply(gsub("^\\.", "", colnames(info$loop)), as.symbol)) %>%
    summarise(mean_stability = mean(correlation),
              sd_stability = sd(correlation))

}
