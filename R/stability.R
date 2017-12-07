#' Estimate the stability across resamples
#'
#' @param importance data.frame or list of feature importances
#' @param method
#'
#' @return list containing:
#' \item{stab_mean}{ mean of pairwise stabilities}
#' \item{stab_sd}{standard deviation of pairwise stabilities}
#' @export
#'@importFrom OmicsMarkeR kuncheva sorensen ochiai jaccard
#'@importFrom OmicsMarkeR canberra_stability canberra spearman
#'@importFrom stats complete.cases cor sd
#'
#' @examples
estim_stability <- function(importance, method = 'cor_spearman', num.features = NULL){

  stability.pairwise = calc_pairwise_stability(importance, method = method,
                                               num.features = num.features)
  stability_mean = mean(stability.pairwise)
  stability_sd = sd(stability.pairwise)
  return(list(stab_mean = stability_mean,
              stab_sd = stability_sd))
}

#' Calculate stability for the importance results
#'
#' @param x list, data.frame or matrix of values to be tested for similarity
#' @param method one of \code{'kuncheva'}, \code{'sorensen'}, \code{'ochiai'}, \code{'jaccard'}
#'
#'
#' @return numeric vector of similarities
#'
#' @examples
calc_pairwise_stability <- function(x, method, num.features = NULL){
  set_methods = c('kuncheva', 'sorensen', 'ochiai', 'jaccard')
  numeric_methods = c('canberra_stability',
          'canberra_spearman','cor_spearman','cor_pearson')
  if (method == 'correlation') {method = 'cor_pearson'}
  if (method %in% set_methods){
    if(!all(sapply(x, class) == 'character')){
      if (all(!is.null(sapply(x, names)))){
        x = lapply(x, names)
      } else {
        stop('for set methods, a character list of selected features should be provided!')
      }
    }
    ncx <- length(x)
    r <- matrix(0, nrow = ncx, ncol = ncx)
    for (i in seq_len(ncx)){
      for (j in seq_len(i)){
        x2 = x[[i]]
        y2 = x[[j]]
        r[i, j] <- switch(method,
                          kuncheva = kuncheva(x2, y2, num.features = num.features),
                          sorensen = sorensen(x2, y2),
                          jaccard  = jaccard(x2, y2),
                          ochiai   = ochiai(x2, y2),
                          all = all_metrics(x2, y2, type = 'set'))
      }
    }
  } else if (method %in% numeric_methods &
             all(sapply(x, function(y) class(y[[1]]) == 'numeric'))) {

    x = do.call(cbind.data.frame, x)
    names(x) = names(x)
    ncx <- ncol(x)
    r <- matrix(0, nrow = ncx, ncol = ncx)
    for (i in seq_len(ncx)) {
      for (j in seq_len(i)) {
        x2 <- x[, i]
        y2 <- x[, j]
        ok <- complete.cases(x2, y2)
        r[i, j] <- switch(method,
                 cor_spearman = cor(x2, y2, method = 'spearman'),
                 cor_pearson = cor(x2, y2, method = 'pearson'),
                 canberra = canberra(x2, y2), 
                 all = all_metrics(x2, y2, type = 'numeric'))
      }
    }
    #check if x is numeric
  } else {
    print(paste('method = ', method))
    flush.console()
    print(class(x[[1]]))
    flush.console()
    stop("invalid method of stability inference!")
  }
  return(r[lower.tri(r)])
}

all_metrics <- function(x, y, type = 'set', num.features = NULL){
  if (type == 'set'){
    kuncheva = kuncheva(x, y, num.features = num.features)
    sorensen = sorensen(x, y)
    jaccard  = jaccard(x, y)
    ochiai   = ochiai(x, y)
    out <- list(kuncheva = kuncheva,
                sorensen = sorensen,
                jaccard = jaccard,
                ochiai = ochiai)
  } else {
    if (type == 'numeric'){
      cor_spearman = cor(x, y, method = 'spearman')
      cor_pearson = cor(x, y, method = 'pearson')
      canberra = canberra(x, y)
      out = list(cor_spearman = cor_spearman,
                 cor_pearson = cor_pearson,
                 canberra = canberra)
    }
  }
  return(out)
}