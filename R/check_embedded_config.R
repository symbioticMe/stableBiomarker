check_embedded_fs <- function(fs.config, config){
  filter_config_params = c('fs_scoring', 'fs_threshold', 'scoring', 'threshold')
  #wrapper_config_params = c()
  if (length(names(fs.config)) > 1){
    warning(paste(paste(names(fs.config), collapse = ', '), 
                  'are the parameters passed for feature selection configuration;', 
                  'for embedded methods these will not be used'))
  }
  if (length(intersect(names(fs.config), filter_config_params)) > 0 |
      sum(grepl('^[fs_scoring|fs_threshold]', names(config))) > 0) {
    warning('these options are valid for filter methods, not embedded, check your configuration')
  }
}