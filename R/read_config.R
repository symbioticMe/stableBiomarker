
read_config <- function(file){
  fc = file(file, encoding = "UTF-8")
  config <- strsplit(readLines(fc), '\n')
  #remove empty lines
  config = config[sapply(config, length) > 0]
  #remove comments
  config <- config[!sapply(config, function(x) startsWith(x[1],'#'))]
  config <- lapply(config, function(string) unlist(strsplit(string, ' = ')))
  if (!all(lengths(config) == 2)) {
    print(head(config[lengths(config) != 2]))
    flush.console()
    stop('Check the config file')
  }

  names(config) <- sapply(config, function(entry) entry[1])
  config <- lapply(config, function(entry) entry[2])
}
