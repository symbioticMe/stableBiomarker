#main script: for each trait

#read the arguments from the command line
write.table(df_out, file=args[2], row.names=FALSE)

#!/usr/bin/env Rscript
library("optparse")

option_list = list(
  make_option(c("-f", "--file"), type="character", default=NULL, 
              help="dataset file name", metavar="character"),
  make_option(c("-r", "--response"), type="character", default=NULL, 
              help="response file name", metavar="character"),
  make_option(c("-c", "--config"), type="character", default=NULL, 
              help="config file name", metavar="character"),
  make_option(c("-rc", "--response_column"), type="character", default=NULL, 
              help="response column name", metavar="character"),
  make_option(c("-o", "--out"), type="character", default="out.txt", 
              help="output RDS name [default= %default]", metavar="character")
); 

opt_parser = OptionParser(option_list=option_list)
opt = parse_args(opt_parser)

##managing null arguments
if (is.null(opt$file)){
  print_help(opt_parser)
  stop("At least one argument must be supplied (input file).n", call.=FALSE)
}


#load the data
response.column = opt$response.column
data = read.data(data.file = opt$file, response.file = opt$response, 
                 response.column = response.column)
#read the configuration file

config = readConfig(opt$config)
method = config$method
fsMethod = config$fs_method
if (is.null(fsMethod)){
  fs_method = 'TopN'
  fs_config = list(topN = min(100, .05*ncol(data)))
} else {
  fs_config = check.fs.config(fsMethod, data)
}

repeats = config$repeats
resamplingConfig = list(method = config$resamplingMethod)

if (resamplingMethod == 'CV'){
  resamplingConfig$folds = config$CV_folds
} else {
  if (resamplingMethod == 'Fraction'){
    resamplingConfig$p = config$fraction
  }
}

#train the model and get the list of features with(out) the 


result = train.main(data, response = response.column,  method = config$method, 
                    fs.method = config$fs_method, 
                    fs.config = ifelse(fs.method == 'TopN', top.n = 100, NULL),
                     = repeats, 
                    resamplingConfig = resamplingConfig)

#save the objects: 1. Feature lists; 2.model performances
saveRDS(result$importance)
saveRDS(result$train)