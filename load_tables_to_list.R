## FUN: function to load data from multiple files and store them in a list of tables

#Parameters:
#inputFiles			Vector of input file names or path to (a single) directory containing input
#					files. If inputFiles is directory, all files matching 'pattern' will be
#					processed. If several diecrories are given, only the first will be considered. 
#idPos 				Position of id tag in filenames of files to load. The id tags are used as
#					names for the ouput list elements. Postions are defined by separators 
#					specified by idSplit. e.g. for 'peaks-h3k4me3-homer.bed' with 
#					idSplit = "-", the idPos for the 'h3k4me3' tag would be 2.
#idSplit			Specifies strings used to split the filename into elements to obtain id.
#					Default is "\\.|-|_", for further info see help for split in strsplit().
#pattern 			Optional pattern (string) for filtering of files in inputFiles (if directory,
#					e.g. file ext). For further info see help for list.files().
#storageType		String specifying the object type for storing the data in the list, can be
#					"matrix" or "data.frame" (default).
#excludeCols		Vector containing names or numbers (not both mixed together!) of columns 
#					to be excluded from input files.
#row.names 			Specifies the number or name of the column containing the row names. If not
#					specified nod row names are set. For further info see help for read.table().
#header				Logical, specifies whether the files contain a first header row.
#verbose 			Logical, specifies whether feedback should be printed (default = TRUE).
#stringsAsFactors 	Logical, specifies whether character vectors should be converted to
#					factors (default = FALSE). For further info see help for read.table().

load.tables.to.list <- function(inputFiles, idPos, idSplit = "\\.|-|_", pattern = NULL,
								storageType = "data.frame", excludeCols = NULL, row.names = NULL,
								header = TRUE, verbose = TRUE, stringsAsFactors = FALSE){

	#if inputFiles is a directory, create list with names of all files in it using pattern.
	if(file.info(inputFiles[1])$isdir){
		inputFiles <- list.files(path=inputFiles[1], pattern=pattern, full.names=TRUE)
	}
	#initialize empty list
	output.list <- list()
	#load and add every file to output.list
	for(i in inputFiles){
		#report feedback if specified
		if(verbose == TRUE){
			cat(paste("\tloading: ", basename(i), "...\n", sep=""))
		}
		#read first line to get info on columns (number, names)
		cols <- scan(file=i, nlines=1, what="raw", quiet=TRUE)
		#create colClasses for all columns and set to NA by default
		colClasses <- structure(rep(x=NA, times=length(cols)), names=cols)
		#set classes of columns to be excluded to "NULL"
		colClasses[excludeCols] <- "NULL"
		#load file i
		inputData <- read.table(file=i, header=header, row.names=row.names,
								stringsAsFactors=stringsAsFactors, colClasses=colClasses)
		#transform inputData to matrix if specified
		if(storageType == "matrix"){
			inputData <- as.matrix(inputData)
		}else if(storageType != "data.frame"){
			stop("Unknown storageType specified!")
		}
		#get "data id" from filename
		i.split <- strsplit(x=basename(i), split=idSplit)
		id <- i.split[[1]][idPos]
		#add to output list
		output.list[[id]] <- inputData
	}
	return(output.list)
}