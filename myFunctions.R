#ed
# View large data frame using without printing the data to command line
ed <- function(x) invisible(edit(x))


#printSummary
# A better print summary using plyr package 2014-03-19 10:37:09
printSummary <- function(data=NULL, measVar, groupVars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {  
	#Inputs:
	#data: dataframe, dataset to analyse
	#measVar: character, name of measured variable in data
	#groupVars: character vector, name or concatenated vector of names of grouping factors in data
	#Example: printSummary(df, measVar,groupVars) 
    require(plyr)

    #Handle NAs  
    len <- function (x, na.rm=FALSE) {
        if (na.rm) sum(!is.na(x))
        else       length(x)  
    }

    #Main summary  
    data2 <- ddply(data, groupVars, .drop=.drop,
          .fun = function(xx, col) {
            c(N = len(xx[[col]], na.rm=na.rm),
              sum = sum(xx[[col]], na.rm=na.rm),
              mean = mean(xx[[col]], na.rm=na.rm),
              median = median(xx[[col]], na.rm=na.rm),
              sd = sd(xx[[col]], na.rm=na.rm),
              mad = mad(xx[[col]], na.rm=na.rm, constant=1) 
            )
          },
          measVar
        )

    #Standard error of the mean  
    data2$sem <- data2$sd / sqrt(data2$N)

    #Confidence interval of 95%  
    data2$CI95 = qnorm(0.975)*data2$sem

	#the standard error of the median is valid for normally distributed populations, but can yield wrong results for extremely non-normal distributions.  It is equal to 1.25*s.e.m. where s.e.m = standard error of the mean.
	data2$seMed <- 1.25*data2$sem

    return(data2)  
} 


#catData
catData <- function(filenames,path,pattern='*dCorr.txt') {
  #Read in and concatenate datasets to dataframe
  #dCorr <- catData(filenames,path,pattern='*dCorr.txt')
  
  for(i in 1:length(filenames)) {
    pathname <- file.path(path,filenames[i])
    fnm <- Sys.glob(file.path(pathname, pattern))
    if (i == 1) {
      data <- read.delim(fnm)
    } else {
      df <- read.delim(fnm)
      data <- rbind(data,df)
    }
  }
  return(data)
}

