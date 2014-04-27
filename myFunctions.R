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


printSummaryPlus <- function(data=NULL, measVar, groupVars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE, makePlots=TRUE, groupInteraction=FALSE) {  
	
	if (groupInteraction==TRUE) { collapStr="*" } else { collapStr="+" }
	data4=NULL
	
	data2 <- printSummary(data, measVar,groupVars,na.rm,conf.interval, .drop)     
	fmla <- as.formula(paste(paste(measVar, collapse= "+"), "~", paste(groupVars, collapse= collapStr)))
	lm.fit <- lm(fmla,data)
	
	if (makePlots==TRUE) {
		dev.new();
		opar <- par(mfrow=c(2,2))
		plot(lm.fit)
		title(main=as.character(as.expression(fmla)))
		par(opar)
	}
	
	if (length(groupVars)==1) {	
		if (length(levels(data[[groupVars]]))<3) {
			data3 <- t.test(fmla,data=data)
		} else {
			data3 <- anova(lm.fit)			
			data4 <- with(data,pairwise.t.test(eval(parse(text=measVar)),eval(parse(text=paste0(groupVars, collapse=":")))))
		}
	} else {
		data3 <- anova(lm.fit)
		data4 <- with(data,pairwise.t.test(eval(parse(text=measVar)),eval(parse(text=paste0(groupVars, collapse=":")))))
	}
	print(data2)
	print(data3)
	if (!is.null(data4)) { print(data4) }
}


#catData
catData <- function(subfolders,path,pattern='*dCorr.txt') {
  #Read in and concatenate datasets to dataframe
  #dCorr <- catData(filenames,path,pattern='*dCorr.txt')
  
  for(i in 1:length(subfolders)) {
    pathname <- file.path(path,subfolders[i])
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

#matlab jet colors lut
jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))

