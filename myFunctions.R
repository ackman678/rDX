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



# ----- Define a function for plotting a matrix ----- #
# from http://www.phaget4.org/R/image_matrix.html

myImagePlot <- function(x, ...){
     min <- min(x)
     max <- max(x)
     yLabels <- rownames(x)
     xLabels <- colnames(x)
     title <-c()
  # check for additional function arguments
  if( length(list(...)) ){
    Lst <- list(...)
    if( !is.null(Lst$zlim) ){
       min <- Lst$zlim[1]
       max <- Lst$zlim[2]
    }
    if( !is.null(Lst$yLabels) ){
       yLabels <- c(Lst$yLabels)
    }
    if( !is.null(Lst$xLabels) ){
       xLabels <- c(Lst$xLabels)
    }
    if( !is.null(Lst$title) ){
       title <- Lst$title
    }
  }
# check for null values
if( is.null(xLabels) ){
   xLabels <- c(1:ncol(x))
}
if( is.null(yLabels) ){
   yLabels <- c(1:nrow(x))
}

layout(matrix(data=c(1,2), nrow=1, ncol=2), widths=c(4,1), heights=c(1,1))

 # Red and green range from 0 to 1 while Blue ranges from 1 to 0
 ColorRamp <- rgb( seq(0,1,length=256),  # Red
                   seq(0,1,length=256),  # Green
                   seq(1,0,length=256))  # Blue
 ColorLevels <- seq(min, max, length=length(ColorRamp))

 # Reverse Y axis
 reverse <- nrow(x) : 1
 yLabels <- yLabels[reverse]
 x <- x[reverse,]

 # Data Map
 par(mar = c(3,5,2.5,2))
 image(1:length(xLabels), 1:length(yLabels), t(x), col=ColorRamp, xlab="",
 ylab="", axes=FALSE, zlim=c(min,max))
 if( !is.null(title) ){
    title(main=title)
 }
axis(BELOW<-1, at=1:length(xLabels), labels=xLabels, cex.axis=0.7)
 axis(LEFT <-2, at=1:length(yLabels), labels=yLabels, las= HORIZONTAL<-1,
 cex.axis=0.7)

 # Color Scale
 par(mar = c(3,2.5,2.5,2))
 image(1, ColorLevels,
      matrix(data=ColorLevels, ncol=length(ColorLevels),nrow=1),
      col=ColorRamp,
      xlab="",ylab="",
      xaxt="n")

 layout(1)
}
# ----- END plot function ----- #




