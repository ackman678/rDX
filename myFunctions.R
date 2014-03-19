#ed==========================================================
# View large data frame using without printing the data to command line
ed <- function(x) invisible(edit(x))

#printSummary================================================
# Print summary stats of a data frame variable aggregated by n factors. Uses a formula interface
#==Improved printSummary, Fall 2011
printSummary <- function(d.name, myFormula, useLog="FALSE") {
	#changed to use formula input format and add mad (median absolute devation) measure 2011-10-05
	#Currently there should be one dependent variable input to formula, and factors should be additive.
	#e.g.  myFormula <- wavefreq.hz*60 ~ genotype + manip + age
#----------Print out N's and summary stats------------------
	#example: printSummary(m2, useLog="TRUE")
m <- aggregate(myFormula,d.name,mean)
med <- aggregate(myFormula,d.name,median)
std <- aggregate(myFormula,d.name,sd)
len <- aggregate(myFormula,d.name,length)

v<-all.vars(myFormula)
v1<-v[1] #this one works, but if the response variable is multiplied or something, will not work for indexing into std and len below for calculation of sem.
v2<-as.character(myFormula)[2]  #this one works, since the second term is the response variable expression as a character
# d.name[[v1]]
idx<-match(colnames(d.name),v[-1])
idx2<-which(idx > 0)
medSD<-aggregate(d.name[[v1]],by=as.list(d.name[,idx2]),mad,constant=1)

sem <- std[[v2]]/sqrt(len[[v2]]) #SEM
CI95<-qnorm(0.975)*sem
# medSD <- mad(CGDspeed$wavespeed.umpersec,constant=1)
#the standard error of the median is valid for normally distributed populations, but can yield wrong results for extremely non-normal distributions.  It is equal to 1.25*s.e.m. where s.e.m = standard error of the mean.
seMed<-1.25*sem

colnames(m)[ncol(m)] <- "mean"
colnames(med)[ncol(med)] <- "median"
colnames(std)[ncol(std)] <- "sd"
colnames(len)[ncol(len)] <- "N"
colnames(medSD)[ncol(medSD)] <- "mad"
print(cbind(len, m[ncol(m)], std[ncol(std)], sem, CI95, med[ncol(med)], medSD[ncol(medSD)], seMed))
}

# # A better print summary using plyr package 2014-03-19 10:37:09
# # TODO: add in median, mad, seMed
# printSummary <- function(data=NULL, measVar, groupVars=NULL, na.rm=FALSE,
                      # conf.interval=.95, .drop=TRUE) {  
	# #Inputs:
	# #data: dataframe, dataset to analyse
	# #measVar: character, name of measured variable in data
	# #groupVars: character vector, name or concatenated vector of names of grouping factors in data
	# #Example: printSummary(df, measVar,groupVars) 
    # require(plyr)

    # #Handle NAs  
    # len <- function (x, na.rm=FALSE) {
        # if (na.rm) sum(!is.na(x))
        # else       length(x)  
    # }

    # #Main summary  
    # data2 <- ddply(data, groupVars, .drop=.drop,
          # .fun = function(xx, col) {
            # c(N    = len(xx[[col]], na.rm=na.rm),
              # sum = sum   (xx[[col]], na.rm=na.rm),
              # mean = mean   (xx[[col]], na.rm=na.rm),
              # sd   = sd     (xx[[col]], na.rm=na.rm)
            # )
          # },
          # measVar
        # )

    # #Standard error of the mean  
    # data2$se <- data2$sd / sqrt(data2$N)

    # #Confidence interval of 95%  
    # data2$CI95 = qnorm(0.975)*data2$se

    # return(data2)  
# } 
