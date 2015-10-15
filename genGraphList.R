genGraphList <- function(df, groupVar, groupVals=NULL, rthresh=-1, makePlots=FALSE, lo=NULL, matchOrder=NULL) {
#Generate list of summary graphs based on grouping factor.
#Plot summary graph and detect modules based on adjacency datafame from a correlation matrix.
#James B. Ackman 2014-03-20 10:07:29
#gList <- genGraphList(d3,"age.g",ageIdx)
## df, dataframe containing adjacency data with node1 and node2 and rvalue columns
## groupVar, string name of grouping factor column
## groupVals, character vector of grouping factor levels
## rthresh, numeric vector of threshold for rvalue to subset df
## makePlots, logical of whether to print and save Force directed Graphs of connectivity
## lo, character name of layout to use for makePlots
## matchOrder, character vector of names of nodes to sort module coloring into TODO: make auto based on n returned communities

##if grouping values (groupVals)  for the grouping variable (groupVar) are passed, use data summary function, otherwise use no summary and just isolate data by the no. of levels for groupVar
if (!is.null(groupVals)) {
edgelistSummary <- ddply(df, c("node1","node2",groupVar), summarize,
rvalue.mean = mean(rvalue),
rvalue.sd = sd(rvalue), 
N = length(rvalue), 
rvalue.sem = rvalue.sd/sqrt(N))
colnames(edgelistSummary)[colnames(edgelistSummary) == 'rvalue.mean'] <- 'rvalue'
} else {
groupVals <- levels(df[[groupVar]])
edgelistSummary <- df
}

k = 0
for(i in 1:length(groupVals)) {
edgelist <- edgelistSummary
edgelist <- edgelist[edgelist[[groupVar]] == groupVals[i], ]  #Index directly into df instead of using subset() so grouping variable can be passed

for(j in 1:length(rthresh)) {
k=k+1
rthr <- rthresh[j]
edgelist<-subset(edgelist,rvalue > rthr)
g <- graph.data.frame(edgelist, directed=FALSE)
E(g)$weight <- E(g)$rvalue
E(g)$width <- 1
E(g)[ weight >= 0.3 ]$width <- 3
E(g)[ weight >= 0.5 ]$width <- 5
E(g)$weight[E(g)$weight<0] <- 0
fastgreedyCom<-fastgreedy.community(g,weights=E(g)$weight)
V(g)$color <- fastgreedyCom$membership

if (!is.null(matchOrder)) {
moduleColors <- myModuleColors(g,matchOrder)
V(g)$color <- moduleColors }

if(makePlots) { 
myForceGraph(g, lo)
title(paste(groupVar,groupVals[i],',fg com def,', lo, 'r>', rthr))

dateStr=format(Sys.time(),"%y%m%d-%H%M%S")
quartz.save(file=paste(dateStr, "-", groupVar, groupVals[i],  ".png",sep=""), type = "png", dpi=150)
quartz.save(file=paste(dateStr, "-", groupVar, groupVals[i],  ".pdf",sep=""), type = "pdf")

print(paste('#',groupVar,groupVals[i],'r>',rthr,'==============================================='))
printGraphInfo(g, fastgreedyCom)
}

#cat list together
if (k==1) {
	gList <- list(g)
	names(gList)[k] <- paste(groupVals[i], '_', rthr, sep="")
} else {
	gList[[k]] <- g
	names(gList)[k] <- paste(groupVals[i], '_', rthr, sep="")
}
}
}
return(gList)
}


myModuleColors <- function(g, matchOrder=NULL) {
#moduleColors <- myModuleColors(g)
#g, igraph graph object
#matchOrder, character vector, module name match order for module color ordering

if (is.null(matchOrder)) {
matchOrder <- c("T.R", "PPC.R", "M1.R", "V1.R", "LS.R")
}

nModules <- max(V(g)$color)
moduleOrder <- V(g)$color
names(moduleOrder) <- V(g)$name
moduleColors <- moduleOrder

mNamesList <- list()
for(i in 1:nModules) {
mNamesList[[i]] <- names(which(moduleOrder == i))
}

j=length(matchOrder)
for (i in 1:length(mNamesList)) {
for (k in 1:length(matchOrder)) {
nInd <- mNamesList[[i]][mNamesList[[i]]==matchOrder[k]]
if (length(nInd>0)) {
# print(paste(i, 'was ', nInd, 'but now will be ', k))
break 
}
}

if (length(nInd>0)) {
print(paste(nInd, 'was ', i, 'but now will be ', k, 'as will ', paste0(mNamesList[[i]],collapse=" ")))
moduleColors[which(!is.na(match(names(moduleOrder),mNamesList[[i]])))] <- k
} else {
j=j+1
print(paste(paste0(mNamesList[[i]],collapse=" "), 'was ', i, 'but now will be ', j))
moduleColors[which(!is.na(match(names(moduleOrder),mNamesList[[i]])))] <- j
}
}
return(moduleColors)
}

myForceGraph <- function(g, lo=NULL, dev=NULL) {
## g, graph object from igraph
## lo, string. Graph layout type. e.g lo <-'layout.fruchterman.reingold', 'layout.kamada.kawai', 'layout.lgl', 'layout.auto'
## dev, numeric. An existing device number if plotting should be passed to an open device window.

if (is.null(lo)) { lo='layout.fruchterman.reingold' }
if (is.null(dev)) { 
dev.new() 
} else {
dev.set(dev)
}

# palette(rainbow(max(V(g)$color),alpha=0.5))
mypalette <- adjustcolor(brewer.pal(max(V(g)$color),"Set1"),0.6)
palette(mypalette)
plot(g, layout=eval(parse(text=lo)), edge.width=E(g)$width, edge.color="black", vertex.label.color="black", vertex.label.family="sans", vertex.label.font=2, vertex.label.cex=0.7)
# palette("default")
}

printGraphInfo <-  function(g, communityStructure) {
	print("community:")
	print(communityStructure)
	
	print("degree:")
	print(degree(g))
	
	print("mean degree:")
	print(mean(degree(g)))
	
	print("deg dist:")
	print(degree.distribution(g))
	
	print("deg dist cumulative:")
	print(degree.distribution(g,cumulative = TRUE))
	
	print("avg path len:")
	print(average.path.length(g))
	
	print("diameter:")
	print(diameter(g))
	
	print("hub score:")
	print(hub.score(g)$vector)
	
	print("global clustering coeff:")
	print(transitivity(g))
	
	print("centralization deg:")
	print(centralization.degree(g))
	
	print("is.connected:")
	print(is.connected(g))
	
	print("no.clusters:")
	print(no.clusters(g))
	
	print("clusters:")
	print(clusters(g))
}


genNodeGraphDf <- function(gList, nshuffle=1000) {
#Generate node:graph (filename) dataframe based on a gList returned from genGraphList
#James B. Ackman 2015-04-21 17:10:59
#dftmp <- genNodeGraphDf(gList)
## gList, list of graphs returned from genGraphlist e.g. gList <- genGraphList(d3,"filename",c(0.1,0.15))
## nshuffle, no. of shuffles of the degree sequence for the random graph comparisons

## Make node:movie based dataset

dftmp <- data.frame(node=character(), clusterCoeff=character(), degree=character(), nodeStrength=character(), btwCentr=character(), btwCentrN=character(), eigCentr=character(), pageRank=character(), gName=character(), age.g=character(), genotype=character(), animal=character(), manip=character(), manip.g=character(), filename=character(), modularity=character(), pathLength=character(), diam=character(), clusterCoeffGlobal=character(),  clusterCoeffGlobalNorm=character(), pathLengthNorm=character(), smallworldness=character(), plFitAlpha=character(), plFitAlphaRand=character(), nclusters=character(), module=character()) #make empty data.frame    
for(i in 1:length(gList)) {    
    gName <- names(gList)[[i]]    
    mat <- get.adjacency(gList[[gName]],attr="rvalue")    
    m2 <- as.matrix(mat)  #default R matrix     

    eigCentr <- evcent(gList[[gName]])$vector
    pageRank <- page.rank(gList[[gName]])$vector    
    btwCentr <- betweenness(gList[[gName]])    
    n = length(V(gList[[gName]])$name) #number of vertices
    btwCentrN <- btwCentr/(n*(n-1))  #normalized to number of possible vertex connections as in [#Sporns:2005]

    m <- data.frame( node=V(gList[[gName]])$name, clusterCoeff=transitivity(gList[[gName]],type="local"), degree=degree(gList[[gName]]), nodeStrength=colSums(m2), btwCentr=btwCentr, eigCentr=eigCentr, pageRank=pageRank, btwCentrN=btwCentrN, module=V(gList[[gName]])$color )    
    m$gName <- as.factor(gName)    
    m$age.g <- as.factor(levels(factor(E(gList[[gName]])$age.g)))    
    m$filename <- as.factor(levels(factor(E(gList[[gName]])$filename)))  
    m$genotype <- as.factor(levels(factor(E(gList[[gName]])$genotype)))  
    m$manip <- as.factor(levels(factor(E(gList[[gName]])$manip)))  
    m$manip.g <- as.factor(levels(factor(E(gList[[gName]])$manip.g)))  
    m$animal <- as.factor(levels(factor(E(gList[[gName]])$animal)))  

    fgCom<-fastgreedy.community(gList[[gName]],weights=E(gList[[gName]])$weight)  
	m$modularity <- modularity(fgCom)   
	m$nclusters <- length(fgCom)

    print(gName)

    diam <- diameter(gList[[gName]], weights=NA, unconnected=TRUE) #unconnected = TRUE (default) or FALSE  
    lg <- average.path.length(gList[[gName]], unconnected=TRUE)  #unconnected = TRUE (default) or FALSE
    cg <- transitivity(gList[[gName]], type="global", isolates="NaN") #isolates = "NaN" (default) or "zero"
    deg <- degree(gList[[gName]])
    fit1 <- power.law.fit(deg,8)  
    m$plFitAlpha <- fit1$alpha
    print(deg)
    print(is.connected(gList[[gName]]))
    res = rep(0,nshuffle)
    cNorm = rep(0,nshuffle)
    lNorm = rep(0,nshuffle)
    cr = rep(0,nshuffle)
    lr = rep(0,nshuffle)
    ar = rep(0,nshuffle)

    if(is.connected(gList[[gName]])) {
        for(j in 1:nshuffle) {
            g = degree.sequence.game(deg, method="vl")  #This randomizes the network, keeping the degree sequence (and number of nodes) intact.
            lr[j] <- average.path.length(g)  
            cr[j] <- transitivity(g, type="global", isolates="NaN") #isolates = "NaN" (default) or "zero"
            fit1 <- power.law.fit(degree(g),8)
            ar[j] <- fit1$alpha
            cNorm[j] <- (cg/cr[j])
            lNorm[j] <- (lg/lr[j]) 
            res[j] <- cNorm[j]/lNorm[j] 
        }
        m$smallworldness = mean(res[res!=Inf])
        m$clusterCoeffGlobalNorm = mean(cNorm[cNorm!=Inf])
        m$pathLengthNorm = mean(lNorm)
        m$clusterCoeffGlobalRand = mean(cr[cr!=Inf])
        m$pathLengthRand = mean(lr)
        m$plFitAlphaRand <- mean(ar)
    } else {
        m$smallworldness = NA
        m$clusterCoeffGlobalNorm = NA
        m$pathLengthNorm = NA
        m$clusterCoeffGlobalRand = NA
        m$pathLengthRand = NA
        m$plFitAlphaRand <- NA
        
        #diam <- diam + length(which( V(gList[[gName]]) %in% unique(as.vector(get.edgelist(gList[[gName]]))) == FALSE))
        # comps <- decompose.graph(gList[[gName]])
        # cDiams <- sapply(comps, function(x) {diameter(x,weights=NA)})
        # cDiams[cDiams == 0] <- 1
        # diam <- sum(cDiams)
        # clg <- sapply(comps, average.path.length)
        # clg[is.na(clg)] <- 1
        # lg <- sum(clg)
    }
	m$diam <- diam
    m$pathLength <- lg
    m$clusterCoeffGlobal <- cg
    dftmp <- rbind(dftmp,m)    
    #print(paste("smallworldness: ", mean(res[res!=Inf])))
}    

    dftmp$r <- as.factor(gsub("[0-9A-Za-z_]+.tif_","",dftmp$gName))    
    rownames(dftmp) <- 1:nrow(dftmp)  

    dftmp$nodeT <- dftmp$node  
    levels(dftmp$nodeT) <- gsub("\\.[LR]","",levels(dftmp$nodeT)) 

return(dftmp)
}
