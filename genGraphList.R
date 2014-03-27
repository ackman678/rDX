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

for(i in 1:nModules) {
cInd=moduleOrder[which(names(moduleOrder)==matchOrder[i])]
if (length(cInd>0)) {
ind2ch=which(moduleOrder==cInd)
moduleColors[ind2ch] = i 
} else { print('module match string not found!') }
}
return(moduleColors)
}

myForceGraph <- function(g, lo=NULL) {
## g, graph object from igraph
## lo, string. Graph layout type. e.g lo <-'layout.fruchterman.reingold', 'layout.kamada.kawai', 'layout.lgl'

if (is.null(lo)) { lo='layout.fruchterman.reingold' }
quartz();
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


