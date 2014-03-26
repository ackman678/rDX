genGraphList <- function(df,groupVar,groupVals,rthresh=-1,makePlots=FALSE, lo=NULL) {
#Generate list of summary graphs based on grouping factor.
#Plot summary graph and detect modules based on adjacency datafame from a correlation matrix.
#James B. Ackman 2014-03-20 10:07:29
#gList <- genGraphList(d3,"age.g",ageIdx)

edgelistSummary <- ddply(df, c("node1","node2",groupVar), summarize,
rvalue.mean = mean(rvalue),
rvalue.sd = sd(rvalue), 
N = length(rvalue), 
rvalue.sem = rvalue.sd/sqrt(N))
colnames(edgelistSummary)[colnames(edgelistSummary) == 'rvalue.mean'] <- 'rvalue'

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


