## Code for Starkweather KE, Reynolds AZ, Zohora F, Alam N. (2022) Shodagor women traders cooperate across domains of work and childcare to solve an adaptive problem. Philosophical Transactions of the Royal Society B.


options(stringsAsFactors=FALSE)


### LOAD DATA AND MAKE GRAPH OBJECTS

require(igraph)

nodelist <- read.csv("Nodelist_deidentified.csv")
cc_edges <- read.csv("ChildcareEdges_deidentified.csv")
ww_edges <- read.csv("WorkEdges_deidentified.csv")
r_edges <- read.csv("RelatednessEdges_deidentified.csv")


cc <- make_empty_graph() + vertices(nodelist$ID, hh=nodelist$HH, color=nodelist$Color, shape=nodelist$Shape)
for(i in 1:nrow(cc_edges)) {
	cc <- cc + edge(cc_edges[i,1], cc_edges[i,2])
}

ww <- make_empty_graph() + vertices(nodelist$ID, hh=nodelist$HH, color=nodelist$Color, shape=nodelist$Shape)
for(i in 1:nrow(ww_edges)) {
	ww <- ww + edge(ww_edges[i,1], ww_edges[i,2])
}
ww_for_model <- as.undirected(ww, mode="collapse")

r <- make_empty_graph() + vertices(nodelist$ID, hh=nodelist$HH, color=nodelist$Color, shape=nodelist$Shape)
for(i in 1:nrow(r_edges)) {
	r <- r + edge(r_edges$Source[i], r_edges[i,2], weight=r_edges[i,3])
}
r <- as.undirected(r, mode="collapse")



### FIGURE 2

cc_plot <- delete.vertices(cc, which(degree(cc)==0))
ww_plot <- delete.vertices(ww, which(degree(ww)==0))

pdf("Network Graphs.pdf", width=11, height=6)
par(mfrow=c(1, 2))
plot(ww_plot, vertex.size=5, vertex.color=V(ww_plot)$color, vertex.shape=V(ww_plot)$shape, vertex.label=NA, edge.color="black", edge.width=1, edge.arrow.size=.25, main="Work")
plot(cc_plot, vertex.size=5, vertex.color=V(cc_plot)$color, vertex.shape=V(cc_plot)$shape, vertex.label=NA, edge.color="black", edge.width=1, edge.arrow.size=.25, main="Childcare")
dev.off()




### ERGM

require(intergraph)
require(statnet)

cc_net <- asNetwork(simplify(cc))
ww_net <- asNetwork(simplify(ww_for_model))
rel_net <- asNetwork(simplify(r))

rel_adj <- get.adjacency(r, attr="weight")
ww_adj <- get.adjacency(ww)
ww_rel <- as.matrix(rel_adj) * as.matrix(ww_adj)
interaction <- graph_from_adjacency_matrix(ww_rel, mode="undirected", weighted=TRUE)
int_net <- asNetwork(interaction)

ergm <- ergm(cc_net ~ edges + edgecov(rel_net, "weight") + edgecov(ww_net) + edgecov(int_net) + mutual + nodematch('hh') + idegree(0) + odegree(0))
summary(ergm)

require(broom)
tidy(ergm, conf.int=TRUE, conf.level=0.95)







### FIGURE 3

require(ggplot2)

work <- read.csv("EgoWorkTies_deidentified.csv")

A <- aggregate(work, by=list(work$NumAlter, work$Egojob), FUN=mean, na.rm=T)
A$GenderWork <- NA
A$GenderWork[A$Group.2=="Fish"] <- "Women's fishing ties"
A$GenderWork[A$Group.2=="Trade"] <- "Women's trading ties"
df1 <- data.frame(Work=A$GenderWork, Alters=A$NumAlter, AvgCount=A$NumKin, Kin="Kin")
df2 <- data.frame(Work=A$GenderWork, Alters=A$NumAlter, AvgCount=A$NumNonKin, Kin="Non-kin")
df <- rbind(df1, df2)

ggplot(df, aes(x = Alters, fill = Kin,
                 y = ifelse(test = Kin == "Kin",
                            yes = -AvgCount, no = AvgCount))) + 
	geom_bar(stat = "identity") +
	scale_y_continuous(breaks=seq(-6,6), labels = abs, limits = 6 * c(-1,1)) +
	labs(x = "Number of Alters", y = "Split of Alters that are Kin/Non-kin") + 
	scale_colour_manual(values = c("darkorchid", "chartreuse3"), aesthetics = c("colour", "fill")) +
	scale_x_continuous(breaks=seq(1,7)) +
	coord_flip() +
	facet_wrap(~ Work) +
	theme_bw() + 
	theme(legend.position = "right",
		legend.title=element_text(color="black", size=12),
		legend.text=element_text(size=12),
		plot.margin=unit(c(.5,.5,.5,.5), "cm"),
		panel.grid.major = element_blank(),
		panel.grid.minor = element_blank(),
		axis.title.y=element_text(size=rel(1), margin=margin(0,15,0,0)),
		axis.title.x=element_text(size=rel(1), margin=margin(15,0,0,0)),
		axis.text.x=element_text(color="black", size=12),
		axis.text.y=element_text(color="black", size=12))




### FIGURE 4

require(ggplot2)

childcare <- read.csv("EgoChildcareTies_deidentified.csv")

A <- aggregate(childcare, by=list(childcare$NumAlter, childcare$Egojob), FUN=mean, na.rm=T)
A$GenderChildcare <- NA
A$Genderchildcare[A$Group.2=="fish"] <- "Women who fish"
A$Genderchildcare[A$Group.2=="trade"] <- "Women who trade"
df1 <- data.frame(childcare=A$Genderchildcare, Alters=A$NumAlter, AvgCount=A$NumKin, Kin="Kin")
df2 <- data.frame(childcare=A$Genderchildcare, Alters=A$NumAlter, AvgCount=A$NumNonKin, Kin="Non-kin")
df <- rbind(df1, df2)


ggplot(df, aes(x = Alters, fill = Kin,
                 y = ifelse(test = Kin == "Kin",
                            yes = -AvgCount, no = AvgCount))) + 
	geom_bar(stat = "identity") +
	scale_y_continuous(breaks=seq(-6,6), labels = abs, limits = 6 * c(-1,1)) +
	labs(x = "Number of Childcare Ties", y = "Split of Alters that are Kin/Non-kin") + 
	scale_colour_manual(values = c("darkcyan", "tomato"), aesthetics = c("colour", "fill")) +
	scale_x_continuous(breaks=seq(1,7)) +
	coord_flip() +
	facet_wrap(~ childcare) +
	theme_bw() + 
	theme(legend.position = "right",
		legend.title=element_text(color="black", size=12),
		legend.text=element_text(size=12),
		plot.margin=unit(c(.5,.5,.5,.5), "cm"),
		panel.grid.major = element_blank(),
		panel.grid.minor = element_blank(),
		axis.title.y=element_text(size=rel(1), margin=margin(0,15,0,0)),
		axis.title.x=element_text(size=rel(1), margin=margin(15,0,0,0)),
		axis.text.x=element_text(color="black", size=12),
		axis.text.y=element_text(color="black", size=12))

