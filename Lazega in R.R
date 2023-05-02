#https://www.stats.ox.ac.uk/~snijders/siena/Lazega_lawyers_data.htm
# This data set comes from a network study of corporate law partnership that was carried out in a Northeastern US corporate law firm, referred to as SG&R, 1988-1991 in New England. It includes (among others) measurements of networks among the 71 attorneys (partners and associates) of this firm, i.e. their strong-coworker network, advice network, friendship network, and indirect control networks. Various members' attributes are also part of the dataset, including seniority, formal status, office in which they work, gender, lawschool attended, individual performance measurements (hours worked, fees brought in), attitudes concerning various management policy options, etc.
## In terms of other background information that is useful to know:
### (A) The lawyers are doing litigation (Court) and corporate (E.g., contracts, advice) work. There is built-in pressures to grow, intake and assignment rules. There are partners and associates and the there is alot of hierarchy,up or out rule, billing targets.
### Huge incentives to behave opportunistically ; thus the dataset is appropriate for the study of social processes that make cooperation among rival partners possible.

# Redrawing the plotly box plot on interactions by practice, split by gender in R
library(readr)
library(dplyr)
library(plotly)

trends <- read_csv("C:/Users/kayal/OneDrive/Documents/Social Network/Lazega-Atts.csv") 
advice <- read_csv("C:/Users/kayal/OneDrive/Documents/Social Network/Lazega-Advice-Net-B_20-_20Sheet_201+_2_.csv")

# create loop to count frequencies of 1s per ID (try as cannot merge otherwise)
interactions <- numeric()
for (i in 1:(nrow(advice)-1)) {
  str_i <- as.character(i)
  new <- sum(advice[str_i])
  interactions <- c(interactions, new)
}

# create loop to create df
numbers <- 1:70

advice_df <- data.frame(
  ID = numbers,
  Interactions = interactions
)

# join by: `df1.col1 == df2.index`
Advice_interactions <- inner_join(trends, advice_df, by = 'ID')

fig <- plot_ly(x = Advice_interactions$practice, y = Advice_interactions$Interactions, color = factor(Advice_interactions$gender) , type = "box")
fig <- layout(fig, xaxis = list(
  ticktext = c("Litigation", "Corporate"),
  title = "No. of interactions by practice, split by gender (1=man,2=woman)"
))

fig

# The above graph is a box plot of the number of interactions each of node in the network. This is split on two levels -- First, it is split by practice (1 = Litigation and 2 = Corporate). This is then further split by men and women (1=man, 2 =woman).

#The first part of this code does the following things: a) Run the code for Girman-Newman and Random Walk to see what the broad communities under both are, b) chose one to graph
library(igraph)

d=read.csv(file ='C:\\Users\\kayal\\OneDrive\\Documents\\Social Network\\Lazega-Advice-Net-B_20-_20Sheet_201+_2_.csv')
m=as.matrix(d)
dmg=graph.adjacency(m)

### Girvan-Newman partitioning ---

gn = edge.betweenness.community (dmg, directed = TRUE)

head(gn)

###adjust plot margins
par(mar = c(1, 1, 1, 1))

plot(gn, dmg)

# Internal viewing only: Shows a network map of the lazega data as split by communities - i.e., to see how the groups cluster.

memb = data.frame(gn$membership)

summary(memb)

### Random walk partitioning ---

walk = walktrap.community(dmg)

head(walk)

#adjust plot margins
par(mar = c(1, 1, 1, 1))

plot(walk, dmg)

# Internal viewing only: Shows a network map of the lazega data as split by communities using a different way of splitting learnt in class - i.e., to see how the groups cluster.
### Internal: After looking at the ways both methods split the communities, it appears that the walk method is a more sensible way of splitting our lazega data.


walk.memb = data.frame(walk$membership)

## making a dataframe of partitioning ---
dd <-data.frame(name = V(dmg)$name)
cb1 <- cbind(dd, memb, walk.memb)
head(cb1)

# Visualised network in R using Random Walk communities as they were more sensible that G-N.

edges = get.edgelist(dmg)

attributesd=read.csv(file ='C:\\Users\\kayal\\OneDrive\\Documents\\Social Network\\Lazega-Atts.csv')

# Second part has two plots created: a) colouring by membership and b) colouring by gender
plot(walk, dmg, col=factor(walk$membership))

### Internal note only: This is just what the next graph looks like if I didn't colour the nodes by gender. Leaving this in in case we need it for discussion.

plot(walk, dmg, col=factor(attributesd$gender))

# The above social network graph has grouped the Lazega nodes using the Random Walk algorithm method to classify groups. Thereafter, the nodes are coloured by gender. As we can see from the graph, the number of women are materially fewer than the men.



