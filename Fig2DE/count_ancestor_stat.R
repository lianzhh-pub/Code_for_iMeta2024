
getParent <- function (tree, node=NULL, tips=NULL, edges=NULL) {
  if (!is.null (node) & length (node) == 1) {
    if (!is.numeric (node)) {
      stop ('Node must be numeric')
    }
    if (node > length(tree$tip.label) + tree$Nnode) {
      stop ('Node not in tree')
    }
    if ((node == length(tree$tip.label) + 1) & is.rooted (tree)) {
      # if node is root, return it
      return (node)
    }
    return (tree$edge[tree$edge[ ,2] == node, 1])
  } else if (!is.null (tips)) {
    if (is.character (tips)) {
      # if tips are labels
      edges <- match (match (tips, tree$tip.label), tree$edge[,2])
    } else {
      # ... else they're numbers
      edges <- match (tips, tree$edge[,2])
    }
  } else if (!is.null (node)) {
    edges <- which (tree$edge[ ,2] %in% node)
  } else if (!is.null (edges)) {
    if (is.character (edges) & !is.null (tree$edge.label)) {
      # assume they are labels
      edges <- match (edges, tree$edge.label)
    }
  } else {
    stop ('Must provide either edges, tips or nodes argument')
  }
  end.nodes <- tree$edge[edges, 1]
  term.node <- length (tree$tip.label) + 1
  while (TRUE){
    if (sum (end.nodes[1] == end.nodes) == length (end.nodes)){
      break
    }
    end.nodes <- sort (end.nodes, TRUE)
    start.node <- end.nodes[1]
    edge <- match (start.node, tree$edge[,2])
    end.node <- tree$edge[edge,1]
    edges <- c(edges, edge)
    end.nodes <- c(end.nodes[!end.nodes %in% start.node], end.node)
  }
  return (end.nodes[1])
}


library(ape)
my_tree <- read.tree("D:/project/KSB1-new/COUNT_R/high_q_time_rmjdfr_set_outgroup.tre")
is.ultrametric(my_tree)
nodepath(my_tree)
pdf("tree.pdf")
plot(reorder(my_tree))
tiplabels(pch = 6)
nodelabels(pch = 9 )
tiplabels()
nodelabels()
#.uncompressTipLabel(my_tree)
dev.off()


nodepath(my_tree,88)
node.height(my_tree)

#kegg <- read.table("D:/project/KSB1-new/plot_data/dollo_rmjdfr_set_outgroup.xls",
#                   sep = "\t", header = T, row.names = 1)

kegg <- read.table("D:/project/KSB1-new/plot_data/dollo_rmjdfr_set_outgroup_hoxYH.xls",
                   sep = "\t", header = T, row.names = 1)

level_info <- read.table("D:/project/KSB1-new/plot_data/kegg_level1.txt",
                         header = T, sep = "\t", quote = "")

NODES <- colnames(kegg)
NODES <- gsub("^N",'',NODES,perl = T)
colname <- colnames(kegg)

nid=141

i = which(getParent(my_tree, node = nid) == NODES)
if(paste("N",nid,sep="") %in% colname){
  subkegg <- kegg[,c(paste("N",nid,sep=""),colname[i])] 
}else{
  subkegg <- kegg[,c(my_tree$tip.label[nid],colname[i])]
}

gain_id <- rownames(subkegg[subkegg[,1]>0 & subkegg[,2]==0,])
loss_id <- rownames(subkegg[subkegg[,1]==0 & subkegg[,2]>0,])

write.table(gain_id,file='clipboard',quote = F, row.names=F, col.names=F)
write.table(loss_id,file='clipboard',quote = F, row.names=F, col.names=F)

gain_vector <- vector()

get_info <- function(x){
  level1 <- unique(level_info[level_info$ko == x,3])
  V <- gain_vector
  V <- append(V,level1)
}

gain_vector <- sapply(gain_id,get_info)
gain_vector <- as.vector(unlist(gain_vector)) 

plotdf <- as.data.frame(table(gain_vector))

plotdf$Freq = plotdf$Freq / sum(plotdf$Freq)

# Compute the cumulative percentages (top of each rectangle)
plotdf$ymax = cumsum(plotdf$Freq)

# Compute the bottom of each rectangle
plotdf$ymin = c(0, head(plotdf$ymax, n=-1))
plotdf$labelPosition <- (plotdf$ymax + plotdf$ymin) / 2
plotdf$label <- paste(plotdf$category, "\n value: ", plotdf$Freq)

# Make the plot
p <- ggplot(plotdf, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=2.5, fill=gain_vector)) +
  geom_rect() +
  geom_label( x=3.5, aes(y=labelPosition, label=label), size=3) +
  scale_fill_brewer(palette=2) +
  coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "none")

ggsave(filename = "N99_gain.pdf", p)

loss_vector <- vector()
get_info <- function(x){
  level1 <- unique(level_info[level_info$ko == x,3])
  V <- loss_vector
  V <- append(V,level1)
}

loss_vector <- sapply(loss_id, get_info)
loss_vector <- as.vector(unlist(loss_vector)) 
plotdf <- as.data.frame(table(loss_vector))

plotdf <- as.data.frame(table(loss_vector))

plotdf$Freq = plotdf$Freq / sum(plotdf$Freq)

# Compute the cumulative percentages (top of each rectangle)
plotdf$ymax = cumsum(plotdf$Freq)

# Compute the bottom of each rectangle
plotdf$ymin = c(0, head(plotdf$ymax, n=-1))
plotdf$labelPosition <- (plotdf$ymax + plotdf$ymin) / 2
plotdf$label <- paste(plotdf$category, "\n value: ", plotdf$Freq)

# Make the plot
p <- ggplot(plotdf, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=2.5, fill=loss_vector)) +
  geom_rect() +
  geom_label( x=3.5, aes(y=labelPosition, label=label), size=3) +
  scale_fill_brewer(palette = 2) +
  coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "none")

ggsave(filename = "N99_loss.pdf", p)
