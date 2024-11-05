kaas <- read.table("kaas.xls",
                   header = T, row.names = 1, sep = "\t")

kaas <- kaas[,-which(colnames(kaas)%in% c("GCA_002898015.1", "GCA_002898095.1", "GCA_003644765.1", "GCA_013151735.1"))]
orders <- read.table("MAGs_order.txt",
                     header = T, row.names = 1, sep = "\t")

library(ggplot2)
library(ggrepel)
library(grid)
library(vegan)
require(scales)

pca_r <- rda(t(kaas))

pca_p <- pca_r$CA$u
pca_p <- as.data.frame(pca_p[,1:2])
pca_p[,3] <-row.names(pca_p)  


ratio.display <- 3/4
ratio.values <- (max(pca_p$PC1)-min(pca_p$PC1))/(max(pca_p$PC2)-min(pca_p$PC2))


pca_arrow <- pca_r$CA$v
pca_arrow <- as.data.frame(pca_arrow[,1:2])
colnames(pca_p)[3] <- "Samples"
#colnames(orders) <- c('Samples', 'Order')
pca_p$orders <- orders[pca_p$Samples,1]


p <- ggplot(pca_p,aes(PC1,PC2,colour=orders)) + geom_point(size=5) + 
  #geom_label(label=row.names(pca_p))+
  #geom_text_repel(aes(label=row.names(pca_p)),size=1,box.padding = unit(1.2,'lines')) +
  ylab(colnames(pca_p)[2])+xlab(colnames(pca_p)[1])

p <- p + geom_hline(yintercept=0, size=0.5) + geom_vline(xintercept=0,size=0.5) + theme_classic() +
  theme(axis.line=element_line(linewidth =0.5), axis.ticks=element_line(linewidth =0.8,color="black"),
        axis.text=element_text(color="black",size=16),axis.title=element_text(color="black",size=16),
        axis.title.y=element_text(vjust=1.4),axis.title.x=element_text(vjust=0.001),
        legend.title=element_text(size=16), panel.border=element_rect(colour="black",fill=NA, size=0.8))+
  coord_fixed(ratio.values/(1/ratio.values)) + stat_ellipse(level = 0.95)

ggsave("pcoa_kaas.pdf",p,width = 10,height = 10)
