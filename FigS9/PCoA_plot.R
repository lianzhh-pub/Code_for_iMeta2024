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

library(ade4)
tab.dist<-vegdist(t(kaas),method='euclidean')

pcoa<- dudi.pco(tab.dist, scan = FALSE,nf=3)
pcoa_eig <- (pcoa$eig)[1:2] / sum(pcoa$eig)
sample_site <- data.frame({pcoa$li})[1:2]
sample_site$names <- rownames(sample_site)
names(sample_site)[1:2] <- c('PCoA1', 'PCoA2')
sample_site$Orders <- orders[sample_site$names,2]

ratio.display <- 3/4
ratio.values <- (max(sample_site$PCoA1)-min(sample_site$PCoA1))/(max(sample_site$PCoA2)-min(sample_site$PCoA2))

pcoa_plot <- ggplot(sample_site, aes(PCoA1, PCoA2,color=Orders)) + #geom_point(size=3)+
  geom_label(label=row.names(sample_site))+
  theme_classic()+
  geom_vline(xintercept = 0, color = 'gray', size = 0.4) + 
  geom_hline(yintercept = 0, color = 'gray', size = 0.4) +
  geom_point(size = 1.5)+  
  stat_ellipse()+
  #scale_color_manual(values = brewer.pal(6,"Set2")) + 
  theme(panel.grid = element_line(color = 'gray', linetype = 2, size = 0.1), 
        panel.background = element_rect(color = 'black', fill = 'transparent'), 
        legend.title=element_blank())+
  coord_fixed(ratio.values/(4/3) )
ggsave("pcoa_kaas.pdf",pcoa_plot,width = 10,height = 10)
