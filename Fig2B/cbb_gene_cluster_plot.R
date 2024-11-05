library(ggplot2)
library(gggenes)
df <- read.table("cbb_gene_cluster.txt",
                 header = T,sep = "\t")
df <- read.table(file = 'clipboard',header = T,sep = "\t")
p <- ggplot(df, aes(xmin = S, xmax = E, y = MAGs, fill = gene,label=gene)) +
  geom_gene_arrow(show.legend = T) +
 # facet_wrap(~ MAGs, scales = "free", ncol = 1) +
  scale_fill_brewer(palette = "Set3") +
  geom_gene_label(align = "centre",size=12) + labs(y="")+
  theme_genes() 
ggsave("cbb_gene_cluster.pdf",p,width = 10,height = 7)


