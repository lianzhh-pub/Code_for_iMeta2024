df <- read.table("Genome_size_and_GC.txt", header=T, row.names=1,sep="\t")

library(ggplot2)
p <- ggplot(df, aes(x=Order, y=Size)) + 
  geom_boxplot() + 
  geom_point(aes(colour = Order,shape=Source),position=position_jitter(0.2),size=3) + 
  theme_bw() 
ggsave(p, filename = "Size.pdf")
q <- ggplot(df, aes(x=Order, y=GC)) + 
  geom_boxplot() + 
  geom_point(aes(colour = Order,shape=Source),position=position_jitter(0.2),size=3) + 
  theme_bw() 
ggsave(q, filename = "GC.pdf")
