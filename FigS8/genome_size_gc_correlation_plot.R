df <- read.table("genome_size_gc_correlation_within_order.txt",
                 header = T, row.names = 1, sep = "\t")
library(ggpubr)
df$Genome_size=df$Genome_size/1000000
p <- ggscatter(df, y = "Genome_size", x = "GC",
          add = "reg.line",                         # Add regression line
          conf.int = TRUE,                          # Add confidence interval
          color = "Order", palette = "Order",           # Color by groups "cyl"
          facet.by = "Order",
          scales = "free"
)

ggsave(filename = "genome_size_gc_correlation.pdf", p, width = 8, height = 6.5)

  ggscatter(df, y = "Genome_size", x = "GC",
            add = "reg.line",                         # Add regression line
            conf.int = TRUE,                          # Add confidence interval
            color = "Order", palette = "Order",           # Color by groups "cyl"
            facet.by = "Order",
            scales = "free"
  )+
  stat_cor(method = 'spearman', aes(color = Order), label.x = 3)

df2 <- read.table("Rep_genome_size_gc_correlation_within_order.txt",
                   header = T, row.names = 1, sep = "\t")
df2$Genome_size=df2$Genome_size/1000000
q<- ggscatter(df2, y = "Genome_size", x = "GC",
               add = "reg.line",                         # Add regression line
               conf.int = TRUE,                          # Add confidence interval
               color = "Order", palette = "Order",           # Color by groups "cyl"
               facet.by = "Order",
               scales = "free"
)
ggsave(filename = "rep_genome_size_gc_correlation.pdf", q, width = 8, height = 6.5)

+stat_cor(method = 'spearman', aes(color = Order), label.x = 3)
