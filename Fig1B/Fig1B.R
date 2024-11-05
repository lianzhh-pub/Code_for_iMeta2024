library(ggpubr)
library(reshape)
df <- read.table("C:/Users/xxx/Desktop/KSB1-new/plot_data/environment_distribution.txt",
                 sep = "\t", header = T)


p <- ggboxplot(df, x = "Type", y = "RA", alpha = 0.5,
          order = c("Animal-associated", "Aquatic", "Hydrothermal_system",
                    "Plant-associated", "Sediment", "Soil", "Other"),
          color = "Order", palette =c("#D37DAA", "#DEA0BA", "#F7D368","#A6C467",
                                     "#68A24F", "#D56463"),
          add = "jitter",outlier.shape = 19,
          x.text.angle = 20,
          ggtheme = theme_bw(),
          ylab = "Relative abundance (%)") +
  #scale_y_log10(limits=c(0,11), breaks = c(0,0.001,0.01,0.1,1,10)) +
  scale_y_log10(breaks = c(0, 0.0001, 0.001, 0.01, 0.1, 1, 10), 
                label  = c(0, 0.0001, 0.001, 0.01, 0.1, 1, 10)) +
  theme(panel.grid.major.x  = element_blank(),
        panel.grid.minor.y  = element_blank(),
        legend.position="none") +
  geom_vline(xintercept = c(1.5,2.5,3.5,4.5,5.5,6.5),
             linetype="dashed")

p <- ggboxplot(df, x = "Order", y = "RA", 
               color = "Order", palette =c("#D37DAA", "#DEA0BA", "#F7D368","#A6C467",
                                           "#68A24F", "#D56463"),
               add = "jitter",outlier.shape = 19,
               x.text.angle = 20,
               ggtheme = theme_bw(),
               ylab = "Relative abundance (%)") +
  scale_y_log10() +
  theme(panel.grid.major.x  = element_blank(),
        panel.grid.minor.y  = element_blank(),
        legend.position="none")

facet(p,scales = "fixed",facet.by = "Type")

ggsave(p, filename = "environment_distribution.pdf")
