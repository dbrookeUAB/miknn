
exp_plot<- function(df){
  require(ggplot2)
  theme_MutInfo <- theme(axis.text.x = element_text(angle = 90),
                         plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
                         axis.title = element_text(face = "bold", size = 16),
                         axis.text = element_text(size = 12),

                         legend.position = "none",
                         legend.direction = "horizontal",
                         legend.title = element_blank())


  ggplot(data = df,aes(x = project_short_name, y = HTSeq__FPKM_UQ, fill = project_short_name))+
    geom_boxplot(outlier.size=0.3)+
    theme_MutInfo+
    ggtitle("Mutual Information between MAPK1 Expression and Cancer Type")+
    xlab("")+
    ylab("Expression (FPKM)")+
    scale_y_continuous(trans = "log10")
}
