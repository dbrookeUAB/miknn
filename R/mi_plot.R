
mi_plot<- function(df){
  require(ggplot2)

  theme_mi_plot <-  theme(axis.text.x = element_text(angle = 90),
                          plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
                          axis.title = element_text(face = "bold", size = 12),
                          axis.text = element_text(size = 10),
                          # plot.background = element_blank(),panel.background = element_rect(fill = "white"), panel.border = element_rect(colour = "black", fill = NA, size = 1),
                          legend.position = "none",
                          legend.direction = "horizontal",
                          legend.title = element_blank())

  ggplot(data=df,aes(x = project_short_name, y = I, fill = project_short_name))+
    geom_col()+
    theme_mi_plot+
    ggtitle("Mutual Information between Expression and Cancer Type")+
    xlab("")+ylab("Mutual Information (bits)")+
    scale_y_continuous(limits = c(0,4))+
    guides(fill=guide_legend(ncol=17))
}

