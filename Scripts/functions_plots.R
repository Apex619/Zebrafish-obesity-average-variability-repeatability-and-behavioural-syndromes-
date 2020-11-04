#### Custom functions

#Function for generating a custom designed violin plot (f1 methods)
violin_plot_custom_hamza <- function(df,param,lab1,lab2) { #only need to add dataframe, parameter, and labels for axes
  ggplot(data = df,aes(x = Tank, y = param, fill = Tank))+
    scale_fill_manual(values = wes_palette("FantasticFox1", n = 2))+ #color scheme
    geom_violin(alpha=0.4, position = position_dodge(width = .75),size=1,color="black")+ #violin plot to display distribution density
    geom_boxplot(notch = TRUE,  outlier.size = -1, color="black",lwd=1.2, alpha = 0.7)+#boxplot to display quantiles
    geom_point( shape = 21,size=2, position = position_jitterdodge(), color="black",alpha=1)+#scatterplot of individual data points
    theme_pubr()+
    labs(title=lab1,y = lab2)+#labels
    rremove("legend.title")+
    theme(panel.border = element_rect(colour = "black", fill=NA, size=2),axis.ticks = element_line(size=2,color="black"),axis.ticks.length=unit(0.2,"cm"),legend.position = c(0.92, 0.85))+      font("xylab",size=15)+font("xy",size=15)+font("xy.text", size = 15) +font("legend.text",size = 15)+
    scale_x_discrete(labels=c("Old" = "Short", "Tall" = "Tall"))+#formatting 
    theme(legend.position = "none")+
    theme(axis.title.x=element_blank())
  
}

#Function for generating a custom designed violin plot (f0)
violin_plot_custom_hamza2 <- function(df,param,lab1,lab2) { #only need to add dataframe, parameter, and labels for axes
  ggplot(data = df,aes(x = Group, y = param, fill = Group))+
    scale_fill_manual(values = wes_palette("FantasticFox1", n = 2))+ #color scheme
    geom_violin(alpha=0.4, position = position_dodge(width = .75),size=1,color="black")+ #violin plot to display distribution density
    geom_boxplot(notch = TRUE,  outlier.size = -1, color="black",lwd=1.2, alpha = 0.7)+#boxplot to display quantiles
    geom_point( shape = 21,size=2, position = position_jitterdodge(), color="black",alpha=1)+#scatterplot of individual data points
    theme_pubr()+
    labs(title=lab1,y = lab2)+#labels
    rremove("legend.title")+
    theme(panel.border = element_rect(colour = "black", fill=NA, size=2),axis.ticks = element_line(size=2,color="black"),axis.ticks.length=unit(0.2,"cm"),legend.position = c(0.92, 0.85))+      font("xylab",size=15)+font("xy",size=15)+font("xy.text", size = 15) +font("legend.text",size = 15)+
    scale_x_discrete(labels=c("Old" = "Short", "Tall" = "Tall"))+#formatting 
    theme(legend.position = "none")+
    theme(axis.title.x=element_blank())
  
}

#Function for generating a custom designed violin plot (f0 learning)
violin_plot_custom_hamza3 <- function(df,param,lab1,lab2) { #only need to add dataframe, parameter, and labels for axes
  ggplot(data = df,aes(x = set, y = param, fill = set))+
    scale_fill_manual(values = wes_palette("FantasticFox1", n = 2))+ #color scheme
    geom_violin(alpha=0.4, position = position_dodge(width = .75),size=1,color="black")+ #violin plot to display distribution density
    geom_boxplot(notch = TRUE,  outlier.size = -1, color="black",lwd=1.2, alpha = 0.7)+#boxplot to display quantiles
    geom_point( shape = 21,size=2, position = position_jitterdodge(), color="black",alpha=1)+#scatterplot of individual data points
    theme_pubr()+
    labs(title=lab1,y = lab2)+#labels
    rremove("legend.title")+
    theme(panel.border = element_rect(colour = "black", fill=NA, size=2),axis.ticks = element_line(size=2,color="black"),axis.ticks.length=unit(0.2,"cm"),legend.position = c(0.92, 0.85))+      font("xylab",size=15)+font("xy",size=15)+font("xy.text", size = 15) +font("legend.text",size = 15)+
    scale_x_discrete(labels=c("control" = "Control", "treatment" = "Treatment"))+#formatting 
    theme(legend.position = "none")+
    theme(axis.title.x=element_blank())
  
}
