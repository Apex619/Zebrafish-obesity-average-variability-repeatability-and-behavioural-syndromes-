#### Custom functions

#Obtaining within and between-individual variances (total distance)
rpt_within_between_tot_dist <- function(df) {
  rpt(scale(tot_dist) ~ (1 |  Fish_ID), grname = c("Fish_ID", "Residual"), data = df, datatype = "Gaussian", nboot = 10000, npermut = 10000, ratio = FALSE)
}

#Calculating repeatability for total distance travelled
rpt_tot_dist <- function(df) {
  x <- rpt(tot_dist ~ (1 |  Fish_ID), grname = "Fish_ID", data = df, datatype = "Gaussian", nboot = 10000, npermut = 10000)
}

#Obtaining within and between-individual variances (time spent in low zone)
rpt_within_between_low_dur <- function(df) {
  rpt(scale(low_dur) ~ (1 |  Fish_ID), grname = c("Fish_ID", "Residual"), data = df, datatype = "Gaussian", nboot = 10000, npermut = 10000, ratio = FALSE)
}

#Calculating repeatability for time spent in low zone
rpt_low_dur <- function(df) {
  x <- rpt(low_dur ~ (1 |  Fish_ID), grname = "Fish_ID", data = df, datatype = "Gaussian", nboot = 10000, npermut = 10000)
}

#Obtaining within and between-individual variances (time spent in mid zone)
rpt_within_between_mid_dur <- function(df) {
  rpt(scale(mid_dur) ~ (1 |  Fish_ID), grname = c("Fish_ID", "Residual"), data = df, datatype = "Gaussian", nboot = 10000, npermut = 10000, ratio = FALSE)
}

#Calculating repeatability for time spent in mid zone
rpt_mid_dur <- function(df) {
  x <- rpt(mid_dur ~ (1 |  Fish_ID), grname = "Fish_ID", data = df, datatype = "Gaussian", nboot = 10000, npermut = 10000)
}

#Obtaining within and between-individual variances (time spent in high zone)
rpt_within_between_high_dur <- function(df) {
  rpt(scale(sqrt(high_dur)) ~ (1 |  Fish_ID), grname = c("Fish_ID", "Residual"), data = df, datatype = "Gaussian", nboot = 10000, npermut = 10000, ratio = FALSE)
}

#Calculating repeatability for time spent in high zone
rpt_high_dur <- function(df) {
  x <- rpt(sqrt(high_dur) ~ (1 |  Fish_ID), grname = "Fish_ID", data = df, datatype = "Gaussian", nboot = 10000, npermut = 10000)
}

#Obtaining within and between-individual variances (time spent freezing)
rpt_within_between_freezing_dur <- function(df) {
  rpt(scale(log(freezing_dur+1)) ~ (1 |  Fish_ID), grname = c("Fish_ID", "Residual"), data = df, datatype = "Gaussian", nboot = 10000, npermut = 10000, ratio = FALSE)
}

#Calculating repeatability for time spent freezing
rpt_freezing_dur <- function(df) {
  x <- rpt(log(freezing_dur+1) ~ (1 |  Fish_ID), grname = "Fish_ID", data = df, datatype = "Gaussian", nboot = 10000, npermut = 10000)
}

#Obtaining within and between-individual variances (latency to high zone)
rpt_within_between_latency <- function(df) {
  rpt(scale(latency_high) ~ (1 |  Fish_ID), grname = c("Fish_ID", "Residual"), data = df, datatype = "Gaussian", nboot = 10000, npermut = 10000, ratio = FALSE)
}

#Calculating repeatability for latency to the high zone
rpt_latency <- function(df) {
  x <- rpt(latency_high ~ (1 |  Fish_ID), grname = "Fish_ID", data = df, datatype = "Gaussian", nboot = 10000, npermut = 10000)
}

#Obtaining within and between-individual variances (entries to high zone)
rpt_within_between_freq_high <- function(df) {
  rpt(scale(sqrt(freq_high)) ~ (1 |  Fish_ID), grname = c("Fish_ID", "Residual"), data = df, datatype = "Gaussian", nboot = 10000, npermut = 10000, ratio = FALSE)
}

#Calculating repeatability for entries to the high zone
rpt_freq <- function(df) {
  x <- rpt(sqrt(freq_high) ~ (1 |  Fish_ID), grname = "Fish_ID", data = df, datatype = "Gaussian", nboot = 10000, npermut = 10000)
}


# Writing functions for getting the difference between two tank's repeatablities
unlist_rptr <- function(rpt_model) { #unlisting the bootstrapped distribution 
  unlist(rpt_model$R_boot)
}

difference_boot <- function(boot1,boot2) { #calculating difference between tall and short (depending on which has the higher repeatability)
  boot1 - boot2
}

quantiles_diff_boot <- function(from_diff_boot) { #obtaining quantiles at 2.5% and 97.5% (becomes 95% CI)
  quantile(from_diff_boot, c(0.025, 0.975))
} 

#Function for generating a custom designed violin plot
violin_plot_custom_hamza <- function(df,param,lab1,lab2) { #only need to add dataframe, parameter, and labels for axes
  ggplot(data = df,aes(x = Tank, y = param, fill = Tank))+
    scale_fill_manual(values = wes_palette("FantasticFox1", n = 2))+ #color schee
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