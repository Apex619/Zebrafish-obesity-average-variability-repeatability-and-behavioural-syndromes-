#### Custom functions

#Calculating repeatability for learning (Zantiks)
rpt_learning <- function(df) {
  x <- rpt(difference ~ (1 |  fishID), grname = "fishID", data = df, datatype = "Gaussian", nboot = 10000, npermut = 10000)
}

#Obtaining within and between-individual variances for learning
rpt_within_learning <- function(df) {
  rpt(scale(difference) ~ (1 |  fishID), grname = c("fishID", "Residual"), data = df, datatype = "Gaussian", nboot = 10000, npermut = 10000, ratio = FALSE)
}


#Calculating repeatability for mean speed
rpt_speed <- function(df) {
  x <- rpt(mean_speed ~ (1 |  Fish_ID), grname = "Fish_ID", data = df, datatype = "Gaussian", nboot = 10000, npermut = 10000)
}

#Obtaining within and between-individual variances for mean speed
rpt_within_speed <- function(df) {
  rpt(scale(mean_speed) ~ (1 |  Fish_ID), grname = c("Fish_ID", "Residual"), data = df, datatype = "Gaussian", nboot = 10000, npermut = 10000, ratio = FALSE)
}

#Calculating repeatability for zone_05_duration
rpt_zone05 <- function(df) {
  x <- rpt(zone_05_dur ~ (1 |  Fish_ID), grname = "Fish_ID", data = df, datatype = "Gaussian", nboot = 10000, npermut = 10000)
}

#Obtaining within and between-individual variances
rpt_within_zone05 <- function(df) {
  rpt(scale(zone_05_dur) ~ (1 |  Fish_ID), grname = c("Fish_ID", "Residual"), data = df, datatype = "Gaussian", nboot = 10000, npermut = 10000, ratio = FALSE)
}



#Calculating repeatability for zone_05_duration (Optimism)
rpt_optimism_zone05 <- function(df) {
  x <- rpt(zone_05_dur ~ (1 |  Fish_ID), grname = "Fish_ID", data = df, datatype = "Gaussian", nboot = 10000, npermut = 10000)
}


#Calculating repeatability for body length (unadjusted for week)
rpt_length <- function(df) {
  x <- rpt(Fish_Length_cm ~ (1 |  Fish_ID), grname = "Fish_ID", data = df, datatype = "Gaussian", nboot = 10000, npermut = 10000)
}

#Calculating repeatability for body length (adjusted for week)
rpt_length2 <- function(df) {
  x <- rpt(Fish_Length_cm ~ Week + (1 |  Fish_ID), grname = "Fish_ID", data = df, datatype = "Gaussian", nboot = 10000, npermut = 10000)
}

#Obtaining within and between-individual variances (body length, adjusted for week)
rpt_within_length <- function(df) {
  rpt(scale(Fish_Length_cm) ~ Week + (1 |  Fish_ID), grname = c("Fish_ID", "Residual"), data = df, datatype = "Gaussian", nboot = 10000, npermut = 10000, ratio = FALSE)
}

#Calculating repeatability for body weight (unadjusted for week)
rpt_weight <- function(df) {
  x <- rpt(Weight.g. ~ Week + (1 |  Fish_ID), grname = "Fish_ID", data = df, datatype = "Gaussian", nboot = 10000, npermut = 10000)
}

#Calculating repeatability for body weight with week (adjusted for week)
rpt_weight2 <- function(df) {
  x <- rpt(Weight.g. ~ Week + (1 |  Fish_ID), grname = "Fish_ID", data = df, datatype = "Gaussian", nboot = 10000, npermut = 10000)
}

#Obtaining within and between-individual variances (body weight without week)
rpt_within_weight <- function(df) {
  rpt(scale(Weight.g.) ~ (1 |  Fish_ID), grname = c("Fish_ID", "Residual"), data = df, datatype = "Gaussian", nboot = 10000, npermut = 10000, ratio = FALSE)
}

#Obtaining within and between-individual variances (body weight with week)
rpt_within_weight2 <- function(df) {
  rpt(scale(Weight.g.) ~ Week + (1 |  Fish_ID), grname = c("Fish_ID", "Residual"), data = df, datatype = "Gaussian", nboot = 10000, npermut = 10000, ratio = FALSE)
}


#Obtaining within and between-individual variances (total distance)
rpt_within_tot_dist <- function(df) {
  rpt(scale(tot_dist) ~ (1 |  Fish_ID), grname = c("Fish_ID", "Residual"), data = df, datatype = "Gaussian", nboot = 100, npermut = 100, ratio = FALSE)
}

#Calculating repeatability for total distance travelled
rpt_tot_dist <- function(df) {
  x <- rpt(tot_dist ~ (1 |  Fish_ID), grname = "Fish_ID", data = df, datatype = "Gaussian", nboot = 100, npermut = 100)
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

#Obtaining within and between-individual variances (time spent in high zone) F0 ONLY
rpt_within_between_high_dur2 <- function(df) {
  rpt(scale(log(high_dur+1)) ~ (1 |  Fish_ID), grname = c("Fish_ID", "Residual"), data = df, datatype = "Gaussian", nboot = 10000, npermut = 10000, ratio = FALSE)
}

#Calculating repeatability for time spent in high zone F0 ONLY
rpt_high_dur2 <- function(df) {
  x <- rpt(log(high_dur+1) ~ (1 |  Fish_ID), grname = "Fish_ID", data = df, datatype = "Gaussian", nboot = 10000, npermut = 10000)
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

