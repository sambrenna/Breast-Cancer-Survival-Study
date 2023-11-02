data("brca")
dim(brca)

# The function Surv(time, event) of survival package allows to create a survival object,
# usually used as a response variable in a model formula.

Surv(brca$Time, brca$Event)
Surv(brca$Time, brca$Event==1)

#-------------------------------------------#
# Kaplan-Meier estimator for survival curve #
#-------------------------------------------#
# The Kaplan-Meier estimator of a survival curve can be computed using the survfit function():
fit <- survfit(Surv(Time, Event==1) ~ 1, data = brca)
names(fit)
summary(fit)

  

# The function survfit() returns a list of variables
names(fit)
# including the following components:
# n: total number of subjects 
# time: the event time points on the curve (t=t*_j)
# n.risk: the number of subjects at risk at time t
# n.event: the number of events that occurred at time t
# n.censor: the number of censored subjects, who exit the risk set at time t
# surv: the kaplan-meier estimator for survival S(t)
# std.err: the standard error for S(t)
# lower, upper: lower and upper confidence limits for the survival curve S(t), respectively.
# cumhaz: the cumulative hazard curve H(t) = - log(S(t))
# std.err: the standard error for H(t)

# Complete table for Kaplan-Meier estimator
summary(fit)

# Median Survival time
# The median survival times represents the time at which the survival probability, S(t), is 0.5.
median_St<-fit$time[fit$surv<=0.5][1]
median_St

# Access to the sort summary table
summary(fit)$table

# By default, the function print() shows a short summary of the survival curves. 
# It prints the number of observations, number of events, the median survival and 
# the confidence limits for the median.
print(fit)

#-------------------------#
# Kaplan-Meier curve plot #
#-------------------------#
# To plot the KM estimator you can use the function plot
plot(fit, conf.int = T, xlab='Time [days]', ylab = 'Survival Probability', col='red',
     main="Kaplan-Meier Curve for Breast Cancer Survival")

# For a better visualization use ggsurvplot() function [package survminer]:
ggsurvplot(fit,
           risk.table = TRUE, # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           surv.median.line = "hv", # Specify median survival
           ggtheme = theme_bw(), # Change ggplot2 theme
           break.time.by=90,
           title="Kaplan-Meier Curve for Breast Cancer Survival")
# At time zero, the survival probability is 1.0 (or 100% of the participants are alive).
# At time 180 (after 6 months), the probability of survival is approximately ?
# The median survival is approximately ? days.
# After ? days, the survival probability is below 25%

#-------------------------------#
# Cumulative incidence function #
#-------------------------------#
# The cumulative incidence, or cumulative failure probability (CFP), shows the cumulative
# probabilities of experiencing the event of interest and it is computed as CFP(t) = P(T<t)
# so can be estimated as 1-S(t):
cumulative_incidence <- 1 - fit$surv
head(cumulative_incidence)

# CFP can be visualized using the ggsurvplot() function [package survminer],
# specifying the option fun='event':
ggsurvplot(fit,
           risk.table = TRUE, # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           surv.median.line = "hv", # Specify median survival
           ggtheme = theme_bw(), # Change ggplot2 theme
           break.time.by=90,
           fun='event',
           title="Cumulative Incidence Curve for Breast Cancer Survival")

#----------------------------#
# Cumulative hazard function #
#----------------------------#
# The cummulative hazard is commonly used to estimate the hazard probability. 
# It's defined as H(t) = -log(S(t)). The cumulative hazard (H(t)) can be interpreted 
# as the cumulative force of mortality. In other words, it corresponds to the number 
# of events that would be expected for each individual by time t if the event were a repeatable process.

# The cumulative hazard H(t) = -log(S(t)) is computed by function survdiff() using the
# Nelson-Aalen cumulative hazard rate estimator and it is given by:
H <- fit$cumhaz
head(H)

# H(t) can be visualized using the ggsurvplot() function [package survminer],
# specifying the option fun='cumhaz':
ggsurvplot(fit,
           risk.table = TRUE, # Add risk table
           ggtheme = theme_bw(), # Change ggplot2 theme
           break.time.by=90,
           fun='cumhaz',
           title="Cumulative Hazard Curve for Breast Cancer Survival")


curves <- data.frame('time' = fit$time,
                     'Survival' = fit$surv,
                     'Cum_incidence' = 1 - fit$surv,
                     'Cum_hazard' = fit$cumhaz
)
head(curves)
#-------------------------------#
# Kaplan-Meier Curves by Tumor Stage#
#-------------------------------#
# We want to consider now the gender groups and investigate if there is a difference
# in terms of survival among the two groups.
fit.stage <- survfit(Surv(Time, Event==1) ~ Stage, data = brca)

# By default, the function print() shows a short summary of the survival curves. 
# It prints the number of observations, number of events, the median survival and 
# the confidence limits for the median for both groups:
print(fit.stage)

# Summary of survival curves
summary(fit.stage)$table

# Complete KM estimation tables
summary(fit.stage)


#-----------------------------#
# Kaplan-Meier plot by Tumor Stage #
#-----------------------------#
plot(fit.stage, conf.int = T, xlab='Time [days]', ylab = 'Survival Probability', col=c("dodgerblue2", "orchid2"))
legend('topright', legend=c('Lower Stage','Higher Stage'), lty=c(1,1), col=c("dodgerblue2", "orchid2"))

ggsurvplot(fit.stage, conf.int = T,
           risk.table = TRUE, # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           surv.median.line = "hv", # Specify median survival
           ggtheme = theme_bw(), # Change ggplot2 theme
           break.time.by=90,
           legend.labs=c("Lower Stage", "Higher Stage"), legend.title="Stage",  
           palette=c("dodgerblue2", "orchid2"), 
           title="Kaplan-Meier Curves by tumor stage for Breast Cancer Survival")

# At time zero, the survival probability is 1.0 (or 100% of the participants are alive).
# At time 180 days (6 months), the probability of survival is approximately ? 
# for patients with a higher stage (stage=1) and ?  for patients with a lower stage (stage=0).
# The median survival is approximately ? days for patients with a higher stage and ? days for patients with a lower stage,
# suggesting a good survival for patients with  alower stage compared to patients with a higher stage.

# Survival probability at time 180 days
summary(fit.stage , times=180)
# Survival probabilities every six months
summary(fit.stage, times=seq(0,365*3,182.5))


#-----------------------------------------#
# Cumulative incidence function by tumor stage #
#-----------------------------------------#
ggsurvplot(fit.stage, conf.int = T,
           risk.table = TRUE, # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           surv.median.line = "hv", # Specify median survival
           ggtheme = theme_bw(), # Change ggplot2 theme
           break.time.by=90,
           legend.labs=c("Lower stage", "Higher stage"), legend.title="Stage",  
           palette=c("dodgerblue2", "orchid2"), 
           title="Cumulative Incidence Curves by tumor stage for Breast Cancer Survival",
           fun='event')

#--------------------------------------#
# Cumulative hazard function by tumor stage #
#--------------------------------------#
ggsurvplot(fit.stage, conf.int = T,
           risk.table = TRUE, # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           ggtheme = theme_bw(), # Change ggplot2 theme
           break.time.by=90,
           legend.labs=c("Lower Stage", "Higher Stage"), legend.title="Stage",  
           palette=c("dodgerblue2", "orchid2"), 
           title="Cumulative Hazards Curves by tumor stage for Breast Cancer Survival",
           fun='cumhaz')


# There appears to be a survival advantage for patients a lower stage with breast cancer compare to patients with a higher stage. 
# However, to evaluate whether this difference is statistically significant requires a 
# formal statistical test --> log-rank test.
# The log-rank test is the most widely used method of comparing two or more survival curves. 
# The null hypothesis is that there is no difference in survival between the two groups. 
# The log rank test is a non-parametric test, which makes no assumptions about the survival distributions. 
# Essentially, the log rank test compares the observed number of events in each group to what would be 
# expected if the null hypothesis were true (i.e., if the survival curves were identical).
# The log rank statistic is approximately distributed as a chi-square test statistic.

# The function survdiff() [in survival package] can be used to compute log-rank test comparing 
# two or more survival curves.
# survdiff() can be used as follow:

survdiff(Surv(Time, Event==1) ~ Stage, data = brca)


# In the ggsurvplot() function we can specify the factor pval=T
# which return the p-values of the log-rank test
ggsurvplot(fit.stage, conf.int = T,
           risk.table = TRUE, # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           surv.median.line = "hv", # Specify median survival
           ggtheme = theme_bw(), # Change ggplot2 theme
           break.time.by=90,
           legend.labs=c("Lower Stage", "Higher Stage"), legend.title="Stage",  
           palette=c("dodgerblue2", "orchid2"), 
           title="Kaplan-Meier Curves by tumor stage for Breast Cancer Survival",
           pval=T)

# The log rank test for difference in survival gives a p-value of p = 0.004, 
# indicating that the stage groups differ significantly in survival.

#--------------#
# Hazard Ratio #
#--------------#
# To quantify the difference in the survivals we can compute the hazard ratio, i.e.
# the ratio between the death hazard of the first group vs the other one.

# From the output of the log-rank test  we can extract the number of observed and expected deaths
# in higher stages and lower stages groups:
#     - observed deaths in lower stages: 57
#     - expected deaths in lower stages: 69.4
#     - observed deaths in Higher stages: 38
#     - expected deaths in Higher Stages: 25.6
# Therefore, the death hazard ratio of males vs females is:
hazard_ratio <- (57/69.4)/(38/25.6)
hazard_ratio

# HR = 0.5533141 < 1 indicating that the risk of deaths in patiets with lower stagest is 0.55... times
#                times the risk of death in patients with higher stages.
# Patients with a higher stage have lower survival probability than patients with a lower stage.
# Being a patient with a lower stage is a protective factor.
#-------------------------------#



# Kaplan-Meier Curves by Precoce Menopause Status#
#-------------------------------#
# We want to consider now the gender groups and investigate if there is a difference
# in terms of survival among the two groups.
fit.men <- survfit(Surv(Time, Event==1) ~ Menopause, data = brca)

# By default, the function print() shows a short summary of the survival curves. 
# It prints the number of observations, number of events, the median survival and 
# the confidence limits for the median for both groups:
print(fit.men)

# Summary of survival curves
summary(fit.men)$table

# Complete KM estimation tables
summary(fit.men)


#-----------------------------#
# Kaplan-Meier plot by Precoce Menopause Status #
#-----------------------------#
plot(fit.men, conf.int = T, xlab='Time [days]', ylab = 'Survival Probability', col=c("dodgerblue2", "orchid2"))
legend('topright', legend=c('Menopause','No Menopause'), lty=c(1,1), col=c("dodgerblue2", "orchid2"))

ggsurvplot(fit.men, conf.int = T,
           risk.table = TRUE, # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           surv.median.line = "hv", # Specify median survival
           ggtheme = theme_bw(), # Change ggplot2 theme
           break.time.by=90,
           legend.labs=c("Menopause", "No Menopause"), legend.title="Menopause",  
           palette=c("dodgerblue2", "orchid2"), 
           title="Kaplan-Meier Curves by precoce menopause status for Breast Cancer Survival")

# At time zero, the survival probability is 1.0 (or 100% of the participants are alive).
# At time 180 days (6 months), the probability of survival is approximately ? 
# for patients with a precoce menopause (stage=0) and ?  for patients with a normal insurgence/no meopause (stage=1).
# The median survival is approximately ? days for patients with a higher stage and ? days for patients with a lower stage,
# suggesting a good survival for patients with  alower stage compared to patients with a higher stage.

# Survival probability at time 180 days
summary(fit.men , times=180)
# Survival probabilities every six months
summary(fit.men, times=seq(0,365*3,182.5))


#-----------------------------------------#
# Cumulative incidence function by menopause status #
#-----------------------------------------#
ggsurvplot(fit.men, conf.int = T,
           risk.table = TRUE, # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           surv.median.line = "hv", # Specify median survival
           ggtheme = theme_bw(), # Change ggplot2 theme
           break.time.by=90,
           legend.labs=c("Menopause", "No Menopause"), legend.title="Menopause",  
           palette=c("dodgerblue2", "orchid2"), 
           title="Cumulative Incidence Curves by Menopause status for Breast Cancer Survival",
           fun='event')

#--------------------------------------#
# Cumulative hazard function by menopause status #
#--------------------------------------#
ggsurvplot(fit.men, conf.int = T,
           risk.table = TRUE, # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           ggtheme = theme_bw(), # Change ggplot2 theme
           break.time.by=90,
           legend.labs=c("Menopause", "No Menopause"), legend.title="Menopause",  
           palette=c("dodgerblue2", "orchid2"), 
           title="Cumulative Hazards Curves by menopause state for Breast Cancer Survival",
           fun='cumhaz')


# The function survdiff() [in survival package] can be used to compute log-rank test comparing 
# two or more survival curves.
# survdiff() can be used as follow:

survdiff(Surv(Time, Event==1) ~ Menopause, data = brca)


# In the ggsurvplot() function we can specify the factor pval=T
# which return the p-values of the log-rank test
ggsurvplot(fit.men, conf.int = T,
           risk.table = TRUE, # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           surv.median.line = "hv", # Specify median survival
           ggtheme = theme_bw(), # Change ggplot2 theme
           break.time.by=90,
           legend.labs=c("Menopause", "No Menopause"), legend.title="Menopause",  
           palette=c("dodgerblue2", "orchid2"), 
           title="Kaplan-Meier Curves by menopause status for Breast Cancer Survival",
           pval=T)

# The log rank test for difference in survival gives a p-value of p = 0.004, 
# indicating that the stage groups differ significantly in survival.

#--------------#
# Hazard Ratio #
#--------------#
# To quantify the difference in the survivals we can compute the hazard ratio, i.e.
# the ratio between the death hazard of the first group vs the other one.

# From the output of the log-rank test  we can extract the number of observed and expected deaths

hazard_ratio <- (343/133)/(650/860)
hazard_ratio


# Kaplan-Meier Curves by LNR#
#-------------------------------#
# We want to consider now the gender groups and investigate if there is a difference
# in terms of survival among the two groups.
fit.lnr <- survfit(Surv(Time, Event==1) ~ LNR_CAT, data = brca)

# By default, the function print() shows a short summary of the survival curves. 
# It prints the number of observations, number of events, the median survival and 
# the confidence limits for the median for both groups:
print(fit.lnr)

# Summary of survival curves
summary(fit.lnr)$table

# Complete KM estimation tables
summary(fit.lnr)


#-----------------------------#
# Kaplan-Meier plot by LNR #
#-----------------------------#
plot(fit.lnr, conf.int = T, xlab='Time [days]', ylab = 'Survival Probability', col=c("dodgerblue2", "orchid2", "green"))
legend('topright', legend=c('Medium','Low', 'High'), lty=c(1,1), col=c("dodgerblue2", "orchid2", "green"))

ggsurvplot(fit.lnr, conf.int = T,
           risk.table = TRUE, # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           surv.median.line = "hv", # Specify median survival
           ggtheme = theme_bw(), # Change ggplot2 theme
           break.time.by=90,
           legend.labs=c("Medium", "Low", "High"), legend.title="LNR",  
           palette=c("dodgerblue2", "orchid2", "green"), 
           title="Kaplan-Meier Curves by LNR for Breast Cancer Survival")

# At time zero, the survival probability is 1.0 (or 100% of the participants are alive).
# At time 180 days (6 months), the probability of survival is approximately ? 


# Survival probability at time 180 days
summary(fit.lnr , times=180)
# Survival probabilities every six months
summary(fit.lnr, times=seq(0,365*3,182.5))


#-----------------------------------------#
# Cumulative incidence function by LNR #
#-----------------------------------------#
ggsurvplot(fit.lnr, conf.int = T,
           risk.table = TRUE, # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           surv.median.line = "hv", # Specify median survival
           ggtheme = theme_bw(), # Change ggplot2 theme
           break.time.by=90,
           legend.labs=c("Medium", "Low", "High"), legend.title="LNR",  
           palette=c("dodgerblue2", "orchid2", "green"), 
           title="Cumulative Incidence Curves by LNR status for Breast Cancer Survival",
           fun='event')

#--------------------------------------#
# Cumulative hazard function by LNR #
#--------------------------------------#
ggsurvplot(fit.lnr, conf.int = T,
           risk.table = TRUE, # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           ggtheme = theme_bw(), # Change ggplot2 theme
           break.time.by=90,
           legend.labs=c("Medium", "Low", "High"), legend.title="LNR",  
           palette=c("dodgerblue2", "orchid2", "darkorange2"), 
           title="Cumulative Hazards Curves by LNR state for Breast Cancer Survival",
           fun='cumhaz')


# The function survdiff() [in survival package] can be used to compute log-rank test comparing 
# two or more survival curves.
# survdiff() can be used as follow:

survdiff(Surv(Time, Event==1) ~ LNR_CAT, data = brca)


# In the ggsurvplot() function we can specify the factor pval=T
# which return the p-values of the log-rank test
ggsurvplot(fit.men, conf.int = T,
           risk.table = TRUE, # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           surv.median.line = "hv", # Specify median survival
           ggtheme = theme_bw(), # Change ggplot2 theme
           break.time.by=90,
           legend.labs=c("Menopause", "No Menopause"), legend.title="Menopause",  
           palette=c("dodgerblue2", "orchid"), 
           title="Kaplan-Meier Curves by menopause status for Breast Cancer Survival",
           pval=T)

# The log rank test for difference in survival gives a p-value of p = 0.004, 
# indicating that the stage groups differ significantly in survival.

#--------------#
# Hazard Ratio #
#--------------#




