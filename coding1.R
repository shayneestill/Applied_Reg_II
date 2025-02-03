#### create the data set ####
data = data.frame(
  time = c(10,13,13,14,17,19,23,25),
  status = c(1,1,0,1,1,0,1,0)
)
head(data)

#### survival function estimate ####
# Greenwood's standard error
# and 95% CI using log-log transformation
# log-log assumes logarithm of the survival function can be modeled linearly over the log-time scale. 
library(survival)
fit = survfit(Surv(time, status)~1, data=data,
              conf.type="log-log")
summary(fit)

#### survival time quartiles and 95% CI ####
quantile(fit, probs=c(0.25, 0.5, 0.75), conf.int=TRUE)

####  mean survival time and standard error #### 
# definition is different from SAS
#rmean = restricted mean. We always need to be restricting at a certain
#event time 
print(fit, rmean="common")

#### plot the Kaplan-Meier survival curve ####
plot(fit, conf.int=T, mark="+", xlab="Time (weeks)",
     ylab="Survival probability")

#### fancier K-M curve ####
library(ggsurvfit)
survfit2(Surv(time, status)~1, data=data, 
         conf.type="log-log") |> 
  ggsurvfit() +
  add_risktable() +
  add_confidence_interval() +
  add_quantile() +
  add_censor_mark() 
