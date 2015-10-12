library(ggplot2)
lambda = 0.2
nosim <- 1000
n <- 40
nums<-matrix(rexp(nosim * n,lambda),nosim)
dat <- data.frame(vars = apply(nums,1,var),means=apply(nums,1,mean))
t_mean<-1/lambda
t_var<-1/lambda^2
#PLOT MEAN
g <- ggplot(dat, aes(x = means)) + geom_histogram( binwidth=.2, colour = "black",fill = "lightblue") 
g <- g + geom_vline(xintercept = t_mean, size = 1)

#PLOT VAR
g <- ggplot(dat, aes(x = vars)) + geom_histogram(binwidth=1.5, colour = "black",fill = "lightblue") 
g <- g + geom_vline(xintercept = t_var, size = 1)


s_means<-rnorm(nosim, mean = t_mean, sd =  sd(dat$means))
s_vars<-rnorm(nosim, mean = t_var, sd =  sd(dat$vars))

#t_test
t.test(dat$means,s_means,paired=FALSE)
t_mean = 1.2062
t.test(dat$vars,s_vars,paired=FALSE)
t_var= -0.2278



#z_test
d_mean<-mean(dat$means)-mean(s_means)
s_mean<-sd(dat$means)-sd(s_means)
d_mean+c(-1,1)*qnorm(.05)*s_mean/sqrt(nosim)

d_var<-mean(dat$vars)-mean(s_vars)
s_var<-sd(dat$vars)-sd(s_vars)
d_var+c(-1,1)*qnorm(.05)*s_var/sqrt(nosim)
