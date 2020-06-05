################ RESEARCH PROJECT ####################################
################ IMPORTANCE SAMPLER ##################################
################ FIRST ALGORITHM FOR SIMPLE PROBLEMS(for known target) #######
################ NORMALIZED IMPORTANCE SAMPLER ##############################
#In this approach, we know the entire target distribution not just a distribution
#proporional to the target.
#For that reason we do not need to normalize the weights.
#import the libraries
library(ggplot2)
library(pracma)
library(gridExtra)

#extra functions we need for the sampler
#generate from the proposal distribution
#... in order to change the parameters depend on the distribution
rProp = function(PropDis,N,...){
  
  #use the function to generate
  X = PropDis(N,...)
  
  return(X)
}


#function to calculate the density of the target distribution
dTarget = function(target,X,...){
  
  #compute the density of the sample for the target distribution
  dens = target(X,...)
  
  return(dens)
}

#function for compute the density of the proposal
dProp = function(prop,X,...){
  
  #use the function to generate
  dens = prop(X,...)
  return(dens)
}


#Importance Sampler
#Normalized method( We know the true distribution)
#Input: rprop = fun to gen from proposal with parameters (a,b),
#dtarget = density of the target distr with par (c,d),
#dprp = density of the proposal with par (a,b),
#N size of the sample
#function for which we want to estimate its integral 

ImpSmler = function(rprop, a, b, dtarget,c,d, dprop,N, f ){ 
  

  #STEP 1: Generate from the proposal distribution sample of size N
  X = rProp(rprop,N,a,b)
  
  #STEP 2: Compute the importance weights(unormalized)
  #density for X sample using the target distribution
  Pi = dTarget(dtarget, X,c,d)
  
  #compute the importance (unormalized) weights
  w = Pi/dprop(X,a,b)
  
  #normalize the weights
  #HERE WE DO NOT( we know the true(normalized) target distribution )
  #W_norm = w/sum(w)
  
  #we return the estimator and the variance of the sample
  MC_estimator = c(mean(f(X)*w),var(f(X)*w))
  
  return(MC_estimator)
  
}

################### EXAMPLES OF THE SAMPLER ###############################

################### FIRST EXAMPLE: Int(f,0,10) ###########################
#First example: Evaluate the integral int(f,0,10)

#Functions we need for the integration
#f : The function for which we want to integrate
######### TRUE VALUE ####### 
f_int = function(x){
  return(exp(-2*abs(x-5)))
}

#TRUE value of the integral
int_true = integral(f_int,0,10) #approximately 1

########## MC APPROACH #######
f = function(x){
  return(10*exp(-2*abs(x-5)))
}

N = 10^5 #sample size
set.seed(1234567890)
X =  runif(N,0,10)
Y = f(X)
MC_appr = c(mean(Y),var(Y)) 

########### IMPORTANCE SAMPLER APPROACH #########
#first a plot as a help to choose proposal
plot_IS = ggplot()+
  geom_line(mapping = aes(x = X,y = Y))


set.seed(1234567890)
IS = ImpSmler(rnorm,5,1,dunif,0,10,dnorm, N,f)

#data frame for presenting the result
df_1 = t(data.frame(c(int_true,0),MC_appr,IS))
rownames(df_1) = c("True_Value", "MC", "IS")
colnames(df_1) = c("Expected Value", "Variance")
print(df_1) #Huge reduce of variance between MC and IS


#################### SECOND EXAMPLE ################################ 
#Calculate the PR(X>3) of a given distribution

###### TRUE VALUE #################
int_true = integral(dnorm,3,Inf)

###### MONTE CARLO APPROACH #######
f = function(x){
  
  res = rep(0,length(x))
  
  for (i in 1:length(x)) {
    if(x[i]>=3){
      res[i] = 1
    }
    else res[i] = 0
  }
  return(res)
}


set.seed(1234567890)
N = 10^5
MC_est = rep(0,N)

for(i in 1:N){
  #generate a sample from normal
  X <- rnorm(100, 0,sd=1)
  
  #The proportion of X>3
  MC_est[i] = length(which(X>3))/length(X)
}

#options(scipen = 999)

#mean and variance of the MC approximation
MC_appr = round(c(mean(MC_est),var(MC_est)),6)

####### IMPORTANCE SAMPLING APPROACH #######
est = rep(0,N)
for(i in 1:N){
  est[i] = ImpSmler(rnorm,4,1,dnorm,0,1,dnorm,100 ,f)[1]
}

IS = c(round(mean(est),5),round(var(est),9))

df_2 = t(data.frame(c(int_true,0),MC_appr,IS))
rownames(df_2) = c("True_Value", "MC", "IS")
colnames(df_2) = c("Expected Value", "Variance")
print(df_2) #again huge reduce of the variance

  #------------------------------------------------------------------------------
#------------------------------------------------------------------------------
################# SECOND APPROCH OF IMPORTANCE SAMPLER #######################
################  ESTIMATE THE POSTERIOR OF A PARAMETER #####################
#Here we assume that we know the target distribution proportionally.
#We also use the logWeights in order to avoid computational problems.

# log posterior of the parameter
#input t: parameter we want to estimate, X : sample
#FOR BETA
logPosterior = function(theta,X){
  n = length(X)
  return((sum(X)+4)*log(theta) + (n*(10-mean(X))+2)*log(1-theta))
}


#generate from the proposal distribution
#PropDis is the function we want to generate, N ; #of sample
#... because we may have different function with dif number of parameters
rProp = function(PropDis,N,...){
  
  #use the function to generate
  X = PropDis(N,...)
  
  return(X)
}

#function for compute the density of the proposal
dProp = function(prop,X,...){
  
  #use the function to generate
  dens = prop(X,...)
  return(dens)
}

#IS in order to approximate the logPosterior
ImpSmler = function(X,rprop, a, b, logPost, dprop,N, f,...){ 
  
  #STEP 1: Generate from the proposal distribution sample of size N
  U = rProp(rprop,N,a,b)
  
  #STEP 2: Compute the importance weights(unormalized)
  #compute the importance (unormalized) weights
  logW = logPost(U,X)- dprop(U,a,b,log = TRUE)
  
  #find the largest value to prevent any underflow
  w_max = max(logW)
  
  #The new weights 
  W = logW - w_max
  
  logNC = log(sum(exp(W))) + w_max - log(N)

 
  #normalize the weights
  norm_W  = exp(W)/sum(exp(W))
  
  # importance sampling estimate
  IS_estimator = sum(f(U)*norm_W)
  
  return(c(logNC,IS_estimator))
  
}

   
f = function(x){
  return(x)
}

#Posterior of the parameter
posterior = function(t){
  n = length(X)
  m = mean(X)
  return((t^(n*m +4))*((1-t)^(n*(10-m) +2)))
}


#We have a beta dis as a prior for the parameter theta

set.seed(1234567890)
X = rbinom(100, 10, 0.4) # Data are Bin distributed
n = length(X)
mu = mean(X)
#Prior of theta is Beta(5,3)
a = 5 
b = 3
C = 10

#We compute the posterior paratemeters for theta in order to aproximate the NC
a_post = n*mu + a 
b_post = n*(10-mu) + b

######Real value of the NC using the beta function
cons_function = beta(a_post,b_post)
log_nc = log(cons_function)

###### Real value of the NC using the integral
NC_integral = log(integral(posterior,0,1))


set.seed(1234567890)
ISA = ImpSmler(X = X,rprop = rbeta,a = C*mean(X),b = C*(10-mean(X)),
               logPost = logPosterior,
               dprop = dbeta,N =  10,f = f)


############VISUALIZATION #########
#Histogram of the Log Norm. Constant(NC)
#Plots for small sample size N = 20

steps = 10^4
NC_est = rep(0,steps)
set.seed(1234567890)
for (i in 1:steps) {
  NC_est[i] = ImpSmler(X = X,rprop = rbeta,a = C*mean(X),b = C*(10-mean(X)),
                       logPost = logPosterior,
                       dprop = dbeta,N =  20,f = f)[1]
  
}


plt1 = ggplot() + 
  geom_histogram(mapping = aes(x = NC_est, y = ..density..),bins = 35, 
                 color = "black",fill = "red2")+
  geom_density(mapping = aes(NC_est), color = "yellow", size = 1, lty = 2)+
  labs(title = "Log(NC) estimation",
       x = "Log(NC)", y = "Density") +
  theme_bw()
  
plt2 = ggplot() + 
  geom_histogram(mapping = aes(x = exp(NC_est), y = ..density..),bins = 35, 
                 color = "black",fill = "red2")+
  scale_x_continuous(trans="log10")+
  geom_density(mapping = aes(exp(NC_est)), color = "yellow", size = 1, lty = 2)+
  labs(title = "NC estimation",
       x = "NC", y = "Density") +
  theme_bw()


#Plot the difference between the real value and the estimation
ratio = exp(NC_est)/exp(NC_integral)# should be close to 1
ratio_20 = mean(ratio)
LogRatio = NC_est - NC_integral #should be close to 0
LogRatio_20 = mean(LogRatio)

#Plot for the Normal Ration 
plt3 = ggplot() + 
  geom_histogram(mapping = aes(x = ratio, y = ..density..),bins = 35, 
                 color = "lightblue",fill = "steelblue")+
  geom_density(mapping = aes(ratio), color = "red", size = 1, lty = 2)+
  geom_vline(xintercept = 1, size = 1, lty = 2)+
  labs(title = "Ratio between real value and estimation",
       x = "Estimation/True_NC", y = "Density") +
  theme_bw()

#Plot for the Log Ratio
plt4 = ggplot() + 
  geom_histogram(mapping = aes(x = LogRatio, y = ..density..),bins = 35, 
                 color = "lightblue",fill = "steelblue")+
  geom_density(mapping = aes(LogRatio), color = "red", size = 1, lty = 2)+
  geom_vline(xintercept = 0, size = 1, lty = 2)+
  labs(title = "Log_Ratio between real value and estimation",
       x = "Log(Estimation)- Log(True_NC)", y = "Density") +
  theme_bw()

NC_plot_20 = grid.arrange(grobs = list(plt1,plt2,plt3,plt4), ncol=2,
                          top = "N = 20")



#Plots for larger sample size N = 100
steps = 10^4
NC_est = rep(0,steps)
set.seed(1234567890)
for (i in 1:steps) {
  NC_est[i] = ImpSmler(X = X,rprop = rbeta,a = C*mean(X),b = C*(10-mean(X)),
                       logPost = logPosterior,
                       dprop = dbeta,N =  100,f = f)[1]
  
}

plt1 = ggplot() + 
  geom_histogram(mapping = aes(x = NC_est, y = ..density..),bins = 35, 
                 color = "black",fill = "red")+
  geom_density(mapping = aes(NC_est), color = "yellow", size = 1, lty = 2)+
  labs(title = "Log(NC) estimation",
       x = "Log(NC)", y = "Density") +
  theme_bw()

plt2 = ggplot() + 
  geom_histogram(mapping = aes(x = exp(NC_est), y = ..density..),bins = 35, 
                 color = "black",fill = "red")+
  scale_x_continuous(trans="log10")+
  geom_density(mapping = aes(exp(NC_est)), color = "yellow", size = 1, lty = 2)+
  labs(title = "NC estimation",
       x = "NC", y = "Density") +
  theme_bw()


#Plot the difference between the real value and the estimation
ratio = exp(NC_est)/exp(NC_integral)# should be close to 1
ratio_100 = (mean(ratio))
LogRatio = NC_est - NC_integral #should be close to 0
Logratio_100 =(mean(LogRatio))

#Plot for the Normal Ration 
plt3 = ggplot() + 
  geom_histogram(mapping = aes(x = ratio, y = ..density..),bins = 35, 
                 color = "lightblue",fill = "steelblue")+
  geom_density(mapping = aes(ratio), color = "red", size = 1, lty = 2)+
  geom_vline(xintercept = 1, size = 1, lty = 2)+
  labs(title = "Ratio between real value and estimation",
       x = "Estimation/True_NC", y = "Density") +
  theme_bw()

#Plot for the Log Ratio
plt4 = ggplot() + 
  geom_histogram(mapping = aes(x = LogRatio, y = ..density..),bins = 35, 
                 color = "lightblue",fill = "steelblue")+
  geom_density(mapping = aes(LogRatio), color = "red", size = 1, lty = 2)+
  geom_vline(xintercept = 0, size = 1, lty = 2)+
  labs(title = "Log_Ratio between real value and estimation",
       x = "Log(Estimation)- Log(True_NC)", y = "Density") +
  theme_bw()

NC_plot_100 = grid.arrange(grobs = list(plt1,plt2,plt3,plt4), ncol=2, 
                           top = "N=100")

#plot for large simple size N = 1000
steps = 10^4
NC_est = rep(0,steps)
set.seed(1234567890)
for (i in 1:steps) {
  NC_est[i] = ImpSmler(X = X,rprop = rbeta,a = C*mean(X),b = C*(10-mean(X)),
                       logPost = logPosterior,
                       dprop = dbeta,N =  1000,f = f)[1]
  
}

plt1 = ggplot() + 
  geom_histogram(mapping = aes(x = NC_est, y = ..density..),bins = 35, 
                 color = "black",fill = "red")+
  geom_density(mapping = aes(NC_est), color = "yellow", size = 1, lty = 2)+
  labs(title = "Log(NC) estimation",
       x = "Log(NC)", y = "Density") +
  theme_bw()

plt2 = ggplot() + 
  geom_histogram(mapping = aes(x = exp(NC_est), y = ..density..),bins = 35, 
                 color = "black",fill = "red")+
  scale_x_continuous(trans="log10")+
  geom_density(mapping = aes(exp(NC_est)), color = "yellow", size = 1, lty = 2)+
  labs(title = "NC estimation",
       x = "NC", y = "Density") +
  theme_bw()


#Plot the difference between the real value and the estimation
ratio = exp(NC_est)/exp(NC_integral)# should be close to 1
ratio_1000 = (mean(ratio))
LogRatio = NC_est - NC_integral #should be close to 0
Logratio_1000 = (mean(LogRatio))

#Plot for the Normal Ration 
plt3 = ggplot() + 
  geom_histogram(mapping = aes(x = ratio, y = ..density..),bins = 35, 
                 color = "lightblue",fill = "steelblue")+
  geom_density(mapping = aes(ratio), color = "red", size = 1, lty = 2)+
  geom_vline(xintercept = 1, size = 1, lty = 2)+
  labs(title = "Ratio between real value and estimation",
       x = "Estimation/True_NC", y = "Density") +
  theme_bw()

#Plot for the Log Ratio
plt4 = ggplot() + 
  geom_histogram(mapping = aes(x = LogRatio, y = ..density..),bins = 35, 
                 color = "lightblue",fill = "steelblue")+
  geom_density(mapping = aes(LogRatio), color = "red", size = 1, lty = 2)+
  geom_vline(xintercept = 0, size = 1, lty = 2)+
  labs(title = "Log_Ratio between real value and estimation",
       x = "Log(Estimation)- Log(True_NC)", y = "Density") +
  theme_bw()

NC_plot_1000 = grid.arrange(grobs = list(plt1,plt2,plt3,plt4), ncol=2,top = "N=1000")

#Data frame with the ratios for differente sample size
ratios = c(ratio_20,ratio_100,ratio_1000)
Log_ratios = c(LogRatio_20,Logratio_100,Logratio_1000)
df = data.frame(ratios,Log_ratios)
rownames(df) = c("N = 20", "N = 100", "n = 1000")
colnames(df) = c("ratio", "Log_ratio")
print(df)



