
#Libraries needed
library(mvtnorm)
library(ggplot2)
library(LaplacesDemon)
library(gridExtra)

source(file = "Functions.R")


#Users input


N_standard = 100 # Total number of particles due to the computational budget
N_bias = 1000
M = N_standard/N_bias #Number of Independent Samplers
dim = 1000 #Dimensions of the problem 

#Parameters we need for the proposals
thetas = rep(c(0.5,1),dim/2)
Sigma = diag(dim)*3
theta_targ = rep(0,dim)

#Run the BIAS CORRECTION METHOD just for K = 1
#The tradeoff is N_standard = N_bias*M
start_time <- Sys.time()
set.seed(1234567890)
logNC_corr = CorrectionMethod(M = M,N = N_bias,dim = dim)
end_time <- Sys.time()
end_time - start_time


start_time <- Sys.time()
set.seed(1234567890)
NC_estim = ImpSmler(rprop = rmvt,a = thetas,b = Sigma,df = 100,
                    dtarget = dmvnorm,c = theta_targ,d = Sigma,
                    dprop = dmvt,N = N_standard)

end_time <- Sys.time()
end_time - start_time

df_Eval = EvalFun(TrueLog = 0,LogLik_Standard = NC_estim,LogLik_BC = logNC_corr)
df_Eval

#Now we run it for many times in order to evaluate the distribution of NC
K = 1000
res = rep(0,K)


start_time <- Sys.time()
set.seed(1234567890)
#just an apply to store all the independent NC estimations
res = sapply(1:K, function(x) CorrectionMethod(M = M,N = N_bias,dim = dim))
end_time <- Sys.time()
end_time - start_time

plot(density(res))


res_Standard = rep(0,K)
start_time <- Sys.time()
set.seed(1234567890)

res_Standard = sapply(1:K, function(x) ImpSmler(rprop = rmvt,
                                                a = thetas,b = Sigma,df = 100,
                                                dtarget = dmvnorm,c = theta_targ,
                                                d = Sigma,dprop = dmvt,N = N_standard))

end_time <- Sys.time()
end_time - start_time

plot(density(res_Standard))


###################EVALUATE THE BIAS CORRECTION METHOD ########################
#We now evaluate the Bias correction method for different M values
#We want to keep the balance between the compuational budget thought
N_standard = 100000 # Total number of particles due to the computational budget
MGrid = c(10,100,200,500,1000,5000,10000,50000)
N_bias = N_standard/MGrid #keep Balance between the standard and Bias correction
dim = 20 #Dimensions of the problem 

#Parameters we need for the proposals
thetas = rep(c(0.5,1),dim/2)
Sigma = diag(dim)*3
theta_targ = rep(0,dim)

K = length(MGrid)

res1 = rep(0,K)
set.seed(1234567890)
res1 = sapply(1:K, function(x) CorrectionMethod(M = MGrid[x],
                                                N = N_bias[x],dim = dim))

#plot Mgrid vs the corrected outputs
ggplot()+
  geom_line(mapping = aes(x = MGrid,y = res))+
  geom_hline(yintercept = 0)

#We can see from the plot above, that values more than M = 1000 are not accurate
#those values start to move away from zero
#Therefore it makes sense to search for more points from 10 to 1000 in order
#to find the optimal M


N_standard = 100000 # Total number of particles due to the computational budget
MGrid = seq(10,500,10)
N_bias = round(N_standard/MGrid) #keep Balance between the standard and Bias correction
dim = 20 #Dimensions of the problem 

#Parameters we need for the proposals
thetas = rep(c(0.5,1),dim/2)
Sigma = diag(dim)*3
theta_targ = rep(0,dim)

K = length(MGrid)

res = rep(0,K)
set.seed(1234567890)
res = sapply(1:K, function(x) CorrectionMethod(M = MGrid[x],
                                                N = N_bias[x],dim = dim))

#plot Mgrid vs the corrected outputs
ggplot()+
  geom_point(mapping = aes(x = MGrid,y = exp(res)))+
  geom_hline(yintercept = 1)

#The plot is NOT clear for which M is the optimal one
#we also calculate the residuals for each estimator
residuals = abs(exp(res) - 1)

################### MSE ###################################################
#we can run both the Standard method and the bias correction method for MSE
N_standard = 100000 # Total number of particles due to the computational budget
M = 100
N_bias = N_standard/M #keep Balance between the standard and Bias correction
dim = 20 #Dimensions of the problem 

#Parameters we need for the proposals
thetas = rep(c(0.5,1),dim/2)
Sigma = diag(dim)*3
theta_targ = rep(0,dim)
K = 1000



#First the Standard Method
res_Standard = rep(0,K)
for (i in 1:K) {
  

res_Standard[i] = ImpSmler(rprop = rmvt,a = thetas,b = Sigma,df = 100,
                                          dtarget = dmvnorm,c = theta_targ,
                                           d = Sigma,dprop = dmvt,N = N_standard)

print(i)
}

#mean of NC = 1.001
#mean of logNC = 0.00089
#MSE = 0.00038


#Now the correction method
set.seed(1234567890)
for (i in 1:1000) {
  
  res[i] = CorrectionMethod(M = M,
                         N = N_bias,dim = dim)
  print(i)
}

#We can estimate the MSE
#Mean of NC = 0.999
#MSE 
mean((exp(res) -1)^2) # 0.0004 super close to 0



