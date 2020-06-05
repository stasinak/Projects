
source(file = "Functions.R")

################### SIMULATE THE DATA ##############################
#Simulate the data
 #set the seed for same results

steps =  1000 #number of steps
Nparticles = 1000#number of particles used

#Simulate steps 
set.seed(12345) #DONT CHANGE THAT
simulation_SSM = SSM_simulation(steps = steps,size = 1,InitMl = rnorm,
                                sd_trans = 1,1,0,10)

true_states = simulation_SSM[[1]] #store the true positions
Obs_states = simulation_SSM[[2]]  #store the observations


####################### TRY DIFFERENT PACKAGESFOR KALMAN FILTER ################

#FKF Library#
# library(FKF)
# 
# dt  = matrix(0) #the intercept of the transition model
# ct = matrix(0) #the intercept of the emission model
# Zt = matrix(1) # the slope of emission
# Tt = matrix(0.9) # the slope of transition
# HHt = matrix(1)
# GGt = matrix(1)
# 
# #a0 = Obs_states[1] # Estimation of the first year flow
# a0 = 0
# P0 = as.matrix(100) # Variance of 'a0'
# 
# Kalman_fkf = fkf(a0 = a0,P0 = P0,dt = dt,ct = ct,Tt = Tt,Zt = Zt,HHt = HHt,
#                  GGt = GGt,yt = rbind(Obs_states))


####My Implementation
#User's input
A = 0.9 #coef of transition model
B = 0 #coef of action
u = 0 #action
C = 1 #coef of emission model
Q = 1 #transition VARIANCE
R = 1 #emissition VARIANCE

#Initial values
mu_0 = 0
Sigma_0 = 100 #initial VARIANCE

Kalman_LogLik = KalmanFilter(A = A,B = B,u = u,C = 1,Q = 1,R = 1,mu_0 = mu_0,
                      Sigma_0 = Sigma_0,y = Obs_states)



#DLM package
library(dlm)
Kalman_dlm = dlm(FF = 1, V = 1, GG = 0.9, W = 1, m0 = 0, C0 = 100)
loglik_dlm = -dlmLL(Obs_states, Kalman_dlm) -((steps)/2)*log(2*pi) #add the constant

loglik_dlm
Kalman_LogLik


########################### EVALUATING THE METHOD ##############################
#Some results until now
#For High dimensions(T =1000) and particle number low, the standard method seems
#to perform better. So we need more Particles 

#Moreover M playes SUPERIMPORTANT ROLE. Almost all the time small M gives 
#better results(close to 10).


###################### FIRST EVALUATION STEP  ##################################
#The first idea is to test the bias correction method for low dimensions


####################### POINT ESTIMATOR #######################################
#Sample from the particle Filter WITHOUT CORRECTION: 
set.seed(12)
partLog =  PartFIlter(steps = steps,partSize = Nparticles,sd_trans = 1,sd_emi = 1,
                      observ =Obs_states,log = TRUE)


partLog

#Now we will use the BIAS CORRECTION METHOD
M = 15
Q = 1
R = 1
#Nparticles 
N_bc = round(Nparticles/M)
N_bc
M

set.seed(12)
partLog_BC = CorrectionMethod_PF(M = M,Observations = Obs_states,Q = Q,
                                 R = R, N = N_bc)

#partLog
partLog_BC
partLog
Kalman_LogLik


#Store all the Particles and Kalman filters and Differencies

dif = c(partLog_BC,partLog)-Kalman_LogLik
ResTable = t(data.frame(c(partLog_BC,partLog),c(Kalman_LogLik),dif))
rownames(ResTable) = c("Particle","Kalman","Difference")
colnames(ResTable) = c("Correction","Standard")
ResTable









########################## DISTRIBUTION OF THE LOGNC ##########################
#in order to see if it is at least a good estimator for that.
#### T = 20, N = 1000, M = (5,7,10,15,20,25,40,50,100,200)



K = 100 #sample size

###############WITHOUT CORRECTION ################
NC_S = rep(0,K)

Nparticles = 1000
set.seed(12)
for (i in 1:K) {
  
  NC_S[i] = PartFIlter(steps = steps,partSize = Nparticles,sd_trans = 1,sd_emi = 1,
                       observ =Obs_states,log = TRUE)
  
  if(i %%100 ==0){
    cat(paste0("iteration:",i,"\n"))
  }
  
}



plt_S = ggplot()+
  geom_density(mapping = aes(NC_S- Kalman_LogLik),fill = "red" ,
               lty = 2, alpha = 0.8)+
  geom_vline(xintercept = 0,  lty = 2, size = 1)

plt_S

length(which(NC_S- Kalman_LogLik<0))/length(NC_S)


############## BIAS CORRECTION ##########################3
M= 5
N_bc= round(Nparticles/M)
K = 10

NC_BC = matrix(0,K,M)
set.seed(12)
for (i in 1:K) {
  
  NC_BC[i,] = CorrectionMethod_PF(M = M,Observations = Obs_states,Q = Q,log_parallel = TRUE,
                             R = R, N = N_bc,gamma = 1)
  
  
  
  if(i %% 100==0) {
    
    # Print on the screen some message
    cat(paste0("iteration: ", i, "\n"))
    #write.csv(NC_BC,"test.csv", row.names = FALSE)
  }
  
  
  
}


 
gammas = seq(0,1,0.01)
MSE = matrix(0,length(gammas),3)
for (g in 1:length(gammas)) {
  MSE[g,] = MSE_min(gamma = gammas[g],logZ = NC_BC,TrueLog = Kalman_LogLik)
  
}

  
ggplot()+geom_line(mapping = aes(x  = gammas, y = MSE))

NC_BC = NC_BC[1:200]
plt_BC = ggplot()+
  geom_density(mapping = aes(NC_BC- Kalman_LogLik),fill = "red" ,
               lty = 2, alpha = 0.8)+
  geom_vline(xintercept = 0,  lty = 2, size = 1)

plt_BC



#Run the Bias correction method 
K =  10000
Nparticles = 100
M = c(5,10)


NC_BC = matrix(0,nrow = K,ncol = length(M))
SMC_NM = matrix(0,nrow = K,ncol = length(M))
vars =  matrix(0,nrow = K,ncol = length(M))
set.seed(12)
for (m in 1:length(M)) {
  
  N_bc = Nparticles/M[m]
  
  print(M[m])
  print(N_bc)
  # temp =  sapply(1:K,FUN = function(x) CorrectionMethod_PF(M = M[i],
  #                                                     Observations = Obs_states,
  #                                                     Q = Q,
  #                                                     R = R,
  #                                                     N = N_bc))
  # 
  for (i in 1:K) {
    
    temp = CorrectionMethod_PF(M = M[m],Observations = Obs_states,Q = Q,
                               R = R, N = N_bc)
    
    vars[i,m] = temp[[1]]
    NC_BC[i,m] = temp[[2]]
    SMC_NM[i,m] = temp[[3]]
    
    if(i %% 100==0) {
      
      # Print on the screen some message
      cat(paste0("iteration: ", i, "\n"))
      #write.csv(NC_BC,"test.csv", row.names = FALSE)
    }
    
    
    
  }

}

  #We have already run the simulations and we have the data in a csv file
#for all different M values
NC = read.csv("Dim_1000/T1000_N1000_SMC(10X100)_BC(10,100)_SMC(10,100)_BC(15,67)_BC(20,50)_BC(25,40)_SMC(100).csv")
NC = NC[,]
NC = as.data.frame(NC)
a  = cbind(NC[,1:2],SMC_NM[,1],NC[,3],SMC_NM[,2],NC[,4],NC_S)
colnames(a) = c("SMC(10X10)","BC(5,20)","SMC(5,20)","BC(10,10)","SMC(10,10)","SMC(20)","SMC(10)")

#write.csv(NC,file = "T20_N1000_M(5,10,20,50,100,1).csv",row.names = FALSE)

#Our first evaluation is to plot LogNC vs M
all_Log = apply(NC, 2, mean)
#low_CI = apply(NC,2,FUN = quantile, probs = c(0.025,0.975))[1,]
#upper_CI = apply(NC,2,FUN = quantile, probs = c(0.025,0.975))[2,]




ggplot()+
  geom_point(mapping = aes(M,all_Log), size = 2)+
  geom_smooth(mapping = aes(M,all_Log))+
  #geom_smooth(mapping = aes(M,low_CI), color = "red", lty = 2)+
  #geom_smooth(mapping = aes(M,upper_CI),color = "red", lty = 2)+
  geom_hline(yintercept = loglik_dlm,lty = 2, color = "red")+
  labs(title = "Log Likelihood for different M", x = "M", y = "Log Likelihood")

#ANALYSIS: #The plot shows that while M increases the estimation becomes really unstable

#We can also evaluate some other statistics for all the results


#Calculate the Bias, Efficiency and MSE
df_Eval = EvalFun(TrueLog = loglik_dlm,NC = NC)
df_Eval

knitr::kable(x = df_Eval,caption = "Performance Criteria for Different M")

M = c(5,10,20,50,100,1)
Nparticles = 1000
N = Nparticles/M

df = NC - Kalman_fkf$logLik

df1 = df[,c(1,ncol(df))]
plt = Density_function(df1,N = N[c(1,ncol(df))],M[c(1,ncol(df))],rep(steps,ncol(NC)),TRUE)
plt = hist_function(df1,N = N[c(1,ncol(df))],M[c(1,ncol(df))],rep(steps,ncol(NC)),TRUE)
#plt = hist_function(df,N = N,M,rep(steps,ncol(NC)),TRUE)


#We could delete from the previous plot for more than 20 in order to get a more clear plot

NC1 = NC[,c(1:6)]
M1 = M[1:6]
all_Log = apply(NC1, 2, mean)
#low_CI = apply(NC1,2,FUN = quantile, probs = c(0.025,0.975))[1,]
#upper_CI = apply(NC1,2,FUN = quantile, probs = c(0.025,0.975))[2,]


ggplot()+
  geom_point(mapping = aes(M1,all_Log), size = 2)+
  geom_smooth(mapping = aes(M1,all_Log),se = FALSE)+
  geom_hline(yintercept = loglik_dlm,lty = 2, color = "red",size = 1)+
  labs(title = "Log Likelihood for different M", x = TeX("$log\\hat{Z} - logp(y_{1:T})$"), y = "Log Likelihood")

#As we can seen more clear now, for low M the estimatorion is really close to the true value
#A value close to 7 it is optimal here.
#It is a low dimensional problem, so it makes sense that we do not need many different independent PF to run
#We will choose 7

#Now we have to plot histogramms and densities bettween them
#First we have to plot the densities between N_S = N_bs*M for the computational cost
#We will choose ony the columns for M = 1 and M = 7


df_dif = NC[,c(1,3)] - loglik_dlm
plt = Density_function(Norm_matrix = df_dif,N = c(1000,143),M = c(1,7),Dim = rep(steps,2),2) + scale_x_continuous(limits = c(-5,5))



plt = hist_function(Norm_matrix = df_dif,N = c(1000,143),M = c(1,7),Dim = rep(steps,2),2)

hist_function(Norm_matrix = df_dif[,1],N = 1000,M = 1,Dim = steps,1)
Density_function(Norm_matrix = df_dif[,1],N = 1000,M = 1,Dim = steps,1)


#It is also suggested to compare N*M for Bias and N for Standard

k =10000
NC_S = rep(0,K)

set.seed(12)
for (i in 1:K) {
  
  NC_S[i] = PartFIlter(steps = steps,partSize = Nparticles,sd_trans = 1,sd_emi = 1,
                       observ =Obs_states,log = TRUE)
  
  if(i%%100 ==0){
    cat(paste0("iteration:",i,"\n"))
  }
  
}

write.csv(x = NC_S,"T20_N1000_M1_N143.csv")
df_dif1 = cbind(NC_S,NC[,3]) - loglik_dlm
plt = Density_function(Norm_matrix = df_dif1,N = c(143,143),M = c(1,7),Dim = rep(steps,2),2)

#And print some statistics between them
#Calculate the Bias, Efficiency and MSE

df_Eval1 = EvalFun(TrueLog = loglik_dlm,NC = cbind(NC_S,NC[,3]))
df_Eval1


######################Trials ###################3

K = 10000
NC_S = rep(0,K)


set.seed(12)
for (i in 1:K) {
  
  NC_S[i] = PartFIlter(steps = steps,partSize = Nparticles,sd_trans = 1,
                       sd_emi = 1,
                       observ =Obs_states,log = TRUE)
  
  #print(i)
  
}
