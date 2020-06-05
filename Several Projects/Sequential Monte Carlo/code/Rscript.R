
#library(ggplot2)
#library(gridExtra)
#library("nimble")
#library(latex2exp)
#library(knitr)
#library(kableExtra)

################## IMPLEMENTATION OF MODELS ####################################
#Transition Model

TransModel = function(size,mu , sd_trans){
  
  #Sample from the normal adding each time the constant
  smple = 0.9*mu + rnorm(size,mean = 0,sd_trans)
  return(smple)
  
}

#Emission Model
#Every we say above holds also for the emission model
EmissModel = function(size, mu , sd_emi){
  
  
  smple = mu + rnorm(n = size, mean = 0, sd = sd_emi)
  return(smple)
}


#The initial model
InitModel = function(init,size,...){
  
  res = init(size,...)
  return(res)
  
}

#Function to simulate the SSM 
#In this function 
SSM_simulation = function(steps,size,InitMl,sd_trans,sd_emi,...){
  
  #generate from the initial model
  #Use the function IniModel in order to use it for different models
  initValues = InitModel(init = InitMl,size, ...)
  
  True_state = rep(0,steps)
  True_state[1] = initValues 
  
  Observations = rep(0,steps)
  Observations[1] = EmissModel(size = 1,True_state[1],sd_emi = sd_emi)
  
  #a for loop to generate from the transition and emission model
  for (i in 2:steps) {
    
    True_state[i] = TransModel(size = size,mu = True_state[i-1],
                               sd_trans = sd_trans)
    
    Observations[i] = EmissModel(size = size,True_state[i],sd_emi = sd_emi)
  }
  
  return(list(True_state,Observations))
  
  
}

#function to evaluate the particles( compute the weights)
#Here we can caclulate the densities of the three normal and average them
#We also have to normalize the weights
evalParticles = function(Observation,particls,s,log){
  
  dens = dnorm(x = Observation,mean = particls,sd = s,log = log)
  
  
  #return the density(Unormalized)
  return(dens)
  
}

##############PARTICLE FILTER FOR SSM ##########################################
#That implementation can be used for predicting steps
# PartFIlter = function(steps,#How many steps we will do
#                       partSize,#How many particles
#                       sd_trans, #sd of transition model
#                       sd_emi,#sd of emition model
#                       observ,
#                       log = TRUE)#observation to evaluate
# {
#   #We store the particles at each step
#   particles = matrix(0,nrow = steps,ncol = partSize)
#   
#   #Initially generate partSize of particles
#   N = ncol(particles)
#   init_particles  = rnorm(partSize,0,10)
#   predictions = rep(0,steps) #store the predictions
#   
#   UnormW = rep(0,steps) #Here the marginal likelihood for each step
#   Weights = matrix(0,nrow = steps,ncol = partSize) #store the weights
#   logNC = rep(0,steps) #store the logLikelihood for each step
#   
#   
#   if (log == FALSE) {
#     
#     
#     #We now do the same for ALL steps, each time the same procedure
#     for (i in 1:(steps)) {
#       
#       if( i == 1){
#         #In this method, we first evaluate the weights, using the function evalParticles
#         #We calculate the weights for each particle in every step
#         #This will give us the probability of my predictions
#         
#         
#         #Now we use the trainsition model
#         #First we sample from the particles that we already have,
#         #But we use the weights in order to sample the most probable
#         #In the initial step, all weights are equal
#         
#         temp_particles = sample(x = init_particles,size = N,replace = TRUE,
#                                 prob = rep(1,N))
#         
#         #After that we sent them to the transition model in order to make a prediction    
#         particles[1,] = TransModel(size = N,mu = temp_particles,
#                                    sd_trans = sd_trans)
#         
#         #finally we update the weights    
#         Weights[1,] = evalParticles(Observation = observ[1],
#                                     particls = particles[1,],
#                                     s = sd_emi,log) 
#         
#         UnormW[1] = mean(Weights[1,])
#         Weights[1,] = Weights[1,]/sum(Weights[1,])
#         
#         predictions[1] = sum(Weights[1,]*particles[1,])
#         
#       }
#       else{
#         #So first we have to propagate the particles one step ahead
#         #We sample using the weights N particles in order to use the most likely
#         temp_particles = sample(x = particles[i-1,],size = N,replace = TRUE,
#                                 prob = Weights[i-1,])
#         
#         #We propagate those particles using the transition model 
#         particles[i,] = TransModel(size = N,mu = temp_particles,
#                                    sd_trans = sd_trans)
#         
#         #Now we update the weights for each particle
#         Weights[i,] = evalParticles(Observation = observ[i],
#                                     particls = particles[i,],
#                                     s = sd_emi,log)
#         
#         #Store the normalization constant of time step i
#         UnormW[i] = mean(Weights[i,])
#         
#         #Normalize the weights
#         Weights[i,] = Weights[i,]/sum(Weights[i,])
#         
#         #predictions[i] = sum(Weights[i,]*particles[i,])
#       }
#     }  
#     return(prod(UnormW))
#   }  
#   else{
#     
#     #We now do the same for ALL steps, each time the same procedure
#     for (i in 1:(steps)) {
#       
#       if(i == 1){
#         
#         temp_particles = sample(x = init_particles,size = N,replace = TRUE,
#                                 prob = rep(1,N))
#         
#         
#         
#         #After that we sent them to the transition model in order to make a prediction    
#         particles[1,] = TransModel(size = N,mu = temp_particles,
#                                    sd_trans = sd_trans)
#         
#         #finally we update the weights    
#         Weights[1,] = evalParticles(Observation = observ[1],
#                                     particls = particles[1,],
#                                     s = sd_emi,log)
#         
#         
#         #find the largest value to prevent any underflow
#         w_max = max(Weights[1,])
#         
#         #The new weights 
#         W = Weights[1,] - w_max
#         
#         logNC[1] = log(sum(exp(W))) + w_max - log(N)
#         
#         Weights[1,] = exp(W)/sum(exp(W))
#         
#         #predictions[1] = sum(Weights[1,]*particles[1,])
#         
#         
#         
#       } 
#       else{
#         #So first we have to propagate the particles one step ahead
#         #We sample using the weights N particles in order to use the most likely
#         temp_particles = sample(x = particles[i-1,],size = N,replace = TRUE,
#                                 prob = Weights[i-1,] )
#         
#         #We propagate those particles using the transition model 
#         particles[i,] = TransModel(size = N,mu = temp_particles,
#                                    sd_trans = sd_trans)
#         
#         
#         #Now we update the weights for each particle
#         Weights[i,] = evalParticles(Observation = observ[i],
#                                     particls = particles[i,],
#                                     s = sd_emi,log)
#         
#         #find the largest value to prevent any underflow
#         w_max = max(Weights[i,])
#         
#         #The new weights 
#         W = Weights[i,] - w_max
#         
#         logNC[i] = log(sum(exp(W))) + w_max - log(N)
#         
#         Weights[i,] = exp(W)/sum(exp(W))
#         
#         
#         #predictions[i] = sum(Weights[i,]*particles[i,])
#         
#       }
#     }  
#     return(sum(logNC))
#   }  
#   #The function returns the Normaliazation constant for each time step
#   
# }

###Another version of PartFilter ONLY FOR LOG LIKELIHOOD 
PartFIlter = function(steps,#How many steps we will do
                      partSize,#How many particles
                      sd_trans, #sd of transition model
                      sd_emi,#sd of emition model
                      observ,
                      log = TRUE)#observation to evaluate
{
  
  #We store the particles at each step
  particles = matrix(0,nrow = steps,ncol = partSize)
  N = ncol(particles)
  Weights = matrix(0,nrow = steps,ncol = partSize) #store the weights
  logNC = rep(0,steps) #store the logLikelihood for each step
  
  
  ##################INITIAL STEP ############################################
  #Initially generate partSize of particles
  init_particles  = rnorm(partSize,0,10)
  temp_particles = init_particles
  
  #Loop over the number of steps
  for (i in 1:steps) {
    
    #for the first step only(because it depends on the initial particles)
    if(i == 1){
      
      #STEP 1: RESAMPLING
      #resample with equal weights
      temp_particles = sample(x = init_particles,size = N,replace = TRUE,
                              prob = rep(1,N))
      
      #STEP 2: PROPAGATING
      #After that we sent them to the transition model in order to make a prediction    
      particles[1,] = TransModel(size = N,mu = temp_particles,
                                 sd_trans = sd_trans)
      
      #STEP 3: UPDATE THE WEIGHTS
      #finally we update the weights    
      Weights[1,] = evalParticles(Observation = observ[1],
                                  particls = particles[1,],
                                  s = sd_emi,log)
      
      
      #find the largest value to prevent any underflow
      w_max = max(Weights[1,])
      
      #The new weights 
      W = Weights[1,] - w_max
      
      #Log likelihood 
      logNC[1] = log(sum(exp(W))) + w_max - log(N)
      
      #Normalize the weights before starting again
      Weights[1,] = exp(W)/sum(exp(W))
      
      
      
      
    }
    else{
      
      #STEP 1: RESAMPLING
      
      #We sample using the weights N particles in order to use the most likely
      temp_particles = sample(x = particles[i-1,],size = N,replace = TRUE,
                              prob = Weights[i-1,] )
      
      #STEP 2: PROPAGATE
      #We propagate those particles using the transition model 
      particles[i,] = TransModel(size = N,mu = temp_particles,
                                 sd_trans = sd_trans)
      
      #STEP 3: UPDATE WEIGHTS
      #Now we update the weights for each particle
      Weights[i,] = evalParticles(Observation = observ[i],
                                  particls = particles[i,],
                                  s = sd_emi,log)
      
      
      #find the largest value to prevent any underflow
      w_max = max(Weights[i,])
      
      #The new weights 
      W = Weights[i,] - w_max
      
      logNC[i] = log(sum(exp(W))) + w_max - log(N)
      
      #Normalize the weights
      
      Weights[i,] = exp(W)/sum(exp(W))
      
    }
  }  
  
  #The function returns the Normaliazation constant(log likelihood)
  return(sum(logNC))
}  


######################### KALMAN FILTER #######################################
#Be carefull that Kalman filter works with the variance 
#Therefore it is better to give the variances as an input
#A second implementation of KALMAN FILTER
KalmanFilter = function(A,B,u,C,Q,R,mu_0,Sigma_0,y){
  
  steps = length(y)
  #all the vectors we need for storing the result
  K =rep(0,steps) #Kalman gain
  Sigmas = rep(0,steps) #Covariance of X
  CovY = rep(0,steps) #Covariance of y
  error = rep(0,steps) #error between predictions 
  YPred = rep(0,steps) #prediction of the observations
  mu = rep(0,steps) #Expected values of X
  CovY0 = C*Sigma_0*t(C) + R #
  
  
  
  for(t in 1:(steps)){
    
    if(t ==1){
      mu_belief = A*mu_0
      Sigma_belief = A*Sigma_0*A + R
      YPred[1] =  C*mu_belief
      CovY[1] = Sigma_belief+Q
    }
    
    else{
      #first my beliefs without the noise
      mu_belief = A*mu[t-1]
      
      Sigma_belief = A*Sigmas[t-1]*A + R # KF uses covariances
      YPred[t] =  mu_belief
      CovY[t] = Sigma_belief+Q
    }
    
    #calculate the Kalman gain
    
    K[t] = Sigma_belief/CovY[t] # KF uses covariances
    
    #Prediction
    
    mu[t] = mu_belief +K[t]*(Obs_states[t] - YPred[t])
    Sigmas[t] = (1-K[t])*Sigma_belief
  }
  
  Lt = CovY
  
  
  y_error = (y - YPred)^2
  logLikelihood = -((steps)/2)*log(2*pi) + sum((-0.5*log(abs((Lt))) - (0.5)*(y_error/Lt)))
  return("Loglikelihood" = logLikelihood )
  
  
}

########################### ANOTHER IMPLEMENTATION ############################
# KalmanFilter = function(A,#coef of transition model
#                         B,#coef of action
#                         u,#action
#                         C,#coef of emission model
#                         Q,#VARIANCE of transition
#                         R,#VARIANC of emission
#                         mu_0,#Inital expected value
#                         Sigma_0,#Initial variance
#                         y) #observations
# {
#   
#   steps = length(y)
#   #all the vectors we need for storing the result
#   K =rep(0,steps) #Kalman gain
#   Sigmas = rep(0,steps+1) #Covariance of X
#   CovY = rep(0,steps+1) #Covariance of y
#   YPred = rep(0,steps+1) #prediction of the observations
#   mu = rep(0,steps+1) #Expected values of X
#   CovY0 = C*Sigma_0*t(C) + R 
#   
#   #for loop getting the filtering distribution 
#   for(t in 1:(steps)){
#     
#     #Recostruction step
#     if(t ==1){
#       K[1] = Sigma_0*t(C)/CovY0
#       YPred[1] = C*mu_0
#       mu[1] = mu_0 + K[1]*(Obs_states[1] - C*mu_0)
#       Sigmas[1] = Sigma_0 - K[1]*Sigma_0
#     }
#     else{
#       K[t] = Sigmas[t]*t(C)/CovY[t]
#       mu[t] = mu[t] + K[t]*(Obs_states[t] - C*mu[t])
#       Sigmas[t] = Sigmas[t] - K[t]*C*Sigmas[t]
#     }
#     
#     
#     #Prediction Step
#     mu[t+1] = A*mu[t] +B*u
#     Sigmas[t+1] = A*Sigmas[t]*A + Q
#     CovY[t+1] = C*Sigmas[t+1]*t(C) + R
#     YPred[t+1] =  C*mu[t+1]
#     
#   }
#   error = abs(y[-1] - YPred[-c(1,steps+1)])
#   
#   
#   Lt = CovY[-c(1,steps+1)]
#   Err = (error^2)/Lt
#   
#   
#   logLikelihood = -((steps)/2)*log(2*pi) - 1/2*sum((log((abs(Lt))) + Err))
#   
#   return(list("Predictions"= mu[-(steps+1)],"CovX" = Sigmas[-(steps+1)],
#               "CovY" = CovY[-(steps+1)], "Ypred" = YPred[-(steps+1)],
#               "LogLikelihood "= logLikelihood))
#   
# }



######################### PLOT FUNCTIONS ######################################



############################# BIAS CORRECTION METHOD ##########################
#we now that for multi - dimensions using IS a bias is introduced.
#In this paper, we introduce a bias correction method in order to reduce this 
#biasness, without making the problem more complex 


CorrectionMethod_Static = function(M,N,dim){
  
  #We now implement the bias correction method
  #We will split the tasks in several (M) IS 
  #Computation cost N_IS = N_bias * M
  
  logNC = rep(0,M)
  
  # for (m in 1:M) {
  #   logNC[m] = ImpSmler(rprop = rmvt,a = thetas,b = Sigma,df = 100,
  #                     dtarget = dmvnorm,c = theta_targ,d = Sigma,
  #                     dprop = dmvt,N = N)
  # 
  # }
  
  logNC = sapply(1:M,FUN = function(x) ImpSmler(rprop = rmvt,a = thetas,
                                                b = Sigma,df = 100,
                                                dtarget = dmvnorm,c = theta_targ,
                                                d = Sigma,
                                                dprop = dmvt,N = N))
  
  #Now it is the time for the correction
  #We add half of the variance into the sample
  bias_corection = logNC + (var(logNC))/2
  
  #The final estimator for Normalization constant is the mean of that new sample
  return(mean(bias_corection))
  
  
}



#Bias Correction method for SSM

CorrectionMethod_PF = function(M,#The number of Different Independent PF
                               Observations, #Observations we already have
                               Q, #transition variance
                               R, #emissition variance
                               N # Particles used (N_stand = N*M cost)
)
{
  
  
  #We now implement the bias correction method
  #We will split the tasks in several (M) PF
  #NOTE : Computation cost N_IS = N_bias * M
  steps = length(Observations)
  
  logNC = rep(0,M)
  
  
  logNC = sapply(1:M,FUN = function(x) PartFIlter(steps = steps,
                                                  partSize = N,
                                                  sd_trans = Q,
                                                  sd_emi = R,
                                                  observ =Observations,
                                                  log = TRUE))
  
  var_cor = var(logNC)
  
  #Now it is the time for the correction
  #We add half of the variance into the sample
  bias_corection = logNC + var_cor/2
  
  
  #The final estimator for Normalization constant is the mean of that new sample
  return(mean(bias_corection))
  
  
}


#Function for evaluating the Bias correction method
# EvalFun = function(TrueLog,LogLik_Standard,LogLik_BC){
#   
#   #Bias for the standard PF
#   Bias_Standard = mean(LogLik_Standard) - TrueLog
#   
#   #Bias for the Bias correction method
#   Bias_BC = mean(LogLik_BC) - TrueLog
#   
#   #Efficiency of SM
#   Efficiency_Standard = var(LogLik_Standard)
#   
#   #Efficieny of BC
#   Efficiency_BC = var(LogLik_BC)
#   
#   
#   #MSE for Standard
#   MSE_Standard = mean((LogLik_Standard - TrueLog)^2)
#   
#   #MSE for BC
#   MSE_BC = mean((LogLik_BC - TrueLog)^2)
#   
#   #Second way for MSE
#   #MSE_Standard2 = Efficiency_Standard + (Bias_Standard)^2
#   #MSE_BC2 = Efficiency_BC + (Bias_BC)^2
#   
#   df = t(data.frame(c(Bias_Standard,Bias_BC),c(Efficiency_Standard,Efficiency_BC),
#                     c(MSE_Standard,MSE_BC)))
#   
#   colnames(df) =  c("Standard","Bias_Correction")
#   rownames(df) = c("Bias","Efficiency","MSE") 
#   
#   return(df)
#   
# }

#Function for evaluating the Bias correction method
EvalFun = function(TrueLog, #The true value of the LogLik(KALMAN FILTER)
                   NC# a df in which each col contains a sample of diff M.
)
{
  
  #Bias
  Bias = apply(NC, 2, mean) - TrueLog
  
  
  #Efficieny
  Efficiency = apply(NC, 2, var)
  
  #MSE
  MSE = apply((NC - TrueLog)^2,2,mean)
  
  
  df = rbind(Bias,Efficiency,MSE)
  
  colnames(df) =  colnames(NC)
  rownames(df) = c("Bias","Efficiency","MSE") 
  
  return(df)
  
}




##########################USE A PACKAGE FOR PARTICLE FILTER ###################

# 
#   stateSpaceCode <- nimbleCode({
#     sigPN ~ dnorm(0, 1)
#     sigOE ~ dnorm(0, 1)
#     x[1] ~ dnorm(0, sd = 10)
#     y[1] ~ dnorm( x[1], sd  = sigOE)
#     for (i in 2:t) {
#       x[i] ~ dnorm(0.9 * x[i - 1] , sd = sigPN)
#       y[i] ~ dnorm( x[i], sd = sigOE)
#     }
#   })
# 
#   ## define data, constants, and initial values
#   data <- list(
#     y = Obs_states
#   )
#   constants <- list(
#     t = length(Obs_states)
#   )
# 
#   inits <- list(
#     sigPN = 1,
#     sigOE = 1
#   )
# 
#   ## build the model
#   stateSpaceModel <- nimbleModel(stateSpaceCode,
#                                  data = data,
#                                  constants = constants,
#                                  inits = inits,
#                                  check = FALSE)
# 
#   bootstrapFilter <- buildBootstrapFilter(stateSpaceModel, nodes = 'x')
#   compiledList <- compileNimble(stateSpaceModel, bootstrapFilter)
# ParNimble <- compiledList$bootstrapFilter$run(Nparticles)
#ParNimble







################### SIMULATE THE DATA ##############################
#Simulate the data
#set the seed for same results

steps =  1000 #number of steps
Nparticles = 1000#number of particles used

#Simulate steps 
set.seed(12345)
simulation_SSM = SSM_simulation(steps = steps,size = 1,InitMl = rnorm,
                                sd_trans = 1,1,0,10)

true_states = simulation_SSM[[1]] #store the true positions
Obs_states = simulation_SSM[[2]]  #store the observations

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
#library(dlm)
#Kalman_dlm = dlm(FF = 1, V = 1, GG = 0.9, W = 1, m0 = 0, C0 = 100)

#loglik_dlm = -dlmLL(Obs_states, Kalman_dlm) -((steps)/2)*log(2*pi) #add the constant

#Run the Bias correction method 
K =  10000
#Nparticles = 1000


###############WITHOUT CORRECTION ################
NC_S = rep(0,K)

set.seed(12)
for (i in 1:K) {
  
  NC_S[i] = PartFIlter(steps = steps,partSize = Nparticles,sd_trans = 1,sd_emi = 1,
                       observ =Obs_states,log = TRUE)
  if(i %% 100==0) {
   
    # Print on the screen some message
    cat(paste0("iteration: ", i, "\n"))
  }


  
}



#write.csv(NC_S,file = "T5000_N1000_M1.csv")

#NC_S = NC_S[1:100]

 plt_S = ggplot()+
   geom_density(mapping = aes(NC_S- Kalman_LogLik),fill = "red" ,
                lty = 2, alpha = 0.8)+
   geom_vline(xintercept = 0,  lty = 2, size = 1)

 plt_S

length(which((NC_S - Kalman_LogLik)<0))/length(NC_S)

############## BIAS CORRECTION ##########################

M= 50
N_bc= round(Nparticles/M)

 NC_BC =rep(0,K)
 set.seed(12)
 for (i in 1:K) {
   
   NC_BC[i] = CorrectionMethod_PF(M = M,Observations = Obs_states,Q = Q,
                                  R = R, N = N_bc)
   
   if(i %% 100==0) {
     
     # Print on the screen some message
     cat(paste0("iteration: ", i, "\n"))
   }
   
 }

    
  # 
# 

plt_BC = ggplot()+
   geom_density(mapping = aes(NC_BC- Kalman_LogLik),fill = "yellow" ,
                lty = 2, alpha = 0.8)+
   geom_vline(xintercept = 0,  lty = 2, size = 1)
# 
 plt_BC


mean(NC_BC)

NC = read.csv("T1000_N1000_M(1,10,15,25).csv")
NC = cbind(NC[,c(1,2,3)],NC_BC,NC[,4])
colnames(NC) = c("Standard","M=10","M =15","M =20", "M =25")
write.csv(NC,"T1000_N1000_M(1,10,15,20,25).csv", row.names = FALSE)





















# 
# M = c(5,10,15,20,50)
# 
# NC_BC = matrix(0,nrow = K,ncol = length(M))
# set.seed(12)
# 
# for (i in 1:length(M)) {
#   
#   N_bc = round(Nparticles/M[i])
#   
#   print(M[i])
#   print(N_bc)
#   temp =  sapply(1:K,FUN = function(x) CorrectionMethod_PF(M = M[i],
#                                                            Observations = Obs_states,
#                                                            Q = Q,
#                                                            R = R,
#                                                            N = N_bc))
#   
#   NC_BC[,i] = temp
# 
#   
# }







