library(ggplot2)
library(gridExtra)
#library("nimble")
library(latex2exp)
library(knitr)
library(kableExtra)

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



####################### IMPORTANCE SAMPLER FOR STATIC PROBLEM ##################



#Generate from the proposal
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

#function to calculate the density of the target distribution
dTarget = function(target,X,...){
  
  #compute the density of the sample for the target distribution
  dens = target(X,...)
  
  return(dens)
}


#IS in order to approximate the logPosterior
#The function is one run of the Importance Sampler

ImpSmler = function(rprop,#generate from the proposal
                    a, b, df, #parameters of the proposal
                    dtarget, #density of the target for evaluating 
                    c,d, #parameters of the target density 
                    dprop, #Proposal dens for the evaluation
                    N) # Samples to generate
{ 
  
  #STEP 1: Generate from the
  #proposal distribution sample of size N
  U = rProp(rprop,N,a,b,df)
  
  #STEP 2: Compute the importance weights(unormalized)
  #compute the importance (unormalized) weights
  logW = dtarget(U,c,d,log = TRUE)- dprop(U,a,b,df,log = TRUE)
  
  #find the largest value to prevent any underflow
  w_max = max(logW)
  
  #The new weights 
  W = logW - w_max
  
  logNC = log(sum(exp(W))) + w_max - log(N)
  
  
  #normalize the weights
  #norm_W  = exp(W)/sum(exp(W))
  
  # importance sampling estimate
  #IS_estimator = sum(f(U)*norm_W)
  
  return(logNC)
  
}

######################### PLOT FUNCTIONS ######################################
#Function for plotting Histograms
theme_set(theme_minimal(base_family = "serif"))
hist_function = function(Norm_matrix,#matrix of normalization constant's
                         N,#vector of Par used for each column
                         M,#Number of independent PF run
                         Dim,#Steps of the data
                         Nplots, #Number of different plots
                         my_color,#vector of colors the user wants
                         Xrange #Vector for extending x axis
                         ){
  
  if(Nplots == 1){
    
    
    lims = c(min(Norm_matrix) - Xrange[1],
             max(Norm_matrix) + Xrange[2])
    iter = length(Norm_matrix)
    
      ggplot() + 
        geom_histogram(mapping = aes(x = Norm_matrix, fill = ..count..),
                       bins = 60)+
        #color = "black",fill = "lightblue", alpha = 0.8)+
        #geom_density(mapping = aes(x = Norm_matrix, y = ..count..), color = "black", size = 0.5, 
         #             lty = 2)+
        geom_vline(xintercept = 0,  lty = 2, size = 0.5)+
        scale_y_continuous(name = "Count") +
        scale_x_continuous(breaks = c(0,seq(round(lims[1]),round(lims[2]),Xrange[3])),
                           limits = lims)+
        scale_fill_gradient("Count", low = my_color[1], high = my_color[2])+
        labs( title = paste("Log Likelihood estimation"),
              subtitle = paste("N =",N,"M =",M,"T = ",Dim),
              x = TeX("$\\log \\hat{ Z} - \\log p(y_{1:T})$"), y = "Density")+
  
      theme(
          plot.title = element_text(hjust = 0.5, size = 12,colour = "black"),    # Center title position and size
          plot.subtitle = element_text(hjust = 0.5, colour = "Black"),            # Center subtitle
          legend.position = "None"# move caption to the left
        )
      
    
    
  }
  
  else{
  
  
    plt_list = list()
    
    for (i in 1:Nplots) {
      Norm_con = Norm_matrix[,i]
      iter = length(Norm_con)
      
      lims = c(min(Norm_con) - Xrange[1],
               max(Norm_con) + Xrange[2])
      
      
      temp = eval(substitute( 
        ggplot() + 
          geom_histogram(mapping = aes(x = Norm_con, fill = ..count..),bins = 60)+
          #geom_density(mapping = aes(x = Norm_con, y = ..count..), color = "black", size = 0.5, 
            #           lty = 2)+
                         #color = "black",fill = "lightblue", alpha = 0.8)+
          # geom_density(mapping = aes(Norm_con), color = "yellow", size = 1, 
          #              lty = 2)+
          geom_vline(xintercept = 0,  lty = 2, size = 0.5)+
          scale_y_continuous(name = "Count") +
          scale_x_continuous(breaks = c(seq(round(lims[1]),round(lims[2]),Xrange[3]),0),
                             limits = lims)+
          scale_fill_gradient("Count", low = my_color[1], high = my_color[2])+
          labs( subtitle = paste("N =",N[i],"M =",M[i],"T = ",Dim[i]),
                x = TeX(("$\\log \\widehat{ Z} - \\log p(y_{1:T})$")), y = "Density"))
        #scale_x_continuous( limits=c(min_v-1, max_v+1)))
        ,list(i = i)) +
        theme(
          plot.title = element_text(hjust = 0.5, size = 14,colour = "black"),    # Center title position and size
          plot.subtitle = element_text(hjust = 0.5, colour = "black"),            # Center subtitle
          legend.position = "None"# move caption to the left
        )
      plt_list[[i]] = temp
      
    }
  
    grid.arrange(do.call("arrangeGrob", c(plt_list, ncol = 2 )),
               top = paste("Log Likelihood estimation"))
  }
}

#Function for ploting densities
Density_function = function(Norm_matrix,#matrix of normalization constant's
                         N,#vector of Par used for each column
                         M,#Number of independent PF run
                         Dim,#Steps of the data
                         Nplots,#Number of different plots
                         my_color, #color that user prefers
                         Xrange #Vector for extending x axis
                         ){
  
  
  
  if (Nplots ==1) {
    
    lims = c(min(Norm_matrix) - Xrange[1],
               max(Norm_matrix) + Xrange[2])
    
    iter = length(Norm_matrix)
    ggplot() + 
      geom_density(mapping = aes(Norm_matrix),fill = my_color ,
                   lty = 2, alpha = 0.8)+
      geom_vline(xintercept = 0,  lty = 2, size = 0.5)+
      scale_x_continuous(breaks = c(0,seq(round(lims[1]),round(lims[2]),Xrange[3])),limits = lims)+
      labs( title = paste("Log Likelihood estimation for",iter,"iterations"),
            subtitle = paste("N =",N,"M =",M,"T = ",Dim),
            x = TeX("$log\\hat{Z} - logp(y_{1:T})$"), y = "Density")+
      theme(
        plot.title = element_text(hjust = 0.5, size = 14,colour = "black"),    # Center title position and size
        plot.subtitle = element_text(hjust = 0.5, colour = "red")            # Center subtitle
      )
    
    
  }else{
  
    plt_list = list()
    
    
    
    for (i in 1:Nplots) {
      Norm_con = Norm_matrix[,i]
      iter = length(Norm_con)
      
      lims = c(min(Norm_con) - Xrange[1],
               max(Norm_con) + Xrange[2])
      
      
      temp = eval(substitute( 
        ggplot() + 
          geom_density(mapping = aes(Norm_con),fill = my_color ,
                       lty = 2, alpha = 0.8)+
          geom_vline(xintercept = 0,  lty = 2, size = 0.5)+
          scale_x_continuous(breaks = c(seq(round(lims[1]),round(lims[2]),Xrange[3]),0),
                             limits = lims)+
          labs( subtitle = paste("N =",N[i],"M =",M[i],"T = ",Dim[i]),
                x = TeX("$\\textit{log}\\widehat{Z} - \\textit{log p(y_{1:T})}$"),
                y = "Density"))
        ,list(i = i))+
        theme(
          plot.title = element_text(hjust = 0.5, size = 14,colour = "black"),    # Center title position and size
          plot.subtitle = element_text(hjust = 0.5, colour = "red")            # Center subtitle
    
        )
      plt_list[[i]] = temp
      
    }
    
    grid.arrange(do.call("arrangeGrob", c(plt_list, ncol=2)),
                 top = paste("Log Likelihood estimation for",iter,"Iterations"))
  }  
}



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
                               N, # Particles used (N_stand = N*M cost)
                               log_parallel = FALSE, #if true Standard SMC is performed
                               gamma = 1 #tuning parameter for the variance,
                               #where 0 =  aver-SMC(N,M) and 1 = BC(M,N)
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
  #If we want to return 
  if(log_parallel == TRUE){
    return(logNC)
  }
  else{
  
  var_cor = var(logNC)
  
  #Now it is the time for the correction
  #We add half of the variance into the sample BUT we use a tuning parameter now
  #That is gamma [0,1] for controling the variance.
  
  bias_corection = logNC + gamma*(var_cor/2)
  
  #Therefore, if  gamma = 0, we just take an average SMC
  #and if gamma = 1, we have the standard BC.
  return(mean(bias_corection))
  }

}

#Function which estimates the MSE
#we want to minimize it 

MSE_min = function(gamma,logZ,TrueLog){
  
  vars = apply(logZ, 1, var)
  
  bias_corection = apply(logZ, 1, mean) + gamma*(vars/2)
  
  
  MSE = mean((bias_corection - TrueLog)^2)
  res = cbind(MSE,var(bias_corection),mean(bias_corection) - TrueLog)
  colnames(res) = c("MSE","Variance","Bias")
  
  #return the MSE, the variance and the bias of the estimator
  return(res)
  
}






#Function for evaluating the Bias correction   method
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
  
  #MSE first way
  RMSE = sqrt(apply((NC - TrueLog)^2,2,mean))
  
  #MSE second way
  #SMSE = sqrt(Efficiency + Bias^2)
  
  df = rbind(Bias,Efficiency,RMSE)
  
  colnames(df) =  colnames(NC)
  rownames(df) = c("Bias","Variance","RMSE") 

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


