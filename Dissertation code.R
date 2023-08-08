fulldata <- load("C:\\Users\\zekun\\OneDrive\\Desktop\\Dissertation on Bayesian\\Data set.Rdata")
mydata <- data_for_presentation[,c("subject_id","task_1","task_2","task_3","task_4","task_5","task_6","task_7")]

y1 <- as.numeric(mydata$task_1)
y2 <- as.numeric(mydata$task_2)
y3 <- as.numeric(mydata$task_3)
y4 <- as.numeric(mydata$task_4)
y5 <- as.numeric(mydata$task_5)
y6 <- as.numeric(mydata$task_6)
y7 <- as.numeric(mydata$task_7)

library("rjags") 

jags_model <- "
    model{
    
    # Sampling distribution
    
      for (i in 1:n) {
      
        y[i] ~ dbern(p[zeta[i]])
        
        # k components
        
        zeta[i] ~ dcat(omega[1:k])
        
      }
      
    # within model parameter priors  
  
      omega[1:k] ~ ddirich(delta[1:k])
  
      for (i in 1:k) {
      
        p[i] ~ dbeta(alpha[i],beta[i])
        

      }
      
    }
"

# set observations

y_obs <- y4
n_obs <- length( y1 )

# set priors

k_fix <- 2
delta_fix <- rep(1,k_fix)
alpha_fix <- c(1,1)
beta_fix <- c(1,1)

# set the list for inputting in the function

data.bayes <- list(y = y_obs, 
                   n = n_obs, 
                   k = k_fix,
                   delta = delta_fix,
                   alpha = alpha_fix,
                   beta = beta_fix
)

# set the JAGS model
model.smpl <- jags.model( file = textConnection(jags_model),
                          data = data.bayes, 
                          n.chains = 1)
# tune the JAGS MCCM sampler
adapt(object = model.smpl, 
      n.iter = 10^4 )
# Generate a posterior sample of size $N=10000$.
Nsample = 10^4              # the size of the sample we ll gona get
n.thin = 10^1               # the thinning (improving) the sample quality
n.iter = Nsample * n.thin   # the number of the total iterations performed
output = jags.samples( model = model.smpl,                          # the model
                       variable.names = c('p','zeta'),    # names of variables to be sampled
                       n.iter = n.iter,                             # size of sample
                       thin = n.thin
) 
# save the sample
save.image(file="BayesianNormalMixtureModel.RData") 
# Check the names of the variables sampled 
names(output)
