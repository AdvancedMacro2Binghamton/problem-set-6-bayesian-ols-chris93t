setwd('C:/Economics PhD/Third Year/Advanced Macro/HW6')

data<-read.csv('card.csv')

data<-data[,c('lwage','educ','exper','smsa')]

results<-lm(data$lwage~data$educ+data$exper+data$smsa)

#OLS Coefficients
print(results$coefficients)


#OLS Residuals
sdev<-sd(results$residuals)
print(sdev)



##Using the MCMC to estimate the posterior

#A prior Function


prior<-function(int,betas,sd){
  
  #We will assume that the betas and sigma square come from normal and the intercept is from a uniform 
  #between 0 and 10. Notice how this assumption rules out everything less than zero and more than 10
  
  pr_betas<-dnorm(betas, mean = 0 , sd = 5, log = T)
  pr_int<-dunif(int, min = 0 , max = 10, log = T)
  pr_sd<-dunif(sd, min = 0 , max = 1, log= T)
  
  all_three_prior<-sum(pr_betas,pr_int,pr_sd)
  
  return(all_three_prior)
  
}


#A Liklihood function


liklihood<-function(int,betas,sd){
  
  
  #This is obtained mathmatically by moving the epsilon which we assume is normally distributed on one side and so on. This is the assumption here
  #Moving things around, epsilon becomes the regression equation which goes into the mean etc..
  
  #y_hat<-int+data.matrix(data[,c('educ','exper','smsa')])%*%betas
  
  y_hat<-int + betas[1]*data$educ+betas[2]*data$exper+betas[3]*data$smsa
  
  liklihood_result_1<-dnorm(data$lwage, mean = y_hat, sd = sd, log = T )
  
  #liklihood_result_1<-NULL
  
  #for(i in 1:length(data$lwage)){
  
  
   #liklihood_result_1[i]<-dnorm(data$lwage[i], mean = y_hat[i], sd = sd, log = T)
  
  #}

  
  
  liklihood_result_2<-sum(liklihood_result_1)
  
  return(liklihood_result_2)
}

#A Posterior Function

posterior<-function(int,betas,sd){
  
  
  return(prior(int,betas,sd)+liklihood(int,betas,sd))
  
}
#A Proposal Function (Moving from one theta to another).. This one is the trickiest..

suggestion<-function(pars){
  
  v<-rnorm((length(pars)-1),0,.001) #Standard error residuals? hmm
  sig<-runif(1,.1,.4)
  
  
  #sig<-.33
  
  
  
  pars_1<-c((pars[1:(length(pars)-1)]+v),sig)

  return(pars_1)
  
}




#A Metropolis Hastings Algorithem

#theta<-c(4.60198824,0.08781045,0.04112071,0.18374575 ,.1)  #Thera_0 intercept, beta 1, 2,3 and sd
theta<-c(7,0.065,0.03,0.1 ,.1)

iterations<-10

values<-matrix(0,iterations,length(theta))
accept<-NULL


for (i in 1:iterations){
  
  values[i,]<-theta
  
  suggest<-suggestion(values[i,])
  
  print(suggest)
  print(values[i,])
  #print(prior(suggest[1],suggest[2:4],suggest[5]))
  #print(prior(values[i,1],values[i,2:4],values[i,5]))
  print(liklihood(suggest[1],suggest[2:4],suggest[5]))
  print(liklihood(values[i,1],values[i,2:4],values[i,5]))
  
  ratio<-posterior(suggest[1],suggest[2:4],suggest[5])-posterior(values[i,1],values[i,2:4],values[i,5])
  
  ratio<-exp(ratio)
  
  print(ratio)
  
  u<-runif(1,0,1)
  
  
  if(u<ratio){
    
    accept[i]<-TRUE
    
    theta<-suggest
    
  }
  
  else{
    
    accept[i]<-FALSE
    theta<-theta
    
  }

  #print(i)  
  
}

rbind(apply(values,2,mean),c(results$coefficients,sdev))
sum(accept==TRUE)/iterations
