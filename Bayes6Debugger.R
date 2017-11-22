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
theta<-c(3,.02,.03,.04,.1)


int<-3.7537694
betas<-c(0.1842009,1.3716344,2.1843266)

sd<-0.7279280

#prior<-function(int,betas,sd){
  
  #We will assume that the betas and sigma square come from normal and the intercept is from a uniform 
  #between 0 and 10. Notice how this assumption rules out everything less than zero and more than 10
  
  pr_betas<-dnorm(betas, mean = 0 , sd = 5, log = T)
  pr_int<-dunif(int, min = 0 , max = 10, log = T)
  pr_sd<-dunif(sd, min = 0 , max = 3, log = T)
  
  all_three_prior<-sum(pr_betas,pr_int,pr_sd)
  
  
#}


#A Liklihood function


#liklihood<-function(int,betas,sd){
  
  
  #This is obtained mathmatically by moving the epsilon which we assume is normally distributed on one side and so on. This is the assumption here
  #Moving things around, epsilon becomes the regression equation which goes into the mean etc..
  
  
  x<-suggest


  y_hat_1<-int+data$educ*betas[1]+data$exper*betas[2]+data$smsa*betas[3]
  
  
  #y_hat_1<-int+data$educ*betas[1]
  
  #y_hat<-int+data.matrix(data[,c('educ','exper','smsa')])%*%betas
  
 
  
  
 
  
  liklihood_result_1<-dnorm(data$lwage, mean = y_hat_1, sd = sd, log = T)
  
 
  
  
  
  liklihood_result_3<-NULL
  
  #for(i in 1:length(data$lwage)){
    
    
   # liklihood_result_3[i]<-dnorm(data$lwage[i], mean = y_hat_1[i], sd = sd)
    
  #}
  
  liklihood_result_2<-sum(liklihood_result_1)
  
  #return(liklihood_result_2)
#}

#A Posterior Function

posterior<-function(int,betas,sd){
  
  
  return(prior(int,betas,sd)+liklihood(int,betas,sd))
  
}
#A Proposal Function (Moving from one theta to another).. This one is the trickiest..

suggestion<-function(pars){
  
  v<-rnorm((length(pars)-1),0,1) #Standard error residuals? hmm
  sig<-runif(1,0,1)
  
  pars_1<-c((pars[1:(length(pars)-1)]+v),sig)
  
  return(pars_1)
  
}




#A Metropolis Hastings Algorithem

#theta<-c(3,.02,.03,.04,.33)  #Thera_0 intercept, beta 1, 2,3 and sd


iterations<-2

values<-matrix(0,iterations,length(theta))

for (i in 1:iterations){
  
  values[i,]<-theta
  
  suggest<-suggestion(values[i,])
  
  print(suggest)
  print(liklihood(suggest[1],suggest[2:4],suggest[5]))
  print(liklihood(values[i,1],values[i,2:4],values[i,5]))
  print(prior(suggest[1],suggest[2:4],suggest[5]))
  print(prior(values[i,1],values[i,2:4],values[i,5]))
  print(posterior(suggest[1],suggest[2:4],suggest[5]))
  print(posterior(values[i,1],values[i,2:4],values[i,5]))
  
  ratio<-posterior(suggest[1],suggest[2:4],suggest[5])-posterior(values[i,1],values[i,2:4],values[i,5])
  
  
  u<-runif(1, min = 0, max = 1)
  
  
  if(u<ratio){
    
    theta<-suggest
    
  }
  
  else{
    
    theta<-theta
    
  }
  
  print(i)  
  
}



