
trueA <- .3
trueB <- 4
trueSd <- .33


# create independent x-values 
x <-data$educ
x_1<-data$exper
x_2<-data$smsa
# create dependent values according to ax + b + N(0,sd)
y <-data$lwage



likelihood <- function(param){
  a = param[1]
  a_1=param[2]
  a_2=param[3]
  b = param[4]
  sd = param[5]
  
  pred = a*x +a_1*x_1 + a_2*x_2 + b
  singlelikelihoods = dnorm(y, mean = pred, sd = sd, log = T)
  sumll = sum(singlelikelihoods)
  return(sumll)   
}

# Example: plot the likelihood profile of the slope a
slopevalues <- function(x){return(likelihood(c(x,.2 ,.3,trueB, trueSd)))}
slopelikelihoods <- lapply(seq(.3, .7, by=.00000000005), slopevalues )
print(slopelikelihoods)
plot (seq(3, 7, by=.05), slopelikelihoods , type="l", xlab = "values of slope parameter a", ylab = "Log likelihood")
