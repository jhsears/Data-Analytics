
# Simulation to Construct a Chi-Square Distribution
# takes a random sample of 5 values from a standard normal distribution
x<-rnorm(10)
x
# [1] -2.569877192 -1.262801368 -1.464577084 -0.002919426  0.402597355 -0.825489124  0.741945578
# [8] -0.952799613  0.476313510 -0.467557131
# takes the sum of the squares of those 5 values - this is the same as taking a 
#   random single value from the chi-square distribution with df=5
sum(x^2)
# [1] 13.09124
mean(x^2)
# [1] 1.309124
median(x^2)
# [1] 0.6159578
sd(x^2)
# [1] 1.981562


# simulates taking 10000 values from the chi-square distribution with df=10
sim_chisq<-replicate(10000,sum(rnorm(10)^2))	
# creates a relative frequency histogram
hist(sim_chisq,freq=FALSE, main= "", ylab = "Relative Frequency", xlab = "Degrees of Freedom")				
# overlays the chi-square curve
continuous_x <- seq(0,200,length=300)			
continuous_chisq<-dchisq(continuous_x,10)
lines(continuous_x, continuous_chisq, col = "red", lwd = 2)

mean(sim_chisq)
# [1] 10.00457
median(sim_chisq)
# [1] 9.352562
sd(sim_chisq)
# [1] 4.426278


# Graphing the Chi-Square Distribution
# graphs the chi-square distribution for specified df

# DF = 1 (black)
curve(dchisq(x, df = 1),0,200, ylab = "Density", xlab = "Degrees of Freedom")			
# overlays another chi-square curve
# DF = 2 (orange)
continuous_chisq<-dchisq(continuous_x,2)		
lines(continuous_x, continuous_chisq, col = "orange", lwd = 2)
# DF = 5 (purple)
continuous_chisq<-dchisq(continuous_x,5)		
lines(continuous_x, continuous_chisq, col = "purple", lwd = 2)
# DF = 10 (red)
continuous_chisq<-dchisq(continuous_x,10)		
lines(continuous_x, continuous_chisq, col = "red", lwd = 2)
# DF = 50 (blue)
continuous_chisq<-dchisq(continuous_x,50)		
lines(continuous_x, continuous_chisq, col = "blue", lwd = 2)
# DF = 90 (green)
continuous_chisq<-dchisq(continuous_x,100)		
lines(continuous_x, continuous_chisq, col = "green", lwd = 2)

# Approximating the Chi-Square Distribution with the Normal Distribution
# graphs the chi-square distribution for specified df (xdf)
xdf = 90
curve(dchisq(x, df = xdf),0,2*xdf, ylab = "Density", xlab = "Degrees of Freedom")		
# overlays normal approximation
continuous_norm<-dnorm(continuous_x,mean=xdf,sd=sqrt(2*xdf))		
lines(continuous_x, continuous_norm, col = "red", lwd = 2)


# creates a data frame allowing for the comparison of the chi-square
# values and the values given by the normal approximation
n=seq(0,xdf,by=.5)	 
chi<-dchisq(n,xdf)	  
norm<-dnorm(n,mean=xdf,sd=sqrt(2*xdf))
data<-data.frame(n,chi,norm,chi-norm)
data
