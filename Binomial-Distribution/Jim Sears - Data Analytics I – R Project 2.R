# Scenario: A store owner estimates that the number of customers who leave the 
#           store without making a purchase is between 10% and 20%.

# Tasks:
# 1.  Investigate the distribution of samples of customers of various sizes.
#     a.  Choose a single value (0.1 < p < 0.2) to represent p, the probability  
#         of a customer leaving the store without making a purchase.
#	    b.  Graph a sequence of binomial distributions for samples of customers of 
#         size n = 10, 50, 100. (Use the same p for all of these distributions.)
# 2.  Compare the binomial distribution to its normal approximation in each case.
#	    a.  Find the mean and standard deviation of your binomial distribution for 
#         each sample size and construct a normal distribution with these parameters.
#	    b.  Graphically/algebraically compare the probabilities from the two distributions.
#	    c.  In section 7.4 of the textbook they discuss a "continuity correction" 
#         that is a more accurate way of estimating binomial probabilities with 
#         the normal distribution. Investigate how the continuity correction compares 
#         to the typical normal approximation for the binomial.
#	3.  Simulate constructing a sampling distribution for the proportion of customers 
#         who leave the store without making a purchase.
#	    a.  The sampling distribution is useful if it is approximately normally distributed, 
#         so choose a sufficiently large value for the sample size of customers, n, 
#         given your previously chosen probability of a customer leaving without 
#         make a purchase, p. (Hint: Think about the conditions needed to invoke the CLT.)
#	    b.  Simulate collecting samples of that size many times (1000 or more). 
#         This is your sampling distribution.
#	    c.  Find the mean and standard deviation of your collection of outcomes.
#	    d.  Compare these values to the theoretical mean and standard deviation 
#         (standard error) of your sampling distribution.

#---------------------------------------------------------------------------------------------------------------
# 15% of customers leave the store without making a purchase
p <- .15 

# sample size 10
n <- 10
x <- seq(0,n,by=1)
bi_10 <- dbinom(x,n,p)
bi_10_mean <- n*p
bi_10_mean # 1.5
bi_10_sd <- sqrt(bi_10_mean*(1-p))
bi_10_sd # 1.129159

continuous_x <- seq(0,n,length=100)
continuous_norm <- dnorm(continuous_x, mean = bi_10_mean, sd = bi_10_sd)
plot(x,bi_10,type= 'h', xlab = "Customers", ylab = "Probability", main = "")
lines(continuous_x, continuous_norm, col = 'red', lwd = '2')

plot(x, bi_10 , type= 'h', xlab = "Customers", ylab = "Probability", main = "")
norm_10 <- dnorm(x, mean = bi_10_mean, sd = bi_10_sd)
lines(x+.2, norm_10, col = 'red', lwd = '2', type = 'h')

bivsnorm_10 <- data.frame(x, bi_10, norm_10, bi_10-norm_10)
bivsnorm_10

# sample size 50
n <- 50
x <- seq(0,n,by=1)
bi_50 <- dbinom(x,n,p)
bi_50_mean <- n*p
bi_50_mean # 7.5
bi_50_sd <- sqrt(bi_50_mean*(1-p))
bi_50_sd # 2.524876

continuous_x <- seq(0,n,length=100)
continuous_norm <- dnorm(continuous_x, mean = bi_50_mean, sd = bi_50_sd)
plot(x,bi_50,type= 'h', xlab = "Customers", ylab = "Probability", main = "")
lines(continuous_x, continuous_norm, col = 'red', lwd = '2')

plot(x, bi_50 , type= 'h', xlab = "Customers", ylab = "Probability", main = "")
norm_50 <- dnorm(x, mean = bi_50_mean, sd = bi_50_sd)
lines(x+.2, norm_50, col = 'red', lwd = '2', type = 'h')

bivsnorm_50 <- data.frame(x, bi_50, norm_50, bi_50-norm_50)
bivsnorm_50

# sample size 100
n <- 100
x <- seq(0,n,by=1)
bi_100 <- dbinom(x,n,p)
bi_100_mean <- n*p
bi_100_mean # 15
bi_100_sd <- sqrt(bi_100_mean*(1-p))
bi_100_sd # 3.570714

continuous_x <- seq(0,n,length=100)
continuous_norm <- dnorm(continuous_x, mean = bi_100_mean, sd = bi_100_sd)
plot(x,bi_100,type= 'h', xlab = "Customers", ylab = "Probability", main = "")
lines(continuous_x, continuous_norm, col = 'red', lwd = '2')

plot(x, bi_100 , type= 'h', xlab = "Customers", ylab = "Probability", main = "")
norm_100 <- dnorm(x, mean = bi_100_mean, sd = bi_100_sd)
lines(x+.2, norm_100, col = 'red', lwd = '2', type = 'h')

bivsnorm_100 <- data.frame(x, bi_100, norm_100, bi_100-norm_100)
bivsnorm_100


#---------------------------------------------------------------------------------------------------------------
# Creating a Simulation of a Sampling Distribution for a Proportion

# n is sample size and p is the population proportion
n <- 100; p <- .15  

sample_props<-rbinom(1000,n,p)/n

# Checking the mean and standard error:
# the mean of the sampling distribution should be p (approximately)
sample_mean <- round(mean(sample_props),4)
sample_mean
# the standard error of the sampling distribution should be sqrt(p(1-p)/n)
sample_sd <- round(sd(sample_props),4)
sample_sd

hist(sample_props, xlab = 'Probability', 
     main = paste("Histogram of 1000 Binomial Probability Experiments of", n, "Trails, p = ", p),
     sub = paste("Mean =", sample_mean, ", SD =", sample_sd)
     )
