# Henry Sun 
# Lab 7 

# Task 1
# plot beta distributions
library(tidyverse)

# a = 2, b = 5
alpha <- 2
beta <- 5

q1.fig.dat <- tibble(x = seq(-0.25, 1.25, length.out=1000))|>   # generate a grid of points
  mutate(beta.pdf = dbeta(x, alpha, beta),                      # compute the beta PDF
         norm.pdf = dnorm(x,                                    # Gaussian distribution with
                          mean = alpha/(alpha+beta),            # same mean and variance
                          sd = sqrt((alpha*beta)/((alpha+beta)^2*(alpha+beta+1)))))
beta1.data = tibble(
  alpha = alpha, 
  beta = beta,
  mean = alpha/(alpha+beta),
  variance = (alpha*beta)/((alpha+beta)^2 *(alpha+beta+1)),
  skewness = (2*(beta-alpha)*sqrt(alpha+beta+1))/((alpha + beta + 2)*sqrt(alpha*beta)),
  kurtosis = (6*((alpha-beta)^2 * (alpha+beta+1) - (alpha*beta)*(alpha+beta+2))) / 
    ((alpha*beta) * (alpha+beta+2) * (alpha+beta+3))
)


beta1 <- ggplot(data= q1.fig.dat)+                                              # specify data
  geom_line(aes(x=x, y=beta.pdf, color="Beta(2,5)")) +                 # plot beta dist
  geom_line(aes(x=x, y=norm.pdf, color="Gaussian(0.2857, 0.0255)")) +  # plot guassian dist
  geom_hline(yintercept=0)+                                            # plot x axis
  theme_bw()+                                                          # change theme
  xlab("x")+                                                           # label x axis
  ylab("Density")+                                                     # label y axis
  scale_color_manual("", values = c("black", "grey"))+                 # change colors
  theme(legend.position = "bottom")                                    # move legend to bottom
beta1

# a = 5, b = 5
alpha <- 5
beta <- 5

q1.fig.dat <- tibble(x = seq(-0.25, 1.25, length.out=1000))|>   # generate a grid of points
  mutate(beta.pdf = dbeta(x, alpha, beta),                      # compute the beta PDF
         norm.pdf = dnorm(x,                                    # Gaussian distribution with
                          mean = alpha/(alpha+beta),            # same mean and variance
                          sd = sqrt((alpha*beta)/((alpha+beta)^2*(alpha+beta+1)))))
beta2.data = tibble(
  alpha = alpha, 
  beta = beta,
  mean = alpha/(alpha+beta),
  var = (alpha*beta)/((alpha+beta)^2 *(alpha+beta+1)),
  skew = (2*(beta-alpha)*sqrt(alpha+beta+1))/((alpha + beta + 2)*sqrt(alpha*beta)),
  kurt = (6*(alpha-beta)^2*(alpha+beta+1)-(alpha*beta*(alpha+beta+2)))/((alpha*beta*(alpha+beta+2)*(alpha+beta+3)))
)


beta2 <- ggplot(data= q1.fig.dat)+                                              # specify data
  geom_line(aes(x=x, y=beta.pdf, color="Beta(5,5)")) +                 # plot beta dist
  geom_line(aes(x=x, y=norm.pdf, color="Gaussian(0.5, 0.0227)")) +  # plot guassian dist
  geom_hline(yintercept=0)+                                            # plot x axis
  theme_bw()+                                                          # change theme
  xlab("x")+                                                           # label x axis
  ylab("Density")+                                                     # label y axis
  scale_color_manual("", values = c("black", "grey"))+                 # change colors
  theme(legend.position = "bottom")                                    # move legend to bottom
beta2

# a = 5, b = 2 
alpha <- 5
beta <- 2

q1.fig.dat <- tibble(x = seq(-0.25, 1.25, length.out=1000))|>   # generate a grid of points
  mutate(beta.pdf = dbeta(x, alpha, beta),                      # compute the beta PDF
         norm.pdf = dnorm(x,                                    # Gaussian distribution with
                          mean = alpha/(alpha+beta),            # same mean and variance
                          sd = sqrt((alpha*beta)/((alpha+beta)^2*(alpha+beta+1)))))
beta3.data = tibble(
  alpha = alpha,
  beta = beta,
  mean = alpha/(alpha+beta),
  var = (alpha*beta)/((alpha+beta)^2 *(alpha+beta+1)),
  skew = (2*(beta-alpha)*sqrt(alpha+beta+1))/((alpha + beta + 2)*sqrt(alpha*beta)),
  kurt = (6*(alpha-beta)^2*(alpha+beta+1)-(alpha*beta*(alpha+beta+2)))/((alpha*beta*(alpha+beta+2)*(alpha+beta+3)))
)


beta3 <- ggplot(data= q1.fig.dat)+                                              # specify data
  geom_line(aes(x=x, y=beta.pdf, color="Beta(5,2)")) +                 # plot beta dist
  geom_line(aes(x=x, y=norm.pdf, color="Gaussian(0.7143, 0.0255)")) +  # plot guassian dist
  geom_hline(yintercept=0)+                                            # plot x axis
  theme_bw()+                                                          # change theme
  xlab("x")+                                                           # label x axis
  ylab("Density")+                                                     # label y axis
  scale_color_manual("", values = c("black", "grey"))+                 # change colors
  theme(legend.position = "bottom")                                    # move legend to bottom
beta3

# a = 0.5, b = 0.5
alpha <- 0.5
beta <- 0.5

q1.fig.dat <- tibble(x = seq(-0.25, 1.25, length.out=1000))|>   # generate a grid of points
  mutate(beta.pdf = dbeta(x, alpha, beta),                      # compute the beta PDF
         norm.pdf = dnorm(x,                                    # Gaussian distribution with
                          mean = alpha/(alpha+beta),            # same mean and variance
                          sd = sqrt((alpha*beta)/((alpha+beta)^2*(alpha+beta+1)))))
beta4.data = tibble(
  alpha = alpha,
  beta = beta,
  mean = alpha/(alpha+beta),
  var = (alpha*beta)/((alpha+beta)^2 *(alpha+beta+1)),
  skew = (2*(beta-alpha)*sqrt(alpha+beta+1))/((alpha + beta + 2)*sqrt(alpha*beta)),
  kurt = (6*(alpha-beta)^2*(alpha+beta+1)-(alpha*beta*(alpha+beta+2)))/((alpha*beta*(alpha+beta+2)*(alpha+beta+3)))
)


beta4 <- ggplot(data= q1.fig.dat)+                                              # specify data
  geom_line(aes(x=x, y=beta.pdf, color="Beta(0.5,0.5)")) +                 # plot beta dist
  geom_line(aes(x=x, y=norm.pdf, color="Gaussian(0.5, 0.125)")) +  # plot guassian dist
  geom_hline(yintercept=0)+                                            # plot x axis
  theme_bw()+                                                          # change theme
  xlab("x")+                                                           # label x axis
  ylab("Density")+                                                     # label y axis
  scale_color_manual("", values = c("black", "grey"))+                 # change colors
  theme(legend.position = "bottom")                                    # move legend to bottom
beta4


beta.data = rbind(beta1.data, beta2.data, beta3.data, beta4.data)
# Task 2
# compute moments

#beta.moment()
beta.moment <- function(alpha, beta, k, centered){
  if (centered == T){
    #integrand1 <- function(x) {x * dbeta(x, alpha, beta)}
    #uncentered <- integrate(integrand1, lower = 0, upper = 1)
    mean <- (alpha)/(alpha + beta)
    integrand2 <- function(x) {(x-mean)^k * dbeta(x, alpha, beta)}
    solution <- integrate(integrand2, lower = 0, upper = 1)
  }
  if(centered == F){
    integrand <- function(x) {x^k * dbeta(x, alpha, beta)}
    solution <- integrate(integrand, lower = 0, upper = 1)
  }
  solution
}
# beta.pop.data <- tibble(alpha = c(2, 5, 5, 0.5),
#                         
#                         beta = c(5, 5, 2, 0.5),
#                         
#                         mean = c(beta.moment(2,5,1,F)$value, beta.moment(5,5,1,F)$value, 
#                                  beta.moment(5,2,1,F)$value, beta.moment(0.5,0.5,1,F)$value),
#                         
#                         var = c(beta.moment(2,5,2,T)$value, beta.moment(5,5,2,T)$value,
#                                 beta.moment(5,2,2,T)$value, beta.moment(0.5,0.5,2,T)$value),
#                         
#                         skew = c(beta.moment(2,5,3,T)$value/((beta.moment(2,5,2,T)$value)^(3/2)),
#                                  beta.moment(5,5,3,T)$value/((beta.moment(5,5,2,T)$value)^(3/2)),
#                                  beta.moment(5,2,3,T)$value/((beta.moment(5,2,2,T)$value)^(3/2)),
#                                  beta.moment(0.5,0.5,3,T)$value/((beta.moment(0.5, 0.5,2,T)$value)^(3/2))),
#                         kurt = c((beta.moment(2,5,4,T)$value/((beta.moment(2,5,2,T)$value)^2)) - 3, 
#                                  (beta.moment(5,5,4,T)$value/((beta.moment(5,5,2,T)$value)^2)) - 3,
#                                  (beta.moment(5,2,4,T)$value/((beta.moment(5,2,2,T)$value)^2)) - 3,
#                                  (beta.moment(0.5,0.5,4,T)$value/((beta.moment(0.5,0.5,2,T)$value)^2)) - 3))

beta1.pop.data <- tibble(alpha = 2,
                         beta = 5, 
                         mean = beta.moment(2,5,1,F)$value, 
                         skew = (beta.moment(2,5,3,T)$value)/((beta.moment(2,5,2,T)$value)^(3/2)),
                         kurt = ((beta.moment(2,5,4,T)$value)/((beta.moment(2,5,2,T)$value)^2)) -3
)
# Task 3
library(e1071)
# for kurtosis and skewness

# beta(2,5)
set.seed(7272) # Set seed so we all get the same results.
sample.size <- 500 # Specify sample details
alpha <- 2
beta <- 5
beta.sample <- rbeta(n = sample.size,  # sample size
                     shape1 = alpha,   # alpha parameter
                     shape2 = beta)    # beta parameter
# numerical summary
numerical.summary.beta1 = summarize(tibble(sample.data = beta.sample), 
                              mean = mean(sample.data),
                              variance = var(sample.data),
                              skewness = skewness(sample.data),
                              kurtosis = kurtosis(sample.data))
b1.fig.dat <- tibble(x = seq(-0.25, 1.25, length.out=1000))|>   # generate a grid of points
  mutate(beta.pdf = dbeta(x, alpha, beta),                      # compute the beta PDF
         norm.pdf = dnorm(x,                                    # Gaussian distribution with
                          mean = alpha/(alpha+beta),            # same mean and variance
                          sd = sqrt((alpha*beta)/((alpha+beta)^2*(alpha+beta+1)))))

ggplot(tibble(sample.data = beta.sample), aes(x=sample.data))+
  geom_histogram(aes(y=after_stat(density), color = "sample.data histogram"))+
  geom_density(aes(color = "sample.data"), key_glyph = draw_key_path)+
  geom_line(data = b1.fig.dat, aes(x = x, y = beta.pdf, color ="Beta(2,5)"))+
  xlab("x")

# Task 4 
library(cumstats)
beta.cumstats <- tibble(n=(1:length(beta.sample)), 
                            mean=cummean((beta.sample)), 
                            variance=cumvar((beta.sample)),
                            skewness=cumskew((beta.sample)), 
                            kurtosis=cumkurt((beta.sample)))
# kurtosis vs excess kurtosis?
cum.mean.plot <- ggplot(beta.cumstats)+
  geom_line(aes(x=n, y = mean))+
  geom_hline(beta1.data, yintercept = mean)
cum.mean.plot

cum.var.plot <- ggplot(beta.cumstats) +
  geom_line(aes(x=n, y = variance))+
  geom_hline(beta1.data, yintercept = variance)
cum.var.plot

cum.skew.plot <- ggplot(beta.cumstats) +
  geom_line(aes(x=n, y = skewness))+
  geom_hline(beta1.data, yintercept = skewness)
cum.skew.plot

# not right...
cum.kurt.plot <- ggplot(beta.cumstats) +
  geom_line(aes(x=n, y = kurtosis))+
  geom_hline(beta1.data, yintercept = kurtosis)
cum.kurt.plot


library(patchwork)
cum.mean.plot / cum.var.plot | cum.skew.plot / cum.kurt.plot
