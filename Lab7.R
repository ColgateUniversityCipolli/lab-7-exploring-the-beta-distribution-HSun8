# Henry Sun 
# Lab 7 

# all libraries
library(tidyverse)

# Task 1
# plot beta distributions
task1.plotting <- function(alpha, beta){
  fig.dat <- tibble(x = seq(-0.25, 1.25, length.out=1000))|>   # generate a grid of points
    mutate(beta.pdf = dbeta(x, alpha, beta),                      # compute the beta PDF
           norm.pdf = dnorm(x,                                    # Gaussian distribution with
                            mean = alpha/(alpha+beta),            # same mean and variance
                            sd = sqrt((alpha*beta)/((alpha+beta)^2*(alpha+beta+1)))))
  beta.pdf.name = paste("Beta(", as.character(alpha), ", ", as.character(beta), ")",  sep = "")
  norm.pdf.name = paste("Gaussian(", as.character(signif(alpha/(alpha+beta), 3)), ", ", 
                        as.character(signif((sqrt((alpha*beta)/((alpha+beta)^2*(alpha+beta+1))))^2 ,3)), ")", sep = "")
  
  task1.plot <- ggplot(data= fig.dat)+                                     # specify data
      geom_line(aes(x=x, y=beta.pdf, color=beta.pdf.name)) +                 # plot beta dist
      geom_line(aes(x=x, y=norm.pdf, color= norm.pdf.name)) +  # plot guassian dist
      geom_hline(yintercept=0)+                                            # plot x axis
      theme_bw()+                                                          # change theme
      xlab("x")+                                                           # label x axis
      ylab("Density")+                                                     # label y axis
      scale_color_manual("", values = c("black", "grey"))+                 # change colors
      theme(legend.position = "bottom")                                    # move legend to bottom
  task1.plot
}
# summarize data (mean, variance, skewness, excess kurtosis)
task1.summarize <- function(alpha, beta){
  task1.data = tibble(
    alpha = alpha, 
    beta = beta,
    mean = alpha/(alpha+beta),
    variance = (alpha*beta)/((alpha+beta)^2 *(alpha+beta+1)),
    skewness = (2*(beta-alpha)*sqrt(alpha+beta+1))/((alpha + beta + 2)*sqrt(alpha*beta)),
    e.kurtosis = (6*((alpha-beta)^2 * (alpha+beta+1) - (alpha*beta)*(alpha+beta+2))) / 
      ((alpha*beta) * (alpha+beta+2) * (alpha+beta+3))
  )
  task1.data
}

# beta(2,5)
case1.plot = task1.plotting(2,5)
case1.data = task1.summarize(2,5)

# beta(5,5)
case2.plot = task1.plotting(5,5)
case2.data = task1.summarize(5,5)

# beta(5,2)
case3.plot = task1.plotting(5,2)
case3.data = task1.summarize(5,2)

# beta(0.5, 0.5)
case4.plot = task1.plotting(0.5,0.5)
case4.data = task1.summarize(0.5,0.5)

task1.summary = rbind(case1.data, case2.data, case3.data, case4.data)

(case1.plot / case2.plot) | (case3.plot / case4.plot)

# a = 2, b = 5
alpha <- 2
beta <- 5

q1.fig.dat <- tibble(x = seq(-0.25, 1.25, length.out=1000))|>   # generate a grid of points
  mutate(beta.pdf = dbeta(x, alpha, beta),                      # compute the beta PDF
         norm.pdf = dnorm(x,                                    # Gaussian distribution with
                          mean = alpha/(alpha+beta),            # same mean and variance
                          sd = sqrt((alpha*beta)/((alpha+beta)^2*(alpha+beta+1)))))
# summarize 
beta1.data = tibble(
  alpha = alpha, 
  beta = beta,
  mean = alpha/(alpha+beta),
  variance = (alpha*beta)/((alpha+beta)^2 *(alpha+beta+1)),
  skewness = (2*(beta-alpha)*sqrt(alpha+beta+1))/((alpha + beta + 2)*sqrt(alpha*beta)),
  e.kurtosis = (6*((alpha-beta)^2 * (alpha+beta+1) - (alpha*beta)*(alpha+beta+2))) / 
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
  variance = (alpha*beta)/((alpha+beta)^2 *(alpha+beta+1)),
  skewness = (2*(beta-alpha)*sqrt(alpha+beta+1))/((alpha + beta + 2)*sqrt(alpha*beta)),
  e.kurtosis = (6*((alpha-beta)^2 * (alpha+beta+1) - (alpha*beta)*(alpha+beta+2))) / 
    ((alpha*beta) * (alpha+beta+2) * (alpha+beta+3))
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
  variance = (alpha*beta)/((alpha+beta)^2 *(alpha+beta+1)),
  skewness = (2*(beta-alpha)*sqrt(alpha+beta+1))/((alpha + beta + 2)*sqrt(alpha*beta)),
  e.kurtosis = (6*((alpha-beta)^2 * (alpha+beta+1) - (alpha*beta)*(alpha+beta+2))) / 
    ((alpha*beta) * (alpha+beta+2) * (alpha+beta+3))
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
  variance = (alpha*beta)/((alpha+beta)^2 *(alpha+beta+1)),
  skewness = (2*(beta-alpha)*sqrt(alpha+beta+1))/((alpha + beta + 2)*sqrt(alpha*beta)),
  e.kurtosis = (6*((alpha-beta)^2 * (alpha+beta+1) - (alpha*beta)*(alpha+beta+2))) / 
    ((alpha*beta) * (alpha+beta+2) * (alpha+beta+3))
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

beta.summary = rbind(beta1.data, beta2.data, beta3.data, beta4.data)

beta1 / beta2 | beta3 / beta4
# Task 2
# compute moments

# beta.moment()
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

# compute for each case
case1.pop.data <- tibble(alpha = 2,
                         beta = 5, 
                         mean = beta.moment(2,5,1,F)$value,
                         variance = beta.moment(2,5,2,T)$value,
                         skewness = (beta.moment(2,5,3,T)$value)/((beta.moment(2,5,2,T)$value)^(3/2)),
                         e.kurtosis = ((beta.moment(2,5,4,T)$value)/((beta.moment(2,5,2,T)$value)^2))-3
)

case2.pop.data <- tibble(alpha = 5,
                         beta = 5, 
                         mean = beta.moment(5,5,1,F)$value, 
                         variance = beta.moment(5,5,2,T)$value,
                         skewness = (beta.moment(5,5,3,T)$value)/((beta.moment(5,5,2,T)$value)^(3/2)),
                         e.kurt = ((beta.moment(5,5,4,T)$value)/((beta.moment(5,5,2,T)$value)^2))-3
)

case3.pop.data <- tibble(alpha = 5,
                         beta = 2, 
                         mean = beta.moment(5,2,1,F)$value, 
                         variance = beta.moment(5,2,2,T)$value,
                         skewness = (beta.moment(5,2,3,T)$value)/((beta.moment(5,2,2,T)$value)^(3/2)),
                         e.kurtosis = ((beta.moment(5,2,4,T)$value)/((beta.moment(5,2,2,T)$value)^2))-3
)

case4.pop.data <- tibble(alpha = 0.5,
                         beta = 0.5, 
                         mean = beta.moment(0.5,0.5,1,F)$value, 
                         variance = beta.moment(0.5,0.5,2,T)$value,
                         skewness = (beta.moment(0.5,0.5,3,T)$value)/((beta.moment(0.5,0.5,2,T)$value)^(3/2)),
                         e.kurtosis = ((beta.moment(0.5,0.5,4,T)$value)/((beta.moment(0.5,0.5,2,T)$value)^2))-3
)
# Task 3
library(e1071)
# for kurtosis and skewness
# data summaries for all cases

task3.numsummary <- function(alpha, beta){
  set.seed(7272) # Set seed so we all get the same results.
  sample.size <- 500 # Specify sample details
  
  beta.sample <- rbeta(n = sample.size,  # sample size
                        shape1 = alpha,   # alpha parameter
                        shape2 = beta)    # beta parameter
  task3.numericalsummary = summarize(tibble(sample.data = beta.sample), 
                                      mean = mean(sample.data),
                                      variance = var(sample.data),
                                      skewness = skewness(sample.data),
                                      # excess kurtosis = kurtosis - 3
                                      e.kurtosis = kurtosis(sample.data) - 3)
  task3.numericalsummary
}

task3.histogram <- function(alpha, beta){
  set.seed(7272) # Set seed so we all get the same results.
  sample.size <- 500 # Specify sample details
  
  fig.dat <- tibble(x = seq(-0.25, 1.25, length.out=1000))|>   # generate a grid of points
    mutate(beta.pdf = dbeta(x, alpha, beta),                      # compute the beta PDF
           norm.pdf = dnorm(x,                                    # Gaussian distribution with
                            mean = alpha/(alpha+beta),            # same mean and variance
                            sd = sqrt((alpha*beta)/((alpha+beta)^2*(alpha+beta+1)))))
  
  beta.sample <- rbeta(n = sample.size,  # sample size
                       shape1 = alpha,   # alpha parameter
                       shape2 = beta)    # beta parameter
  beta.pdf.name = paste("Beta(", as.character(alpha), ", ", as.character(beta), ")",  sep = "")
  histogram <- ggplot(tibble(sample.data = beta.sample), aes(x=sample.data))+
    geom_histogram(aes(y=after_stat(density), color = "sample.data histogram"))+
    geom_density(aes(color = "sample.data"), key_glyph = draw_key_path)+
    geom_line(data = fig.dat, aes(x = x, y = beta.pdf, color = beta.pdf.name))+
    xlab("x")
  histogram
}

# numerical summary
case1.sample = task3.numsummary(2,5)
case2.sample = task3.numsummary(5,5)
case3.sample = task3.numsummary(5,2)
case4.sample = task3.numsummary(0.5,0.5)

# histograms
case1.histogram = task3.histogram(2,5)
case2.histogram = task3.histogram(5,5)
case3.histogram = task3.histogram(5,2)
case4.histogram = task3.histogram(0.5,0.5)


# beta(2,5)
set.seed(7272) # Set seed so we all get the same results.
sample.size <- 500 # Specify sample details
alpha <- 2
beta <- 5
beta.sample1 <- rbeta(n = sample.size,  # sample size
                     shape1 = alpha,   # alpha parameter
                     shape2 = beta)    # beta parameter
# numerical summary
numerical.summary.beta1 = summarize(tibble(sample.data = beta.sample1), 
                              mean = mean(sample.data),
                              variance = var(sample.data),
                              skewness = skewness(sample.data),
                              kurtosis = kurtosis(sample.data) - 3)
b1.fig.dat <- tibble(x = seq(-0.25, 1.25, length.out=1000))|>   # generate a grid of points
  mutate(beta.pdf = dbeta(x, alpha, beta),                      # compute the beta PDF
         norm.pdf = dnorm(x,                                    # Gaussian distribution with
                          mean = alpha/(alpha+beta),            # same mean and variance
                          sd = sqrt((alpha*beta)/((alpha+beta)^2*(alpha+beta+1)))))

aeugh <- ggplot(tibble(sample.data = beta.sample1), aes(x=sample.data))+
  geom_histogram(aes(y=after_stat(density), color = "sample.data histogram"))+
  geom_density(aes(color = "sample.data"), key_glyph = draw_key_path)+
  geom_line(data = b1.fig.dat, aes(x = x, y = beta.pdf, color ="Beta(2,5)"))+
  xlab("x")

aeugh + case1.histogram
# beta(5,5)
set.seed(7272) # Set seed so we all get the same results.
sample.size <- 500 # Specify sample details
alpha <- 5
beta <- 5
beta.sample2 <- rbeta(n = sample.size,  # sample size
                     shape1 = alpha,   # alpha parameter
                     shape2 = beta)    # beta parameter
# numerical summary
numerical.summary.beta2 = summarize(tibble(sample.data = beta.sample2), 
                                    mean = mean(sample.data),
                                    variance = var(sample.data),
                                    skewness = skewness(sample.data),
                                    kurtosis = kurtosis(sample.data))
b1.fig.dat <- tibble(x = seq(-0.25, 1.25, length.out=1000))|>   # generate a grid of points
  mutate(beta.pdf = dbeta(x, alpha, beta),                      # compute the beta PDF
         norm.pdf = dnorm(x,                                    # Gaussian distribution with
                          mean = alpha/(alpha+beta),            # same mean and variance
                          sd = sqrt((alpha*beta)/((alpha+beta)^2*(alpha+beta+1)))))

testing <- ggplot(tibble(sample.data = beta.sample2), aes(x=sample.data))+
  geom_histogram(aes(y=after_stat(density), color = "sample.data histogram"))+
  geom_density(aes(color = "sample.data"), key_glyph = draw_key_path)+
  geom_line(data = b1.fig.dat, aes(x = x, y = beta.pdf, color ="Beta(5,5)"))+
  xlab("x")

# beta(5,2)
set.seed(7272) # Set seed so we all get the same results.
sample.size <- 500 # Specify sample details
alpha <- 5
beta <- 2
beta.sample3 <- rbeta(n = sample.size,  # sample size
                     shape1 = alpha,   # alpha parameter
                     shape2 = beta)    # beta parameter
# numerical summary
numerical.summary.beta1 = summarize(tibble(sample.data = beta.sample3), 
                                    mean = mean(sample.data),
                                    variance = var(sample.data),
                                    skewness = skewness(sample.data),
                                    kurtosis = kurtosis(sample.data))
b1.fig.dat <- tibble(x = seq(-0.25, 1.25, length.out=1000))|>   # generate a grid of points
  mutate(beta.pdf = dbeta(x, alpha, beta),                      # compute the beta PDF
         norm.pdf = dnorm(x,                                    # Gaussian distribution with
                          mean = alpha/(alpha+beta),            # same mean and variance
                          sd = sqrt((alpha*beta)/((alpha+beta)^2*(alpha+beta+1)))))

ggplot(tibble(sample.data = beta.sample3), aes(x=sample.data))+
  geom_histogram(aes(y=after_stat(density), color = "sample.data histogram"))+
  geom_density(aes(color = "sample.data"), key_glyph = draw_key_path)+
  geom_line(data = b1.fig.dat, aes(x = x, y = beta.pdf, color ="Beta(5,2)"))+
  xlab("x")

# beta(0.5,0.5)
set.seed(7272) # Set seed so we all get the same results.
sample.size <- 500 # Specify sample details
alpha <- 0.5
beta <- 0.5
beta.sample4 <- rbeta(n = sample.size,  # sample size
                     shape1 = alpha,   # alpha parameter
                     shape2 = beta)    # beta parameter
# numerical summary
numerical.summary.beta4 = summarize(tibble(sample.data = beta.sample4), 
                                    mean = mean(sample.data),
                                    variance = var(sample.data),
                                    skewness = skewness(sample.data),
                                    kurtosis = kurtosis(sample.data) - 3)
b1.fig.dat <- tibble(x = seq(-0.25, 1.25, length.out=1000))|>   # generate a grid of points
  mutate(beta.pdf = dbeta(x, alpha, beta),                      # compute the beta PDF
         norm.pdf = dnorm(x,                                    # Gaussian distribution with
                          mean = alpha/(alpha+beta),            # same mean and variance
                          sd = sqrt((alpha*beta)/((alpha+beta)^2*(alpha+beta+1)))))

ggplot(tibble(sample.data = beta.sample4), aes(x=sample.data))+
  geom_histogram(aes(y=after_stat(density), color = "sample.data histogram"))+
  geom_density(aes(color = "sample.data"), key_glyph = draw_key_path)+
  geom_line(data = b1.fig.dat, aes(x = x, y = beta.pdf, color ="Beta(0.5,0.5)"))+
  xlab("x")

# Task 4 
library(cumstats)
beta1.cumstats <- tibble(n=(1:length(beta.sample1)), 
                            mean=cummean((beta.sample1)), 
                            variance=cumvar((beta.sample1)),
                            skewness=cumskew((beta.sample1)), 
                            kurtosis=cumkurt((beta.sample1)))
# kurtosis vs excess kurtosis?
cum.mean.plot <- ggplot(beta1.cumstats)+
  geom_line(aes(x=n, y = mean))+
  geom_hline(data=beta1.data, aes(yintercept = mean))  
cum.mean.plot

cum.var.plot <- ggplot(beta1.cumstats) +
  geom_line(aes(x=n, y = variance))+
  geom_hline(data=beta1.data, aes(yintercept = variance))
cum.var.plot

cum.skew.plot <- ggplot(beta1.cumstats) +
  geom_line(aes(x=n, y = skewness))+
  geom_hline(data=beta1.data, aes(yintercept = skewness))
cum.skew.plot

# not right...
cum.kurt.plot <- ggplot(beta1.cumstats) +
  geom_line(aes(x=n, y = kurtosis)) +
  geom_hline(data=beta1.data, aes(yintercept = e.kurtosis + 3))
cum.kurt.plot

library(patchwork)
cum.mean.plot / cum.var.plot | cum.skew.plot / cum.kurt.plot

# for loop
alpha <- 2
beta <- 5
sample.size <- 500
for (i in 2:50){
  set.seed(7272 + i)
  beta.sample.p4 <- rbeta(n = sample.size,  # sample size
                        shape1 = alpha,   # alpha parameter
                        shape2 = beta)    # beta parameter
  betap4.cumstats <- tibble(n=(1:length(beta.sample.p4)), 
                           mean=cummean((beta.sample.p4)), 
                           variance=cumvar((beta.sample.p4)),
                           skewness=cumskew((beta.sample.p4)), 
                           kurtosis=cumkurt((beta.sample.p4)))
  
  cum.mean.plot <- cum.mean.plot + 
    geom_line(data = betap4.cumstats, aes(x=n, y=mean), color = i)
  
  cum.var.plot <- cum.var.plot + 
    geom_line(data = betap4.cumstats, aes(x=n, y=variance), color = i)
  
  cum.skew.plot <- cum.skew.plot + 
    geom_line(data = betap4.cumstats, aes(x=n, y=skewness), color = i)
  
  cum.kurt.plot <- cum.kurt.plot + 
    geom_line(data = betap4.cumstats, aes(x=n, y=kurtosis), color = i)
  
}
cum.mean.plot / cum.var.plot | cum.skew.plot / cum.kurt.plot


# task 5 
# how can we model the variation? 
alpha <- 2
beta <- 5
sample.size <- 500

stats.p5 = tibble(mean = numeric(),
                  variance = numeric(),
                  skewness = numeric(),
                  kurtosis = numeric())
  
for (i in 1:1000){
  set.seed(7272 + i)
  beta.sample.p5 <- rbeta(n = sample.size,  # sample size
                          shape1 = alpha,   # alpha parameter
                          shape2 = beta)    # beta parameter
  mean=mean((beta.sample.p5)) 
  variance=var((beta.sample.p5))
  skewness=skewness((beta.sample.p5))
  kurtosis=kurtosis((beta.sample.p5))
  
  stats.p5 <- bind_rows(stats.p5, tibble(mean, variance, skewness, kurtosis))
}

# histogram
a <- ggplot(stats.p5)+
  geom_histogram(aes(x = mean, y=after_stat(density)))+
  geom_density(aes(x=mean))

eu <- ggplot(stats.p5)+
  geom_histogram(aes(x = variance, y=after_stat(density)))+
  geom_density(aes(x=variance))

g <- ggplot(stats.p5)+
  geom_histogram(aes(x = skewness, y=after_stat(density)))+
  geom_density(aes(x=skewness))

h <- ggplot(stats.p5)+
  geom_histogram(aes(x = kurtosis, y=after_stat(density)))+
  geom_density(aes(x=kurtosis))

a / eu | g / h

# looks like normal distribution.... 

