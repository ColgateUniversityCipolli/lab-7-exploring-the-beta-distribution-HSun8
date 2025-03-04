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
  mean = alpha/(alpha+beta),
  var = (alpha*beta)/((alpha+beta)^2 *(alpha+beta+1)),
  skew = (2*(beta-alpha)*sqrt(alpha+beta+1))/((alpha + beta + 2)*sqrt(alpha*beta)),
  kurt = (6*(alpha-beta)^2*(alpha+beta+1)-(alpha*beta*(alpha+beta+2)))/((alpha*beta*(alpha+beta+2)*(alpha+beta+3)))
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

