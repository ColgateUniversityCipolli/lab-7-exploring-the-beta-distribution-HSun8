# Henry Sun 
# Lab 7 

################################################################################
# all libraries
library(tidyverse)
library(e1071) # kurtosis and skewness (task 3)
library(cumstats) # task 4
library(patchwork) # combine plots

################################################################################
# Task 1
# describe the population distribution

# function for plotting distributions
task1.plotting <- function(alpha, beta){
  fig.dat <- tibble(x = seq(-0.25, 1.25, length.out=1000))|>      # generate a grid of points
    mutate(beta.pdf = dbeta(x, alpha, beta),                      # compute the beta PDF
           norm.pdf = dnorm(x,                                    # Gaussian distribution with
                            mean = alpha/(alpha+beta),            # same mean and variance
                            sd = sqrt((alpha*beta)/((alpha+beta)^2*(alpha+beta+1)))))
  
  # names used to keep consistent with different alpha, beta
  beta.pdf.name = paste("Beta(", as.character(alpha), ", ", as.character(beta), 
                        ")",  sep = "")
  norm.pdf.name = paste("Gaussian(", as.character(signif(alpha/(alpha+beta), 3)), 
                        ", ", as.character(signif((sqrt((alpha*beta)/
                        ((alpha+beta)^2*(alpha+beta+1))))^2 ,3)), ")", sep = "")
  

  task1.plot <- ggplot(data= fig.dat)+                                     # specify data
      geom_line(aes(x=x, y=beta.pdf, color=beta.pdf.name)) +               # plot beta dist
      geom_line(aes(x=x, y=norm.pdf, color= norm.pdf.name)) +              # plot gaussian dist
      geom_hline(yintercept=0)+                                            # plot x axis
      theme_bw()+                                                          # change theme
      xlab("x")+                                                           # label x axis
      ylab("Density")+                                                     # label y axis
      scale_color_manual("", values = c("black", "grey"))+                 # change colors
      theme(legend.position = "bottom")                                    # move legend to bottom
  task1.plot
}
# function for summarizing data summarize (mean, variance, skewness, excess kurtosis)
task1.summarize <- function(alpha, beta){
  task1.data = tibble(
    alpha = alpha, 
    beta = beta,
    mean = alpha/(alpha+beta),
    variance = (alpha*beta)/((alpha+beta)^2 *(alpha+beta+1)),
    skewness = (2*(beta-alpha)*sqrt(alpha+beta+1))/((alpha + beta + 2)*sqrt(alpha*beta)),
    e.kurtosis = (6 * ((alpha - beta)^2 * (alpha + beta + 1) - (alpha * beta * 
                 (alpha + beta + 2)))) / ((alpha * beta) * (alpha + beta + 2) * 
                 (alpha + beta + 3))
  )
  task1.data
}

# plot and summarize for each case
# case 1, beta(2,5)
case1.plot = task1.plotting(2,5)
case1.data = task1.summarize(2,5)

# case 2, beta(5,5)
case2.plot = task1.plotting(5,5)
case2.data = task1.summarize(5,5)

# case 3, beta(5,2)
case3.plot = task1.plotting(5,2)
case3.data = task1.summarize(5,2)

# case 4, beta(0.5, 0.5)
case4.plot = task1.plotting(0.5,0.5)
case4.data = task1.summarize(0.5,0.5)

# create table summarizing data from all cases
task1.summary = rbind(case1.data, case2.data, case3.data, case4.data)

# create plots (using patchwork) to summarize data from all cases
task1.plots <- (case1.plot / case2.plot) | (case3.plot / case4.plot)

################################################################################
# Task 2
# compute the moments

# beta.moment() function
beta.moment <- function(alpha, beta, k, centered){
  # centered moment
  if (centered == T){
    mean <- (alpha)/(alpha + beta)
    integrand2 <- function(x) {(x-mean)^k * dbeta(x, alpha, beta)}
    solution <- integrate(integrand2, lower = 0, upper = 1)
  }
  # uncentered moment
  if(centered == F){
    integrand <- function(x) {x^k * dbeta(x, alpha, beta)}
    solution <- integrate(integrand, lower = 0, upper = 1)
  }
  solution
}

# compute for each case, 
# using beta.moment() to compute pop level characteristics

# beta(2,5)
case1.pop.data <- tibble(alpha = 2,
                         beta = 5, 
                         mean = beta.moment(2,5,1,F)$value,
                         variance = beta.moment(2,5,2,T)$value,
                         skewness = (beta.moment(2,5,3,T)$value)/
                                    ((beta.moment(2,5,2,T)$value)^(3/2)),
                         e.kurtosis = ((beta.moment(2,5,4,T)$value)/
                                      ((beta.moment(2,5,2,T)$value)^2))-3
)

# beta(5,5)
case2.pop.data <- tibble(alpha = 5,
                         beta = 5, 
                         mean = beta.moment(5,5,1,F)$value, 
                         variance = beta.moment(5,5,2,T)$value,
                         skewness = (beta.moment(5,5,3,T)$value)/
                                    ((beta.moment(5,5,2,T)$value)^(3/2)),
                         e.kurt = ((beta.moment(5,5,4,T)$value)/
                                  ((beta.moment(5,5,2,T)$value)^2))-3
)

# beta(5,2)
case3.pop.data <- tibble(alpha = 5,
                         beta = 2, 
                         mean = beta.moment(5,2,1,F)$value, 
                         variance = beta.moment(5,2,2,T)$value,
                         skewness = (beta.moment(5,2,3,T)$value)/
                                    ((beta.moment(5,2,2,T)$value)^(3/2)),
                         e.kurtosis = ((beta.moment(5,2,4,T)$value)/
                                      ((beta.moment(5,2,2,T)$value)^2))-3
)

# beta(0.5,0.5)
case4.pop.data <- tibble(alpha = 0.5,
                         beta = 0.5, 
                         mean = beta.moment(0.5,0.5,1,F)$value, 
                         variance = beta.moment(0.5,0.5,2,T)$value,
                         skewness = (beta.moment(0.5,0.5,3,T)$value)/
                                    ((beta.moment(0.5,0.5,2,T)$value)^(3/2)),
                         e.kurtosis = ((beta.moment(0.5,0.5,4,T)$value)/
                                      ((beta.moment(0.5,0.5,2,T)$value)^2))-3
)

################################################################################
# Task 3
# do data summaries help?

# function to create numerical summaries for sample size = 500
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
  # return beta.sample, as it is used in task 4
  list(beta.sample, task3.numericalsummary)
}

# function to create histogram + density for sample size = 500
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
  beta.pdf.name = paste("Beta(", as.character(alpha), ", ", as.character(beta), 
                        ")",  sep = "")
  histogram <- ggplot(tibble(sample.data = beta.sample), aes(x=sample.data))+
    geom_histogram(aes(y=after_stat(density), color = "Sample Data Histogram"))+
    geom_density(aes(color = "Sample Data Density"), key_glyph = draw_key_path)+
    geom_line(data = fig.dat, aes(x = x, y = beta.pdf, color = beta.pdf.name))+
    xlab("x")+
    ylab("Density")
  histogram
}

# numerical summary
case1.sample = task3.numsummary(2,5)[[2]]
case2.sample = task3.numsummary(5,5)[[2]]
case3.sample = task3.numsummary(5,2)[[2]]
case4.sample = task3.numsummary(0.5,0.5)[[2]]

# histograms
case1.histogram = task3.histogram(2,5)
case2.histogram = task3.histogram(5,5)
case3.histogram = task3.histogram(5,2)
case4.histogram = task3.histogram(0.5,0.5)

# create histograms using patchwork
task3.histograms <- case1.histogram / case2.histogram | 
                    case3.histogram / case4.histogram

################################################################################
# Task 4 
# is sample size important?

# beta(2,5) sample from before
beta.task4.sample = task3.numsummary(2,5)[[1]]

# compute cumulative numsums for given sample
beta.cumstats.test <- tibble(n=(1:length(beta.task4.sample)), 
                            mean=cummean((beta.task4.sample)), 
                            variance=cumvar((beta.task4.sample)),
                            skewness=cumskew((beta.task4.sample)), 
                         # NOTE: this is kurtosis, not excess kurtosis
                         # in order to transform into e.kurtosis,
                         # e.kurt =  kurt - 3
                            kurtosis=cumkurt((beta.task4.sample)))

# cum mean plot 
cum.mean.plot <- ggplot(beta.cumstats.test)+
  geom_line(aes(x=n, y = mean))+
  geom_hline(data=case1.data, aes(yintercept = mean))  
#cum.mean.plot

# cum var plot
cum.var.plot <- ggplot(beta.cumstats.test) +
  geom_line(aes(x=n, y = variance))+
  geom_hline(data=case1.data, aes(yintercept = variance))
#cum.var.plot

# cum skewness plot
cum.skew.plot <- ggplot(beta.cumstats.test) +
  geom_line(aes(x=n, y = skewness))+
  geom_hline(data=case1.data, aes(yintercept = skewness))
#cum.skew.plot

# cum kurtosis plot (not excess kurtosis)
cum.kurt.plot <- ggplot(beta.cumstats.test) +
  geom_line(aes(x=n, y = kurtosis)) +
  # e.kurt turned into kurt
  geom_hline(data=case1.data, aes(yintercept = e.kurtosis + 3))
#cum.kurt.plot

#testing
#cum.mean.plot / cum.var.plot | cum.skew.plot / cum.kurt.plot

# for loop
# sample size = 500
alpha <- 2
beta <- 5
sample.size <- 500
for (i in 2:50){
  set.seed(7272 + i)
  # create new sample in each iteration
  beta.sample.loop <- rbeta(n = sample.size,  # sample size
                            shape1 = alpha,   # alpha parameter
                            shape2 = beta)    # beta parameter
  # create tibble for new sample
  beta.cumstats.f <- tibble(n=(1:length(beta.sample.loop)), 
                           mean=cummean((beta.sample.loop)), 
                           variance=cumvar((beta.sample.loop)),
                           skewness=cumskew((beta.sample.loop)), 
                           kurtosis=cumkurt((beta.sample.loop)))
  # update cum mean plot
  cum.mean.plot <- cum.mean.plot + 
    geom_line(data = beta.cumstats.f, aes(x=n, y=mean), color = i)
  # update cum var plot
  cum.var.plot <- cum.var.plot + 
    geom_line(data = beta.cumstats.f, aes(x=n, y=variance), color = i)
  # update cum skew plot
  cum.skew.plot <- cum.skew.plot + 
    geom_line(data = beta.cumstats.f, aes(x=n, y=skewness), color = i)
  # update cum kurtosis plot
  cum.kurt.plot <- cum.kurt.plot + 
    geom_line(data = beta.cumstats.f, aes(x=n, y=kurtosis), color = i)
  
}

# combine all plots using patchwork
cum.plots <- cum.mean.plot / cum.var.plot | cum.skew.plot / cum.kurt.plot

################################################################################
# Task 5 
# how can we model the variation? 

# simulate new data from beta(2,5) dist
alpha <- 2
beta <- 5

# same sample size
sample.size <- 500

# create tibble for summary
stats.task5 = tibble(mean = numeric(),
                  variance = numeric(),
                  skewness = numeric(),
                  kurtosis = numeric())
  
for (i in 1:1000){
  set.seed(7272 + i)
  beta.sample.task5 <- rbeta(n = sample.size,  # sample size
                          shape1 = alpha,   # alpha parameter
                          shape2 = beta)    # beta parameter
  mean=mean((beta.sample.task5)) 
  variance=var((beta.sample.task5))
  skewness=skewness((beta.sample.task5))
  kurtosis=kurtosis((beta.sample.task5))
  
  stats.task5 <- bind_rows(stats.task5, tibble(mean, variance, skewness, kurtosis))
}

# histogram + density for mean
mean.dist <- ggplot(stats.task5)+
  geom_histogram(aes(x = mean, y=after_stat(density), 
                     color = "Mean Distribution Histogram"))+
  geom_density(aes(x=mean, color = "Mean Density"), 
               key_glyph = draw_key_path)
# histogram + density for variance
variance.dist <- ggplot(stats.task5)+
  geom_histogram(aes(x = variance, y=after_stat(density),
                     color = "Variance Distribution Histogram"))+
  geom_density(aes(x=variance, color = "Variance Density"), 
               key_glyph = draw_key_path)
# histogram + density for skewness
skewness.dist <- ggplot(stats.task5)+
  geom_histogram(aes(x = skewness, y=after_stat(density),
                     color = "Skewness Distribution Histogram"))+
  geom_density(aes(x=skewness, color = "Skewness Density"), 
               key_glyph = draw_key_path)
# histogram + density for kurtosis
kurtosis.dist <- ggplot(stats.task5)+
  geom_histogram(aes(x = kurtosis, y=after_stat(density),
                     color = "Kurtosis Distribution Histogram"))+
  geom_density(aes(x=kurtosis, color = "Kurtosis Density"), 
               key_glyph = draw_key_path)

# combine plots
stats.dist <- mean.dist / variance.dist | skewness.dist / kurtosis.dist
stats.dist

# looks like normal distribution.... 

# Task 5 
# collect and clean data

# collect data
# select only 2022 data
death.dat = read_csv("API_SP.DYN.CDRT.IN_DS2_en_csv_v2_76451.csv") |>
  select(-...69) |>
  select("Country Name", "Country Code", "Indicator Name", "Indicator Code",
         "2022") |>
  rename(deaths.per.1k = "2022") |>
  mutate(death.prop = deaths.per.1k/1000)

death.prop.data = death.dat |>
  select(death.prop)


  
  
