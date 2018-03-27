#---------------------------------------------------------
# 
# Authors      : Jonathan Sweeney
# Date         : 3/24/2017
# Description  : The following code examines whether sea turtle interactions have been decreasing
#                 or increasing over time.
#
#---------------------------------------------------------

# Clear memory
rm(list=ls())

# Install all packages necessary to run code
#install.packages(c("tidyverse", "rstan"))

# Load libraries
library(tidyverse)
library(rstan)

#---------------------------------------------------------
# Executed statements
#---------------------------------------------------------

# Generate data from http://www.fpir.noaa.gov/SFD/SFD_turtleint.html.
dat <- data_frame(year = c(2004:2018),                                        # Year observed   
            LH_count = c(1, 12, 17, 15, 0, 3, 7, 12, 6, 6, 15, 13, 15, 21, 31), # Loggerhead count
            LB_count = c(1, 8, 1, 5, 2, 9, 8, 16, 7, 11, 16, 5, 5, 4, 5))     # Leatherback count

# Plot points for LH and LB and total
ggplot(data = dat) +
  geom_point(mapping = aes(x = year, y = LH_count), colour = "green", alpha = 1/5) +
  geom_point(mapping = aes(x = year, y = LB_count), colour = "blue", alpha = 1/5) +
  geom_point(mapping = aes(x = year, y = LH_count + LB_count), colour = "black") +
  labs(x = "Year", y = "Turtle interactions")

ggsave("turtle_data.pdf")


## Fit the loggerhead model
# Transform data to be sent to Stan
stan.data <- lst(x1 = dat$year-min(dat$year),
                 x2 = seq(from = min(x1), to = max(x1) + 5.75, by = 0.25),
                 z1 = dat$LH_count,
                 N1 = length(dat$year),
                 N2 = length(x2),
                 alpha_rho = 4,
                 beta_rho = 1)
  
fit <- stan(file = "bycatch_rate_GP.stan",
            data = stan.data,
            iter = 8000,
            chains = 4,
            control = list(adapt_delta = 0.99))

## Fit the leatherback model
stan.data1 <- lst(x1 = dat$year-min(dat$year),
                 x2 = seq(from = min(x1), to = max(x1) + 5.75, by = 0.25),
                 z1 = dat$LB_count,
                 N1 = length(dat$year),
                 N2 = length(x2),
                 alpha_rho = 4,
                 beta_rho = 1)

fit1 <- stan(file = "bycatch_rate_GP.stan",
            data = stan.data1,
            iter = 8000,
            chains = 4,
            control = list(adapt_delta = 0.99))

ext.model <- extract(fit)
ext.model1 <- extract(fit1)

dat.y1 <- apply(exp(ext.model$y1), 2, quantile, probs = c(0.1, 0.5, 0.9))
dat.y2 <- apply(exp(ext.model$y2), 2, quantile, probs = c(0.1, 0.5, 0.9))

dat1.y1 <- apply(exp(ext.model1$y1), 2, quantile, probs = c(0.1, 0.5, 0.9))
dat1.y2 <- apply(exp(ext.model1$y2), 2, quantile, probs = c(0.1, 0.5, 0.9))

# Transpose matrix for plotting
dat.y1 <- as.data.frame(t(dat.y1))
dat.y2 <- as.data.frame(t(dat.y2))

dat1.y1 <- as.data.frame(t(dat1.y1))
dat1.y2 <- as.data.frame(t(dat1.y2))

# Append a date series
dat.y2 <- mutate(dat.y2, 
       yq = seq.Date(from = as.Date("2004/1/1"), to = as.Date("2023/12/31"), by = "quarter"))

dat1.y2 <- mutate(dat1.y2, 
                 yq = seq.Date(from = as.Date("2004/1/1"), to = as.Date("2023/12/31"), by = "quarter"))

# rename percentile columns
colnames(dat.y2)[1] <- "p10"
colnames(dat.y2)[2] <- "p50"
colnames(dat.y2)[3] <- "p90"

colnames(dat1.y2)[1] <- "p10"
colnames(dat1.y2)[2] <- "p50"
colnames(dat1.y2)[3] <- "p90"

# Fix dat year date
dat$year <- as.Date(as.character(dat$year), "%Y")

# Generate plot of forcast
ggplot() +
  geom_line(data = dat.y2, mapping = aes(x = yq, y = p50), colour = "darkgreen") +
  geom_ribbon(data = dat.y2, mapping = aes(x= yq, ymin = p10, ymax = p90), alpha =0.5, colour = "darkgreen", fill = "green") +
  geom_point(data = dat, aes(x = year, y = LH_count), colour = "darkgreen") +
  labs(x = "Year", y = "Loggerhead turtle interactions")

ggsave("lh_forecast.pdf")
  
ggplot() +
  geom_line(data = dat1.y2, mapping = aes(x = yq, y = p50), colour = "darkblue") +
  geom_ribbon(data = dat1.y2, mapping = aes(x= yq, ymin = p10, ymax = p90), alpha =0.5, colour = "darkblue", fill = "blue") +
  geom_point(data = dat, aes(x = year, y = LB_count), colour = "darkblue") +
  labs(x = "Year", y = "Leatherback turtle interactions")

ggsave("lb_forecast.pdf")
