#---------------------------------
# Autoregressive models AR(p) in R
#---------------------------------

# parameters
mu = phi = 0.6; n = 100; sigma = 2

# sample
set.seed(2023)
xt = arima.sim(list(order=c(1,0,0), ar = phi), sd = sqrt(sigma), n = n)
xt - mu  

# sample mean
mean(xt - mu)
# [1] -0.1307798

# 95% Confidence Interval
CI = c(mean(xt - mu) - (qnorm(0.975) * (sqrt(2) / (sqrt(n)*(1-phi)))), 
       mean(xt - mu) + (qnorm(0.975) * (sqrt(2)/ (sqrt(n)*(1-phi))))
       )
CI
# -0.8237318  0.5621721

# data reshaping
df = data.frame(series = xt-mu, 
                index = seq(1,100))

# visualization
library(tidyverse)

ggplot(df, aes(x = index, y = series)) +
  geom_line(color = 'darkblue') + 
  geom_point(size = 0.6) +
  labs(title = 'AR(0.6) - sample of size 100',
       subtitle = 'Time series plot',
       y="Series", x="index") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=9, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))

#----
# end
#----