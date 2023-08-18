library(tidyverse)
library(here)
library(readxl)
library(stargazer)
library(broom)
library(lmtest)
library(sandwich)
library(ggplot2)
library(strucchange)
library(Hmisc)
library(corrplot)
library(writexl)

write_xlsx(reg_data2, str_c(wd, '/DataSet.xlsx'))

wd <- here()

reg_data <- read_xlsx(str_c(wd, '/Dataset.xlsx'))

summary(reg_data)

stargazer(as.data.frame(reg_data), 
          title = 'Summary Statistics',
          summary.stat = c('max', 'min', 'mean', 'median', 'sd'),
          omit = c('year', 'l_emissions', 'l_consumption', 'hdd', 'cdd'),
          type = 'html')

# Plot dependent variables.

plot_data <- reg_data %>%
  group_by(year) %>%
  summarise(capacity = sum(capacity), emissions = sum(emissions)) %>%
  ungroup()

ggplot(data = plot_data, aes(x = year, y = capacity)) +
  geom_line() +
  theme_minimal() +
  ggtitle('Figure 1: Solar Capacity Over Time') +
  xlab('Year') +
  ylab('Capacity')

ggplot(data = plot_data, aes(x = year, y = emissions)) +
  geom_line() +
  theme_minimal() +
  ggtitle('Figure 2: Electricity Emissions Over Time') +
  xlab('Year') +
  ylab('Emissions')

# Run a basic regression.

eq1 <- emissions ~ tariff + gdp + consumption + white + dem + capacity + 
  max_tm + factor(county) + factor(year)
eq2 <- capacity ~ tariff + gdp + consumption + white + dem + max_tm +
  factor(county) + factor(year)

output <- lm(eq1, data = reg_data)
summary(output)

output3 <- lm(eq2, data = reg_data)

stargazer(output3, output, type = 'html', 
          title = 'Effects of Feed-in Tariff',
          keep = c('tariff', 'gdp', 'consumption', 'white', 'dem', 'capacity',
                   'min_tm', 'max_tm'))

# Correlation Matrix.

corr_matrix <- rcorr(as.matrix(reg_data %>% 
                                 select(-c(county, year, hdd, cdd, emissions)) %>%
                                 rename('Cap' = capacity,
                                        'FiT' = tariff,
                                        'GDP' = gdp,
                                        'Cons' = consumption,
                                        'W' = white,
                                        'D' = dem,
                                        'MinTm' = min_tm,
                                        'MaxTm' = max_tm)))

corr_coef <- corr_matrix$r

corrplot(corr_coef)

reg_data2 <- reg_data %>% 
  select(county, year, emissions, capacity, tariff, gdp, 
         consumption, white, dem, min_tm, max_tm)

# Breusch-Pagan Test.

boop <- tidy(bptest(eq1, data = reg_data2)) %>%
  rbind(tidy(bptest(eq2, data = reg_data2)))

# Run regression with robust standard errors.
output2 <- coeftest(output, vcov = vcovHC(output, type = 'HC1'))
output4 <- coeftest(output3, vcov = vcovHC(output3, type = 'HC1'))

stargazer(output2, output4, type = 'html', 
          title = 'Effects of Feed-in Tariff',
          keep = c('tariff', 'gdp', 'consumption', 'white', 'dem', 'capacity'),
          column.labels = c('emissions', 'capacity'))

stargazer(output3, output, type = 'html', 
          title = 'Effects of Feed-in Tariff',
          keep = c('tariff', 'gdp', 'consumption', 'white', 'dem', 'capacity',
                   'min_tm', 'max_tm'),
          column.labels = c('capacity', 'emissions'),
          se = lapply(list(output3, output), 
                      function(x) sqrt(diag(vcovHC(x, type = 'HC1')))))

# Plots.
ggplot(data = reg_data,
       aes(x = tariff, y = emissions, color = county)) +
  geom_point()

# Durbin-Watson Test.

dw <- tidy(dwtest(output3)) %>%
  rbind(tidy(dwtest(output)))

# Ramsey Reset Test.

reset <- tidy(resettest(eq2, power = 2:3, type = c("fitted", "regressor",
                                         "princomp"), data = reg_data2)) %>%
  rbind(tidy(resettest(eq1, power = 2:3, type = c("fitted", "regressor",
                                                  "princomp"), data = reg_data2)))

# Chow test.

# sctest(eq1, type = "Chow", point = _, data = reg_data)


