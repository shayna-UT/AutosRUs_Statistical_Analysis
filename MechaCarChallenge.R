library(tidyverse)
library(qwraps2)

################################################################################
# MPG Regression

# Import dataset
mechaCar_data <- read.csv('MechaCar_mpg.csv')
head(mechaCar_data) # view the data

# Generate multiple linear regression model
lm(mpg ~ vehicle.length + vehicle.weight + spoiler.angle + ground.clearance + AWD, data=mechaCar_data) 

# Generate summary statistics
summary(lm(mpg ~ vehicle.length + vehicle.weight + spoiler.angle + ground.clearance + AWD, data=mechaCar_data)) 

################################################################################
# Suspension Coil Summary

# Import dataset
suspensionCoil_data <- read.csv('Suspension_Coil.csv')
head(suspensionCoil_data) # view the data

# Mean
mean(suspensionCoil_data$PSI)
  # output: 1499.531 PSI

# Median
median(suspensionCoil_data$PSI)
  # output: 1499.747 PSI

# Variance
var(suspensionCoil_data$PSI)
  # output: 76.23459 PSI

# Standard Deviation
sd(suspensionCoil_data$PSI)
  # output: 8.731242 PSI

# Build Summary table using qwraps2 library
our_summary <- list('Suspension Coil Summary Table'=
                    list("mean" = ~ format(round(mean(suspensionCoil_data$PSI),3)),
                         "median" = ~ format(round(median(suspensionCoil_data$PSI),3)),
                         "variance" = ~ format(round(var(suspensionCoil_data$PSI),3)),
                         "std. deviation" = ~ format(round(sd(suspensionCoil_data$PSI),3)))
                    )
suspensionCoil_table <- summary_table(suspensionCoil_data, our_summary)
  # view the table be clicking on "suspensionCoil_table" in your Global Environment

################################################################################
# Suspension Coil T-Test

# Determine if the suspension coil's pound-per-inch results are statistically different 
# from the mean population results of 1,500 pounds per inch. 

# Visualize distribution using density plot
ggplot(suspensionCoil_data,aes(x=PSI)) + geom_density() 
  # The output appears to have a bell-shaped curve indicating normality

# Test the normality of the distribution 
shapiro.test(suspensionCoil_data$PSI)
  # Since the p-value (6.011e-11) is much lower than 0.05, the data is NOT normally distributed

# Filter the data
lot1 <- suspensionCoil_data %>% filter(Manufacturing_Lot=="Lot1")
lot2 <- suspensionCoil_data %>% filter(Manufacturing_Lot=="Lot2")
lot3 <- suspensionCoil_data %>% filter(Manufacturing_Lot=="Lot3")

# Test the normality of the distribution 
shapiro.test(lot1$PSI)
    # Since the p-value (0.01091) is lower than 0.05, the data is NOT normally distributed
# Visualize distribution using density plot
ggplot(lot1,aes(x=PSI)) + geom_density() 
    # The data distribution appears to be left-skewed (the left tail is longer than the right)

# Test the normality of the distribution 
shapiro.test(lot2$PSI)
    # Since the p-value (0.3004) is greater than 0.05, the data is normally distributed
# Visualize distribution using density plot
ggplot(lot2,aes(x=PSI)) + geom_density()
    # The distribution approximates a normal distribution

# Test the normality of the distribution
shapiro.test(lot3$PSI)
    # Since the p-value (0.8299) is greater than 0.05, the data is normally distributed
# Visualize distribution using density plot
ggplot(lot3,aes(x=PSI)) + geom_density()
    # The distribution approximates a normal distribution

# Compare sample versus population mean = approx. 1500 PSI
lot2_sample <- lot2 %>% sample_n(25)
t.test(lot2_sample$PSI,mu=mean(suspensionCoil_data$PSI)) 
    # If the p-value is above 0.05, the two means are statistically similar

# Compare sample versus population mean = approx. 1500 PSI
lot3_sample <- lot3 %>% sample_n(25)
t.test(lot3_sample$PSI,mu=mean(suspensionCoil_data$PSI)) 
    # If the p-value is above 0.05, the two means are statistically similar

