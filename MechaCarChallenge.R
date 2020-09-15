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
