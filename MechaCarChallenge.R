library(tidyverse)

# Import dataset
mechaCar_data <- read.csv('MechaCar_mpg.csv')
head(mechaCar_data)

# Generate multiple linear regression model
lm(mpg ~ vehicle.length + vehicle.weight + spoiler.angle + ground.clearance + AWD, data=mechaCar_data) 

# Generate summary statistics
summary(lm(mpg ~ vehicle.length + vehicle.weight + spoiler.angle + ground.clearance + AWD, data=mechaCar_data)) 
