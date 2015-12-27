setwd("Google Drive/Coursera/Data Science Specialization/Machine Learning/Week Three/machinelearning/")
dataTraining <- read.csv("../pml-training.csv",header = T)
dataTesting <- read.csv("../pml-testing.csv",header = T)

str(dataTraining)
