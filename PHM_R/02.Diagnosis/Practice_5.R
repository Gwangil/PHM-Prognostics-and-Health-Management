# The original MATLAB code was developed by Prof. Hyunseok Oh (Gwangju Institute of Science and Technology, South Korea, hsoh@gist.ac.kr).

rm(list=ls())
##### Load Data ----------------------------------------------------------------------------------
setwd('D:/PHM/PHM_R/02.Diagnosis')
load('saved_data/train_test_data.rds')
source('subfunction/Real_Predict_Plot.R')

##### Linear SVM ---------------------------------------------------------------------------------
# Training
library(e1071)
Linear_Model <- svm(y = train_label, x = train_data, data = data.frame(train_data, train_label),
                    type = 'C-classification',
                    kernel = 'linear',
                    cost = 1)

Linear_Model <- svm(y = train_label, x = train_data, data = data.frame(train_data, train_label),
                    type = 'C-classification',
                    kernel = 'polynomial',
                    degree = 1,
                    coef0 = 0,  # polynomial kernel with degree = 1, coef0 = 0 is equivalent linear kernel in Matlab
                    cost = 1,  # BoxConstraint in Matlab == cost in R
                    gamma = 1)  # gamma = 1 in R == sigma(Kernelscale)^2 in Matlab

# Test
predict_label <- predict(Linear_Model, newdata = data.frame(test_data))
Linear_test_accuracy <- sum(predict_label == test_label) / length(test_label) * 100

##### Plot ---------------------------------------------------------------------------------------
# Plot Real and Predict Data
Real_Predict_Plot(predict_label, test_data, test_label)