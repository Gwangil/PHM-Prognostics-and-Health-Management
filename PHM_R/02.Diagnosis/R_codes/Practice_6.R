# The original MATLAB code was developed by Prof. Hyunseok Oh (Gwangju Institute of Science and Technology, South Korea, hsoh@gist.ac.kr).

rm(list=ls())
##### Load Data ----------------------------------------------------------------------------------
setwd('D:/PHM/PHM_R/02.Diagnosis')
load('saved_data/train_test_data.rds')
source('subfunction/Real_Predict_Plot.R')

##### Kernel SVM ---------------------------------------------------------------------------------
library(e1071)
Kernel_Model <- svm(y = train_label, x = train_data, data = data.frame(train_data, train_label),
                    type = 'C-classification',
                    kernel = 'radial',
                    cost = 1,
                    gamma = 1)

# Test
predict_label <- predict(Kernel_Model, newdata = data.frame(test_data))
Kernel_test_accuracy <- sum(predict_label == test_label) / length(test_label) * 100

##### Plot ---------------------------------------------------------------------------------------
# Plot Real and Predict Data
Real_Predict_Plot(predict_label, test_data, test_label)