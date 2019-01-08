# The original MATLAB code was developed by Prof. Hyunseok Oh (Gwangju Institute of Science and Technology, South Korea, hsoh@gist.ac.kr).

rm(list=ls())
##### Load Data ----------------------------------------------------------------------------------
setwd('D:/PHM/PHM_R/02.Diagnosis')
load('saved_data/train_test_valid_data.rds')
source('subfunction/Real_Predict_2DPlot.R')

##### SVM Test -----------------------------------------------------------------------------------
library(e1071)

Gaussian_SVM <- svm(y = train_label, x = train_data, data = data.frame(train_data, train_label),
                    type = 'C-classification',
                    cost = 1,
                    kernel = 'radial',
                    gamma = 1)

predict_label <- predict(Gaussian_SVM, test_data)
Test_accuracy <- sum(predict_label == test_label) / length(test_label) * 100

##### Plot ---------------------------------------------------------------------------------------
Real_Predict_2DPlot(predict_label, test_data, test_label)