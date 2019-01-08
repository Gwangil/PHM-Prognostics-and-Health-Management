# The original MATLAB code was developed by Prof. Hyunseok Oh (Gwangju Institute of Science and Technology, South Korea, hsoh@gist.ac.kr).

rm(list=ls())
##### Load PCA Result ----------------------------------------------------------------------------
setwd('D:/PHM/PHM_R/02.Diagnosis')
load('saved_data/pca.rds')
source('subfunction/Train_Test_Plot.R')

##### Set Training, Test Data (Randomized Selection) ---------------------------------------------
# pca_data <- score[, 1:3]
# y_label <- rep(1:4, times = c(20, 10, 20, 20))
# 
# idx <- sample(1:nrow(pca_data), nrow(pca_data) * 0.2)
# 
# train_data <- pca_data[-idx, ]
# train_label <- y_label[-idx]
# 
# test_data <- pca_data[idx, ]
# test_label <- y_label[idx]

##### Set Training, Test Data (Deterministic Selection) ------------------------------------------
pca_data <- score[, 1:3]

Unbalance_F1 <- pca_data[1:10, ]
Unbalance_F2 <- pca_data[11:20, ]
Misalign_F1 <- pca_data[21:30, ]
Inner_F1 <- pca_data[31:40, ]
Inner_F2 <- pca_data[41:50, ]
Outer_F1 <- pca_data[51:60, ]
Outer_F2 <- pca_data[61:70, ]

train_data <- rbind(Unbalance_F1[1:8, ], Unbalance_F2[1:8, ], Misalign_F1[1:8, ],
                    Inner_F1[1:8, ], Inner_F2[1:8, ], Outer_F1[1:8, ], Outer_F2[1:8, ])
train_label <- rep(1:4, times = c(20, 10, 20, 20) * 0.8)

test_data <- rbind(Unbalance_F1[9:10, ], Unbalance_F2[9:10, ], Misalign_F1[9:10, ],
                   Inner_F1[9:10, ], Inner_F2[9:10, ], Outer_F1[9:10, ], Outer_F2[9:10, ])
test_label <- rep(1:4, times = c(20, 10, 20, 20) * 0.2)

##### PCA Plot -----------------------------------------------------------------------------------
Train_Test_Plot(train_data, train_label, test_data, test_label)

##### Data Save ----------------------------------------------------------------------------------
save(train_data, train_label, test_data, test_label, file = 'saved_data/train_test_data.rds')
