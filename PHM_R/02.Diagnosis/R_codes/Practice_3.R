# The original MATLAB code was developed by Prof. Hyunseok Oh (Gwangju Institute of Science and Technology, South Korea, hsoh@gist.ac.kr).

rm(list=ls())
##### Load PCA Result ----------------------------------------------------------------------------
setwd('D:/PHM/PHM_R/02.Diagnosis')
load('saved_data/pca.rds')

##### K-Nearest Neighbor -------------------------------------------------------------------------
### Training Data
pca_train_data <- score[, 1:2]
y_label <- rep(1:4, times = c(20, 10, 20, 20))

### Predict
test_point <- matrix(c(0, 8, -4, 0), 2, 2)
label <- FNN::knn(train = pca_train_data, test = test_point, cl = y_label, k = 5)

### Plot
par(pty = "s")
plot(pca_train_data, pch = 16, col = y_label+1, xlab = 'Feature1', ylab = 'Feature2')
points(test_point, type = 'p', pch = "X")
points(pca_train_data[attr(label, "nn.index"),], type = 'p', cex = 2)