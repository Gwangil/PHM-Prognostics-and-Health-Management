# The original MATLAB code was developed by Prof. Hyunseok Oh (Gwangju Institute of Science and Technology, South Korea, hsoh@gist.ac.kr).

rm(list=ls())
##### Load Data ----------------------------------------------------------------------------------
setwd('D:/PHM/PHM_R/02.Diagnosis')
load('saved_data/pca.rds')

##### Artificial Neural Network ------------------------------------------------------------------
library(keras)

pca_train_data = score[, 1:3]
# y_label <- matrix(0, 70, 4)
# y_ctgr <- cbind(1:70, 'y_label' = factor(c(rep(1, 20), rep(2, 10), rep(3, 20), rep(4, 20))))
# y_label[y_ctgr] <- 1
y_label <- to_categorical(c(rep(1, 20), rep(2, 10), rep(3, 20), rep(4, 20)))[, -1]
y_ctgr <- c(rep(1, 20), rep(2, 10), rep(3, 20), rep(4, 20))

idx <- sample(1:3, size = nrow(pca_train_data), prob = c(0.6, 0.2, 0.2), replace = T)

input_train <- pca_train_data[idx == 1, ]
output_train <- y_label[idx == 1, ]
real_train <- y_ctgr[idx == 1]
input_valid <- pca_train_data[idx == 2, ]
output_valid <- y_label[idx == 2, ]
real_valid <- y_ctgr[idx == 2]
input_test <- pca_train_data[idx == 3, ]
output_test <- y_label[idx == 3, ]
real_test <- y_ctgr[idx == 3]

model <- keras_model_sequential()
model %>% 
  layer_dense(units = 10, activation = "softmax", input_shape = c(3)) %>% 
  layer_dense(unit = 4, activation = 'softmax')

model %>% compile(
  optimizer = optimizer_sgd(lr = 0.01),
  loss = loss_categorical_crossentropy,
  metric = 'accuracy'
)

history <- model %>% fit(
  x = input_train,
  y = output_train,
  batch_size = 1,
  epochs = 1000,
  validation_data = list(input_valid, output_valid),
  callbacks = callback_early_stopping(monitor = "val_loss",
                                      min_delta = 0,
                                      patience = 0,
                                      verbose = 1),
  view_metrics = T
)

pred_train <- predict_classes(model, input_train) + 1
pred_valid <- predict_classes(model, input_valid) + 1
pred_test <- predict_classes(model, input_test) + 1

table(real_train, pred_train)
prop.table(table(real_train, pred_train)) * 100
prop.table(table(real_train, pred_train) , 1) * 100
prop.table(table(real_train, pred_train) , 2) * 100