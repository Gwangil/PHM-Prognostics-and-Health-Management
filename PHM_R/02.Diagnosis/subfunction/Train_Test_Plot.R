# The original MATLAB code was developed by Prof. Hyunseok Oh (Gwangju Institute of Science and Technology, South Korea, hsoh@gist.ac.kr).

Train_Test_Plot <- function(train_data, train_label, test_data, test_label){
  l <- ceiling(max(abs(c(train_data, test_data))))
  rgl::open3d(windowRect = c(1000, 100, 1800, 900))
  rgl::plot3d(train_data, col = train_label + 1, size = 1, type = 's',
              xlab = 'PC 1', ylab = "PC 2", zlab = "PC 3",
              xlim = c(-l, l), ylim = c(-l, l), zlim = c(-l, l))
  rgl::plot3d(test_data, col = test_label + 1, size = 10, type = 'p', add = T)
  rgl::legend3d("right",legend = paste(c('-sphere', '-dot        '),
                                       rep(c('Unbalance', "Misalign", "Inner", "Outer"), each = 2),
                                       c("(train)", '(test)')),
                pch = 16,  col = rep(2:5, each = 2))
}