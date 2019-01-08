# The original MATLAB code was developed by Prof. Hyunseok Oh (Gwangju Institute of Science and Technology, South Korea, hsoh@gist.ac.kr).

Real_Predict_Plot <- function(predict_label, test_data, test_label){
  l <- ceiling(max(abs(test_data)))
  rgl::open3d(windowRect = c(1000, 100, 1800, 900))
  rgl::plot3d(test_data, col = test_label + 1, size = 1, type = 's',
              xlab = 'PC 1', ylab = "PC 2", zlab = "PC 3",
              xlim = c(-l, l), ylim = c(-l, l), zlim = c(-l, l))
  rgl::plot3d(test_data, col = as.numeric(predict_label) + 1, size = 15, type = 'p', add = T)
  rgl::legend3d("right",legend = paste(c('-sphere', '-dot        '),
                                       rep(c('Unbalance', "Misalign", "Inner", "Outer"), each = 2),
                                       c("(Real)", '(Predict)')),
                pch = 16,  col = rep(2:5, each = 2))
}