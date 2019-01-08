# The original MATLAB code was developed by Prof. Hyunseok Oh (Gwangju Institute of Science and Technology, South Korea, hsoh@gist.ac.kr).

Real_Predict_2DPlot <- function(predict_label, test_data, test_label) {
  par(pty = 's', xpd = TRUE)
  l = ceiling(max(test_data))
  
  plot(test_data, pch = 16, col = test_label,
       xlim = c(-1, 1) * l, ylim = c(-1, 1) * l)
  points(test_data, pch = 5, col = predict_label, lwd = 2, cex = 1.5)
  legend("right", legend = paste(rep(c('Unbalance', "Misalign", "Inner", "Outer"), each = 2),
                                 c("(Real)", '(Predict)')),
         inset = c(-0.6, 0), bty = "n", pch = c(16, 5), col = rep(1:4, each = 2))
}