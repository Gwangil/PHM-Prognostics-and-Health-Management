# The original MATLAB code was developed by Prof. Hyunseok Oh (Gwangju Institute of Science and Technology, South Korea, hsoh@gist.ac.kr).

data_partition_manual <- function(pca_data) {
  pca_data <- as.matrix(pca_data)
  Unbalance_F1 <- pca_data[1:10, ,drop = F]
  Unbalance_F2 <- pca_data[11:20, ,drop = F]
  Misalign_F1 <- pca_data[21:30, ,drop = F]
  Inner_F1 <- pca_data[31:40, ,drop = F]
  Inner_F2 <- pca_data[41:50, ,drop = F]
  Outer_F1 <- pca_data[51:60, ,drop = F]
  Outer_F2 <- pca_data[61:70, ,drop = F]
  
  assign("train_data",
         rbind(Unbalance_F1[1:6, ,drop = F], Unbalance_F2[1:6, ,drop = F], Misalign_F1[1:6, ,drop = F],
                    Inner_F1[1:6, ,drop = F], Inner_F2[1:6, ,drop = F], Outer_F1[1:6, ,drop = F], Outer_F2[1:6, ,drop = F]),
         envir = .GlobalEnv)
  assign("train_label",
         as.factor(rep(1:4, times = c(20, 10, 20, 20) * 0.6)),
         envir = .GlobalEnv)
  
  assign("valid_data",
         rbind(Unbalance_F1[7:8, ,drop = F], Unbalance_F2[7:8, ,drop = F], Misalign_F1[7:8, ,drop = F],
               Inner_F1[7:8, ,drop = F], Inner_F2[7:8, ,drop = F], Outer_F1[7:8, ,drop = F], Outer_F2[7:8, ,drop = F]),
         envir = .GlobalEnv)
  assign("valid_label",
         as.factor(rep(1:4, times = c(20, 10, 20, 20) * 0.2)),
         envir = .GlobalEnv)
  
  assign("test_data",
         rbind(Unbalance_F1[9:10, ,drop = F], Unbalance_F2[9:10, ,drop = F], Misalign_F1[9:10, ,drop = F],
               Inner_F1[9:10, ,drop = F], Inner_F2[9:10, ,drop = F], Outer_F1[9:10, ,drop = F], Outer_F2[9:10, ,drop = F]),
         envir = .GlobalEnv)
  assign("test_label",
         as.factor(rep(1:4, times = c(20, 10, 20, 20) * 0.2)),
         envir = .GlobalEnv)
}