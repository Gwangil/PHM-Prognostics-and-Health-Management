# The original MATLAB code was developed by Prof. Hyunseok Oh (Gwangju Institute of Science and Technology, South Korea, hsoh@gist.ac.kr).

positiveFFT <- function(y, Fs) {
  N <- length(y)
  k <- 0:(N - 1)
  T <- N / Fs
  freq <- k / T
  
  X = fft(y) / N * 2
  
  cutOff <- ceiling(N / 2)
  
  X <- abs(X[1:cutOff])
  freq <- freq[1:cutOff]
  return(X)
}