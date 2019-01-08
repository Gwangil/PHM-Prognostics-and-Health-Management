# The original MATLAB code was developed by Prof. Hyunseok Oh (Gwangju Institute of Science and Technology, South Korea, hsoh@gist.ac.kr).

Make_Featureset <- function(Data) {
  N = length(Data) / 10  # divide to 10 split(1 sec * 10)
  Fs <- 5120
  k <- 0:(N - 1)
  P <- N / Fs  # period = 1 / frequency
  freq <- k[1:ceiling(N / 2)] / P
  
  tempdata02 <- {}
  Freq_tempdata02 <- {}
  
  for (i in 1:10) {
    temp_data02 <- Data[((i - 1) * N + 1):(i * N)]
    tempdata02 <- cbind(tempdata02, temp_data02)
    Freq_tempdata02 <- cbind(Freq_tempdata02, positiveFFT(temp_data02, Fs))
  }
  
  # Time-domain health features
  F_Max <- apply(tempdata02, 2, max)
  F_Mean <- colMeans(tempdata02)
  F_RMS <- apply(tempdata02, 2, function(x) sqrt(mean(x ^ 2)))
  F_Skewness <- moments::skewness(tempdata02)
  F_Kurtosis <- moments::kurtosis(tempdata02)
  F_CrestFactor <- F_Max / F_RMS
  F_ShapeFactor <- F_Max / F_Mean
  F_ImpulseFactor <- F_RMS / F_Mean
  
  Time_Featureset <- cbind(F_Max, F_Mean, F_RMS, F_Skewness, F_Kurtosis,
                           F_CrestFactor, F_ShapeFactor, F_ImpulseFactor)
  
  # Freq.-domain health teatures
  RPM <- 3600
  Freq_1x <- RPM / 60
  theta <- 0  # Angular (degrees)
  Bd <- 6.747  # Ball diameter (mm)
  Pd <- 28.5  # Pitch diameter (mm)
  N <- 8  # Number of bearing ball
  FTF <- (Freq_1x / 2) * (1 - Bd * cos(theta) / Pd)  # Fundamental train Frequency or Cage Frequency
  BPFI <- (Freq_1x * N / 2) * (1 + Bd * cos(theta) / Pd)  # Ball Pass Frequency of Inner Race
  BPFO <- (Freq_1x * N / 2) * (1 - Bd * cos(theta) / Pd)  # Ball Pass Frequency of Outer Race
  BSF <- (Freq_1x * Pd / (2 * Bd)) * (1 - (Bd * cos(theta) / Pd) ^ 2)  # Ball Spin Frequency
  
  Magnitude_under0p49x <- Find_Freqmagnitude(0:(Freq_1x * 0.49), freq, Freq_tempdata02)
  Magnitude_0p51to0p99x <- Find_Freqmagnitude((Freq_1x * 0.51):(Freq_1x * 0.99), freq, Freq_tempdata02)
  Magnitude_3to5x <- Find_Freqmagnitude((Freq_1x * 3):(Freq_1x * 5), freq, Freq_tempdata02)
  Magnitude_1to10x <- Find_Freqmagnitude((Freq_1x):(Freq_1x * 10), freq, Freq_tempdata02)
  Magnitude_FTF <- Find_Freqmagnitude(FTF, freq, Freq_tempdata02)
  Magnitude_BPFI <- Find_Freqmagnitude(BPFI, freq, Freq_tempdata02)
  Magnitude_BPFO <- Find_Freqmagnitude(BPFO, freq, Freq_tempdata02)
  Magnitude_BSF <- Find_Freqmagnitude(BSF, freq, Freq_tempdata02)
  Magnitude_0p5x <- Find_Freqmagnitude(Freq_1x * 0.5, freq, Freq_tempdata02)
  Magnitude_1X <- Find_Freqmagnitude(Freq_1x, freq, Freq_tempdata02)
  Magnitude_2X <- Find_Freqmagnitude(Freq_1x * 2, freq, Freq_tempdata02)
  Magnitude_3X <- Find_Freqmagnitude(Freq_1x * 3, freq, Freq_tempdata02)
  Magnitude_5X <- Find_Freqmagnitude(Freq_1x * 5, freq, Freq_tempdata02)
  Magnitude_7X <- Find_Freqmagnitude(Freq_1x * 7, freq, Freq_tempdata02)
  Magnitude_9X <- Find_Freqmagnitude(Freq_1x * 9, freq, Freq_tempdata02)
  
  FF_a <- Magnitude_1X
  FF_b <- Magnitude_under0p49x / Magnitude_1X
  FF_c <- Magnitude_0p5x / Magnitude_1X
  FF_d <- Magnitude_0p51to0p99x / Magnitude_1X
  FF_e <- Magnitude_2X / Magnitude_1X
  FF_f <- Magnitude_3to5x / Magnitude_1X
  FF_g <- apply(rbind(Magnitude_3X, Magnitude_5X, Magnitude_7X, Magnitude_9X), 2, mean) / Magnitude_1X
  FF_h <- Magnitude_1to10x / Magnitude_1X
  FF_FTF <- Magnitude_FTF
  FF_BPFI <- Magnitude_BPFI
  FF_BPFO <- Magnitude_BPFO
  FF_BSF <- Magnitude_BSF
  
  Freq_Featureset <- cbind(FF_a, FF_b, FF_c, FF_d, FF_e, FF_f, FF_g, FF_h, FF_FTF, FF_BPFI, FF_BPFO, FF_BSF)
  Featureset <- cbind(Time_Featureset, Freq_Featureset)
  
  return(Featureset)
}