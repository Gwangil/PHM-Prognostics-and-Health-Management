# The original MATLAB code was developed by Prof. Hyunseok Oh (Gwangju Institute of Science and Technology, South Korea, hsoh@gist.ac.kr).

rm(list=ls())
##### Add subfunction ----------------------------------------------------------------------------
setwd('D:/PHM/PHM_R/02.Diagnosis')
source('subfunction/positiveFFT.R')
source('subfunction/Find_Freqmagnitude.R')
source('subfunction/Make_Featureset.R')

##### Feature Extraction -------------------------------------------------------------------------
unbalance <- R.matlab::readMat('D:/PHM/PHM_R/data_files/unbalance.mat')$unbalance
misalign <- R.matlab::readMat('D:/PHM/PHM_R/data_files/misalign.mat')$misalign
outer <- R.matlab::readMat('D:/PHM/PHM_R/data_files/outerrace_fault.mat')$outer
inner <- R.matlab::readMat('D:/PHM/PHM_R/data_files/innerrace_fault.mat')$inner

Time <- unbalance[, 1]
N = length(Time) / 10  # divide to 10 split(1 sec * 10)
unbalance02 <- {}

Fs <- 5120
k <- 0:(N - 1)
P <- N / Fs  # period = 1 / frequency
freq <- k[1:ceiling(N / 2)] / P
Freq_unbalance02 <- {}

for (i in 1:10) {
  temp_data02 <- unbalance[((i - 1) * N + 1):(i * N), 2]
  unbalance02 <- cbind(unbalance02, temp_data02)
  Freq_unbalance02 <- cbind(Freq_unbalance02, positiveFFT(temp_data02, Fs))
}

# Time-domain health features
TF_Max <- apply(unbalance02, 2, max)
TF_Mean <- colMeans(unbalance02)
TF_RMS <- apply(unbalance02, 2, function(x) sqrt(mean(x ^ 2)))
TF_Skewness <- moments::skewness(unbalance02)
TF_Kurtosis <- moments::kurtosis(unbalance02)
TF_CrestFactor <- TF_Max / TF_RMS
TF_ShapeFactor <- TF_Max / TF_Mean
TF_ImpulseFactor <- TF_RMS / TF_Mean

Time_Featureset <- cbind(TF_Max, TF_Mean, TF_RMS, TF_Skewness, TF_Kurtosis,
                         TF_CrestFactor, TF_ShapeFactor, TF_ImpulseFactor)

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

Magnitude_under0p49x <- Find_Freqmagnitude(0:(Freq_1x * 0.49), freq, Freq_unbalance02)
Magnitude_0p51to0p99x <- Find_Freqmagnitude((Freq_1x * 0.51):(Freq_1x * 0.99), freq, Freq_unbalance02)
Magnitude_3to5x <- Find_Freqmagnitude((Freq_1x * 3):(Freq_1x * 5), freq, Freq_unbalance02)
Magnitude_1to10x <- Find_Freqmagnitude((Freq_1x):(Freq_1x * 10), freq, Freq_unbalance02)
Magnitude_FTF <- Find_Freqmagnitude(FTF, freq, Freq_unbalance02)
Magnitude_BPFI <- Find_Freqmagnitude(BPFI, freq, Freq_unbalance02)
Magnitude_BPFO <- Find_Freqmagnitude(BPFO, freq, Freq_unbalance02)
Magnitude_BSF <- Find_Freqmagnitude(BSF, freq, Freq_unbalance02)
Magnitude_0p5x <- Find_Freqmagnitude(Freq_1x * 0.5, freq, Freq_unbalance02)
Magnitude_1X <- Find_Freqmagnitude(Freq_1x, freq, Freq_unbalance02)
Magnitude_2X <- Find_Freqmagnitude(Freq_1x * 2, freq, Freq_unbalance02)
Magnitude_3X <- Find_Freqmagnitude(Freq_1x * 3, freq, Freq_unbalance02)
Magnitude_5X <- Find_Freqmagnitude(Freq_1x * 5, freq, Freq_unbalance02)
Magnitude_7X <- Find_Freqmagnitude(Freq_1x * 7, freq, Freq_unbalance02)
Magnitude_9X <- Find_Freqmagnitude(Freq_1x * 9, freq, Freq_unbalance02)

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

# Unbalance
# Unbalance02_featureset <- Make_Featureset(unbalance[, 2])
Unbalance03_featureset <- Make_Featureset(unbalance[, 3])
Unbalance04_featureset <- Make_Featureset(unbalance[, 4])

# Misalignment
# Misalign02_featureset <- Make_Featureset(misalign[, 2])
Misalign03_featureset <- Make_Featureset(misalign[, 3])

# Innerrace_fault
# Inner02_featureset <- Make_Featureset(inner[, 2])
Inner03_featureset <- Make_Featureset(inner[, 3])
Inner04_featureset <- Make_Featureset(inner[, 4])

# Outerrace_fault
# Outer02_featureset <- Make_Featureset(outer[, 2])
Outer03_featureset <- Make_Featureset(outer[, 3])
Outer04_featureset <- Make_Featureset(outer[, 4])

Featureset_Total <- rbind(Unbalance03_featureset, Unbalance04_featureset,
                          Misalign03_featureset, 
                          Inner03_featureset, Inner04_featureset,
                          Outer03_featureset, Outer04_featureset)
rownames(Featureset_Total) <- NULL

##### Data Save ----------------------------------------------------------------------------------
save(Featureset_Total, file = 'saved_data/Featureset.rds')
