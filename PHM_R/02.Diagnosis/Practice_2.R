# The original MATLAB code was developed by Prof. Hyunseok Oh (Gwangju Institute of Science and Technology, South Korea, hsoh@gist.ac.kr).

rm(list=ls())
##### Load Featureset ----------------------------------------------------------------------------
setwd('D:/PHM/PHM_R/02.Diagnosis')
load('saved_data/Featureset.rds')

##### Data Normalization -------------------------------------------------------------------------
Normalized_data <- scale(Featureset_Total)

##### Dimension Reduction: PCA -------------------------------------------------------------------
covx <- cov(Normalized_data)

pca <- princomp(Normalized_data)

score <- pca$scores
coeff <- pca$loadings  # eigen(covx)$vectors
explained <- (pca$sdev^2 / sum(pca$sdev^2)) * 100
latent <- eigen(covx)$values
mu <- pca$center
# tsquared : Hotelling T squared do not use this time

##### Save PCA Results ---------------------------------------------------------------------------
save(coeff, explained, latent, mu, score, file = 'saved_data/pca.rds')
