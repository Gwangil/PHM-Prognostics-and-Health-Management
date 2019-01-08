# The original MATLAB code was developed by Prof. Hyunseok Oh (Gwangju Institute of Science and Technology, South Korea, hsoh@gist.ac.kr).

Find_Freqmagnitude <- function(target_frequence, freq_array, magnitude_array) {
  if (length(target_frequence) > 1) {
    xscalar <- 0
  } else {
    xscalar <- 1
  }
  
  Magnitude_list <- {}
  if (xscalar == 1) {
    target_index <- which((target_frequence * 0.97 <= freq_array) & (freq_array <= target_frequence * 1.03))
    for (i in 1:ncol(magnitude_array)) {
      temp_array <- magnitude_array[, i]
      Magnitude <- max(temp_array[target_index])
      Magnitude_list <- c(Magnitude_list, Magnitude)
    }
  }
  
  if (xscalar == 0) {
    target_index <- which((min(target_frequence) <= freq_array) & (freq_array <= max(target_frequence)))
    for (i in 1:ncol(magnitude_array)) {
      temp_array <- magnitude_array[, i]
      Magnitude <- mean(temp_array[target_index])
      Magnitude_list <- c(Magnitude_list, Magnitude)
    }
  }
  return(Magnitude_list)
}