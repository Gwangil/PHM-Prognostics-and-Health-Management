##### 3.1. Convolution Demo ----------------------------------------------------------------------
# Convolve a rectangular pulse with itself

# pulse
x <- c(0, 0, 0, 1, 1, 1, 0, 0, 0)

# convolve pulse with itself
conv <- function(x, y, conj = TRUE, type = c("circular", "open", "filter")){
  convolve(x, y[length(y):1], conj = conj, type = type)
}
y <- conv(x, x, type = 'open')

n <- 0:(length(y) - 1)

par(mfrow = c(2, 1), mar = c(3, 2, 2, 1))
plot(x, col = 'deepskyblue', pch = 16, cex = 1, xlim = c(0, 16), ylim = c(-0.2, 1.2), main = 'pulse')
arrows(x0 = 1:9, y0 = 0, x1 = 1:9, y1 = x, col = 'deepskyblue', length = 0, angle = 0, lwd = 2)
abline(h = 0)

plot(n, y, pch = 16, col = 'deepskyblue', cex = 1, xlim = c(0, 16), ylim = c(-0.2, 3.2), main = 'pulse * pulse')
arrows(x0 = n, y0 = 0, x1 = n, y1 = y, col = 'deepskyblue', length = 0, angle = 0, lwd = 2)
abline(h = 0)

##### 3.2. Denoising a Piecewise Smooth Signal ---------------------------------------------------
# piecewise smooth signal
N <- 50
n <- 0:(N - 1)

x <- sin(pi/N*n)*c(rep(1, N/2), rep(-1, N/2))
xn <- x + 0.1 * rnorm(N, 0, 1)

# construct moving average filter impulse response of length M
M <- 5
h <- rep(0, N)
h[1:5] <- 1 / M

# convolve noisy signal with impulse response
y <- conv(x, h, type = 'open')

par(mfrow = c(2, 2), mar = c(2, 2, 2, 1))
plot(x, pch = "", ylim = c(-1.5, 1.5), xlim = c(1, N), main = "piecewise smooth signal")
arrows(x0 = n, y0 = 0, x1 = n, y1 = x, col = 'deepskyblue', length = 0, angle = 0, lwd = 2)
abline(h = 0)

plot(xn, pch = "", ylim = c(-1.5, 1.5), xlim = c(1, N), main = "piecewise smooth signal + noise")
arrows(x0 = n, y0 = 0, x1 = n, y1 = xn, col = 'deepskyblue', length = 0, angle = 0, lwd = 2)
abline(h = 0)

plot(h, pch = "", ylim = c(-1.5, 1.5), xlim = c(1, N), main = "impulse response")
arrows(x0 = n, y0 = 0, x1 = n, y1 = h, col = 'deepskyblue', length = 0, angle = 0, lwd = 2)
abline(h = 0)

plot(y, pch = "", ylim = c(-1.5, 1.5), xlim = c(1, N), main = "convouluted output")
arrows(x0 = n, y0 = 0, x1 = n, y1 = y, col = 'deepskyblue', length = 0, angle = 0, lwd = 2)
abline(h = 0)

##### 3.3. Edge Detection ------------------------------------------------------------------------
# haar wavelet edge detector
M <- 4
h <- rep(0, N)
h[1:2] <- -1 / M
h[3:4] <- 1 / M

# convolve noisy signal with impulse response
y <- conv(xn, h, type = 'open')

par(mfrow = c(2, 2), mar = c(2, 2, 2, 1))
plot(x, pch = "", ylim = c(-1.5, 1.5), xlim = c(1, N), main = "piecewise smooth signal")
arrows(x0 = n, y0 = 0, x1 = n, y1 = x, col = 'deepskyblue', length = 0, angle = 0, lwd = 2)
abline(h = 0)

plot(xn, pch = "", ylim = c(-1.5, 1.5), xlim = c(1, N), main = "piecewise smooth signal + noise")
arrows(x0 = n, y0 = 0, x1 = n, y1 = xn, col = 'deepskyblue', length = 0, angle = 0, lwd = 2)
abline(h = 0)

plot(h, pch = "", ylim = c(-1.5, 1.5), xlim = c(1, N), main = "impulse response")
arrows(x0 = n, y0 = 0, x1 = n, y1 = h, col = 'deepskyblue', length = 0, angle = 0, lwd = 2)
abline(h = 0)

plot(y, pch = "", ylim = c(-1.5, 1.5), xlim = c(1, N), main = "convouluted output")
arrows(x0 = n, y0 = 0, x1 = n, y1 = y, col = 'deepskyblue', length = 0, angle = 0, lwd = 2)
abline(h = 0)

##### 3.4. Convolution on Audio ------------------------------------------------------------------
x <- tuneR::readWave('D:/PHM_R/data_files/violin_origional.wav')
Fs <- x@samp.rate
x <- x@left

x <- x / max(x)  # normalize
sound <- function(data, samp.rate) {
  W <- tuneR::normalize(tuneR::Wave(data, samp.rate = samp.rate, bit = 16), unit = "16")
  tuneR::play(W)
}
sound(x, Fs)  # play a wave file with sampling rate Fs

# plot wave file
plot(1:length(x)/Fs, x, type = 'l', lwd = 2, col = 'deepskyblue', xlab = 'time in sec', ylab = '')

# impulse response in a closed room (by gunshot)
h <- tuneR::readWave('D:/PHM_R/data_files/gunshot.wav')
Fs <- h@samp.rate
h <- h@left
h <- h / max(h)
sound(h, Fs)

plot(1:length(h)/Fs, h, type = 'l', lwd = 2, col = 'deepskyblue', xlab = 'time in sec', ylab = '')

y <- conv(x, h, type = 'open')
y <- y / max(y)

par(mfrow = c(2, 1), mar = c(2, 2, 2, 1))
plot(1:length(x)/Fs, x, type = 'l', lwd = 2, col = 'deepskyblue', main = 'original', xlab = '', ylab = '', xlim = c(0, 6))
plot(1:length(y)/Fs, y, type = 'l', lwd = 2, col = 'deepskyblue', main = 'convoluted', xlab = '', ylab = '', xlim = c(0, 6))

# image how the music played in a closed room sounds like
sound(y, Fs)

##### 4.1. Removing Shot Noise in Audio ----------------------------------------------------------
x <- tuneR::readWave('D:/PHM_R/data_files/violin_origional.wav')
Fs <- x@samp.rate
x <- x@left

x <- x / max(x)  # normalize
sound(x, Fs)  # play a wave file with sampling rate Fs


# generate an audio signal with a salt and pepper noise: "on and off" pixels
shot_noise <- rep(0, length(x))
shot_noise[sample(1:length(x), length(x) * 0.04)] <- 1
x_noise <- x + shot_noise - mean(shot_noise);

sound(x_noise,Fs)

# apply a linear low-pass filter
h = c(1,1,1)/3
x_avg = conv(x_noise, h, type = 'open')
sound(x_avg,Fs)  # does not work very well

# apply a nonlinear filter
x_median = zoo::rollmedian(x_noise,7)
sound(x_median,Fs)  # WOW !!!

par(mfrow = c(4, 1), mar = c(2, 2, 1, 1))
plot(x, type = 'l', col = 'deepskyblue', lwd = 2)
plot(x_noise, type = 'l', col = 'deepskyblue', lwd = 2)
plot(x_avg, type = 'l', col = 'deepskyblue', lwd = 2)
plot(x_median, type = 'l', col = 'deepskyblue', lwd = 2)
