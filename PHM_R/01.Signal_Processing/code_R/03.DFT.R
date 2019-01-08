##### 1.1. Delta Dirac Function ------------------------------------------------------------------
theta = seq(0, 6 * pi, length.out = 2 ^ 8)
x1 <- cos(theta)
plot(theta, x1, type = 'l', col = 'deepskyblue', lwd = 2, xlab = expression(theta), ylab = '', ylim = c(-1.1, 1.1))

x2 <- x1 + cos(2 * theta)
x3 <- x2 + cos(3 * theta)
x4 <- x3 + cos(4 * theta)

par(mfrow = c(3, 1), mar = c(4, 2, 1, 1))
plot(theta, x2 / 2, type = 'l', col = 'deepskyblue', lwd = 2, xlab = expression(theta), ylab = '', ylim = c(-1.1, 1.1))
plot(theta, x3 / 3, type = 'l', col = 'deepskyblue', lwd = 2, xlab = expression(theta), ylab = '', ylim = c(-1.1, 1.1))
plot(theta, x4 / 4, type = 'l', col = 'deepskyblue', lwd = 2, xlab = expression(theta), ylab = '', ylim = c(-1.1, 1.1))

x <- rep(0, 2 ^ 8)

N <- 30
for (n in 1:N) {
  x <- x + cos(n * theta)
}

plot(theta, x / N, type = 'l', col = 'deepskyblue', lwd = 2, xlab = expression(theta), ylab = '')

##### 1.2. Squere Wave ---------------------------------------------------------------------------
N <- 2 ^ 8
x <- rep(0, N)
theta <- seq(0, 2 * pi, length.out = N)

for (n in seq(1, 20, 2)){
  x <- x + 4 / (pi * n) * sin(n * theta)
}

plot(theta / pi, x, type = 'l', col = 'deepskyblue', lwd = 2, xlab = expression(theta), ylab = '')

##### 1.3. How to Decompose a Singal into Funcdamental Harmonic Sinusoids ------------------------
xt <- fft(x, N) / N
plot(0:(N - 1), abs(xt), pch = "", xlab = 'k', ylab = '|X[k]|')
arrows(x0 = 0:(N - 1), y0 = 0, x1 = 0:(N - 1), y1 = abs(xt), col = 'deepskyblue', length = 0, angle = 0, lwd = 2)
abline(h = 0)


plot(0:(N - 1), abs(xt), pch = 16, xlab = 'k', ylab = '|X[k]|', main = 'zoomed', xlim = c(0, 20), col = 'deepskyblue')
arrows(x0 = 0:(N - 1), y0 = 0, x1 = 0:(N - 1), y1 = abs(xt), col = 'deepskyblue', length = 0, angle = 0, lwd = 2)
abline(h = 0)

##### 3.1. Fast Fourier Transformation(FFT) ------------------------------------------------------
k <- 1
N <- 8
n <- 0:(N - 1)

x <- exp(complex(imaginary = 2 * pi / N * k * n))
xt <- fft(x) / N

layout(matrix(c(1, 1, 2, 0), 2, 2))
plot(Re(x), Im(x), pch = 16, col = 'deepskyblue', cex = 1,
     xlab = 'Real {x}', ylab = 'Im {x}', main = expression(paste(e^(j * 2 * pi * k * n / N), ', k = 1')))

plot(n, abs(xt), pch = 16, col = 'deepskyblue', cex = 1, main = 'Result of FFT', xlab = 'k', ylab = '|X[k]|')
arrows(x0 = n, y0 = 0, x1 = n, y1 = abs(xt), col = 'deepskyblue', length = 0, angle = 0, lwd = 2)
abline(h = 0)

k <- 1
N <- 8
n <- 0:(N - 1)

x <- cos(2 * pi / N * k * n)
xt <- fft(x) / N

par(mfrow = c(2, 1), mar = c(4, 4, 1, 1))
plot(n, x, pch = 16, col = 'deepskyblue', xlim = c(0, 8), main = expression(paste(cos(j * 2 * pi * k * n / N), ', k = 1')),
     xlab = 'n', ylab = 'x[n]')
arrows(x0 = n, y0 = 0, x1 = n, y1 = x, col = 'deepskyblue', length = 0, angle = 0, lwd = 2)
abline(h = 0)

plot(n, abs(xt), pch = 16, col = 'deepskyblue', xlim = c(0, 8), main = 'Result of FFT', xlab = 'k', ylab = '|X[k]|')
arrows(x0 = n, y0 = 0, x1 = n, y1 = abs(xt), col = 'deepskyblue', length = 0, angle = 0, lwd = 2)
abline(h = 0)

##### 3.2. DFT Frequencies -----------------------------------------------------------------------
k <- 1  # index for freqeuncy
N <- 8
n <- 0:(N - 1)  # sampling period

x <- cos(2 * pi / N * k * n)  # harmonic complex exponential
xt <- fft(x) / N
fftshift <- function(data) {
  c(tail(data, round(length(data[-1])/2)), head(data, -round(length(data[-1])/2)))
}
xtshift <- fftshift(xt)

kr <- c(seq(0, N / 2 - 1), seq(-N / 2, -1))
ks <- fftshift(kr)

par(mfrow = c(3, 1), mar = c(4, 4, 2, 1))
plot(n, x, pch = 16, col = 'deepskyblue', xlim = c(0, 8), main = expression(paste(cos(j * 2 * pi * k * n / N), ', k = 1')),
     xlab = 'n', ylab = 'x[n]')
arrows(x0 = n, y0 = 0, x1 = n, y1 = x, col = 'deepskyblue', length = 0, angle = 0, lwd = 2)
abline(h = 0)

plot(n, abs(xt), pch = 16, col = 'deepskyblue', xlim = c(0, 8), main = 'Result of FFT', xlab = 'k', ylab = '|X[k]|')
arrows(x0 = n, y0 = 0, x1 = n, y1 = abs(xt), col = 'deepskyblue', length = 0, angle = 0, lwd = 2)
abline(h = 0)

plot(ks, abs(xtshift), pch = 16, col = 'deepskyblue', xlim = c(-4, 4), main = 'Result of Shifted FFT', xlab = 'k', ylab = '|X[k]|')
arrows(x0 = ks, y0 = 0, x1 = ks, y1 = abs(xtshift), col = 'deepskyblue', length = 0, angle = 0, lwd = 2)
abline(h = 0)

##### 4.1. Filters -------------------------------------------------------------------------------
# low-pass filter
N <- 16
n <- 0:(N - 1)

h <- rep(0, N)
h[1] <- 1
h[2] <- 1

kr <- c(seq(0, N / 2 - 1), seq(-N / 2, -1))
ks <- fftshift(kr)

ht <- fft(h)[1:N]  # fft(x, N) in matlab: N-point fft select N-point or padding 0 upto N-point
htshift <- fftshift(ht)

par(mfrow = c(2, 1), mar = c(4, 4, 2, 1))
plot(n, h, pch = 16, col = 'deepskyblue', main = 'Low-pass Filter',
     xlab = 'n', ylab = 'h[n]')
arrows(x0 = n, y0 = 0, x1 = n, y1 = h, col = 'deepskyblue', length = 0, angle = 0, lwd = 2)
abline(h = 0)

plot(ks, abs(htshift), pch = 16, col = 'deepskyblue', xlim = c(-N/2, N/2), xlab = 'k', ylab = '|H[k]|')
arrows(x0 = ks, y0 = 0, x1 = ks, y1 = abs(htshift), col = 'deepskyblue', length = 0, angle = 0, lwd = 2)
abline(h = 0)

# high-pass filter
N <- 16
n <- 0:(N - 1)

h <- rep(0, N)
h[1] <- 1
h[2] <- -1

kr <- c(seq(0, N / 2 - 1), seq(-N / 2, -1))
ks <- fftshift(kr)

ht <- fft(h)[1:N]  # fft(x, N) in matlab: N-point fft select N-point or padding 0 upto N-point
htshift <- fftshift(ht)

par(mfrow = c(2, 1), mar = c(4, 4, 2, 1))
plot(n, h, pch = 16, col = 'deepskyblue', main = 'High-pass Filter',
     xlab = 'n', ylab = 'h[n]')
arrows(x0 = n, y0 = 0, x1 = n, y1 = h, col = 'deepskyblue', length = 0, angle = 0, lwd = 2)
abline(h = 0)

plot(ks, abs(htshift), pch = 16, col = 'deepskyblue', xlim = c(-N/2, N/2), xlab = 'k', ylab = '|H[k]|')
arrows(x0 = ks, y0 = 0, x1 = ks, y1 = abs(htshift), col = 'deepskyblue', length = 0, angle = 0, lwd = 2)
abline(h = 0)

# band-pass filter
N <- 16
n <- 0:(N - 1)

h <- rep(0, N)
h[1] <- 1
h[2] <- 0
h[3] <- -1

kr <- c(seq(0, N / 2 - 1), seq(-N / 2, -1))
ks <- fftshift(kr)

ht <- fft(h)[1:N]  # fft(x, N) in matlab: N-point fft select N-point or padding 0 upto N-point
htshift <- fftshift(ht)

par(mfrow = c(2, 1), mar = c(4, 4, 2, 1))
plot(n, h, pch = 16, col = 'deepskyblue', main = 'Band-pass Filter',
     xlab = 'n', ylab = 'h[n]')
arrows(x0 = n, y0 = 0, x1 = n, y1 = h, col = 'deepskyblue', length = 0, angle = 0, lwd = 2)
abline(h = 0)

plot(ks, abs(htshift), pch = 16, col = 'deepskyblue', xlim = c(-N/2, N/2), xlab = 'k', ylab = '|H[k]|')
arrows(x0 = ks, y0 = 0, x1 = ks, y1 = abs(htshift), col = 'deepskyblue', length = 0, angle = 0, lwd = 2)
abline(h = 0)

##### 4.2. FFT and Convolution -------------------------------------------------------------------
# piecewise smooth signal
N <- 50
n <- 0:(N - 1)

x <- sin(pi / N * n) * c(rep(1, N /2 ), rep(-1, N / 2))
xn <- x + 0.1 * rnorm(N)

# construct moving average filter impulse response of length M
M <- 5
h <- rep(0, N)
h[1:M] <- 1 / M

# convolve noisy signal with impulse response
conv <- function(x, y, conj = TRUE, type = c("circular", "open", "filter")){
  convolve(x, y[length(y):1], conj = conj, type = type)
}
y <- conv(xn, h, type = 'open')
# y1 <- signal::conv(xn, h)

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

k <- 0:(N - 1)

ht <- fft(h)
xnt <- fft(xn)
ifft <- function(data) {
  Re(fft(data, inverse = T) / length(data))
}
# signal::ifft()
# fftw::IFFT()
xf <- ifft(ht * xnt)

par(mfrow = c(2, 1), mar = c(2, 2, 2, 1))
plot(y, pch = "", ylim = c(-1.5, 1.5), xlim = c(1, N), main = "convolution")
arrows(x0 = n, y0 = 0, x1 = n, y1 = y, col = 'deepskyblue', length = 0, angle = 0, lwd = 2)
abline(h = 0)

plot(k, xf, pch = "", ylim = c(-1.5, 1.5), xlim = c(1, N), main = "IFFT", xlab = 'k')
arrows(x0 = k, y0 = 0, x1 = k, y1 = xf, col = 'deepskyblue', length = 0, angle = 0, lwd = 2)
abline(h = 0)

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

k <- 0:(N - 1)

ht <- fft(h)
xnt <- fft(xn)
xf <- ifft(ht * xnt)

par(mfrow = c(2, 1), mar = c(2, 2, 2, 1))
plot(y, pch = "", ylim = c(-1.5, 1.5), xlim = c(1, N), main = "convolution")
arrows(x0 = n, y0 = 0, x1 = n, y1 = y, col = 'deepskyblue', length = 0, angle = 0, lwd = 2)
abline(h = 0)

plot(k, xf, pch = "", ylim = c(-1.5, 1.5), xlim = c(1, N), main = "IFFT", xlab = 'k')
arrows(x0 = k, y0 = 0, x1 = k, y1 = xf, col = 'deepskyblue', length = 0, angle = 0, lwd = 2)
abline(h = 0)