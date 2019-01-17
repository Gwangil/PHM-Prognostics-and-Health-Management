##### 1.1 Plot Real Signals ----------------------------------------------------------------------
t <- seq(0, 2, 0.01)
x <- sin(2 * pi * t)

plot(t, x, type = 'l', lwd = 2, col = 'deepskyblue', ylim = c(-1.1, 1.1), xlab = 't [sec]', ylab = 'x(t)')


N <- 20
n <- 0:(N - 1)
x <- sin(2 * pi/N * n)

par(mfrow = c(2, 1), mar = c(4, 4, 1, 1))
plot(n, x, pch = 16, col = 'deepskyblue')

plot(n, x, col = 'deepskyblue', cex = 1.5)
arrows(x0 = n, y0 = 0, x1 = n, y1 = x, col = 'deepskyblue', length = 0, angle = 0, lwd = 2)
abline(h = 0)

##### 1.2 Signal Sounds --------------------------------------------------------------------------
fs <- 44100  # sampling frequency
N <- 145000  # # of data points

# signal: "Alas, Poor Yorick!"
alas <- R.matlab::readMat("D:/PHM_R/data_files/hamlet.mat")$alas
hamlet <- 2 * alas[1:N]

plot(0:(N - 1) / fs, hamlet, type = 'l', col = 'deepskyblue', ylab = "", xlab = "")

sound <- function(data, samp.rate) {
  W <- tuneR::normalize(tuneR::Wave(data, samp.rate = samp.rate, bit = 16), unit = "16")
  tuneR::play(W)
}
sound(hamlet, fs)

# cosine wave
n <- 0:(N - 1)
coswave <- cos(2 * pi / 200 * n)
plot(n / fs, coswave, type = 'l', lwd = 2, col = 'deepskyblue', xlim = c(1, 1.1), xlab = "", ylab = "")

sound(coswave, fs)

# chirp (https://en.wikipedia.org/wiki/Chirp)
chirp <- 0.3 * cos(2 * pi / 3000000 * n ^ 2)
plot(n / fs, chirp, type = 'l', lwd = 2, col = 'deepskyblue', xlim = c(0, 0.4), xlab = "", ylab = "")

sound(chirp, fs)

# white gaussian noise
whitenoise <- 0.1 * rnorm(N, 0, 1)
plot(n / fs, whitenoise, type = 'l', lwd = 2, col = 'deepskyblue', xlim = c(0, 0.4), xlab = "", ylab = "")

sound(whitenoise, fs)

##### 3. Discrete Sinusoids ----------------------------------------------------------------------
n <- 0:7
x1 <- cos(pi * n)
x2 <- cos(3/2 * pi * n)

par(mfrow = c(2, 1), mar = c(4, 2, 1, 1))
plot(n, x1, col = 'deepskyblue', cex = 1.5, main = expression(x[1]), xlab = "")
arrows(x0 = n, y0 = 0, x1 = n, y1 = x1, col = 'deepskyblue', length = 0, angle = 0, lwd = 2)
abline(h = 0)

plot(n, x2, col = 'deepskyblue', cex = 1.5, main = expression(x[2]))
arrows(x0 = n, y0 = 0, x1 = n, y1 = x2, col = 'deepskyblue', length = 0, angle = 0, lwd = 2)
abline(h = 0)

##### 3.1. Frequency in Discrete Sinusoids -------------------------------------------------------

n <- 0:31
x1 <- cos(0 * pi * n)
x2 <- cos(1/8 * pi * n)
x3 <- cos(2/8 * pi * n)
x4 <- cos(1 * pi * n)

par(mfrow = c(4, 1), mar = c(2.5, 4, 1, 1))
plot(n, x1, col = 'deepskyblue', pch = 16, cex = 1.5, main = expression(x[1]),
     xlim = c(0, 31), ylim = c(-1.5, 1.5), ylab = '0')
arrows(x0 = n, y0 = 0, x1 = n, y1 = x1, col = 'deepskyblue', length = 0, angle = 0, lwd = 2)
abline(h = 0)

plot(n, x2, col = 'deepskyblue', pch = 16, cex = 1.5, main = expression(x[2]),
     xlim = c(0, 31), ylim = c(-1.5, 1.5), ylab = expression(1/8 ~ pi))
arrows(x0 = n, y0 = 0, x1 = n, y1 = x2, col = 'deepskyblue', length = 0, angle = 0, lwd = 2)
abline(h = 0)

plot(n, x3, col = 'deepskyblue', pch = 16, cex = 1.5, main = expression(x[3]),
     xlim = c(0, 31), ylim = c(-1.5, 1.5), ylab = expression(2/8 ~ pi))
arrows(x0 = n, y0 = 0, x1 = n, y1 = x3, col = 'deepskyblue', length = 0, angle = 0, lwd = 2)
abline(h = 0)

plot(n, x4, col = 'deepskyblue', pch = 16, cex = 1.5, main = expression(x[4]),
     xlim = c(0, 31), ylim = c(-1.5, 1.5), ylab = expression(8/8 ~ pi))
arrows(x0 = n, y0 = 0, x1 = n, y1 = x4, col = 'deepskyblue', length = 0, angle = 0, lwd = 2)
abline(h = 0)

n <- 0:31
x5 <- cos(2 * pi * n)
x6 <- cos(15/8 * pi * n)
x7 <- cos(14/8 * pi * n)
x8 <- cos(1 * pi * n)

par(mfrow = c(4, 1), mar = c(2.5, 4, 1, 1))
plot(n, x5, col = 'deepskyblue', pch = 16, cex = 1.5, main = expression(x[1]),
     xlim = c(0, 31), ylim = c(-1.5, 1.5), ylab = expression(2 ~ pi))
arrows(x0 = n, y0 = 0, x1 = n, y1 = x5, col = 'deepskyblue', length = 0, angle = 0, lwd = 2)
abline(h = 0)

plot(n, x6, col = 'deepskyblue', pch = 16, cex = 1.5, main = expression(x[2]),
     xlim = c(0, 31), ylim = c(-1.5, 1.5), ylab = expression(15/8 ~ pi))
arrows(x0 = n, y0 = 0, x1 = n, y1 = x6, col = 'deepskyblue', length = 0, angle = 0, lwd = 2)
abline(h = 0)

plot(n, x7, col = 'deepskyblue', pch = 16, cex = 1.5, main = expression(x[3]),
     xlim = c(0, 31), ylim = c(-1.5, 1.5), ylab = expression(14/8 ~ pi))
arrows(x0 = n, y0 = 0, x1 = n, y1 = x7, col = 'deepskyblue', length = 0, angle = 0, lwd = 2)
abline(h = 0)

plot(n, x8, col = 'deepskyblue', pch = 16, cex = 1.5, main = expression(x[4]),
     xlim = c(0, 31), ylim = c(-1.5, 1.5), ylab = expression(8/8 ~ pi))
arrows(x0 = n, y0 = 0, x1 = n, y1 = x8, col = 'deepskyblue', length = 0, angle = 0, lwd = 2)
abline(h = 0)

##### 3.2. Aliasing ------------------------------------------------------------------------------
t <- seq(0, 10 * 2 * pi, length.out = 300)
x <- sin(t)
y <- cos(t)

par(mfrow = c(2, 1), mar = c(2, 4, 1, 1))
plot(t, x, col = 'deepskyblue', type = 'l', lwd = 2, main = "", ylab = 'sin(t)')
plot(t, y, col = 'deepskyblue', type = 'l', lwd = 2, main = "", ylab = 'cos(t)')

ts <- seq(0, 10 * 2 * pi, length.out = 12)
xs <- sin(ts)
ys <- cos(ts)

par(mfrow = c(2, 1), mar = c(2, 4, 1, 1))
plot(t, x, col = 'deepskyblue', type = 'l', lwd = 2, main = "", ylab = 'sin(t)')
lines(ts, xs, col = 'orange', type = 'o', lwd = 2, lty = 2)
plot(t, y, col = 'deepskyblue', type = 'l', lwd = 2, main = "", ylab = 'cos(t)')
lines(ts, ys, col = 'orange', type = 'o', lwd = 2, lty = 2)

ts <- seq(0, 10 * 2 * pi, length.out = 11)
xs <- sin(ts)
ys <- cos(ts)

par(mfrow = c(2, 1), mar = c(2, 4, 1, 1))
plot(t, x, col = 'deepskyblue', type = 'l', lwd = 2, main = "", ylab = 'sin(t)')
lines(ts, xs, col = 'orange', type = 'o', lwd = 2, lty = 2)
plot(t, y, col = 'deepskyblue', type = 'l', lwd = 2, main = "", ylab = 'cos(t)')
lines(ts, ys, col = 'orange', type = 'o', lwd = 2, lty = 2)

ts <- seq(0, 10 * 2 * pi, length.out = 21)
xs <- sin(ts)
ys <- cos(ts)

par(mfrow = c(2, 1), mar = c(2, 4, 1, 1))
plot(t, x, col = 'deepskyblue', type = 'l', lwd = 2, main = "", ylab = 'sin(t)')
lines(ts, xs, col = 'orange', type = 'o', lwd = 2, lty = 2)
plot(t, y, col = 'deepskyblue', type = 'l', lwd = 2, main = "", ylab = 'cos(t)')
lines(ts, ys, col = 'orange', type = 'o', lwd = 2, lty = 2)

##### 4.1. Damped Free Oscillation ---------------------------------------------------------------
# real and image parts
N <- 20
n <- 0:(2 * N - 1)
x <- exp(-n/N) * exp(complex(imaginary = 2 * pi / N * n))

par(mfrow = c(2, 1), mar = c(2, 4, 1, 1))
plot(n, Re(x), col = 'deepskyblue', cex = 1, main = '', ylim = c(-1, 1), ylab = 'real')
arrows(x0 = n, y0 = 0, x1 = n, y1 = Re(x), col = 'deepskyblue', length = 0, angle = 0, lwd = 2)
abline(h = 0)
matlines(x = n, y = cbind(1 * exp(-n / N), -1 * exp(-n / N)), type = 'l', lwd = 2, lty = 2, col = 'red')

plot(n, Im(x), col = 'deepskyblue', cex = 1, main = '', ylim = c(-1, 1), ylab = 'real')
arrows(x0 = n, y0 = 0, x1 = n, y1 = Im(x), col = 'deepskyblue', length = 0, angle = 0, lwd = 2)
abline(h = 0)
matlines(x = n, y = cbind(1 * exp(-n / N), -1 * exp(-n / N)), type = 'l', lwd = 2, lty = 2, col = 'red')

plot_ly(x = ~n, y = ~Re(x), z = ~Im(x), type = "scatter3d", mode = "markers")

# polar coordinate
plot_ly(theta = ~Arg(x)/pi*180, r = ~Mod(x), type = 'scatterpolar', mode = "markers")
