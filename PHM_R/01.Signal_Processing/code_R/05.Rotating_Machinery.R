##### 1. Data from Our Testbed -------------------------------------------------------------------
filename <- 'D:/PHM_R/data_files/unbalance.mat'
data <- R.matlab::readMat(filename)$unbalance

time <- data[, 1]
x <- data[, 4]

plot(time, x, type = 'l', col = 'deepskyblue', lwd = 1, xlim = c(0, 0.3), ylim = c(-1, 1),
     xlab = 't', ylab = 'x[n]')

##### 2.1. Feature Extraction from Time Domain ---------------------------------------------------
# mean
mean(x)

# variance
var(x)

# rms
sqrt(mean(x ^ 2))

# skewness
moments::skewness(x)

# kurtosis
moments::kurtosis(x)

# crest factor
max(x) / sqrt(mean(x ^ 2))

# impulse factor
max(x) / mean(x)

# shape factor
sqrt(mean(x ^ 2)) / mean(x)

##### 2.2. Feature Extraction from Frequency Domain ----------------------------------------------
T <- time[2] - time[1]
Fs <- 1 / T

N <- length(time)
L <- N / Fs
t <- seq(0, tail(time, 1), T)

# single-sided fft
xt <- fft(x) / N
xtss <- xt[1:(N / 2 + 1)]
xtss[2:(length(xtss) - 1)] <- xtss[2:(length(xtss) - 1)] * 2

k <- 0:(N - 1)
f <- (Fs / N) * k
fss <- f[1:(N / 2 + 1)]

# plot
par(mfrow = c(2, 1), mar = c(4, 4, 2, 1))
plot(fss, abs(xtss), type = 'l', col = 'deepskyblue', lwd = 1,
     xlab = '', ylab = '|X(f)|', main = 'Single-Sided Amplitude Spectrum of X[n]')
plot(fss, abs(xtss), type = 'l', col = 'deepskyblue', lwd = 1,
     xlab = 'f(Hz)', ylab = '|X(f)|', main = '', xlim = c(0, 200))

amplitude <- abs(xtss)

BPFO <- 183.183
c <- 20

filter01 <- rep(0, length(fss))
filter01[ceiling(N / Fs * (BPFO - c)):ceiling(N / Fs * (BPFO + c))] <- 1

(X1 <- max(amplitude * filter01))

##### 3.1. Unbalance -----------------------------------------------------------------------------
filename <- 'D:/PHM_R/data_files/unbalance.mat'
data <- R.matlab::readMat(filename)$unbalance

time <- data[, 1]
unbal02 <- data[, 2]
unbal03 <- data[, 3]
unbal04 <- data[, 4]

par(mfrow = c(3, 1), mar = c(4, 4, 2, 1), oma = c(0, 0, 0, 0))
plot(time, unbal02, type = 'l', col = 'deepskyblue', lwd = 1, main = 'Unbalance 02 (normal)',
     xlab = '', ylab = 'x(t)', xlim = c(0, 0.3), ylim = c(-1, 1))
plot(time, unbal03, type = 'l', col = 'deepskyblue', lwd = 1, main = 'Unbalance 03',
     xlab = '', ylab = 'x(t)', xlim = c(0, 0.3), ylim = c(-1, 1))
plot(time, unbal04, type = 'l', col = 'deepskyblue', lwd = 1, main = 'Unbalance 04',
     xlab = 'time', ylab = 'x(t)', xlim = c(0, 0.3), ylim = c(-1, 1))

T <- time[2] - time[1]
Fs <- 1 / T
N <- length(time)
L <- N / Fs
t <- seq(0, tail(time , 1), T)

# single-sided fft
ut2 <- fft(unbal02) / N
ut2ss <- ut2[1:(N / 2 + 1)]
ut2ss[2:(length(ut2ss) - 1)] <- 2 * ut2ss[2:(length(ut2ss) - 1)]

ut3 <- fft(unbal03) / N
ut3ss <- ut3[1:(N / 2 + 1)]
ut3ss[2:(length(ut3ss) - 1)] <- 2 * ut3ss[2:(length(ut3ss) - 1)]

ut4 <- fft(unbal04) / N
ut4ss <- ut4[1:(N / 2 + 1)]
ut4ss[2:(length(ut4ss) - 1)] <- 2 * ut4ss[2:(length(ut4ss) - 1)]

k <- 0:(N - 1)
f <- (Fs / N) * k
fss <- f[1:(N / 2 + 1)]

# plot
par(mfrow = c(3, 1), mar = c(4, 4, 2, 1), oma = c(0, 0, 0, 0))
plot(fss, abs(ut2ss), type = 'l', col = 'deepskyblue', lwd = 1, main = 'Normal',
     xlab = '', ylab = '|X(f)|', xlim = c(0, 150), ylim = c(0, 0.25))
plot(fss, abs(ut3ss), type = 'l', col = 'deepskyblue', lwd = 1, main = 'Unbalance 03',
     xlab = '', ylab = '|X(f)|', xlim = c(0, 150), ylim = c(0, 0.25))
plot(fss, abs(ut4ss), type = 'l', col = 'deepskyblue', lwd = 1, main = 'Unbalance 04',
     xlab = 'f', ylab = '|X(f)|', xlim = c(0, 150), ylim = c(0, 0.25))

##### 3.2. Misalignment --------------------------------------------------------------------------
filename <- 'D:/PHM_R/data_files/misalign.mat'
data <- R.matlab::readMat(filename)$misalign

time <- data[, 1]
misalign02 <- data[, 2]
misalign03 <- data[, 3]

par(mfrow = c(2, 1), mar = c(1, 1, 1, 1), oma = c(1, 1, 1, 1))
plot(time, misalign02, type = 'l', col = 'deepskyblue', lwd = 1,
     xlim = c(0, 0.3), ylim = c(-1, 1), xlab = '', ylab = '', main = '')
plot(time, misalign03, type = 'l', col = 'deepskyblue', lwd = 1,
     xlim = c(0, 0.3), ylim = c(-1, 1), xlab = '', ylab = '', main = '')

T <- time[2] - time[1]
Fs <- 1 / T
N <- length(time)
L <- N / Fs
t <- seq(0, tail(time, 1), T)

# single-sided fft
mt2 <- fft(misalign02) / N
mt2ss <- mt2[1:(N / 2 + 1)]
mt2ss[2:(length(mt2ss) - 1)] <- 2 * mt2ss[2:(length(mt2ss) - 1)]

mt3 <- fft(misalign03) / N
mt3ss <- mt3[1:(N / 2 + 1)]
mt3ss[2:(length(mt3ss) - 1)] <- 2 * mt3ss[2:(length(mt3ss) - 1)]

k <- 0:(N - 1)
f <- (Fs / N) * k
fss <- f[1:(N / 2 + 1)]

# plot
par(mfrow = c(2, 1), mar = c(4, 4, 2, 1), oma = c(0, 0, 0, 0))
plot(fss, abs(mt2ss), type = 'l', col = 'deepskyblue', lwd = 1, main = 'Misalignment 02',
     xlab = '', ylab = '|X(f)|', xlim = c(0, 150), ylim = c(0, 0.25))
plot(fss, abs(mt3ss), type = 'l', col = 'deepskyblue', lwd = 1, main = 'Misalignment 03',
     xlab = 'f (Hz)', ylab = '|X(f)|', xlim = c(0, 150), ylim = c(0, 0.25))

##### 3.3. Outer Race ----------------------------------------------------------------------------
filename <- 'D:/PHM_R/data_files/outerrace_fault.mat'
data <- R.matlab::readMat(filename)$outer

time <- data[, 1]
out02 <- data[, 2]
out03 <- data[, 3]
out04 <- data[, 4]

par(mfrow = c(3, 1), mar = c(4, 4, 2, 1), oma = c(1, 1, 1, 1))
plot(time, out02, type = 'l', col = 'deepskyblue', xlim = c(0, 0.3), ylim = c(-1, 1),
     xlab = '', ylab = 'x(t)', main = 'outer 02')
plot(time, out03, type = 'l', col = 'deepskyblue', xlim = c(0, 0.3), ylim = c(-1, 1),
     xlab = '', ylab = 'x(t)', main = 'outer 03')
plot(time, out04, type = 'l', col = 'deepskyblue', xlim = c(0, 0.3), ylim = c(-1, 1),
     xlab = 't', ylab = 'x(t)', main = 'outer 04')

T <- time[2] - time[1]
Fs <- 1 / T
N <- length(time)
L <- N / Fs
t <- seq(0, tail(time, 1), T)

ot2 <- fft(out02) / N
ot2ss <- ot2[1:(N / 2 + 1)]
ot2ss[2:(length(ot2ss) - 1)] <- 2 * ot2ss[2:(length(ot2ss) - 1)]

ot3 <- fft(out03) / N
ot3ss <- ot3[1:(N / 2 + 1)]
ot3ss[2:(length(ot3ss) - 1)] <- 2 * ot3ss[2:(length(ot3ss) - 1)]

ot4 <- fft(out04) / N
ot4ss <- ot4[1:(N / 2 + 1)]
ot4ss[2:(length(ot4ss) - 1)] <- 2 * ot4ss[2:(length(ot4ss) - 1)]

k <- 0:(N - 1)
f <- (Fs / N) * k
fss <- f[1:(N / 2 + 1)]

BSF <- 119.621
BPFO <- 183.183
BDF <- 239.242
BPFI <- 296.817

par(mfrow = c(3, 1), mar = c(4, 4, 2, 1), oma = c(1, 1, 1, 1))
plot(fss, abs(ot2ss), type = 'l', col = 'deepskyblue', xlim = c(0, 400), ylim = c(0, 0.2),
     main = 'outer 02', xlab = '', ylab = '|X(f)|')
abline(v = c(BSF, BPFO, BDF, BPFI), lty = 2, col = c('green', 'red', 'blue', 'magenta'))
plot(fss, abs(ot3ss), type = 'l', col = 'deepskyblue', xlim = c(0, 400), ylim = c(0, 0.2),
     main = 'outer 03', xlab = '', ylab = '|X(f)|')
abline(v = c(BSF, BPFO, BDF, BPFI), lty = 2, col = c('green', 'red', 'blue', 'magenta'))
plot(fss, abs(ot4ss), type = 'l', col = 'deepskyblue', xlim = c(0, 400), ylim = c(0, 0.2),
     main = 'outer 04', xlab = 'f', ylab = '|X(f)|')
abline(v = c(BSF, BPFO, BDF, BPFI), lty = 2, col = c('green', 'red', 'blue', 'magenta'))

# smoothing using moving average (MA) model

windowsize <- 5
b <- rep(1, windowsize) / windowsize
a <- 1

out02ma <- signal::filter(b, a, out02)  # ARMA filter AR ~ a, MA ~ b
out02mar <- out02 - out02ma  # use residual

out03ma <- signal::filter(b, a, out03)
out03mar <- out03 - out03ma

out04ma <- signal::filter(b, a, out04)
out04mar <- out04 - out04ma

# envelope analysis

out02mare <- abs(hht::HilbertTransform(out02mar))
out03mare <- abs(hht::HilbertTransform(out03mar))
out04mare <- abs(hht::HilbertTransform(out04mar))

out02marez <- out02mare - mean(out02mare)  # remove DC offset
out03marez <- out03mare - mean(out03mare)
out04marez <- out04mare - mean(out04mare)

T <- time[2] - time[1]
Fs <- 1 / T
N <- length(time)
L <- N / Fs
t <- seq(0, tail(time, 1), T)

# single-sided fft
oet2 <- fft(out02marez) / N
oet2ss <- oet2[1:(N / 2 + 1)]
oet2ss[2:(length(oet2ss) - 1)] <- 2 * oet2ss[2:(length(oet2ss) - 1)]

oet3 <- fft(out03marez) / N
oet3ss <- oet3[1:(N / 2 + 1)]
oet3ss[2:(length(oet3ss) - 1)] <- 2 * oet3ss[2:(length(oet3ss) - 1)]

oet4 <- fft(out04marez) / N
oet4ss <- oet4[1:(N / 2 + 1)]
oet4ss[2:(length(oet4ss) - 1)] <- 2 * oet4ss[2:(length(oet4ss) - 1)]

k <- 0:(N - 1)
f <- (Fs / N) * k
fss <- f[1:(N / 2 + 1)]

par(mfrow = c(3, 1), mar = c(4, 4, 2, 1), oma = c(1, 1, 1, 1))
plot(fss, abs(oet2ss), type = 'l', col = 'deepskyblue', xlim = c(0, 400), ylim = c(0, 0.2),
     main = 'outer 02', xlab = '', ylab = '|X(f)|')
abline(v = c(BSF, BPFO, BDF, BPFI), lty = 2, col = c('green', 'red', 'blue', 'magenta'))
plot(fss, abs(oet3ss), type = 'l', col = 'deepskyblue', xlim = c(0, 400), ylim = c(0, 0.2),
     main = 'outer 03', xlab = '', ylab = '|X(f)|')
abline(v = c(BSF, BPFO, BDF, BPFI), lty = 2, col = c('green', 'red', 'blue', 'magenta'))
plot(fss, abs(oet4ss), type = 'l', col = 'deepskyblue', xlim = c(0, 400), ylim = c(0, 0.2),
     main = 'outer 04', xlab = 'f', ylab = '|X(f)|')
abline(v = c(BSF, BPFO, BDF, BPFI), lty = 2, col = c('green', 'red', 'blue', 'magenta'))

##### 3.4. Inner Race ----------------------------------------------------------------------------
filename <- 'D:/PHM_R/data_files/innerrace_fault.mat'
data <- R.matlab::readMat(filename)$inner

time <- data[, 1]
in02 <- data[, 2]
in03 <- data[, 3]
in04 <- data[, 4]

par(mfrow = c(3, 1), mar = c(4, 4, 1.5, 1), oma = c(1, 1, 1, 1))
plot(time, in02, type = 'l', col = 'deepskyblue', xlim = c(0, 10), ylim = c(-1, 1),
     xlab = '', ylab = 'x(t)', main = 'inner 02')
plot(time, in03, type = 'l', col = 'deepskyblue', xlim = c(0, 10), ylim = c(-1, 1),
     xlab = '', ylab = 'x(t)', main = 'inner 03')
plot(time, in04, type = 'l', col = 'deepskyblue', xlim = c(0, 10), ylim = c(-1, 1),
     xlab = 't', ylab = 'x(t)', main = 'inner 04')

# smoothing using moving average (MA) model

windowsize <- 5
b <- rep(1, windowsize) / windowsize
a <- 1

in02ma <- signal::filter(b, a, in02)  # ARMA filter AR ~ a, MA ~ b
in02mar <- in02 - in02ma  # use residual

in03ma <- signal::filter(b, a, in03)
in03mar <- in03 - in03ma

in04ma <- signal::filter(b, a, in04)
in04mar <- in04 - in04ma

# envelope analysis

in02mare <- abs(hht::HilbertTransform(in02mar))
in03mare <- abs(hht::HilbertTransform(in03mar))
in04mare <- abs(hht::HilbertTransform(in04mar))

in02marez <- in02mare - mean(in02mare)  # remove DC offset
in03marez <- in03mare - mean(in03mare)
in04marez <- in04mare - mean(in04mare)

T <- time[2] - time[1]
Fs <- 1 / T
N <- length(time)
L <- N / Fs
t <- seq(0, tail(time, 1), T)

# single-sided fft
iet2 <- fft(in02marez) / N
iet2ss <- iet2[1:(N / 2 + 1)]
iet2ss[2:(length(iet2ss) - 1)] <- 2 * iet2ss[2:(length(iet2ss) - 1)]

iet3 <- fft(in03marez) / N
iet3ss <- iet3[1:(N / 2 + 1)]
iet3ss[2:(length(iet3ss) - 1)] <- 2 * iet3ss[2:(length(iet3ss) - 1)]

iet4 <- fft(in04marez) / N
iet4ss <- iet4[1:(N / 2 + 1)]
iet4ss[2:(length(iet4ss) - 1)] <- 2 * iet4ss[2:(length(iet4ss) - 1)]

k <- 0:(N - 1)
f <- (Fs / N) * k
fss <- f[1:(N / 2 + 1)]

par(mfrow = c(3, 1), mar = c(4, 4, 2, 1), oma = c(1, 1, 1, 1))
plot(fss, abs(iet2ss), type = 'l', col = 'deepskyblue', xlim = c(0, 400), ylim = c(0, 0.2),
     main = 'inner 02', xlab = '', ylab = '|X(f)|')
abline(v = c(BSF, BPFO, BDF, BPFI), lty = 2, col = c('green', 'red', 'blue', 'magenta'))
plot(fss, abs(iet3ss), type = 'l', col = 'deepskyblue', xlim = c(0, 400), ylim = c(0, 0.2),
     main = 'inner 03', xlab = '', ylab = '|X(f)|')
abline(v = c(BSF, BPFO, BDF, BPFI), lty = 2, col = c('green', 'red', 'blue', 'magenta'))
plot(fss, abs(iet4ss), type = 'l', col = 'deepskyblue', xlim = c(0, 400), ylim = c(0, 0.2),
     main = 'inner 04', xlab = 'f', ylab = '|X(f)|')
abline(v = c(BSF, BPFO, BDF, BPFI), lty = 2, col = c('green', 'red', 'blue', 'magenta'))