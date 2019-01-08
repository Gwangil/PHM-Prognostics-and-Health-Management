##### 1.1. Implementing FFT routine --------------------------------------------------------------
Fs <- 1000  # Sampling frequency
T <- 1 / Fs  # Sampling period ( or sampling interval)

N <- 5000  # Total data points (singal length)

t <- (0:(N - 1)) * T  # Time vector (time range)

k <- 0:(N - 1)  # vector from 0 to N-1
f <- (Fs / N) * k  # frequency range

x <- 2 * cos(2 * pi * 60 * t)
xn <- x + 1 * rnorm(length(t))

plot(t, xn, type = 'l', col = 'deepskyblue', lwd = 2, xlim = c(0, 0.4), xlab = 't(sec)', ylab = '')

# original fft

xnt <- fft(xn) / N
fftshift <- function(data) {
  c(tail(data, round(length(data[-1])/2)), head(data, -round(length(data[-1])/2)))
}
xntshift <- fftshift(xnt)

kr <- c(seq(0, N / 2 - 1), seq(-N / 2, -1))
fr <- (Fs / N) * kr
fs <- fftshift(fr)

par(mfrow = c(2, 1), mar = c(4, 4, 1, 1))
plot(f, abs(xnt),  type = 'l', lwd = 2, col = 'deepskyblue',
     main = "FFT", xlab = "f", ylab = '|X(f)|', ylim = c(0, 1.1))
plot(fs, abs(xntshift),  type = 'l', lwd = 2, col = 'deepskyblue',
     main = "Shifted FFT", xlab = "f", ylab = '|X(f)|', ylim = c(0, 1.1))

# single-side fft

xnt <- fft(xn) / N
xntss <- xnt[1:(N/2 + 1)]
xntss[2:(length(xntss) - 1)] = 2 * xntss[2:(length(xntss) - 1)]

fss <- f[1:(N / 2 + 1)]

par(mfrow = c(2, 1), mar = c(4, 4, 1, 1))
plot(fs, abs(xntshift), type = 'l', lwd = 2, col = 'deepskyblue',
     main = "Shifted FFT", xlab = "f", ylab = '|X(f)|', ylim = c(0, 1.1))
plot(fss, abs(xntss), type = 'l', lwd = 2, col = 'deepskyblue',
     main = "Single-sided FFT", xlab = "f", ylab = '|X(f)|')

##### 2. Extract Amplitude of the Specific Frequency ---------------------------------------------

amplitude <- abs(xntss)

f1 <- 60
c <- 20

filter01 <- rep(0, length(fss))
filter01[ceiling(N / Fs * (f1 - c)):ceiling(N / Fs * (f1 + c))] <- 1

X1 <- max(amplitude * filter01)
cat('X1 =', X1)
plot(fss, amplitude * filter01, type = 'l', col = 'deepskyblue', lwd = 2, ylab = '|X(f)|', xlab = 'f')

##### 3.1. Beats ---------------------------------------------------------------------------------
Fs <- 44100  # Smapling frequency
T <- 1 / Fs  # Sampling period (or sampling interval)

N <- 5 * Fs  # Total data points (signal length)

t <- (0:(N - 1)) * T  # Time vector (time range)

k <- 0:(N - 1)  # vector from 0 to N-1
f <- (Fs / N) * k  # frequency range

x <- cos(2 * pi * 440 * t) + cos(2 * pi * 441 * t)

par(mfrow = c(2, 1), mar = c(4, 4, 2, 1))
plot(t, x, type = 'l', col = 'deepskyblue', lwd = 2,
     main = 'beat', xlab = 't', ylab = 'x(t)', ylim = c(-3, 3))
plot(t, x, type = 'l', col = 'deepskyblue', lwd = 2,
     main = 'zoomed', xlab = 't', ylab = 'x(t)', ylim = c(-3, 3), xlim = c(0.4, 0.6))

sound <- function(data, samp.rate) {
  W <- tuneR::normalize(tuneR::Wave(data, samp.rate = samp.rate, bit = 16), unit = "16")
  tuneR::play(W)
}
sound(x, Fs)

xexp <- x * exp(-0.3 * t)

plot(t, xexp, type = 'l', col = 'deepskyblue', lwd = 2,
     main = 'beat', xlab = 't', ylab = 'x(t)', ylim = c(-3, 3))

sound(xexp, Fs)

##### 3.2. Modulation and Demodulation -----------------------------------------------------------
Fs <- 44100  # Smapling frequency
T <- 1 / Fs  # Sampling period (or sampling interval)

N <- Fs * 5 # Total data points (signal length)

t <- (0:(N - 1)) * T  # Time vector (time range)

k <- 0:(N - 1)  # vector from 0 to N-1
f <- (Fs / N) * k  # frequency range

sig <- cos(2 * pi * 1 * t) + 2
carrier <- cos(2 * pi * 100 * t)

x <- sig * carrier

plot(t, x, type = 'l', col = 'deepskyblue', lwd = 2,
     main = '', xlab = 't', ylab = 'x(t)', xlim = c(0, 2), ylim = c(-4, 4))
sound(x, Fs)

xt <- fft(x) / N

par(mfrow = c(2, 1), mar = c(4, 4, 2, 1))
plot(f, abs(xt), type = 'l', col = 'deepskyblue', lwd = 2,
     main = 'FFT', xlab = 'f', ylab = '|X(f)|')
plot(f, abs(xt), type = 'l', col = 'deepskyblue', lwd = 2,
     main = 'zoomed', xlab = 'f', ylab = '|X(f)|', xlim = c(90, 110))

# extract envelope using Hilbert transform
xe <- abs(hht::HilbertTransform(x)) # xe <- hht::HilbertEnvelope(x)
# xe <- abs(seewave::hilbert(x, Fs))

par(mfrow = c(2, 1), mar = c(4, 4, 2, 1))
plot(t, xe, type = 'l', col = 'deepskyblue', lwd = 2,
     main = 'Extracted envelope', xlab = 't', ylab = 'x(t)', ylim = c(-4, 4))
plot(t, x, type = 'l', col = 'deepskyblue', lwd = 2,
     main = '', xlab = 't', ylab = 'x(t)', ylim = c(-4, 4))
matlines(t, cbind(1 * xe, -1 * xe), col = 'red', lty = 1, lwd = 2)

xet <- fft(xe) / N
plot(f, abs(xet), type = 'l', col = 'deepskyblue', lwd = 2,
     main = 'FFT of Envelope', xlab = 'f', xlim = c(0, 10))

##### 4. STFT(Short-Time Fourier Transform) ------------------------------------------------------
Fs <- 2 ^ 10  # Sampling frequency
T <- 1 / Fs  # Sampling period (or sampling interval)

N <- Fs * 2  # Total data points (signal length)

t <- (0:(N - 1)) * T  # Time vector(time range)

k <- 0:(N - 1)  # vector from 0 to N-1
f <- (Fs / N) * k  # frequency range

x1 <- cos(2 * pi * 50 * t)
x2 <- cos(2 * pi * 100 * t)

x <- rep(0, length(t))
x[1:(N / 2)] <- x1[1:(N / 2)]
x[(N / 2 + 1):length(x)] <- x2[(N / 2 +1):length(x)]


par(mfrow = c(2, 1), mar = c(4, 4, 2, 1))
plot(t, x, type = 'l', col = 'deepskyblue', lwd = 2,
     main = '', xlab = 't', ylab = 'X(t)')
plot(t, x, type = 'l', col = 'deepskyblue', lwd = 2,
     main = '', xlab = 't', ylab = 'X(t)', xlim = c(0.9, 1.1))

xt <- fft(x) / N

par(mfrow = c(2, 1), mar = c(4, 4, 2, 1))
plot(f, abs(xt), type = 'l', col = 'deepskyblue', lwd = 2,
     main = '', xlab = 'f', ylab = '|X(f)|')
plot(f, abs(xt), type = 'l', col = 'deepskyblue', lwd = 2,
     main = 'zoomed', xlab = 'f', ylab = '|X(f)|', xlim = c(0, 300))

windowsize <- 2 ^ 7
# window <- e1071::hanning.window(windowsize)
window <- seewave::ftwindow(wl = windowsize, wn = "hanning")
# nfft <- windowsize
noverlab <- windowsize / 2  # 50% in seewave::spectro

# sft <- e1071::stft(x, win = windowsize, inc = noverlab, wtype = 'hanning.window')
# plot(sft, xaxt = 'n', xlab = 't', yaxt = 'n', ylab = 'f')
# axis(side = 1, at = seq(1, 31, length.out = 9), labels = round(t[seq(1, 31, length.out = 9) * 64], 1) )
# axis(side = 2, at = seq(0, 64, length.out = 11), labels = round(c(0, f[seq(0, 64, length.out = 11) * 31]) / 2))

STF <- seewave::spectro(wave = x, f = Fs, wl = windowsize, wn = 'hanning',
                 ovlp = 50, osc = T, dBref=2*10e-5)
seewave::spectro3D(wave = x, f = Fs, wl = windowsize, wn = 'hanning',
                 ovlp = 50, dBref=2*10e-5)