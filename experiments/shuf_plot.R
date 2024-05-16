require("data.table")
require("ggplot2")


# One seq shuffed entirely multiple times
d0 <- fread("shuf_data0.csv")
d0$pos <- 1:nrow(d0)
d0 <- melt(d0, id.vars=c("pos"), variable.name="seq", value.name="nll")

d0.fft <- fread("shuf_data0_fft.csv")
d0.fft <- melt(d0.fft, id.vars=c("freqs"), variable.name="seq", value.name="power")
# Smoothed plot of spectra
p <- ggplot(d0.fft, aes(freqs, power)) +
    geom_smooth() +
    theme_bw() + theme(plot.title = element_text(hjust = 0.5, vjust=-8, size = 12)) +
    ggtitle("Shuffled data 0") +
    labs(x = bquote(omega[k]), y = bquote(X(omega[k])))
ggsave("shuf_data0_fft.pdf", plot=p, width=5, height=5)

# FFT on normalized nll
d0.fftnorm <- fread("shuf_data0_fftnorm.csv")
d0.fftnorm <- melt(d0.fftnorm, id.vars=c("freqs"), variable.name="seq", value.name="power")
# Smoothed plot of spectra
p <- ggplot(d0.fftnorm, aes(freqs, power)) +
    geom_smooth() +
    theme_bw() + theme(plot.title = element_text(hjust = 0.5, vjust=-8, size = 12)) +
    ggtitle("Shuffled data 0") +
    labs(x = bquote(omega[k]), y = bquote(X(omega[k])))
ggsave("shuf_data0_fftnorm.pdf", plot=p, width=5, height=5)


