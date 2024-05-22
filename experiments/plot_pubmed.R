require("ggplot2")
require("data.table")


read_nll <- function(file_path, norm=FALSE) {
    nll_list <- c()
    len_list <- c()
    conn <- file(file_path, "r")
    sid <- 0
    while (TRUE) {
        line <- readLines(conn, n=1)
        if (length(line) == 0) {
            break
        }
        nll <- as.numeric(strsplit(line, " ")[[1]])
        if (norm) {
            nll <- (nll - mean(nll)) / sd(nll)
        }
        nll_list <- c(nll_list, nll)
        len_list <- c(len_list, rep(sid, length(nll)))
        sid <- sid + 1
    }
    close(conn)
    data.table(value = as.numeric(nll_list), sid = as.integer(len_list))
}




###
# NLL estimated with entire sequence "Question: ... Answer: ..."
###

# GPT-4
d.gpt4.orig <- fread("../data/gpt-4/pubmed_AnsInCtx_gpt-4.original.mistral.fftnorm.txt")
d.gpt4.samp <- fread("../data/gpt-4/pubmed_AnsInCtx_gpt-4.sampled.mistral.fftnorm.txt")
d.gpt4.orig <- add_sid(d.gpt4.orig)
d.gpt4.samp <- add_sid(d.gpt4.samp)
d.gpt4.orig$type <- "Human"
d.gpt4.samp$type <- "GPT-4"
d.gpt4 <- rbind(d.gpt4.orig, d.gpt4.samp)
# Smoothed plot
p <- ggplot(d.gpt4, aes(freq, power, color=type)) +
    geom_smooth() +
    theme_bw() + theme(plot.title = element_text(hjust = 0.5, vjust=-8, size = 12)) +
    ggtitle("PubMed: Human vs. GPT-4") +
    labs(x = bquote(omega[k]), y = bquote(X(omega[k])))
ggsave("gpt4_human_pubmed_AnsInCtx_mistral_fftnorm.pdf", plot=p, width=5, height=5)

# Plot human alone
p <- ggplot(d.gpt4.orig, aes(freq, power)) +
    geom_smooth() +
    theme_bw() + theme(plot.title = element_text(hjust = 0.5, vjust=-8, size = 12)) +
    ggtitle("PubMed: Human") +
    labs(x = bquote(omega[k]), y = bquote(X(omega[k])))
ggsave("human_pubmed_AnsInCtx_mistral_fftnorm.pdf", plot=p, width=5, height=5)

# Examine human nll distribution
nll.gpt4.orig <- read_nll("../data/gpt-4/pubmed_AnsInCtx_gpt-4.original.mistral.nll.txt")
p <- ggplot(nll.gpt4.orig, aes(value)) +
    geom_density() +
    theme_bw() + theme(plot.title = element_text(hjust = 0.5, vjust=-8, size = 12)) +
    ggtitle("PubMed: Human NLL") +
    labs(x = "NLL", y = "Density")
ggsave("human_pubmed_AnsInCtx_mistral_nll.pdf", plot=p, width=5, height=5)


# NLL logzs, FFT real
nlllogzs.fftreal.mis.orig <- fread("../data/gpt-4/pubmed_AnsInCtx_gpt-4.original.mistral.nlllogzs.fftreal.txt")
nlllogzs.fftreal.mis.samp <- fread("../data/gpt-4/pubmed_AnsInCtx_gpt-4.sampled.mistral.nlllogzs.fftreal.txt")
nlllogzs.fftreal.mis.orig$type <- "Human"
nlllogzs.fftreal.mis.samp$type <- "GPT-4"
nlllogzs.fftreal.mis <- rbind(nlllogzs.fftreal.mis.orig, nlllogzs.fftreal.mis.samp)
p <- ggplot(nlllogzs.fftreal.mis, aes(freq, power, color=type)) +
    geom_smooth() +
    theme_bw() + theme(plot.title = element_text(hjust = 0.5, vjust=-8, size = 12)) +
    ggtitle("PubMed: Human vs. GPT-4 \nNLL logzs, FFT real, est Mistral)") +
    labs(x = bquote(omega[k]), y = bquote(X(omega[k])))
ggsave("gpt4_human_pubmed_AnsInCtx_mistral_nlllogzs_fftreal.pdf", plot=p, width=5, height=5)



###
# NLL estimated with partial sequence after "Answer:"
###

# Human and GPT-4, est w/ gpt2

# NLL raw, FFT norm, 
nll.fftnorm.gpt2.orig <- fread("../data/gpt-4/pubmed_Ans_gpt-4.original.gpt2.nll.fftnorm.txt")
nll.fftnorm.gpt2.samp <- fread("../data/gpt-4/pubmed_Ans_gpt-4.sampled.gpt2.nll.fftnorm.txt")
nll.fftnorm.gpt2.orig$type <- "Human"
nll.fftnorm.gpt2.samp$type <- "GPT-4"
nll.fftnorm.gpt2 <- rbind(nll.fftnorm.gpt2.orig, nll.fftnorm.gpt2.samp)
p <- ggplot(nll.fftnorm.gpt2, aes(freq, power, color=type)) +
    geom_smooth() +
    theme_bw() + theme(plot.title = element_text(hjust = 0.5, vjust=-12, size = 12)) +
    ggtitle("PubMed: Human vs. GPT-4 \nNLL raw, FFT norm, est GPT2)") +
    labs(x = bquote(omega[k]), y = bquote(X(omega[k])))
ggsave("gpt4_human_pubmed_Ans_gpt2_nllraw_fftnorm.pdf", plot=p, width=5, height=5)

# NLL zscore, FFT norm
nllzs.fftnorm.gpt2.orig <- fread("../data/gpt-4/pubmed_Ans_gpt-4.original.gpt2.nllzs.fftnorm.txt")
nllzs.fftnorm.gpt2.samp <- fread("../data/gpt-4/pubmed_Ans_gpt-4.sampled.gpt2.nllzs.fftnorm.txt")
nllzs.fftnorm.gpt2.orig$type <- "Human"
nllzs.fftnorm.gpt2.samp$type <- "GPT-4"
nllzs.fftnorm.gpt2 <- rbind(nllzs.fftnorm.gpt2.orig, nllzs.fftnorm.gpt2.samp)
p <- ggplot(nllzs.fftnorm.gpt2, aes(freq, power, color=type)) +
    geom_smooth() +
    theme_bw() + theme(plot.title = element_text(hjust = 0.5, vjust=-12, size = 12)) +
    ggtitle("PubMed: Human vs. GPT-4 \nNLL zscore, FFT norm, est GPT2)") +
    labs(x = bquote(omega[k]), y = bquote(X(omega[k])))
ggsave("gpt4_human_pubmed_Ans_gpt2_nllzs_fftnorm.pdf", plot=p, width=5, height=5)

# NLL minmax, FFT norm
nllmm.fftnorm.gpt2.orig <- fread("../data/gpt-4/pubmed_Ans_gpt-4.original.gpt2.nllmm.fftnorm.txt")
nllmm.fftnorm.gpt2.samp <- fread("../data/gpt-4/pubmed_Ans_gpt-4.sampled.gpt2.nllmm.fftnorm.txt")
nllmm.fftnorm.gpt2.orig$type <- "Human"
nllmm.fftnorm.gpt2.samp$type <- "GPT-4"
nllmm.fftnorm.gpt2 <- rbind(nllmm.fftnorm.gpt2.orig, nllmm.fftnorm.gpt2.samp)
p <- ggplot(nllmm.fftnorm.gpt2, aes(freq, power, color=type)) +
    geom_smooth() +
    theme_bw() + theme(plot.title = element_text(hjust = 0.5, vjust=-12, size = 12)) +
    ggtitle("PubMed: Human vs. GPT-4 \nNLL minmax, FFT norm, est GPT2)") +
    labs(x = bquote(omega[k]), y = bquote(X(omega[k])))
ggsave("gpt4_human_pubmed_Ans_gpt2_nllmm_fftnorm.pdf", plot=p, width=5, height=5)


# NLL raw, FFT real
nll.fftreal.gpt2.orig <- fread("../data/gpt-4/pubmed_Ans_gpt-4.original.gpt2.nll.fftreal.txt")
nll.fftreal.gpt2.samp <- fread("../data/gpt-4/pubmed_Ans_gpt-4.sampled.gpt2.nll.fftreal.txt")
nll.fftreal.gpt2.orig$type <- "Human"
nll.fftreal.gpt2.samp$type <- "GPT-4"
nll.fftreal.gpt2 <- rbind(nll.fftreal.gpt2.orig, nll.fftreal.gpt2.samp)
p <- ggplot(nll.fftreal.gpt2, aes(freq, power, color=type)) +
    geom_smooth() +
    theme_bw() + theme(plot.title = element_text(hjust = 0.5, vjust=-12, size = 12)) +
    ggtitle("PubMed: Human vs. GPT-4 \nNLL raw, FFT real, est GPT2)") +
    labs(x = bquote(omega[k]), y = bquote(X(omega[k])))
ggsave("gpt4_human_pubmed_Ans_gpt2_nllraw_fftreal.pdf", plot=p, width=5, height=5)

# NLL zscore, FFT real
nllzs.fftreal.gpt2.orig <- fread("../data/gpt-4/pubmed_Ans_gpt-4.original.gpt2.nllzs.fftreal.txt")
nllzs.fftreal.gpt2.samp <- fread("../data/gpt-4/pubmed_Ans_gpt-4.sampled.gpt2.nllzs.fftreal.txt")
nllzs.fftreal.gpt2.orig$type <- "Human"
nllzs.fftreal.gpt2.samp$type <- "GPT-4"
nllzs.fftreal.gpt2 <- rbind(nllzs.fftreal.gpt2.orig, nllzs.fftreal.gpt2.samp)
p <- ggplot(nllzs.fftreal.gpt2, aes(freq, power, color=type)) +
    geom_smooth() +
    theme_bw() + theme(plot.title = element_text(hjust = 0.5, vjust=-12, size = 12)) +
    ggtitle("PubMed: Human vs. GPT-4 \nNLL zscore, FFT real, est GPT2)") +
    labs(x = bquote(omega[k]), y = bquote(X(omega[k])))
ggsave("gpt4_human_pubmed_Ans_gpt2_nllzs_fftreal.pdf", plot=p, width=5, height=5)

# NLL minmax, FFT real
nllmm.fftreal.gpt2.orig <- fread("../data/gpt-4/pubmed_Ans_gpt-4.original.gpt2.nllmm.fftreal.txt")
nllmm.fftreal.gpt2.samp <- fread("../data/gpt-4/pubmed_Ans_gpt-4.sampled.gpt2.nllmm.fftreal.txt")
nllmm.fftreal.gpt2.orig$type <- "Human"
nllmm.fftreal.gpt2.samp$type <- "GPT-4"
nllmm.fftreal.gpt2 <- rbind(nllmm.fftreal.gpt2.orig, nllmm.fftreal.gpt2.samp)
p <- ggplot(nllmm.fftreal.gpt2, aes(freq, power, color=type)) +
    geom_smooth() +
    theme_bw() + theme(plot.title = element_text(hjust = 0.5, vjust=-12, size = 12)) +
    ggtitle("PubMed: Human vs. GPT-4 \nNLL minmax, FFT real, est GPT2)") +
    labs(x = bquote(omega[k]), y = bquote(X(omega[k])))
ggsave("gpt4_human_pubmed_Ans_gpt2_nllmm_fftreal.pdf", plot=p, width=5, height=5)

# NLL log, FFT real
nlllog.fftreal.gpt2.orig <- fread("../data/gpt-4/pubmed_Ans_gpt-4.original.gpt2.nlllog.fftreal.txt")
nlllog.fftreal.gpt2.samp <- fread("../data/gpt-4/pubmed_Ans_gpt-4.sampled.gpt2.nlllog.fftreal.txt")
nlllog.fftreal.gpt2.orig$type <- "Human"
nlllog.fftreal.gpt2.samp$type <- "GPT-4"
nlllog.fftreal.gpt2 <- rbind(nlllog.fftreal.gpt2.orig, nlllog.fftreal.gpt2.samp)
p <- ggplot(nlllog.fftreal.gpt2, aes(freq, power, color=type)) +
    geom_smooth() +
    theme_bw() + theme(plot.title = element_text(hjust = 0.5, vjust=-12, size = 12)) +
    ggtitle("PubMed: Human vs. GPT-4 \nNLL log, FFT real, est GPT2)") +
    labs(x = bquote(omega[k]), y = bquote(X(omega[k])))
ggsave("gpt4_human_pubmed_Ans_gpt2_nlllog_fftreal.pdf", plot=p, width=5, height=5)

# NLL logzs, FFT real
nlllogzs.fftreal.gpt2.orig <- fread("../data/gpt-4/pubmed_Ans_gpt-4.original.gpt2.nlllogzs.fftreal.txt")
nlllogzs.fftreal.gpt2.samp <- fread("../data/gpt-4/pubmed_Ans_gpt-4.sampled.gpt2.nlllogzs.fftreal.txt")
nlllogzs.fftreal.gpt2.orig$type <- "Human"
nlllogzs.fftreal.gpt2.samp$type <- "GPT-4"
nlllogzs.fftreal.gpt2 <- rbind(nlllogzs.fftreal.gpt2.orig, nlllogzs.fftreal.gpt2.samp)
p <- ggplot(nlllogzs.fftreal.gpt2, aes(freq, power, color=type)) +
    geom_smooth() +
    theme_bw() + theme(plot.title = element_text(hjust = 0.5, vjust=-12, size = 12)) +
    ggtitle("PubMed: Human vs. GPT-4 \nNLL logzs, FFT real, est GPT2)") +
    labs(x = bquote(omega[k]), y = bquote(X(omega[k])))
ggsave("gpt4_human_pubmed_Ans_gpt2_nlllogzs_fftreal.pdf", plot=p, width=5, height=5)


# NLL raw, FFT imag
nll.fftimag.gpt2.orig <- fread("../data/gpt-4/pubmed_Ans_gpt-4.original.gpt2.nll.fftimag.txt")
nll.fftimag.gpt2.samp <- fread("../data/gpt-4/pubmed_Ans_gpt-4.sampled.gpt2.nll.fftimag.txt")
nll.fftimag.gpt2.orig$type <- "Human"
nll.fftimag.gpt2.samp$type <- "GPT-4"
nll.fftimag.gpt2 <- rbind(nll.fftimag.gpt2.orig, nll.fftimag.gpt2.samp)
p <- ggplot(nll.fftimag.gpt2, aes(freq, power, color=type)) +
    geom_smooth() +
    theme_bw() + theme(plot.title = element_text(hjust = 0.5, vjust=-12, size = 12)) +
    ggtitle("PubMed: Human vs. GPT-4 \nNLL raw, FFT imag, est GPT2)") +
    labs(x = bquote(omega[k]), y = bquote(X(omega[k])))
ggsave("gpt4_human_pubmed_Ans_gpt2_nllraw_fftimag.pdf", plot=p, width=5, height=5)

# NLL zscore, FFT imag
nllzs.fftimag.gpt2.orig <- fread("../data/gpt-4/pubmed_Ans_gpt-4.original.gpt2.nllzs.fftimag.txt")
nllzs.fftimag.gpt2.samp <- fread("../data/gpt-4/pubmed_Ans_gpt-4.sampled.gpt2.nllzs.fftimag.txt")
nllzs.fftimag.gpt2.orig$type <- "Human"
nllzs.fftimag.gpt2.samp$type <- "GPT-4"
nllzs.fftimag.gpt2 <- rbind(nllzs.fftimag.gpt2.orig, nllzs.fftimag.gpt2.samp)
p <- ggplot(nllzs.fftimag.gpt2, aes(freq, power, color=type)) +
    geom_smooth() +
    theme_bw() + theme(plot.title = element_text(hjust = 0.5, vjust=-12, size = 12)) +
    ggtitle("PubMed: Human vs. GPT-4 \nNLL zscore, FFT imag, est GPT2)") +
    labs(x = bquote(omega[k]), y = bquote(X(omega[k])))
ggsave("gpt4_human_pubmed_Ans_gpt2_nllzs_fftimag.pdf", plot=p, width=5, height=5)


# Human and GPT-4, est w/ gpt2xl
# NLL logzs, FFT real
nlllogzs.fftreal.gpt2xl.orig <- fread("../data/gpt-4/pubmed_Ans_gpt-4.original.gpt2xl.nlllogzs.fftreal.txt")
nlllogzs.fftreal.gpt2xl.samp <- fread("../data/gpt-4/pubmed_Ans_gpt-4.sampled.gpt2xl.nlllogzs.fftreal.txt")
nlllogzs.fftreal.gpt2xl.orig$type <- "Human"
nlllogzs.fftreal.gpt2xl.samp$type <- "GPT-4"
nlllogzs.fftreal.gpt2xl <- rbind(nlllogzs.fftreal.gpt2xl.orig, nlllogzs.fftreal.gpt2xl.samp)
p <- ggplot(nlllogzs.fftreal.gpt2xl, aes(freq, power, color=type)) +
    geom_smooth() +
    theme_bw() + theme(plot.title = element_text(hjust = 0.5, vjust=-12, size = 12)) +
    ggtitle("PubMed: Human vs. GPT-4 \nNLL logzs, FFT real, est GPT2XL)") +
    labs(x = bquote(omega[k]), y = bquote(X(omega[k])))
ggsave("gpt4_human_pubmed_Ans_gpt2xl_nlllogzs_fftreal.pdf", plot=p, width=5, height=5)

# NLL logzs, FFT imag
nlllogzs.fftimag.gpt2xl.orig <- fread("../data/gpt-4/pubmed_Ans_gpt-4.original.gpt2xl.nlllogzs.fftimag.txt")
nlllogzs.fftimag.gpt2xl.samp <- fread("../data/gpt-4/pubmed_Ans_gpt-4.sampled.gpt2xl.nlllogzs.fftimag.txt")
nlllogzs.fftimag.gpt2xl.orig$type <- "Human"
nlllogzs.fftimag.gpt2xl.samp$type <- "GPT-4"
nlllogzs.fftimag.gpt2xl <- rbind(nlllogzs.fftimag.gpt2xl.orig, nlllogzs.fftimag.gpt2xl.samp)
p <- ggplot(nlllogzs.fftimag.gpt2xl, aes(freq, power, color=type)) +
    geom_smooth() +
    theme_bw() + theme(plot.title = element_text(hjust = 0.5, vjust=-12, size = 12)) +
    ggtitle("PubMed: Human vs. GPT-4 \nNLL logzs, FFT imag, est GPT2XL)") +
    labs(x = bquote(omega[k]), y = bquote(X(omega[k])))
ggsave("gpt4_human_pubmed_Ans_gpt2xl_nlllogzs_fftimag.pdf", plot=p, width=5, height=5)


## Use density plot to check the distribution of `power`
p <- ggplot(d.gpt4, aes(power, fill=type)) +
    geom_density(alpha=0.5) +
    theme_bw() + theme(plot.title = element_text(hjust = 0.5, vjust=-8, size = 12)) +
    ggtitle("PubMed: Human vs. GPT-4") +
    labs(x = bquote(X(omega[k])), y = "Density")
ggsave("gpt4_human_pubmed_Ans_gpt2xl_fftnorm_density.pdf", plot=p, width=5, height=5)

# log-transform on `power`
min(d.gpt4$power) # ~ -19.38
d.gpt4$logpower <- log(d.gpt4$power + 25)
p <- ggplot(d.gpt4, aes(logpower, fill=type)) +
    geom_density(alpha=0.5) +
    theme_bw() + theme(plot.title = element_text(hjust = 0.5, vjust=-8, size = 12)) +
    ggtitle("PubMed: Human vs. GPT-4") +
    labs(x = bquote(log(X(omega[k]) + 25)), y = "Density")
ggsave("gpt4_human_pubmed_Ans_gpt2xl_fftnorm_logdensity.pdf", plot=p, width=5, height=5)

d.gpt4$loglogpower <- log(d.gpt4$logpower)
p <- ggplot(d.gpt4, aes(loglogpower, fill=type)) +
    geom_density(alpha=0.5) +
    theme_bw() + theme(plot.title = element_text(hjust = 0.5, vjust=-8, size = 12)) +
    ggtitle("PubMed: Human vs. GPT-4") +
    labs(x = bquote(log(log(X(omega[k]) + 25))), y = "Density")
ggsave("gpt4_human_pubmed_Ans_gpt2xl_fftnorm_loglogdensity.pdf", plot=p, width=5, height=5)

# distinguish by `bin`
p <- ggplot(d.gpt4, aes(power, fill=type)) +
    geom_density(alpha=0.5) +
    facet_wrap(~bin) +
    theme_bw() + theme(plot.title = element_text(hjust = 0.5, vjust=0, size = 12)) +
    ggtitle("PubMed: Human vs. GPT-4 (by freq bin)") +
    labs(x = bquote(X(omega[k])), y = "Density")
ggsave("gpt4_human_pubmed_Ans_gpt2xl_fftnorm_density_bybin.pdf", width = 10, height = 10)

p <- ggplot(d.gpt4, aes(logpower, fill=type)) +
    geom_density(alpha=0.5) +
    facet_wrap(~bin) +
    theme_bw() + theme(plot.title = element_text(hjust = 0.5, vjust=0, size = 12)) +
    ggtitle("PubMed: Human vs. GPT-4 (by freq bin)") +
    labs(x = bquote(log(X(omega[k]) + 25)), y = "Density")
ggsave("gpt4_human_pubmed_Ans_gpt2xl_fftnorm_logdensity_bybin.pdf", width = 10, height = 10)