require("ggplot2")
require("data.table")

# Add sequence ID
add_sid <- function(dt) {
    dt[, freq2 := shift(freq, 1, type="lead", fill=0.5)]
    dt$diffSeries <- dt$freq > dt$freq2
    dt$sid <- cumsum(dt$diffSeries)
    dt$sid <- shift(dt$sid, 1, type="lag", fill=0)
    dt[, .(freq, power, sid)]
}

###
# NLL estimated with entire sequence "Question: ... Answer: ..."
###

# GPT-4
# d.gpt4.orig <- fread("../data/gpt-4/writing_AnsInCtx_gpt-4.original.mistral.fftnorm.txt")
# d.gpt4.samp <- fread("../data/gpt-4/writing_AnsInCtx_gpt-4.sampled.mistral.fftnorm.txt")
# d.gpt4.orig <- fread("fft/writing_gpt-4.bigram.original.fft.txt")
# d.gpt4.samp <- fread("fft/writing_gpt-4.bigram.sampled.fft.txt")

# d.gpt4.orig <- fread("data/gpt-4/verb_masked/fftnorm/writing_gpt-4.original.2gram_mask_period_0.fftnorm.txt")
# d.gpt4.samp <- fread("data/gpt-4/verb_masked/fftnorm/writing_gpt-4.sampled.2gram_mask_period_0.fftnorm.txt")

d.gpt4.orig <- fread("data/gpt-4/verb_masked/fftnorm/writing_gpt-4.sampled.gpt2_mask_verb.fftnorm.txt")
d.gpt4.samp <- fread("data/gpt-4/verb_masked/fftnorm/writing_gpt-4.sampled.gpt2.nll.fftnorm.txt")
d.gpt4.human <- fread("data/gpt-4/verb_masked/fftnorm/writing_gpt-4.original.gpt2.nll.fftnorm.txt")
d.gpt4.human_verb_masked <- fread("data/gpt-4/verb_masked/fftnorm/writing_gpt-4.original.gpt2_mask_verb.fftnorm.txt")

d.gpt4.orig <- add_sid(d.gpt4.orig)
d.gpt4.samp <- add_sid(d.gpt4.samp)
d.gpt4.human <- add_sid(d.gpt4.human)
d.gpt4.human_verb_masked <- add_sid(d.gpt4.human_verb_masked)

d.gpt4.orig$type <- "GPT-4 verb Masked"
d.gpt4.samp$type <- "GPT-4"
d.gpt4.human$type <- "Human"
d.gpt4.human_verb_masked$type <- "Human Verb Masked"

d.gpt4 <- rbind(d.gpt4.orig, d.gpt4.samp, d.gpt4.human, d.gpt4.human_verb_masked)
# Smoothed plot

p <- ggplot(d.gpt4, aes(freq, power, color=type)) +
    geom_smooth()
    theme_bw() + theme(plot.title = element_text(hjust = 0.5, vjust=-8, size = 12)) +
    ggtitle("writing: Human vs. GPT-4") +
    labs(x = bquote(omega[k]), y = bquote(X(omega[k])))
ggsave("plot/pdf/verb/writing_verb_mask_zscore_fftnorm_4.pdf", plot=p, width=5, height=5)

