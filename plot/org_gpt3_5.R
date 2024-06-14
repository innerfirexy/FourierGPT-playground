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


d.gpt4.orig_pubmed <- fread("data/gpt-3.5/bigram/fftnorm/pubmed_gpt-3.5-turbo.original.bigram.fftnorm.txt")
d.gpt4.samp_pubmed <- fread("data/gpt-3.5/bigram/fftnorm/pubmed_gpt-3.5-turbo.sampled.bigram.fftnorm.txt")

d.gpt4.orig_pubmed <- add_sid(d.gpt4.orig_pubmed)
d.gpt4.samp_pubmed <- add_sid(d.gpt4.samp_pubmed)

d.gpt4.orig_pubmed$type <- "Human"
d.gpt4.samp_pubmed$type <- "GPT-3.5"

d.gpt4 <- rbind(d.gpt4.orig_pubmed, d.gpt4.samp_pubmed)
# Smoothed plot

p <- ggplot(d.gpt4, aes(freq, power, color=type)) +
    geom_smooth() +
    labs(x = bquote(omega[k]), y = bquote(X(omega[k]))) +
    ggtitle("Pubmed: Human vs. GPT-3.5 (by Bigram)") +
    theme_bw() + 
    theme(plot.title = element_text(hjust = 0.5, vjust=-8, size = 12))
    
ggsave("plot/pdf/org/gpt-3.5-turbo_bigram_pubmed_fftnorm.pdf", plot=p, width=5, height=5)




d.gpt4.orig_xsum <- fread("data/gpt-3.5/bigram/fftnorm/xsum_gpt-3.5-turbo.original.bigram.fftnorm.txt")
d.gpt4.samp_xsum <- fread("data/gpt-3.5/bigram/fftnorm/xsum_gpt-3.5-turbo.sampled.bigram.fftnorm.txt")

d.gpt4.orig_xsum <- add_sid(d.gpt4.orig_xsum)
d.gpt4.samp_xsum <- add_sid(d.gpt4.samp_xsum)

d.gpt4.orig_xsum$type <- "Human"
d.gpt4.samp_xsum$type <- "GPT-3.5"

d.gpt4 <- rbind(d.gpt4.orig_xsum, d.gpt4.samp_xsum)
# Smoothed plot

p <- ggplot(d.gpt4, aes(freq, power, color=type)) +
    geom_smooth() +
    labs(x = bquote(omega[k]), y = bquote(X(omega[k]))) +
    ggtitle("Xsum: Human vs. GPT-3.5 (by Bigram)") +
    theme_bw() + 
    theme(plot.title = element_text(hjust = 0.5, vjust=-8, size = 12))
    
ggsave("plot/pdf/org/gpt-3.5-turbo_bigram_xsum_fftnorm.pdf", plot=p, width=5, height=5)





d.gpt4.orig_writing <- fread("data/gpt-3.5/bigram/fftnorm/writing_gpt-3.5-turbo.original.bigram.fftnorm.txt")
d.gpt4.samp_writing <- fread("data/gpt-3.5/bigram/fftnorm/writing_gpt-3.5-turbo.sampled.bigram.fftnorm.txt")

d.gpt4.orig_writing <- add_sid(d.gpt4.orig_writing)
d.gpt4.samp_writing <- add_sid(d.gpt4.samp_writing)

d.gpt4.orig_writing$type <- "Human"
d.gpt4.samp_writing$type <- "GPT-3.5"

d.gpt4 <- rbind(d.gpt4.orig_writing, d.gpt4.samp_writing)
# Smoothed plot

p <- ggplot(d.gpt4, aes(freq, power, color=type)) +
    geom_smooth() +
    labs(x = bquote(omega[k]), y = bquote(X(omega[k]))) +
    ggtitle("Writing: Human vs. GPT-3.5 (by Bigram)") +
    theme_bw() + 
    theme(plot.title = element_text(hjust = 0.5, vjust=-8, size = 12))
    
ggsave("plot/pdf/org/gpt-3.5-turbo_bigram_writing_fftnorm.pdf", plot=p, width=5, height=5)