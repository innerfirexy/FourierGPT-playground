d.gpt4.orig <- fread("data/davinci/bigram/fftnorm/pubmed_davinci.original.bigram.fftnorm.txt")
d.gpt4.samp <- fread("data/davinci/bigram/fftnorm/pubmed_davinci.sampled.bigram.fftnorm.txt")
d.gpt4.orig_verb_mask <- fread("data/davinci/bigram/verb_mask/fftnorm/pubmed_davinci.original.bigram_verb_mask.fftnorm.txt")
d.gpt4.samp_verb_mask <- fread("data/davinci/bigram/verb_mask/fftnorm/pubmed_davinci.sampled.bigram_verb_mask.fftnorm.txt")

d.gpt4.orig <- add_sid(d.gpt4.orig)
d.gpt4.samp <- add_sid(d.gpt4.samp)
d.gpt4.orig_verb_mask <- add_sid(d.gpt4.orig_verb_mask)
d.gpt4.samp_verb_mask <- add_sid(d.gpt4.samp_verb_mask)

d.gpt4.orig$type <- "Human"
d.gpt4.samp$type <- "Davinci"
d.gpt4.orig_verb_mask$type <- "Human Verb Masked"
d.gpt4.samp_verb_mask$type <- "Davinci Verb Masked"

d.gpt4 <- rbind(d.gpt4.orig, d.gpt4.samp, d.gpt4.orig_verb_mask, d.gpt4.samp_verb_mask)
# Smoothed plot

p <- ggplot(d.gpt4, aes(freq, power, color=type)) +
    geom_smooth()+
    labs(x = bquote(omega[k]), y = bquote(X(omega[k]))) +
    ggtitle("Pubmed: Human vs. Davinci (by Bigram)") + 
    theme_bw() + 
    theme(plot.title = element_text(hjust = 0.5, vjust=-8, size = 12))
    
ggsave("plot/pdf/bigram_verb_mask/davinci_pubmed_verb_mask.pdf", plot=p, width=6, height=5)



# GPT4:  Writing
d.gpt4.orig <- fread("data/davinci/bigram/fftnorm/writing_davinci.original.bigram.fftnorm.txt")
d.gpt4.samp <- fread("data/davinci/bigram/fftnorm/writing_davinci.sampled.bigram.fftnorm.txt")
d.gpt4.orig_verb_mask <- fread("data/davinci/bigram/verb_mask/fftnorm/writing_davinci.original.bigram_verb_mask.fftnorm.txt")
d.gpt4.samp_verb_mask <- fread("data/davinci/bigram/verb_mask/fftnorm/writing_davinci.sampled.bigram_verb_mask.fftnorm.txt")

d.gpt4.orig <- add_sid(d.gpt4.orig)
d.gpt4.samp <- add_sid(d.gpt4.samp)
d.gpt4.orig_verb_mask <- add_sid(d.gpt4.orig_verb_mask)
d.gpt4.samp_verb_mask <- add_sid(d.gpt4.samp_verb_mask)

d.gpt4.orig$type <- "Human"
d.gpt4.samp$type <- "Davinci"
d.gpt4.orig_verb_mask$type <- "Human Verb Masked"
d.gpt4.samp_verb_mask$type <- "Davinci Verb Masked"

d.gpt4 <- rbind(d.gpt4.orig, d.gpt4.samp, d.gpt4.orig_verb_mask, d.gpt4.samp_verb_mask)
# Smoothed plot

p <- ggplot(d.gpt4, aes(freq, power, color=type)) +
    geom_smooth()+
    labs(x = bquote(omega[k]), y = bquote(X(omega[k]))) +
    ggtitle("Writing: Human vs. Davinci (by Bigram)") + 
    theme_bw() + 
    theme(plot.title = element_text(hjust = 0.5, vjust=-8, size = 12))
    
ggsave("plot/pdf/bigram_verb_mask/davinci_writing_verb_mask.pdf", plot=p, width=6, height=5)


# GPT4:  Xsum
d.gpt4.orig <- fread("data/davinci/bigram/fftnorm/xsum_davinci.original.bigram.fftnorm.txt")
d.gpt4.samp <- fread("data/davinci/bigram/fftnorm/xsum_davinci.sampled.bigram.fftnorm.txt")
d.gpt4.orig_verb_mask <- fread("data/davinci/bigram/verb_mask/fftnorm/xsum_davinci.original.bigram_verb_mask.fftnorm.txt")
d.gpt4.samp_verb_mask <- fread("data/davinci/bigram/verb_mask/fftnorm/xsum_davinci.sampled.bigram_verb_mask.fftnorm.txt")

d.gpt4.orig <- add_sid(d.gpt4.orig)
d.gpt4.samp <- add_sid(d.gpt4.samp)
d.gpt4.orig_verb_mask <- add_sid(d.gpt4.orig_verb_mask)
d.gpt4.samp_verb_mask <- add_sid(d.gpt4.samp_verb_mask)

d.gpt4.orig$type <- "Human"
d.gpt4.samp$type <- "Davinci"
d.gpt4.orig_verb_mask$type <- "Human Verb Masked"
d.gpt4.samp_verb_mask$type <- "Davinci Verb Masked"

d.gpt4 <- rbind(d.gpt4.orig, d.gpt4.samp, d.gpt4.orig_verb_mask, d.gpt4.samp_verb_mask)
# Smoothed plot

p <- ggplot(d.gpt4, aes(freq, power, color=type)) +
    geom_smooth()+
    labs(x = bquote(omega[k]), y = bquote(X(omega[k]))) +
    ggtitle("Xsum: Human vs. Davinci (by Bigram)") + 
    theme_bw() + 
    theme(plot.title = element_text(hjust = 0.5, vjust=-8, size = 12))
    
ggsave("plot/pdf/bigram_verb_mask/davinci_xsum_verb_mask.pdf", plot=p, width=6, height=5)