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




d.gpt4.orig <- fread("data/gpt-4/bigram/fftnorm/pubmed_gpt-4.original.bigram.fftnorm.txt")
d.gpt4.samp <- fread("data/gpt-4/bigram/fftnorm/pubmed_gpt-4.sampled.bigram.fftnorm.txt")
d.gpt4.orig_verb_mask <- fread("data/gpt-4/bigram/verb_mask/fftnorm/pubmed_gpt-4.original.bigram_verb_mask.fftnorm.txt")
d.gpt4.samp_verb_mask <- fread("data/gpt-4/bigram/verb_mask/fftnorm/pubmed_gpt-4.sampled.bigram_verb_mask.fftnorm.txt")

d.gpt4.orig <- add_sid(d.gpt4.orig)
d.gpt4.samp <- add_sid(d.gpt4.samp)
d.gpt4.orig_verb_mask <- add_sid(d.gpt4.orig_verb_mask)
d.gpt4.samp_verb_mask <- add_sid(d.gpt4.samp_verb_mask)

d.gpt4.orig$type <- "Human"
d.gpt4.samp$type <- "GPT-4"
d.gpt4.orig_verb_mask$type <- "Human Verb Masked"
d.gpt4.samp_verb_mask$type <- "GPT-4 Verb Masked"

d.gpt4 <- rbind(d.gpt4.orig, d.gpt4.samp, d.gpt4.orig_verb_mask, d.gpt4.samp_verb_mask)
# Smoothed plot

p <- ggplot(d.gpt4, aes(freq, power, color=type)) +
    geom_smooth(data=d.gpt4.orig, aes(freq, power), linetype="dashed", se = FALSE) +
    geom_smooth(data=d.gpt4.samp, aes(freq, power), linetype="solid", se = FALSE) +
    geom_smooth(data=d.gpt4.orig_verb_mask, aes(freq, power), linetype="dashed", se = FALSE) +
    geom_smooth(data=d.gpt4.samp_verb_mask, aes(freq, power), linetype="solid", se = FALSE) +

    theme_bw() +
    labs(x = bquote(omega[k]), y = bquote(X(omega[k]))) +
    ggtitle("Pubmed: Human vs. GPT-4 (by Bigram)") + 
    theme(plot.title = element_text(hjust = 0.5, vjust=-8, size = 12),
          legend.text = element_text(size = 8),
          legend.key.width = unit(1, "cm"),  
          legend.position = "inside",
          legend.position.inside = c(0.75, 0.2)
          )
    
ggsave("plot/pdf/bigram_verb_mask/gpt4_pubmed_bigram_verb_mask.pdf", plot=p, width=5, height=5)



# GPT4:  Writing
d.gpt4.orig <- fread("data/gpt-4/bigram/fftnorm/writing_gpt-4.original.bigram.fftnorm.txt")
d.gpt4.samp <- fread("data/gpt-4/bigram/fftnorm/writing_gpt-4.sampled.bigram.fftnorm.txt")
d.gpt4.orig_verb_mask <- fread("data/gpt-4/bigram/verb_mask/fftnorm/writing_gpt-4.original.bigram_verb_mask.fftnorm.txt")
d.gpt4.samp_verb_mask <- fread("data/gpt-4/bigram/verb_mask/fftnorm/writing_gpt-4.sampled.bigram_verb_mask.fftnorm.txt")

d.gpt4.orig <- add_sid(d.gpt4.orig)
d.gpt4.samp <- add_sid(d.gpt4.samp)
d.gpt4.orig_verb_mask <- add_sid(d.gpt4.orig_verb_mask)
d.gpt4.samp_verb_mask <- add_sid(d.gpt4.samp_verb_mask)

d.gpt4.orig$type <- "Human"
d.gpt4.samp$type <- "GPT-4"
d.gpt4.orig_verb_mask$type <- "Human Verb Masked"
d.gpt4.samp_verb_mask$type <- "GPT-4 Verb Masked"

d.gpt4 <- rbind(d.gpt4.orig, d.gpt4.samp, d.gpt4.orig_verb_mask, d.gpt4.samp_verb_mask)
# Smoothed plot

p <- ggplot(d.gpt4, aes(freq, power, color=type)) +
    geom_smooth(data=d.gpt4.orig, aes(freq, power), linetype="dashed", se = FALSE) +
    geom_smooth(data=d.gpt4.samp, aes(freq, power), linetype="solid", se = FALSE) +
    geom_smooth(data=d.gpt4.orig_verb_mask, aes(freq, power), linetype="dashed", se = FALSE) +
    geom_smooth(data=d.gpt4.samp_verb_mask, aes(freq, power), linetype="solid", se = FALSE) +

    theme_bw() +
    labs(x = bquote(omega[k]), y = bquote(X(omega[k]))) +
    ggtitle("Writing: Human vs. GPT-4 (by Bigram)") + 
    theme(plot.title = element_text(hjust = 0.5, vjust=-8, size = 12),
          legend.text = element_text(size = 8),
          legend.key.width = unit(1, "cm"),  
          legend.position = "inside",
          legend.position.inside = c(0.75, 0.2)
          )
    
ggsave("plot/pdf/bigram_verb_mask/gpt4_writing_bigram_verb_mask.pdf", plot=p, width=5, height=5)


# GPT4:  Xsum
d.gpt4.orig <- fread("data/gpt-4/bigram/fftnorm/xsum_gpt-4.original.bigram.fftnorm.txt")
d.gpt4.samp <- fread("data/gpt-4/bigram/fftnorm/xsum_gpt-4.sampled.bigram.fftnorm.txt")
d.gpt4.orig_verb_mask <- fread("data/gpt-4/bigram/verb_mask/fftnorm/xsum_gpt-4.original.bigram_verb_mask.fftnorm.txt")
d.gpt4.samp_verb_mask <- fread("data/gpt-4/bigram/verb_mask/fftnorm/xsum_gpt-4.sampled.bigram_verb_mask.fftnorm.txt")

d.gpt4.orig <- add_sid(d.gpt4.orig)
d.gpt4.samp <- add_sid(d.gpt4.samp)
d.gpt4.orig_verb_mask <- add_sid(d.gpt4.orig_verb_mask)
d.gpt4.samp_verb_mask <- add_sid(d.gpt4.samp_verb_mask)

d.gpt4.orig$type <- "Human"
d.gpt4.samp$type <- "GPT-4"
d.gpt4.orig_verb_mask$type <- "Human Verb Masked"
d.gpt4.samp_verb_mask$type <- "GPT-4 Verb Masked"

d.gpt4 <- rbind(d.gpt4.orig, d.gpt4.samp, d.gpt4.orig_verb_mask, d.gpt4.samp_verb_mask)
# Smoothed plot

p <- ggplot(d.gpt4, aes(freq, power, color=type)) +
    geom_smooth(data=d.gpt4.orig, aes(freq, power), linetype="dashed", se = FALSE) +
    geom_smooth(data=d.gpt4.samp, aes(freq, power), linetype="solid", se = FALSE) +
    geom_smooth(data=d.gpt4.orig_verb_mask, aes(freq, power), linetype="dashed", se = FALSE) +
    geom_smooth(data=d.gpt4.samp_verb_mask, aes(freq, power), linetype="solid", se = FALSE) +

    theme_bw() +
    labs(x = bquote(omega[k]), y = bquote(X(omega[k]))) +
    ggtitle("Xsum: Human vs. GPT-4 (by Bigram)") + 
    theme(plot.title = element_text(hjust = 0.5, vjust=-8, size = 12),
          legend.text = element_text(size = 8),
          legend.key.width = unit(1, "cm"),  
          legend.position = "inside",
          legend.position.inside = c(0.75, 0.2)
          )
    
ggsave("plot/pdf/bigram_verb_mask/gpt4_xsum_bigram_verb_mask.pdf", plot=p, width=5, height=5)




d.gpt4.orig <- fread("data/gpt-3.5/bigram/fftnorm/pubmed_gpt-3.5-turbo.original.bigram.fftnorm.txt")
d.gpt4.samp <- fread("data/gpt-3.5/bigram/fftnorm/pubmed_gpt-3.5-turbo.sampled.bigram.fftnorm.txt")
d.gpt4.orig_verb_mask <- fread("data/gpt-3.5/bigram/verb_mask/fftnorm/pubmed_gpt-3.5-turbo.original.bigram_verb_mask.fftnorm.txt")
d.gpt4.samp_verb_mask <- fread("data/gpt-3.5/bigram/verb_mask/fftnorm/pubmed_gpt-3.5-turbo.sampled.bigram_verb_mask.fftnorm.txt")

d.gpt4.orig <- add_sid(d.gpt4.orig)
d.gpt4.samp <- add_sid(d.gpt4.samp)
d.gpt4.orig_verb_mask <- add_sid(d.gpt4.orig_verb_mask)
d.gpt4.samp_verb_mask <- add_sid(d.gpt4.samp_verb_mask)

d.gpt4.orig$type <- "Human"
d.gpt4.samp$type <- "GPT-3.5"
d.gpt4.orig_verb_mask$type <- "Human Verb Masked"
d.gpt4.samp_verb_mask$type <- "GPT-3.5 Verb Masked"

d.gpt4 <- rbind(d.gpt4.orig, d.gpt4.samp, d.gpt4.orig_verb_mask, d.gpt4.samp_verb_mask)
# Smoothed plot

p <- ggplot(d.gpt4, aes(freq, power, color=type)) +
    geom_smooth(data=d.gpt4.orig, aes(freq, power), linetype="dashed", se = FALSE) +
    geom_smooth(data=d.gpt4.samp, aes(freq, power), linetype="solid", se = FALSE) +
    geom_smooth(data=d.gpt4.orig_verb_mask, aes(freq, power), linetype="dashed", se = FALSE) +
    geom_smooth(data=d.gpt4.samp_verb_mask, aes(freq, power), linetype="solid", se = FALSE) +

    theme_bw() +
    labs(x = bquote(omega[k]), y = bquote(X(omega[k]))) +
    ggtitle("Pubmed: Human vs. GPT-3.5 (by Bigram)") + 
    theme(plot.title = element_text(hjust = 0.5, vjust=-8, size = 12),
          legend.text = element_text(size = 8),
          legend.key.width = unit(1, "cm"),  
          legend.position = "inside",
          legend.position.inside = c(0.75, 0.2)
          )
    
ggsave("plot/pdf/bigram_verb_mask/gpt-3.5-turbo_pubmed_bigram_verb_mask.pdf", plot=p, width=5, height=5)



# GPT4:  Writing
d.gpt4.orig <- fread("data/gpt-3.5/bigram/fftnorm/writing_gpt-3.5-turbo.original.bigram.fftnorm.txt")
d.gpt4.samp <- fread("data/gpt-3.5/bigram/fftnorm/writing_gpt-3.5-turbo.sampled.bigram.fftnorm.txt")
d.gpt4.orig_verb_mask <- fread("data/gpt-3.5/bigram/verb_mask/fftnorm/writing_gpt-3.5-turbo.original.bigram_verb_mask.fftnorm.txt")
d.gpt4.samp_verb_mask <- fread("data/gpt-3.5/bigram/verb_mask/fftnorm/writing_gpt-3.5-turbo.sampled.bigram_verb_mask.fftnorm.txt")

d.gpt4.orig <- add_sid(d.gpt4.orig)
d.gpt4.samp <- add_sid(d.gpt4.samp)
d.gpt4.orig_verb_mask <- add_sid(d.gpt4.orig_verb_mask)
d.gpt4.samp_verb_mask <- add_sid(d.gpt4.samp_verb_mask)

d.gpt4.orig$type <- "Human"
d.gpt4.samp$type <- "GPT-3.5"
d.gpt4.orig_verb_mask$type <- "Human Verb Masked"
d.gpt4.samp_verb_mask$type <- "GPT-3.5 Verb Masked"

d.gpt4 <- rbind(d.gpt4.orig, d.gpt4.samp, d.gpt4.orig_verb_mask, d.gpt4.samp_verb_mask)
# Smoothed plot

p <- ggplot(d.gpt4, aes(freq, power, color=type)) +
    geom_smooth(data=d.gpt4.orig, aes(freq, power), linetype="dashed", se = FALSE) +
    geom_smooth(data=d.gpt4.samp, aes(freq, power), linetype="solid", se = FALSE) +
    geom_smooth(data=d.gpt4.orig_verb_mask, aes(freq, power), linetype="dashed", se = FALSE) +
    geom_smooth(data=d.gpt4.samp_verb_mask, aes(freq, power), linetype="solid", se = FALSE) +

    theme_bw() +
    labs(x = bquote(omega[k]), y = bquote(X(omega[k]))) +
    ggtitle("Writing: Human vs. GPT-3.5 (by Bigram)") + 
    theme(plot.title = element_text(hjust = 0.5, vjust=-8, size = 12),
          legend.text = element_text(size = 8),
          legend.key.width = unit(1, "cm"),  
          legend.position = "inside",
          legend.position.inside = c(0.75, 0.2)
          )
    
ggsave("plot/pdf/bigram_verb_mask/gpt-3.5-turbo_writing_bigram_verb_mask.pdf", plot=p, width=5, height=5)


# GPT4:  Xsum
d.gpt4.orig <- fread("data/gpt-3.5/bigram/fftnorm/xsum_gpt-3.5-turbo.original.bigram.fftnorm.txt")
d.gpt4.samp <- fread("data/gpt-3.5/bigram/fftnorm/xsum_gpt-3.5-turbo.sampled.bigram.fftnorm.txt")
d.gpt4.orig_verb_mask <- fread("data/gpt-3.5/bigram/verb_mask/fftnorm/xsum_gpt-3.5-turbo.original.bigram_verb_mask.fftnorm.txt")
d.gpt4.samp_verb_mask <- fread("data/gpt-3.5/bigram/verb_mask/fftnorm/xsum_gpt-3.5-turbo.sampled.bigram_verb_mask.fftnorm.txt")

d.gpt4.orig <- add_sid(d.gpt4.orig)
d.gpt4.samp <- add_sid(d.gpt4.samp)
d.gpt4.orig_verb_mask <- add_sid(d.gpt4.orig_verb_mask)
d.gpt4.samp_verb_mask <- add_sid(d.gpt4.samp_verb_mask)

d.gpt4.orig$type <- "Human"
d.gpt4.samp$type <- "GPT-3.5"
d.gpt4.orig_verb_mask$type <- "Human Verb Masked"
d.gpt4.samp_verb_mask$type <- "GPT-3.5 Verb Masked"

d.gpt4 <- rbind(d.gpt4.orig, d.gpt4.samp, d.gpt4.orig_verb_mask, d.gpt4.samp_verb_mask)
# Smoothed plot

p <- ggplot(d.gpt4, aes(freq, power, color=type)) +
    geom_smooth(data=d.gpt4.orig, aes(freq, power), linetype="dashed", se = FALSE) +
    geom_smooth(data=d.gpt4.samp, aes(freq, power), linetype="solid", se = FALSE) +
    geom_smooth(data=d.gpt4.orig_verb_mask, aes(freq, power), linetype="dashed", se = FALSE) +
    geom_smooth(data=d.gpt4.samp_verb_mask, aes(freq, power), linetype="solid", se = FALSE) +

    theme_bw() +
    labs(x = bquote(omega[k]), y = bquote(X(omega[k]))) +
    ggtitle("Xsum: Human vs. GPT-3.5 (by Bigram)") + 
    theme(plot.title = element_text(hjust = 0.5, vjust=-8, size = 12),
          legend.text = element_text(size = 8),
          legend.key.width = unit(1, "cm"),  
          legend.position = "inside",
          legend.position.inside = c(0.75, 0.2)
          )
    
ggsave("plot/pdf/bigram_verb_mask/gpt-3.5-turbo_xsum_bigram_verb_mask.pdf", plot=p, width=5, height=5)







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
    geom_smooth(data=d.gpt4.orig, aes(freq, power), linetype="dashed", se = FALSE) +
    geom_smooth(data=d.gpt4.samp, aes(freq, power), linetype="solid", se = FALSE) +
    geom_smooth(data=d.gpt4.orig_verb_mask, aes(freq, power), linetype="dashed", se = FALSE) +
    geom_smooth(data=d.gpt4.samp_verb_mask, aes(freq, power), linetype="solid", se = FALSE) +

    theme_bw() +
    labs(x = bquote(omega[k]), y = bquote(X(omega[k]))) +
    ggtitle("Pubmed: Human vs. Davinci (by Bigram)") + 
    theme(plot.title = element_text(hjust = 0.5, vjust=-8, size = 12),
          legend.text = element_text(size = 8),
          legend.key.width = unit(1, "cm"),  
          legend.position = "inside",
          legend.position.inside = c(0.75, 0.2)
          )
    
ggsave("plot/pdf/bigram_verb_mask/davinci_pubmed_bigram_verb_mask.pdf", plot=p, width=5, height=5)



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
    geom_smooth(data=d.gpt4.orig, aes(freq, power), linetype="dashed", se = FALSE) +
    geom_smooth(data=d.gpt4.samp, aes(freq, power), linetype="solid", se = FALSE) +
    geom_smooth(data=d.gpt4.orig_verb_mask, aes(freq, power), linetype="dashed", se = FALSE) +
    geom_smooth(data=d.gpt4.samp_verb_mask, aes(freq, power), linetype="solid", se = FALSE) +

    theme_bw() +
    labs(x = bquote(omega[k]), y = bquote(X(omega[k]))) +
    ggtitle("Writing: Human vs. Davinci (by Bigram)") + 
    theme(plot.title = element_text(hjust = 0.5, vjust=-8, size = 12),
          legend.text = element_text(size = 8),
          legend.key.width = unit(1, "cm"),  
          legend.position = "inside",
          legend.position.inside = c(0.75, 0.2)
          )
    
ggsave("plot/pdf/bigram_verb_mask/davinci_writing_bigram_verb_mask.pdf", plot=p, width=5, height=5)


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
    geom_smooth(data=d.gpt4.orig, aes(freq, power), linetype="dashed", se = FALSE) +
    geom_smooth(data=d.gpt4.samp, aes(freq, power), linetype="solid", se = FALSE) +
    geom_smooth(data=d.gpt4.orig_verb_mask, aes(freq, power), linetype="dashed", se = FALSE) +
    geom_smooth(data=d.gpt4.samp_verb_mask, aes(freq, power), linetype="solid", se = FALSE) +

    theme_bw() +
    labs(x = bquote(omega[k]), y = bquote(X(omega[k]))) +
    ggtitle("Xsum: Human vs. Davinci (by Bigram)") + 
    theme(plot.title = element_text(hjust = 0.5, vjust=-8, size = 12),
          legend.text = element_text(size = 8),
          legend.key.width = unit(1, "cm"),  
          legend.position = "inside",
          legend.position.inside = c(0.75, 0.2)
          )
    
ggsave("plot/pdf/bigram_verb_mask/davinci_xsum_bigram_verb_mask.pdf", plot=p, width=5, height=5)