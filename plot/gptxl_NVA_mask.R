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




d.gpt4.orig <- fread("data/gpt-4/gpt2_un_mask/fftnorm/pubmed_gpt-4.original.gpt2.fftnorm.txt")
d.gpt4.samp <- fread("data/gpt-4/gpt2_un_mask/fftnorm/pubmed_gpt-4.sampled.gpt2.fftnorm.txt")
d.gpt4.orig_NVA_mask <- fread("data/gpt-4/gpt2_NVA_mask/fftnorm/pubmed_gpt-4.original.gpt2_mask_NVA.fftnorm.txt")
d.gpt4.samp_NVA_mask <- fread("data/gpt-4/gpt2_NVA_mask/fftnorm/pubmed_gpt-4.sampled.gpt2_mask_NVA.fftnorm.txt")

d.gpt4.orig <- add_sid(d.gpt4.orig)
d.gpt4.samp <- add_sid(d.gpt4.samp)
d.gpt4.orig_NVA_mask <- add_sid(d.gpt4.orig_NVA_mask)
d.gpt4.samp_NVA_mask <- add_sid(d.gpt4.samp_NVA_mask)

d.gpt4.orig$type <- "Human"
d.gpt4.samp$type <- "GPT-4"
d.gpt4.orig_NVA_mask$type <- "Human NVA Masked"
d.gpt4.samp_NVA_mask$type <- "GPT-4 NVA Masked"

d.gpt4 <- rbind(d.gpt4.orig, d.gpt4.samp, d.gpt4.orig_NVA_mask, d.gpt4.samp_NVA_mask)
# Smoothed plot

p <- ggplot(d.gpt4, aes(freq, power, color=type)) +

    geom_smooth(data=d.gpt4.orig, aes(freq, power), linetype="dashed", se = FALSE) +
    geom_smooth(data=d.gpt4.samp, aes(freq, power), linetype="solid", se = FALSE) +
    geom_smooth(data=d.gpt4.orig_NVA_mask, aes(freq, power), linetype="dashed", se = FALSE) +
    geom_smooth(data=d.gpt4.samp_NVA_mask, aes(freq, power), linetype="solid", se = FALSE) +

    theme_bw() +
    labs(x = bquote(omega[k]), y = bquote(X(omega[k]))) +
    ggtitle("Pubmed: Human vs. GPT-4 (by GPT-2)") + 
    theme(plot.title = element_text(hjust = 0.5, vjust=-8, size = 12),
          legend.text = element_text(size = 8),
          legend.key.width = unit(1, "cm"),  
          legend.position = "inside",
          legend.position.inside = c(0.75, 0.2)
          )
    
ggsave("plot/pdf/gpt2_NVA_mask/gpt4_pubmed_gpt2_NVA_mask.pdf", plot=p, width=5, height=5)



# GPT4:  Writing
d.gpt4.orig <- fread("data/gpt-4/gpt2_un_mask/fftnorm/writing_gpt-4.original.gpt2.fftnorm.txt")
d.gpt4.samp <- fread("data/gpt-4/gpt2_un_mask/fftnorm/writing_gpt-4.sampled.gpt2.fftnorm.txt")
d.gpt4.orig_NVA_mask <- fread("data/gpt-4/gpt2_NVA_mask/fftnorm/writing_gpt-4.original.gpt2_mask_NVA.fftnorm.txt")
d.gpt4.samp_NVA_mask <- fread("data/gpt-4/gpt2_NVA_mask/fftnorm/writing_gpt-4.sampled.gpt2_mask_NVA.fftnorm.txt")

d.gpt4.orig <- add_sid(d.gpt4.orig)
d.gpt4.samp <- add_sid(d.gpt4.samp)
d.gpt4.orig_NVA_mask <- add_sid(d.gpt4.orig_NVA_mask)
d.gpt4.samp_NVA_mask <- add_sid(d.gpt4.samp_NVA_mask)

d.gpt4.orig$type <- "Human"
d.gpt4.samp$type <- "GPT-4"
d.gpt4.orig_NVA_mask$type <- "Human NVA Masked"
d.gpt4.samp_NVA_mask$type <- "GPT-4 NVA Masked"

d.gpt4 <- rbind(d.gpt4.orig, d.gpt4.samp, d.gpt4.orig_NVA_mask, d.gpt4.samp_NVA_mask)
# Smoothed plot

p <- ggplot(d.gpt4, aes(freq, power, color=type)) +
    geom_smooth(data=d.gpt4.orig, aes(freq, power), linetype="dashed", se = FALSE) +
    geom_smooth(data=d.gpt4.samp, aes(freq, power), linetype="solid", se = FALSE) +
    geom_smooth(data=d.gpt4.orig_NVA_mask, aes(freq, power), linetype="dashed", se = FALSE) +
    geom_smooth(data=d.gpt4.samp_NVA_mask, aes(freq, power), linetype="solid", se = FALSE) +

    theme_bw() +
    labs(x = bquote(omega[k]), y = bquote(X(omega[k]))) +
    ggtitle("Writing: Human vs. GPT-4 (by GPT-2)") + 
    theme(plot.title = element_text(hjust = 0.5, vjust=-8, size = 12),
          legend.text = element_text(size = 8),
          legend.key.width = unit(1, "cm"),  
          legend.position = "inside",
          legend.position.inside = c(0.75, 0.2)
          )
    
ggsave("plot/pdf/gpt2_NVA_mask/gpt4_writing_gpt2_NVA_mask.pdf", plot=p, width=5, height=5)


# GPT4:  Xsum
d.gpt4.orig <- fread("data/gpt-4/gpt2_un_mask/fftnorm/xsum_gpt-4.original.gpt2.fftnorm.txt")
d.gpt4.samp <- fread("data/gpt-4/gpt2_un_mask/fftnorm/xsum_gpt-4.sampled.gpt2.fftnorm.txt")
d.gpt4.orig_NVA_mask <- fread("data/gpt-4/gpt2_NVA_mask/fftnorm/xsum_gpt-4.original.gpt2_mask_NVA.fftnorm.txt")
d.gpt4.samp_NVA_mask <- fread("data/gpt-4/gpt2_NVA_mask/fftnorm/xsum_gpt-4.sampled.gpt2_mask_NVA.fftnorm.txt")

d.gpt4.orig <- add_sid(d.gpt4.orig)
d.gpt4.samp <- add_sid(d.gpt4.samp)
d.gpt4.orig_NVA_mask <- add_sid(d.gpt4.orig_NVA_mask)
d.gpt4.samp_NVA_mask <- add_sid(d.gpt4.samp_NVA_mask)

d.gpt4.orig$type <- "Human"
d.gpt4.samp$type <- "GPT-4"
d.gpt4.orig_NVA_mask$type <- "Human NVA Masked"
d.gpt4.samp_NVA_mask$type <- "GPT-4 NVA Masked"

d.gpt4 <- rbind(d.gpt4.orig, d.gpt4.samp, d.gpt4.orig_NVA_mask, d.gpt4.samp_NVA_mask)
# Smoothed plot

p <- ggplot(d.gpt4, aes(freq, power, color=type)) +
    geom_smooth(data=d.gpt4.orig, aes(freq, power), linetype="dashed", se = FALSE) +
    geom_smooth(data=d.gpt4.samp, aes(freq, power), linetype="solid", se = FALSE) +
    geom_smooth(data=d.gpt4.orig_NVA_mask, aes(freq, power), linetype="dashed", se = FALSE) +
    geom_smooth(data=d.gpt4.samp_NVA_mask, aes(freq, power), linetype="solid", se = FALSE) +

    theme_bw() +
    labs(x = bquote(omega[k]), y = bquote(X(omega[k]))) +
    ggtitle("Xsum: Human vs. GPT-4 (by GPT-2)") + 
    theme(plot.title = element_text(hjust = 0.5, vjust=-8, size = 12),
          legend.text = element_text(size = 8),
          legend.key.width = unit(1, "cm"),  
          legend.position = "inside",
          legend.position.inside = c(0.75, 0.2)
          )
    
ggsave("plot/pdf/gpt2_NVA_mask/gpt4_xsum_gpt2_NVA_mask.pdf", plot=p, width=5, height=5)




d.gpt4.orig <- fread("data/gpt-3.5/gpt2_un_mask/fftnorm/pubmed_gpt-3.5-turbo.original.gpt2.fftnorm.txt")
d.gpt4.samp <- fread("data/gpt-3.5/gpt2_un_mask/fftnorm/pubmed_gpt-3.5-turbo.sampled.gpt2.fftnorm.txt")
d.gpt4.orig_NVA_mask <- fread("data/gpt-3.5/gpt2_NVA_mask/fftnorm/pubmed_gpt-3.5-turbo.original.gpt2_mask_NVA.fftnorm.txt")
d.gpt4.samp_NVA_mask <- fread("data/gpt-3.5/gpt2_NVA_mask/fftnorm/pubmed_gpt-3.5-turbo.sampled.gpt2_mask_NVA.fftnorm.txt")

d.gpt4.orig <- add_sid(d.gpt4.orig)
d.gpt4.samp <- add_sid(d.gpt4.samp)
d.gpt4.orig_NVA_mask <- add_sid(d.gpt4.orig_NVA_mask)
d.gpt4.samp_NVA_mask <- add_sid(d.gpt4.samp_NVA_mask)

d.gpt4.orig$type <- "Human"
d.gpt4.samp$type <- "GPT-3.5"
d.gpt4.orig_NVA_mask$type <- "Human NVA Masked"
d.gpt4.samp_NVA_mask$type <- "GPT-3.5 NVA Masked"

d.gpt4 <- rbind(d.gpt4.orig, d.gpt4.samp, d.gpt4.orig_NVA_mask, d.gpt4.samp_NVA_mask)
# Smoothed plot

p <- ggplot(d.gpt4, aes(freq, power, color=type)) +
    geom_smooth(data=d.gpt4.orig, aes(freq, power), linetype="dashed", se = FALSE) +
    geom_smooth(data=d.gpt4.samp, aes(freq, power), linetype="solid", se = FALSE) +
    geom_smooth(data=d.gpt4.orig_NVA_mask, aes(freq, power), linetype="dashed", se = FALSE) +
    geom_smooth(data=d.gpt4.samp_NVA_mask, aes(freq, power), linetype="solid", se = FALSE) +

    theme_bw() +
    labs(x = bquote(omega[k]), y = bquote(X(omega[k]))) +
    ggtitle("Pubmed: Human vs. GPT-3.5 (by GPT-2)") + 
    theme(plot.title = element_text(hjust = 0.5, vjust=-8, size = 12),
          legend.text = element_text(size = 8),
          legend.key.width = unit(1, "cm"),  
          legend.position = "inside",
          legend.position.inside = c(0.75, 0.2)
          )
    
ggsave("plot/pdf/gpt2_NVA_mask/gpt-3.5-turbo_pubmed_gpt2_NVA_mask.pdf", plot=p, width=5, height=5)



# GPT4:  Writing
d.gpt4.orig <- fread("data/gpt-3.5/gpt2_un_mask/fftnorm/writing_gpt-3.5-turbo.original.gpt2.fftnorm.txt")
d.gpt4.samp <- fread("data/gpt-3.5/gpt2_un_mask/fftnorm/writing_gpt-3.5-turbo.sampled.gpt2.fftnorm.txt")
d.gpt4.orig_NVA_mask <- fread("data/gpt-3.5/gpt2_NVA_mask/fftnorm/writing_gpt-3.5-turbo.original.gpt2_mask_NVA.fftnorm.txt")
d.gpt4.samp_NVA_mask <- fread("data/gpt-3.5/gpt2_NVA_mask/fftnorm/writing_gpt-3.5-turbo.sampled.gpt2_mask_NVA.fftnorm.txt")

d.gpt4.orig <- add_sid(d.gpt4.orig)
d.gpt4.samp <- add_sid(d.gpt4.samp)
d.gpt4.orig_NVA_mask <- add_sid(d.gpt4.orig_NVA_mask)
d.gpt4.samp_NVA_mask <- add_sid(d.gpt4.samp_NVA_mask)

d.gpt4.orig$type <- "Human"
d.gpt4.samp$type <- "GPT-3.5"
d.gpt4.orig_NVA_mask$type <- "Human NVA Masked"
d.gpt4.samp_NVA_mask$type <- "GPT-3.5 NVA Masked"

d.gpt4 <- rbind(d.gpt4.orig, d.gpt4.samp, d.gpt4.orig_NVA_mask, d.gpt4.samp_NVA_mask)
# Smoothed plot

p <- ggplot(d.gpt4, aes(freq, power, color=type)) +
    geom_smooth(data=d.gpt4.orig, aes(freq, power), linetype="dashed", se = FALSE) +
    geom_smooth(data=d.gpt4.samp, aes(freq, power), linetype="solid", se = FALSE) +
    geom_smooth(data=d.gpt4.orig_NVA_mask, aes(freq, power), linetype="dashed", se = FALSE) +
    geom_smooth(data=d.gpt4.samp_NVA_mask, aes(freq, power), linetype="solid", se = FALSE) +

    theme_bw() +
    labs(x = bquote(omega[k]), y = bquote(X(omega[k]))) +
    ggtitle("Writing: Human vs. GPT-3.5 (by GPT-2)") + 
    theme(plot.title = element_text(hjust = 0.5, vjust=-8, size = 12),
          legend.text = element_text(size = 8),
          legend.key.width = unit(1, "cm"),  
          legend.position = "inside",
          legend.position.inside = c(0.75, 0.2)
          )
    
ggsave("plot/pdf/gpt2_NVA_mask/gpt-3.5-turbo_writing_gpt2_NVA_mask.pdf", plot=p, width=5, height=5)


# GPT4:  Xsum
d.gpt4.orig <- fread("data/gpt-3.5/gpt2_un_mask/fftnorm/xsum_gpt-3.5-turbo.original.gpt2.fftnorm.txt")
d.gpt4.samp <- fread("data/gpt-3.5/gpt2_un_mask/fftnorm/xsum_gpt-3.5-turbo.sampled.gpt2.fftnorm.txt")
d.gpt4.orig_NVA_mask <- fread("data/gpt-3.5/gpt2_NVA_mask/fftnorm/xsum_gpt-3.5-turbo.original.gpt2_mask_NVA.fftnorm.txt")
d.gpt4.samp_NVA_mask <- fread("data/gpt-3.5/gpt2_NVA_mask/fftnorm/xsum_gpt-3.5-turbo.sampled.gpt2_mask_NVA.fftnorm.txt")

d.gpt4.orig <- add_sid(d.gpt4.orig)
d.gpt4.samp <- add_sid(d.gpt4.samp)
d.gpt4.orig_NVA_mask <- add_sid(d.gpt4.orig_NVA_mask)
d.gpt4.samp_NVA_mask <- add_sid(d.gpt4.samp_NVA_mask)

d.gpt4.orig$type <- "Human"
d.gpt4.samp$type <- "GPT-3.5"
d.gpt4.orig_NVA_mask$type <- "Human NVA Masked"
d.gpt4.samp_NVA_mask$type <- "GPT-3.5 NVA Masked"

d.gpt4 <- rbind(d.gpt4.orig, d.gpt4.samp, d.gpt4.orig_NVA_mask, d.gpt4.samp_NVA_mask)
# Smoothed plot

p <- ggplot(d.gpt4, aes(freq, power, color=type)) +
    geom_smooth(data=d.gpt4.orig, aes(freq, power), linetype="dashed", se = FALSE) +
    geom_smooth(data=d.gpt4.samp, aes(freq, power), linetype="solid", se = FALSE) +
    geom_smooth(data=d.gpt4.orig_NVA_mask, aes(freq, power), linetype="dashed", se = FALSE) +
    geom_smooth(data=d.gpt4.samp_NVA_mask, aes(freq, power), linetype="solid", se = FALSE) +

    theme_bw() +
    labs(x = bquote(omega[k]), y = bquote(X(omega[k]))) +
    ggtitle("Xsum: Human vs. GPT-3.5 (by GPT-2)") + 
    theme(plot.title = element_text(hjust = 0.5, vjust=-8, size = 12),
          legend.text = element_text(size = 8),
          legend.key.width = unit(1, "cm"),  
          legend.position = "inside",
          legend.position.inside = c(0.75, 0.2)
          )
    
ggsave("plot/pdf/gpt2_NVA_mask/gpt-3.5-turbo_xsum_gpt2_NVA_mask.pdf", plot=p, width=5, height=5)







d.gpt4.orig <- fread("data/davinci/gpt2_un_mask/fftnorm/pubmed_davinci.original.gpt2.fftnorm.txt")
d.gpt4.samp <- fread("data/davinci/gpt2_un_mask/fftnorm/pubmed_davinci.sampled.gpt2.fftnorm.txt")
d.gpt4.orig_NVA_mask <- fread("data/davinci/gpt2_NVA_mask/fftnorm/pubmed_davinci.original.gpt2_mask_NVA.fftnorm.txt")
d.gpt4.samp_NVA_mask <- fread("data/davinci/gpt2_NVA_mask/fftnorm/pubmed_davinci.sampled.gpt2_mask_NVA.fftnorm.txt")

d.gpt4.orig <- add_sid(d.gpt4.orig)
d.gpt4.samp <- add_sid(d.gpt4.samp)
d.gpt4.orig_NVA_mask <- add_sid(d.gpt4.orig_NVA_mask)
d.gpt4.samp_NVA_mask <- add_sid(d.gpt4.samp_NVA_mask)

d.gpt4.orig$type <- "Human"
d.gpt4.samp$type <- "Davinci"
d.gpt4.orig_NVA_mask$type <- "Human NVA Masked"
d.gpt4.samp_NVA_mask$type <- "Davinci NVA Masked"

d.gpt4 <- rbind(d.gpt4.orig, d.gpt4.samp, d.gpt4.orig_NVA_mask, d.gpt4.samp_NVA_mask)
# Smoothed plot

p <- ggplot(d.gpt4, aes(freq, power, color=type)) +
    geom_smooth(data=d.gpt4.orig, aes(freq, power), linetype="dashed", se = FALSE) +
    geom_smooth(data=d.gpt4.samp, aes(freq, power), linetype="solid", se = FALSE) +
    geom_smooth(data=d.gpt4.orig_NVA_mask, aes(freq, power), linetype="dashed", se = FALSE) +
    geom_smooth(data=d.gpt4.samp_NVA_mask, aes(freq, power), linetype="solid", se = FALSE) +

    theme_bw() +
    labs(x = bquote(omega[k]), y = bquote(X(omega[k]))) +
    ggtitle("Pubmed: Human vs. Davinci (by GPT-2)") + 
    theme(plot.title = element_text(hjust = 0.5, vjust=-8, size = 12),
          legend.text = element_text(size = 8),
          legend.key.width = unit(1, "cm"),  
          legend.position = "inside",
          legend.position.inside = c(0.75, 0.2)
          )
    
ggsave("plot/pdf/gpt2_NVA_mask/davinci_pubmed_gpt2_NVA_mask.pdf", plot=p, width=5, height=5)



# GPT4:  Writing
d.gpt4.orig <- fread("data/davinci/gpt2_un_mask/fftnorm/writing_davinci.original.gpt2.fftnorm.txt")
d.gpt4.samp <- fread("data/davinci/gpt2_un_mask/fftnorm/writing_davinci.sampled.gpt2.fftnorm.txt")
d.gpt4.orig_NVA_mask <- fread("data/davinci/gpt2_NVA_mask/fftnorm/writing_davinci.original.gpt2_mask_NVA.fftnorm.txt")
d.gpt4.samp_NVA_mask <- fread("data/davinci/gpt2_NVA_mask/fftnorm/writing_davinci.sampled.gpt2_mask_NVA.fftnorm.txt")

d.gpt4.orig <- add_sid(d.gpt4.orig)
d.gpt4.samp <- add_sid(d.gpt4.samp)
d.gpt4.orig_NVA_mask <- add_sid(d.gpt4.orig_NVA_mask)
d.gpt4.samp_NVA_mask <- add_sid(d.gpt4.samp_NVA_mask)

d.gpt4.orig$type <- "Human"
d.gpt4.samp$type <- "Davinci"
d.gpt4.orig_NVA_mask$type <- "Human NVA Masked"
d.gpt4.samp_NVA_mask$type <- "Davinci NVA Masked"

d.gpt4 <- rbind(d.gpt4.orig, d.gpt4.samp, d.gpt4.orig_NVA_mask, d.gpt4.samp_NVA_mask)
# Smoothed plot

p <- ggplot(d.gpt4, aes(freq, power, color=type)) +
    geom_smooth(data=d.gpt4.orig, aes(freq, power), linetype="dashed", se = FALSE) +
    geom_smooth(data=d.gpt4.samp, aes(freq, power), linetype="solid", se = FALSE) +
    geom_smooth(data=d.gpt4.orig_NVA_mask, aes(freq, power), linetype="dashed", se = FALSE) +
    geom_smooth(data=d.gpt4.samp_NVA_mask, aes(freq, power), linetype="solid", se = FALSE) +

    theme_bw() +
    labs(x = bquote(omega[k]), y = bquote(X(omega[k]))) +
    ggtitle("Writing: Human vs. Davinci (by GPT-2)") + 
    theme(plot.title = element_text(hjust = 0.5, vjust=-8, size = 12),
          legend.text = element_text(size = 8),
          legend.key.width = unit(1, "cm"),  
          legend.position = "inside",
          legend.position.inside = c(0.75, 0.2)
          )
    
ggsave("plot/pdf/gpt2_NVA_mask/davinci_writing_gpt2_NVA_mask.pdf", plot=p, width=5, height=5)


# GPT4:  Xsum
d.gpt4.orig <- fread("data/davinci/gpt2_un_mask/fftnorm/xsum_davinci.original.gpt2.fftnorm.txt")
d.gpt4.samp <- fread("data/davinci/gpt2_un_mask/fftnorm/xsum_davinci.sampled.gpt2.fftnorm.txt")
d.gpt4.orig_NVA_mask <- fread("data/davinci/gpt2_NVA_mask/fftnorm/xsum_davinci.original.gpt2_mask_NVA.fftnorm.txt")
d.gpt4.samp_NVA_mask <- fread("data/davinci/gpt2_NVA_mask/fftnorm/xsum_davinci.sampled.gpt2_mask_NVA.fftnorm.txt")

d.gpt4.orig <- add_sid(d.gpt4.orig)
d.gpt4.samp <- add_sid(d.gpt4.samp)
d.gpt4.orig_NVA_mask <- add_sid(d.gpt4.orig_NVA_mask)
d.gpt4.samp_NVA_mask <- add_sid(d.gpt4.samp_NVA_mask)

d.gpt4.orig$type <- "Human"
d.gpt4.samp$type <- "Davinci"
d.gpt4.orig_NVA_mask$type <- "Human NVA Masked"
d.gpt4.samp_NVA_mask$type <- "Davinci NVA Masked"

d.gpt4 <- rbind(d.gpt4.orig, d.gpt4.samp, d.gpt4.orig_NVA_mask, d.gpt4.samp_NVA_mask)
# Smoothed plot

p <- ggplot(d.gpt4, aes(freq, power, color=type)) +
    geom_smooth(data=d.gpt4.orig, aes(freq, power), linetype="dashed", se = FALSE) +
    geom_smooth(data=d.gpt4.samp, aes(freq, power), linetype="solid", se = FALSE) +
    geom_smooth(data=d.gpt4.orig_NVA_mask, aes(freq, power), linetype="dashed", se = FALSE) +
    geom_smooth(data=d.gpt4.samp_NVA_mask, aes(freq, power), linetype="solid", se = FALSE) +

    theme_bw() +
    labs(x = bquote(omega[k]), y = bquote(X(omega[k]))) +
    ggtitle("Xsum: Human vs. Davinci (by GPT-2)") + 
    theme(plot.title = element_text(hjust = 0.5, vjust=-8, size = 12),
          legend.text = element_text(size = 8),
          legend.key.width = unit(1, "cm"),  
          legend.position = "inside",
          legend.position.inside = c(0.75, 0.2)
          )
    
ggsave("plot/pdf/gpt2_NVA_mask/davinci_xsum_gpt2_NVA_mask.pdf", plot=p, width=5, height=5)