require("data.table")
require("ggplot2")
require("stringr")


## Circular mean data
genre <- "writing"
est_name <- "gpt2xl"

# Read the data
d1 <- fread(str_interp("../data/gpt-4/${genre}_gpt-4.original.${est_name}.nlllogzs.fftnorm.circlemean.txt"))
d1$Source <- "Human"
d2 <- fread(str_interp("../data/gpt-4/${genre}_gpt-4.sampled.${est_name}.nlllogzs.fftnorm.circlemean.txt"))
d2$Source <- "Sampled"
d <- rbind(d1, d2)

# Bin plot
d$bin <- cut(d$freq, breaks=seq(0, 0.5, 0.05), include.lowest=TRUE)
p <- ggplot(d, aes(bin, power, fill=Source)) +
    geom_boxplot() +
    theme_bw() + theme(plot.title = element_text(hjust = 0.5, vjust=-8, size = 12)) +
    ggtitle(str_interp("${genre}, est. ${est_name}")) +
    labs(x = bquote(omega[k]), y = bquote(X(omega[k])))
ggsave(str_interp("circlemean_bin_${genre}_${est_name}.pdf"), plot=p, width=8, height=5)


## Circular full data
df <- fread("../data/gpt-4/pubmed_gpt-4.gpt2xl.nlllogzs.fftnorm.circlefull.txt")
setnames(df, "type", "Source")
df$bin <- cut(df$freq, breaks=seq(0, 0.5, 0.05), include.lowest=TRUE)
p <- ggplot(df, aes(bin, power, fill=Source)) +
    geom_boxplot() +
    theme_bw() + theme(plot.title = element_text(hjust = 0.5, vjust=-8, size = 12)) +
    ggtitle("Pubmed, est. gpt2xl") +
    labs(x = bquote(omega[k]), y = bquote(X(omega[k])))
ggsave("circlefull_bin_pubmed_gpt2xl.pdf", plot=p, width=8, height=5)



## T-test for each bin
table(d.gpt4$bin)
#   [0,0.05] (0.05,0.1] (0.1,0.15] (0.15,0.2] (0.2,0.25] (0.25,0.3] (0.3,0.35] 
#        826        680        681        677        696        640        658 
# (0.35,0.4] (0.4,0.45] (0.45,0.5] 
#        699        663        582

# Get summary of `power` for each bin
d.gpt4[, .(mean=mean(power), sd=sd(power)), by=.(bin, type)]
#            bin  type        mean       sd