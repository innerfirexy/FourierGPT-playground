require("data.table")
require("ggplot2")
require("stringr")

# Select data
genre <- "writing"
est_name <- "mistral"

df <- fread(str_interp("../data/gpt-4/${genre}_gpt-4.${est_name}.nlllogzs.fftnorm.circlefull.txt"))
setnames(df, "type", "Source")

# Removing outliers in spectrum before smooth plot
df.sub <- df[power <= mean(power) + 3 * sd(power) & power >= mean(power) - 3 * sd(power)]
p <- ggplot(df.sub, aes(freq, power, color=Source)) +
    geom_smooth() +
    theme_bw() + theme(plot.title = element_text(hjust = 0.5, vjust=-8, size = 12)) +
    ggtitle(str_interp("${genre}, est. ${est_name}, outliers removed (3*std)")) +
    labs(x = bquote(omega[k]), y = bquote(X(omega[k])))
ggsave(str_interp("circlefull_smooth_out3_${genre}_${est_name}.pdf"), plot=p, width=5, height=5)


df.sub <- df[power <= mean(power) + 2 * sd(power) & power >= mean(power) - 2 * sd(power)]
p <- ggplot(df.sub, aes(freq, power, color=Source)) +
    geom_smooth() +
    theme_bw() + theme(plot.title = element_text(hjust = 0.5, vjust=-8, size = 12)) +
    ggtitle(str_interp("${genre}, est. ${est_name}, outliers removed (2*std)")) +
    labs(x = bquote(omega[k]), y = bquote(X(omega[k])))
ggsave(str_interp("circlefull_smooth_out2_${genre}_${est_name}.pdf"), plot=p, width=5, height=5)

# No outlier removal
p <- ggplot(df, aes(freq, power, color=Source)) +
    geom_smooth() +
    theme_bw() + theme(plot.title = element_text(hjust = 0.5, vjust=-8, size = 12)) +
    ggtitle(str_interp("${genre}, est. ${est_name}")) +
    labs(x = bquote(omega[k]), y = bquote(X(omega[k])))
ggsave(str_interp("circlefull_smooth_${genre}_${est_name}.pdf"), plot=p, width=5, height=5)