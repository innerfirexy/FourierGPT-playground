require("data.table")
require("ggplot2")
require("stringr")
require("mgcv")

# The function that returns predictions from GAM model
get_GAM_pred <- function(gam_model, n_interp = 500) {
    x <- data.table(freq = seq(0, 0.5, 0.5 / n_interp))
    y <- as.numeric(predict(gam_model, x))
    d <- data.table(x = x$freq, y = y)
    d
}


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



## Quickly plot the `gold standard`
genre <- "writing"
est_name <- "mistral"

df <- fread(str_interp("../data/gpt-4/${genre}_gpt-4.${est_name}.nlllogzs.fftnorm.circlefull.txt"))
setnames(df, "type", "Source")
# rm outliers
df.sub <- df[power <= mean(power) + 3 * sd(power) & power >= mean(power) - 3 * sd(power)]

gam.human <- gam(power ~ s(freq, bs = "cs"), data = df.sub[Source=="Human"])
gam.model <- gam(power ~ s(freq, bs = "cs"), data = df.sub[Source=="Sampled"])

gampred.human <- get_GAM_pred(gam.human)
gampred.human$Source <- "Human"
gampred.model <- get_GAM_pred(gam.model)
gampred.model$Source <- "Sampled"
gampred <- rbind(gampred.human, gampred.model) # These two plot as `gold standard`


# Select some circular spectra from df and plot together with gold standard
example <- "Sampled" # "Human" or "Sampled"
example_sid <- 30
df.tmp <- df[Source==example & sid == example_sid]

p <- ggplot(df.tmp, aes(freq, power)) +
    geom_smooth() +
    geom_line(data=gampred, aes(x=x, y=y, color=Source), linetype="dashed") +
    theme_bw() + theme(legend.position = "top") + 
    ggtitle(str_interp("${genre}, est. ${est_name}\n ${example} sid=${example_sid}, with gold standard")) +
    labs(x = bquote(omega[k]), y = bquote(X(omega[k])))
ggsave(str_interp("circlefull_smooth_${genre}_${est_name}_${example}_example_gold.pdf"), plot=p, width=5, height=5)
