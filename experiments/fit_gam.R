require("mgcv")
require("data.table")
require("ggplot2")


# The function that returns predictions from GAM model
get_GAM_pred <- function(gam_model, n_interp = 500) {
    x <- data.table(freq = seq(0, 0.5, 0.5 / n_interp))
    y <- as.numeric(predict(gam_model, x))
    d <- data.table(x = x$freq, y = y)
    d
}

# Read circular full data 
pubmed.mistral.circlefull <- fread("../data/gpt-4/pubmed_gpt-4.mistral.nlllogzs.fftnorm.circlefull.txt")

# Fit GAM models and make predictions
gam.pubmed.orig <- gam(power ~ s(freq, bs = "cs"), data = pubmed.mistral.circlefull[type=="Human"])
pred.pubmed.orig <- get_GAM_pred(gam.pubmed.orig)
pred.pubmed.orig$type <- "Human"

gam.pubmed.samp <- gam(power ~ s(freq, bs = "cs"), data = pubmed.mistral.circlefull[type=="Sampled"])
pred.pubmed.samp <- get_GAM_pred(gam.pubmed.samp)
pred.pubmed.samp$type <- "Sampled"

pred.pubmed <- rbind(pred.pubmed.orig, pred.pubmed.samp)
# Plot the predictions
p <- ggplot(pred.pubmed, aes(x = x, y = y, color = type)) +
    geom_line() +
    labs(title = "GAM-Predicted Spectra", x = "Frequency", y = "Power") +
    theme_bw() + theme(legend.position = "top")
ggsave("gampred_pubmed_gpt-4_mistral_circlefull", plot=p, width=5, height=5)


## Read non-circular data
pubmed.mistral.orig <- fread("../data/gpt-4/pubmed_gpt-4.original.mistral.nlllogzs.fftnorm.txt")
pubmed.mistral.samp <- fread("../data/gpt-4/pubmed_gpt-4.sampled.mistral.nlllogzs.fftnorm.txt")
pubmed.mistral.orig$type <- "Human"
pubmed.mistral.samp$type <- "Sampled"
pubmed.mistral <- rbind(pubmed.mistral.orig, pubmed.mistral.samp)

# Fit GAM models and make predictions
gam.pubmed.orig <- gam(power ~ s(freq, bs = "cs"), data = pubmed.mistral[type=="Human"])
pred.pubmed.orig <- get_GAM_pred(gam.pubmed.orig)
pred.pubmed.orig$type <- "Human"

gam.pubmed.samp <- gam(power ~ s(freq, bs = "cs"), data = pubmed.mistral[type=="Sampled"])
pred.pubmed.samp <- get_GAM_pred(gam.pubmed.samp)
pred.pubmed.samp$type <- "Sampled"

pred.pubmed <- rbind(pred.pubmed.orig, pred.pubmed.samp)
# Plot the predictions
p <- ggplot(pred.pubmed, aes(x = x, y = y, color = type)) +
    geom_line() +
    labs(title = "GAM-Predicted Spectra", x = "Frequency", y = "Power") +
    theme_bw() + theme(legend.position = "top")
ggsave("gampred_pubmed_gpt-4_mistral_noncircle.pdf", plot=p, width=5, height=5)