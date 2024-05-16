require("ggplot2")
require("data.table")


## GPT-4, est Mistral
# NLL logsz, FFT real
nlllogsz.fftreal.mis.orig = fread("../data/gpt-4/writing_gpt-4.original.mistral.nlllogzs.fftreal.txt")
nlllogsz.fftreal.mis.samp = fread("../data/gpt-4/writing_gpt-4.sampled.mistral.nlllogzs.fftreal.txt")
nlllogzs.fftreal.mis.orig$type <- "Human"
nlllogzs.fftreal.mis.samp$type <- "GPT-4"
nlllogzs.fftreal.mis <- rbind(nlllogzs.fftreal.mis.orig, nlllogzs.fftreal.mis.samp)
p <- ggplot(nlllogzs.fftreal.mis, aes(freq, power, color=type)) +
    geom_smooth() +
    theme_bw() + theme(plot.title = element_text(hjust = 0.5, vjust=-8, size = 12)) +
    ggtitle("Writing: Human vs. GPT-4 \nNLL logzs, FFT real, est Mistral)") +
    labs(x = bquote(omega[k]), y = bquote(X(omega[k])))
ggsave("gpt4_human_writing_mistral_nlllogzs_fftreal.pdf", plot=p, width=5, height=5)