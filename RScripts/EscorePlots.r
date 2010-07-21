# Required Libraries
library(ggplot2)

# Sourcing the Data
source("EscoreRawScore.r", chdir = TRUE)

# Examining the distribution of escores
p <- ggplot(
        data = dd,
        aes(x = rawscore), 
) + opts(title = "Raw Score Distributions by Instructor")

# Histogram of Rawscores
p <- p + geom_histogram(binwidth = 1.0) + facet_grid(instructor ~ .)
p
p + aes(x = escore) + opts(title = "Escore Distributions by Instructor") + facet_grid(instructor ~ .)

# Histogram of Rawscores with Escores overlaid
p + aes(fill = as.factor(escore)) + scale_fill_brewer(pal = "Greens")
p + aes(fill = as.factor(escore)) + scale_fill_brewer(pal = "Blues")
p + aes(fill = as.factor(escore)) + scale_fill_brewer(pal = "YlGnBu")

# TukeyHSD MCP Plots
plot(eratio.tukey)
title(sub = "Eratio")

plot(rawratio.tukey)
title(sub = "Rawratio")