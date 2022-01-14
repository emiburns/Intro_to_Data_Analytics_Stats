library(ggplot2)

#histogram function
ggHistogram <- function(colname) {
    out <- 1.5 * (quantile(colname, .75, names=F))
    p <- ggplot(data=df_clean, aes(x=colname)) +
        geom_histogram(binwidth = 10000, col="darkgreen", fill="green", alpha = .2) + 
        geom_vline(aes(xintercept=out), color="cyan3") +
        xlab(colname)
    return(p)
}