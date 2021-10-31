#!/usr/bin/Rscript
library(ggplot2)

files <- list.files(
    path = "results/1-SI",
    pattern = "*.csv",
    full.names=TRUE
)

# Load all data into a labeled data.frame
load_df <- function(files) {
  ret <- data.frame(row.names = c("time", "op", "t", "group"))
  for (file in files) {
    curr <- data.frame(
        read.csv(file, header=FALSE, col.names = "time"), 
        op = as.integer(sub(".*_op([0-9]+).*$", "\\1", file)),
        t  = sub(".*_t([0-9]+).*$", "\\1", file),
        group = sub("results/1-SI/(.*)_op.*$", "\\1", file)
    )
    ret <- rbind(ret, curr)
  }
  ret
}
data <- load_df(files)
## data <- load_df(files[][1:18])

###################

plotSI <- function(op) {
  filtered <- data [
      data$op == op
  ,]
  
  final_data <- do.call(
      data.frame,
      aggregate(
          filtered$time,
          list(t = filtered$t, group = filtered$group),
          FUN = function(x) c(m = mean(x), s = sd(x))
      )
  )

  print(final_data)

  plot <- ggplot(
      final_data,
      aes(
          y = x.m,
          x = t,
          group = paste(group),
          color = group,
          shape = group
    )) +
    geom_point() +
    geom_errorbar(aes(ymin = x.m-x.s, ymax = x.m+x.s), width = 0.15) +
    geom_line() +
    ## geom_text(
    ##     aes(
    ##         label = formatC(x.m, format = "e", digits = 3),
    ##         y = x.m + (max(x.m) * 0.07),
    ##     )
    ## ) +
    labs(x = "Threads") +
    labs(y = "Time") +
    scale_color_manual(values=c("red", "black"))
  plot
}

## plotSI(1000)
## plotSI(10000)
plotSI(100000)
ggsave("unique_id_graph_100000.pdf", width = 6, height = 4)

