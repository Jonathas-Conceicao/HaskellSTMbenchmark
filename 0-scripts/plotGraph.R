#!/usr/bin/Rscript
library(ggplot2)

files <- list.files(
    path = "results/2-LL",
    pattern = "*.csv",
    full.names=TRUE
)

# Load all data into a labeled data.frame
load_df <- function(files) {
  ret <- data.frame(row.names = c("time", "op", "ln", "t", "group"))
  for (file in files) {
    curr <- data.frame(
        read.csv(file, header=FALSE, col.names = "time"), 
        op = as.integer(sub(".*_op([0-9]+).*$", "\\1", file)),
        ln = as.integer(sub(".*_ln([0-9]+).*$", "\\1", file)),
        t  = sub(".*_t([0-9]+).*$", "\\1", file),
        group = sub("results/2-LL/(.*)_op.*$", "\\1", file)
    )
    ret <- rbind(ret, curr)
  }
  ret
}
data <- load_df(files)
## data <- load_df(files[][1:18])

###################

plotLL <- function(ln, op) {
  filtered <- data [
      data$op == op &
      data$ln == ln
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
          ## shape = group
    )) +
    geom_point() +
    geom_errorbar(aes(ymin = x.m-x.s, ymax = x.m+x.s), width = 0.15) +
    geom_line() +
    geom_text(
        aes(
            label = formatC(x.m, format = "e", digits = 3),
            y = x.m + (max(x.m) * 0.07),
        )
    ) +
    labs(title = sprintf("Linked List with %i elements and %i operations", ln, op)) +
    labs(x = "Length") +
    labs(y = "Time") +
    scale_color_manual(values=c("red", "black"))
  plot
}

## plotLL(500, 200)
## plotLL(1000, 200)
## plotLL(1500, 200)
## plotLL(500, 2000)
## plotLL(1000, 2000)
## plotLL(1500, 2000)
