#!/usr/bin/Rscript
library(ggplot2)

files <- list.files(
    path = "results/X-Set",
    pattern = "*.csv",
    full.names=TRUE
)

# Load all data into a labeled data.frame
load_df <- function(files) {
  ret <- data.frame(row.names = c("time", "w", "t", "group"))
  for (file in files) {
    curr <- data.frame(
        read.csv(file, header=FALSE, col.names = "time"), 
        w = as.integer(sub(".*_w([0-9]+).*$", "\\1", file)),
        t  = sub(".*_t([0-9]+).*$", "\\1", file),
        group = sub("results/X-Set/(.*)_w.*$", "\\1", file)
    )
    ret <- rbind(ret, curr)
  }
  ret
}
data <- load_df(files)

## data <- load_df(files[][1:18])

###################

plotSet <- function(w) {
  filtered <- data [
      data$w == w
  ,]

  final_data <- data.frame()
  for (group in unique(filtered$group)) {
    baseline <- mean(filtered [
        filtered$t == "1" &
        filtered$group == group
       ,]$time)
    for (t in unique(filtered$t)) {
      x <- filtered [
          filtered$t == t &
          filtered$group == group
         ,]$time
      final_data <- rbind(final_data, data.frame(t = t, group = group, x.sp = baseline / mean(x)))
    }
  }

  print(final_data)

  plot <- ggplot(
      final_data,
      aes(
          y = x.sp,
          x = t,
          group = paste(group),
          color = group,
          shape = group
    )) +
    geom_point(size=3) +
    geom_line() +
    geom_text(
        aes(
            label = formatC(x.sp, digits = 3),
            y = x.sp + (max(x.sp) * 0.04),
        )
    ) +
    labs(x = "Threads") +
    labs(y = "Speedup") +
    scale_color_manual(values=c("red", "black"))
  plot
}

plotSet(0)
ggsave("set_graph_0.pdf", width = 6, height = 4)
plotSet(1)
ggsave("set_graph_1.pdf", width = 6, height = 4)

