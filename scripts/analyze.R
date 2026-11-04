library(ggplot2)
library(dplyr)

# ---- 1. read the data ------------------------------------------------------
dir <- "data/liya_2026-11-04"
files <- list.files(dir, pattern = "\\.csv$", full.names = TRUE)

read_one <- function(f) {
  d <- read.csv(f, header = FALSE, skip = 1,
                col.names = c("size", "algo", "time"), strip.white = TRUE)
  d$algo <- tolower(d$algo)
  d$algo[d$algo == "built-in"] <- "builtin"
  name <- sub("\\.csv$", "", basename(f))
  rest <- sub("^measurements_", "", name)
  d$experiment <- ifelse(grepl("^(all|random|reps|size)_", rest),
                         sub("_.*$", "", rest), "baseline")
  d$run <- sub("^(all|random|reps|size)_", "", rest)
  d
}
df <- do.call(rbind, lapply(files, read_one))

# ---- 2. names shown on the figures -----------------------------------------
exp_labels <- c(
  baseline = "Original\n5 sizes, 5 reps, fixed order",
  size     = "More sizes\n10 sizes, 5 reps, fixed order",
  reps     = "More repetitions\n5 sizes, 30 reps, fixed order",
  random   = "Random order\n5 sizes, 5 reps, random order",
  all      = "All changes\n10 sizes, 30 reps, random order")
df$experiment <- factor(exp_labels[df$experiment], levels = exp_labels)

df <- df %>% group_by(experiment) %>%
  mutate(test_no = match(run, sort(unique(run)))) %>% ungroup()
df$test <- factor(paste("Test", df$test_no))

# ---- 3. mean and 95% confidence interval -----------------------------------
s <- df %>%
  group_by(experiment, test, size, algo) %>%
  summarise(n = n(), mean = mean(time), sd = sd(time),
            ci = qt(0.975, n - 1) * sd / sqrt(n), .groups = "drop") %>%
  mutate(lower = mean - ci, upper = mean + ci)

# how many times slower is the parallel program than the sequential one
r <- s %>% group_by(experiment, test, size) %>%
  summarise(ratio = mean[algo == "parallel"] / mean[algo == "sequential"],
            .groups = "drop")

cat("\n== parallel time / sequential time (above 1 = parallel is slower) ==\n")
r_print <- as.data.frame(r)
r_print$experiment <- sub("\n", " - ", r_print$experiment)
print(r_print, digits = 3)

# how precise is each experiment
cat("\n== Average width of the 95% CI, in % of the mean (smaller = more precise) ==\n")
ci_tab <- s %>% mutate(rel_ci = 100 * ci / mean) %>%
  group_by(experiment) %>%
  summarise(avg_ci_percent = mean(rel_ci), .groups = "drop")
ci_tab$experiment <- sub("\n", " - ", ci_tab$experiment)
print(as.data.frame(ci_tab), digits = 3)

# ---- 4. figures: one per experiment ----------------------------------------
algo_labels <- c(sequential = "sequential (1 thread)",
                 parallel   = "parallel (several threads)",
                 builtin    = "builtin (C library qsort)")
s$algo <- factor(algo_labels[as.character(s$algo)], levels = algo_labels)

# colors, point shapes and line types 
pal <- c("#1F77B4", "#FF7F0E", "#B22222")   # blue, orange, dark red
shp <- c(16, 17, 15)                         # circle, triangle, square
lty <- c("solid", "solid", "dashed")
names(pal) <- names(shp) <- names(lty) <- algo_labels

file_names <- c("fig1_original", "fig2_more_sizes", "fig3_more_reps",
                "fig4_random_order", "fig5_all_changes")

for (i in seq_along(levels(s$experiment))) {
  lab <- levels(s$experiment)[i]
  d <- filter(s, experiment == lab)
  p <- ggplot(d, aes(x = size, y = mean, color = algo,
                     shape = algo, linetype = algo)) +
    # 95% confidence interval: vertical bar at each point
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.07,
                  linewidth = 0.6, linetype = "solid") +
    geom_line(linewidth = 0.8) +
    geom_point(size = 2.5) +
    scale_color_manual(values = pal) +
    scale_shape_manual(values = shp) +
    scale_linetype_manual(values = lty) +
    scale_x_log10(labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
    scale_y_log10(labels = scales::label_number(accuracy = 0.00001,
                                                drop0trailing = TRUE)) +
    facet_wrap(~test, nrow = 1) +
    labs(x = "Array size (number of integers to sort, log scale)",
         y = "Time to sort (seconds, log scale)",
         color = "Sorting program", shape = "Sorting program",
         linetype = "Sorting program",
         title = gsub("\n", ": ", lab),
         subtitle = "Each point = mean of the repetitions; vertical bar = 95% confidence interval (often too small to see).") +
    theme_bw() + theme(legend.position = "bottom")
  ggsave(file.path(dir, paste0(file_names[i], ".png")), p, width = 11, height = 4.8)
}