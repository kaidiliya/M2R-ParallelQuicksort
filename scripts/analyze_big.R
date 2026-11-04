library(ggplot2)
library(dplyr)

# Run from the project root:  Rscript scripts/analyze_big.R > data/liya_2026-11-04_big/analysis_big_output.txt 2>&1
# Same figures as analyze.R, but for the BIG experiments (sizes up to 10,000,000).
dir <- "data/liya_2026-11-04_big"
files <- list.files(dir, pattern = "\\.csv$", full.names = TRUE)

# ---- 1. read the data ------------------------------------------------------
read_one <- function(f) {
  d <- read.csv(f, header = FALSE, skip = 1,
                col.names = c("size", "algo", "time"), strip.white = TRUE)
  d$algo <- tolower(d$algo)
  d$algo[d$algo == "built-in"] <- "builtin"
  name <- sub("\\.csv$", "", basename(f))
  rest <- sub("^measurements_", "", name)
  d$experiment <- sub("_.*$", "", rest)        # bigsize / bigsizewithall / bigbigsize
  d$run <- sub("^[^_]*_", "", rest)            # start time of the run
  d
}
df <- do.call(rbind, lapply(files, read_one))

# ---- 2. names shown on the figures -----------------------------------------
exp_labels <- c(
  bigsize        = "Big sizes\n6 sizes up to 10M, 5 reps, fixed order",
  bigsizewithall = "Big sizes, all changes\n6 sizes up to 10M, 30 reps, random order",
  bigbigsize     = "Huge sizes\nsizes up to 100M, 5 reps, fixed order")
file_names <- c(bigsize = "fig6_big_sizes",
                bigsizewithall = "fig7_big_all_changes",
                bigbigsize = "fig8_huge_sizes")
keep <- names(exp_labels)[names(exp_labels) %in% unique(df$experiment)]
exp_labels <- exp_labels[keep]
df$exp_key <- df$experiment
df$experiment <- factor(exp_labels[df$experiment], levels = exp_labels)

# Test 1, 2 = the runs of the same experiment, in time order
df <- df %>% group_by(experiment) %>%
  mutate(test_no = match(run, sort(unique(run)))) %>% ungroup()
df$test <- factor(paste("Test", df$test_no))

# ---- 3. mean and 95% confidence interval -----------------------------------
s <- df %>%
  group_by(experiment, exp_key, test, size, algo) %>%
  summarise(n = n(), mean = mean(time), sd = sd(time),
            ci = qt(0.975, n - 1) * sd / sqrt(n), .groups = "drop") %>%
  mutate(lower = mean - ci, upper = mean + ci)

r <- s %>% group_by(experiment, test, size) %>%
  summarise(ratio = mean[algo == "parallel"] / mean[algo == "sequential"],
            .groups = "drop")

cat("\n== parallel time / sequential time (above 1 = parallel is slower; below 1 = parallel is faster) ==\n")
r_print <- as.data.frame(r)
r_print$experiment <- sub("\n", " - ", r_print$experiment)
print(r_print, digits = 3)

cat("\n== Average width of the 95% CI, in % of the mean (smaller = more precise) ==\n")
ci_tab <- s %>% mutate(rel_ci = 100 * ci / mean) %>%
  group_by(experiment) %>%
  summarise(avg_ci_percent = mean(rel_ci), .groups = "drop")
ci_tab$experiment <- sub("\n", " - ", ci_tab$experiment)
print(as.data.frame(ci_tab), digits = 3)

# ---- 4. figures: all three programs in one panel, total time in seconds -----
algo_labels <- c(sequential = "sequential (1 thread)",
                 parallel   = "parallel (several threads)",
                 builtin    = "builtin (C library qsort)")
s$algo <- factor(algo_labels[as.character(s$algo)], levels = algo_labels)

pal <- c("#1F77B4", "#FF7F0E", "#B22222")   # blue, orange, dark red
shp <- c(16, 17, 15)
lty <- c("solid", "solid", "dashed")
names(pal) <- names(shp) <- names(lty) <- algo_labels

for (lab in levels(s$experiment)) {
  d   <- filter(s, experiment == lab)
  key <- d$exp_key[1]
  p <- ggplot(d, aes(x = size, y = mean, color = algo, shape = algo,
                     linetype = algo)) +
    geom_line(linewidth = 0.8) +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.08,
                  linewidth = 0.6, linetype = "solid") +
    geom_point(size = 2.5) +
    scale_color_manual(values = pal) +
    scale_shape_manual(values = shp) +
    scale_linetype_manual(values = lty) +
    scale_x_log10(labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
    scale_y_log10(labels = scales::label_number(accuracy = 0.00001,
                                                drop0trailing = TRUE)) +
    facet_wrap(~ test) +
    labs(x = "Array size (number of integers to sort, log scale)",
         y = "Time to sort (seconds, log scale)",
         color = "Sorting program", shape = "Sorting program",
         linetype = "Sorting program",
         title = gsub("\n", ": ", lab),
         subtitle = "Each point = mean of the repetitions; vertical bar = 95% confidence interval (often too small to see)") +
    theme_bw() + theme(legend.position = "bottom")
  ggsave(file.path(dir, paste0(file_names[[key]], ".png")), p, width = 11, height = 6)
}