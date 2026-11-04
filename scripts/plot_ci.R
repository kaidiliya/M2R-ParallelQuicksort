library(ggplot2)
library(dplyr)


# Idea: take ONE array size (1,000,000 integers) and ONE test (Test 1) of each
# experiment, and show every single repetition as a dot. Then the mean and the
# 95% confidence interval are drawn on top of the dots, so you can see what
# the interval means and why 30 repetitions give a narrower interval than 5.

dir <- "data/liya_2026-11-04"
files <- list.files(dir, pattern = "\\.csv$", full.names = TRUE)

# ---- read the data -------------------------------------
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

exp_short <- c(
  baseline = "Original\n5 reps, fixed order",
  size     = "More sizes\n5 reps, fixed order",
  reps     = "More repetitions\n30 reps, fixed order",
  random   = "Random order\n5 reps, random order",
  all      = "All changes\n30 reps, random order")
df$experiment <- factor(exp_short[df$experiment], levels = exp_short)

# Test 1, 2, 3 = the three runs of the same experiment, in time order
df <- df %>% group_by(experiment) %>%
  mutate(test_no = match(run, sort(unique(run)))) %>% ungroup()

algo_labels <- c(sequential = "sequential (1 thread)",
                 parallel   = "parallel (several threads)",
                 builtin    = "builtin (C library qsort)")
df$algo <- factor(algo_labels[df$algo], levels = algo_labels)

# ---- keep one size and one test --------------------------------------------
raw <- df %>% filter(size == 1000000, test_no == 1)

# mean and 95% confidence interval
sm <- raw %>%
  group_by(experiment, algo) %>%
  summarise(n = n(), mean = mean(time), sd = sd(time),
            ci = qt(0.975, n - 1) * sd / sqrt(n), .groups = "drop") %>%
  mutate(lower = mean - ci, upper = mean + ci)

cat("\n== 1,000,000 integers, Test 1: number of repetitions and CI ==\n")
print(as.data.frame(sm %>% mutate(ci_percent = 100 * ci / mean)), digits = 4)

# ---- figure ----------------------------------------------------------------
p <- ggplot() +
  # grey dots = every single repetition
  geom_jitter(data = raw, aes(x = 1, y = time), width = 0.15, height = 0,
              color = "grey40", alpha = 0.6, size = 1.6) +
  # red bar = 95% confidence interval of the mean
  geom_errorbar(data = sm, aes(x = 1, ymin = lower, ymax = upper),
                width = 0.35, linewidth = 1.2, color = "red") +
  # red diamond = mean
  geom_point(data = sm, aes(x = 1, y = mean), shape = 18, size = 4,
             color = "red") +
  facet_grid(algo ~ experiment, scales = "free_y") +
  scale_x_continuous(limits = c(0.5, 1.5)) +
  labs(x = NULL, y = "Time to sort 1,000,000 integers (seconds)",
       title = "What the confidence interval is (1,000,000 integers, Test 1 of each experiment)",
       subtitle = "Grey dot = one repetition. Red diamond = mean. Red bar = 95% confidence interval of the mean.") +
  theme_bw() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
        strip.text.x = element_text(size = 8))
ggsave(file.path(dir, "fig_ci_dots.png"), p, width = 12, height = 7)