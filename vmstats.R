library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

vmstats = read_lines("vm_stats_df_ALL_RUNS_1644336236.txt") %>%
  str_replace("^ +", "")
vmstats = tibble(raw_line = vmstats)

header = filter(vmstats, grepl("^r", raw_line)) %>%
  head(1) %>%
  unlist()

vmstats = filter(vmstats, ! grepl("(^r)|(^proc)", raw_line)) %>%
  separate(raw_line, into = tibble_header, sep = "\\s+") %>%
  mutate(across(.fns = as.numeric)) %>%
  mutate(minute = 1:n())

p = vmstats %>%
  select(`RAM used by programs (k)` = buff,
         `RAM free (k)` = free,
         `%CPU Utilization (user processes)` = us,
         `% CPU Utilization (system processes)` = sy,
         minute) %>%
  pivot_longer(-minute) %>%
  ggplot(aes(minute, value)) +
    geom_line() +
    facet_wrap(~name, scales = 'free_y', ncol = 1) +
    labs(x = "Time (minutes)", y = "")