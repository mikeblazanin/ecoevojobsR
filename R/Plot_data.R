dat <- read.csv("./data-raw/ecoevojobsR_faculty.csv",
                stringsAsFactors = FALSE)

library(ggplot2)
library(dplyr)

dat$Timestamp <- lubridate::mdy_hm(dat$Timestamp)

dat_sum_type <-
  summarize(group_by(dat, Position.Type),
            n = n())
dat$Position_plot <-
  ifelse(dat_sum_type$n[match(dat$Position.Type, dat_sum_type$Position.Type)] > 3,
         dat$Position.Type,
         "Other")
dat_sum_rank <-
  summarize(group_by(dat, Rank),
            n = n())
dat$Rank_plot <-
  ifelse(dat_sum_rank$n[match(dat$Rank, dat_sum_rank$Rank)] > 5,
         dat$Rank,
         "Other")
dat_sum_classif <-
  summarize(group_by(dat, X2021.Basic.Classification),
            n = n())
dat$Classif_plot <-
  ifelse(dat_sum_classif$n[match(dat$X2021.Basic.Classification, dat_sum_classif$X2021.Basic.Classification)] > 5,
         dat$X2021.Basic.Classification,
         "Other")

ggplot(dat,
       aes(x = Timestamp, fill = Position_plot)) +
  geom_bar(stat = "bin")

ggplot(dat,
       aes(x = Timestamp, fill = Rank_plot)) +
  geom_bar(stat = "bin") +
  #guides(fill = "none") +
  NULL

ggplot(dat,
       aes(x = Timestamp, fill = Classif_plot)) +
  geom_bar(stat = "bin")

ggplot(dat,
       aes(x = Timestamp, fill = Matched.status)) +
  geom_bar(stat = "bin")
