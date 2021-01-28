library(lme4)
library(broom.mixed)
library(ggpubr)

real.data <- read_csv("tt.bigtable.real.all.csv")
rand.data <- read_csv("tt.bigtable.rand.all.csv")
code.areas <- read_csv("coded.regions.csv")

tseyel.real <- real.data %>%
  filter(grepl("(Tse)|(Yel)", language)) %>%
  mutate(
    prompt.cx.bin = ifelse(grepl("^[UMF]C", prompt.spkr), 1, 0),
    response.cx.bin = ifelse(grepl("^[UMF]C", response.spkr), 1, 0),
    prompt.ma.bin = ifelse(grepl("^MA", prompt.spkr), 1, 0),
    response.ma.bin = ifelse(grepl("^MA", response.spkr), 1, 0),
    prompt.fa.bin = ifelse(grepl("^FA", prompt.spkr), 1, 0),
    response.fa.bin = ifelse(grepl("^FA", response.spkr), 1, 0),
    n.tt.cx = prompt.cx.bin + response.cx.bin,
    n.tt.ma = prompt.ma.bin + response.ma.bin,
    n.tt.fa = prompt.fa.bin + response.fa.bin,
    n.tt.all = n.tt.cx + n.tt.ma + n.tt.fa,
    language = case_when(
      language == "Yeli Dnye" ~ "Yélî Dnye",
      TRUE ~ language
    ))

tseyel.rand <- rand.data %>%
  filter(grepl("(Tse)|(Yel)", language)) %>%
  mutate(
    prompt.cx.bin = ifelse(grepl("^[UMF]C", prompt.spkr), 1, 0),
    response.cx.bin = ifelse(grepl("^[UMF]C", response.spkr), 1, 0),
    prompt.ma.bin = ifelse(grepl("^MA", prompt.spkr), 1, 0),
    response.ma.bin = ifelse(grepl("^MA", response.spkr), 1, 0),
    prompt.fa.bin = ifelse(grepl("^FA", prompt.spkr), 1, 0),
    response.fa.bin = ifelse(grepl("^FA", response.spkr), 1, 0),
    n.tt.cx = prompt.cx.bin + response.cx.bin,
    n.tt.ma = prompt.ma.bin + response.ma.bin,
    n.tt.fa = prompt.fa.bin + response.fa.bin,
    n.tt.all = n.tt.cx + n.tt.ma + n.tt.fa,
    language = case_when(
      language == "Yeli Dnye" ~ "Yélî Dnye",
      TRUE ~ language
    )) %>%
  group_by(random.run.num, filename, annot.clip, language, age_mo_round) %>%
  summarize(
    tot.n.tt.cx = sum(n.tt.cx),
    tot.n.tt.ma = sum(n.tt.ma),
    tot.n.tt.fa = sum(n.tt.fa),
    tot.n.tt.all = sum(n.tt.all)) %>%
  full_join(code.areas, by = c("filename", "annot.clip")) %>%
  filter(grepl("(Tse)|(Yél)", language)) %>%
  mutate(
    ttr.cx = tot.n.tt.cx/(annot.clip.dur.ms/60000),
    ttr.ma = tot.n.tt.ma/(annot.clip.dur.ms/60000),
    ttr.fa = tot.n.tt.fa/(annot.clip.dur.ms/60000),
    ttr.all = tot.n.tt.all/(annot.clip.dur.ms/60000),
    `Clip type` = ifelse(grepl("(va-)|(tt-)|(ext)", annot.clip),
                         "active", "random")) %>%
  group_by(random.run.num, filename, `Clip type`, language, age_mo_round) %>%
  summarize(mean.ttr.all = mean(ttr.all)) %>%
  group_by(filename, `Clip type`, language, age_mo_round) %>%
  summarize(mean.mean.ttr.all = mean(mean.ttr.all)) %>%
  group_by(`Clip type`, language) %>%
  summarize(avg.ttr = mean(mean.mean.ttr.all),
            sem.ttr = sem(mean.mean.ttr.all),
            ymin = avg.ttr - sem.ttr,
            ymax = avg.ttr + sem.ttr)

tseyel.real.ttr.summary <- tseyel.real %>%
  group_by(filename, annot.clip, language, age_mo_round) %>%
  summarize(
    tot.n.tt.cx = sum(n.tt.cx),
    tot.n.tt.ma = sum(n.tt.ma),
    tot.n.tt.fa = sum(n.tt.fa),
    tot.n.tt.all = sum(n.tt.all)) %>%
  full_join(code.areas, by = c("filename", "annot.clip")) %>%
  filter(grepl("(Tse)|(Yél)", language)) %>%
  mutate(
    ttr.cx = tot.n.tt.cx/(annot.clip.dur.ms/60000),
    ttr.ma = tot.n.tt.ma/(annot.clip.dur.ms/60000),
    ttr.fa = tot.n.tt.fa/(annot.clip.dur.ms/60000),
    ttr.all = tot.n.tt.all/(annot.clip.dur.ms/60000),
    `Clip type` = ifelse(grepl("(va-)|(tt-)|(ext)", annot.clip),
                         "active", "random"))

tseyel.ttr.plot <- ggplot(data = tseyel.real.ttr.summary,
                          aes(x = language, y = ttr.all,
                              fill = `Clip type`, color = `Clip type`)) +
  geom_flat_violin(position = position_nudge(x = 0.15), alpha = 0.5) +
  geom_point(position = position_jitter(width = .1), alpha = 0.5) +
  geom_boxplot(width = 0.1, outlier.shape = NA, alpha = 0.5, color = "black") +
  theme_bw() +
  labs(y = "Turn transitions/min", x = "Corpus") +
  expand_limits(x = 2.75) +
  scale_fill_manual(values = c("red", "steelblue1")) +
  scale_color_manual(values = c("red", "steelblue1")) +
  geom_pointrange(data = tseyel.rand,
                  aes(x = language, y = avg.ttr,
                      ymin = ymin, ymax = ymax), color = "black",
                  position = position_dodge(0.1)) +
  theme(legend.position = "bottom")
ggexport(tseyel.ttr.plot,
         filename = paste0("figs/","ttr-tsyd.png"),
         width = 1250, height = 1250, res = 300)

tseyel.real.is.summary.byclip <- tseyel.real %>%
  filter(!is.na(intseq.num)) %>%
  mutate(
    prompt.cx = ifelse(grepl("^[UMF]C", prompt.spkr), 1, 0),
    response.cx = ifelse(grepl("^[UMF]C", response.spkr), 1, 0),
    prompt.ma = ifelse(grepl("^MA", prompt.spkr), 1, 0),
    response.ma = ifelse(grepl("^MA", response.spkr), 1, 0),
    prompt.fa = ifelse(grepl("^FA", prompt.spkr), 1, 0),
    response.fa = ifelse(grepl("^FA", response.spkr), 1, 0)) %>%
  group_by(filename, annot.clip, intseq.num, language, age_mo_round) %>%
  summarize(
    is.has.cx = sum(prompt.cx) + sum(response.cx) > 0,
    is.has.ma = sum(prompt.ma) + sum(response.ma) > 0,
    is.has.fa = sum(prompt.fa) + sum(response.fa) > 0) %>%
  group_by(filename, annot.clip, language, age_mo_round) %>%
  summarize(
    prop.is.cx = mean(is.has.cx),
    prop.is.ma = mean(is.has.ma),
    prop.is.fa = mean(is.has.fa))

tseyel.real.is.summary <- tseyel.real %>%
  filter(!is.na(intseq.num)) %>%
  mutate(
    prompt.cx = ifelse(grepl("^[UMF]C", prompt.spkr), 1, 0),
    response.cx = ifelse(grepl("^[UMF]C", response.spkr), 1, 0),
    prompt.ma = ifelse(grepl("^MA", prompt.spkr), 1, 0),
    response.ma = ifelse(grepl("^MA", response.spkr), 1, 0),
    prompt.fa = ifelse(grepl("^FA", prompt.spkr), 1, 0),
    response.fa = ifelse(grepl("^FA", response.spkr), 1, 0)) %>%
  group_by(filename, intseq.num, language, age_mo_round) %>%
  summarize(
    is.has.cx = sum(prompt.cx) + sum(response.cx) > 0,
    is.has.ma = sum(prompt.ma) + sum(response.ma) > 0,
    is.has.fa = sum(prompt.fa) + sum(response.fa) > 0) %>%
  group_by(filename, language, age_mo_round) %>%
  summarize(
    prop.is.cx = mean(is.has.cx),
    prop.is.ma = mean(is.has.ma),
    prop.is.fa = mean(is.has.fa))

tseyel.is.plot <- ggplot(data = tseyel.real.is.summary,
                         aes(x = age_mo_round, y = prop.is.cx,
                             color = language, fill = language,
                             linetype = language)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm") +
  theme_bw() +
  labs(y = "Prop.seqs. w/ other children", x = "Age (months)") +
  guides(color = guide_legend(title = "Corpus"),
         fill = guide_legend(title = "Corpus"),
         linetype = guide_legend(title = "Corpus")) +
  scale_fill_manual(values = c("orchid", "orchid4")) +
  scale_color_manual(values = c("orchid", "orchid4")) +
  theme(legend.position = "bottom")
ggexport(tseyel.is.plot,
         filename = paste0("figs/","is-tsyd.png"),
         width = 1250, height = 900, res = 300)





tsi.real <- real.data %>%
  filter(grepl("Tsi", language)) %>%
  mutate(
    prompt.cx.bin = ifelse(grepl("(^CX)|(^UC)", prompt.spkr), 1, 0),
    response.cx.bin = ifelse(grepl("(^CX)|(^UC)", response.spkr), 1, 0),
    prompt.ma.bin = ifelse(grepl("^MA", prompt.spkr), 1, 0),
    response.ma.bin = ifelse(grepl("^MA", response.spkr), 1, 0),
    prompt.fa.bin = ifelse(grepl("^FA", prompt.spkr), 1, 0),
    response.fa.bin = ifelse(grepl("^FA", response.spkr), 1, 0),
    n.tt.cx = prompt.cx.bin + response.cx.bin,
    n.tt.ma = prompt.ma.bin + response.ma.bin,
    n.tt.fa = prompt.fa.bin + response.fa.bin,
    n.tt.all = n.tt.cx + n.tt.ma + n.tt.fa,
    language = case_when(
      language == "Tsimane" ~ "Tsimane'",
      TRUE ~ language),
    rec_type = case_when(
      rec_type == "daylong_LENA" ~ "LENA",
      rec_type == "daylong_nonLENA" ~ "manual",
      TRUE ~ rec_type
    ))

tsi.rand <- rand.data %>%
  filter(grepl("Tsi", language)) %>%
  mutate(
    prompt.cx.bin = ifelse(grepl("(^CX)|(^UC)", prompt.spkr), 1, 0),
    response.cx.bin = ifelse(grepl("(^CX)|(^UC)", response.spkr), 1, 0),
    prompt.ma.bin = ifelse(grepl("^MA", prompt.spkr), 1, 0),
    response.ma.bin = ifelse(grepl("^MA", response.spkr), 1, 0),
    prompt.fa.bin = ifelse(grepl("^FA", prompt.spkr), 1, 0),
    response.fa.bin = ifelse(grepl("^FA", response.spkr), 1, 0),
    n.tt.cx = prompt.cx.bin + response.cx.bin,
    n.tt.ma = prompt.ma.bin + response.ma.bin,
    n.tt.fa = prompt.fa.bin + response.fa.bin,
    n.tt.all = n.tt.cx + n.tt.ma + n.tt.fa,
    language = case_when(
      language == "Tsimane" ~ "Tsimane'",
      TRUE ~ language),
    rec_type = case_when(
      rec_type == "daylong_LENA" ~ "LENA",
      rec_type == "daylong_nonLENA" ~ "manual",
      TRUE ~ rec_type
    )) %>%
  group_by(random.run.num, filename, annot.clip, rec_type, language, age_mo_round) %>%
  summarize(
    tot.n.tt.cx = sum(n.tt.cx),
    tot.n.tt.ma = sum(n.tt.ma),
    tot.n.tt.fa = sum(n.tt.fa),
    tot.n.tt.all = sum(n.tt.all)) %>%
  full_join(code.areas, by = c("filename", "annot.clip")) %>%
  filter(grepl("Tsi", language)) %>%
  mutate(
    ttr.cx = tot.n.tt.cx/(annot.clip.dur.ms/60000),
    ttr.ma = tot.n.tt.ma/(annot.clip.dur.ms/60000),
    ttr.fa = tot.n.tt.fa/(annot.clip.dur.ms/60000),
    ttr.all = tot.n.tt.all/(annot.clip.dur.ms/60000)) %>%
  group_by(random.run.num, filename, rec_type, language, age_mo_round) %>%
  summarize(mean.ttr.all = mean(ttr.all)) %>%
  group_by(language, rec_type) %>%
  summarize(avg.ttr = mean(mean.ttr.all),
            sem.ttr = sem(mean.ttr.all),
            ymin = avg.ttr - sem.ttr,
            ymax = avg.ttr + sem.ttr)

tsi.real.ttr.summary <- tsi.real %>%
  group_by(filename, annot.clip, rec_type, language, age_mo_round) %>%
  summarize(
    tot.n.tt.cx = sum(n.tt.cx),
    tot.n.tt.ma = sum(n.tt.ma),
    tot.n.tt.fa = sum(n.tt.fa),
    tot.n.tt.all = sum(n.tt.all)) %>%
  full_join(code.areas, by = c("filename", "annot.clip")) %>%
  filter(grepl("Tsi", language)) %>%
  mutate(
    ttr.cx = tot.n.tt.cx/(annot.clip.dur.ms/60000),
    ttr.ma = tot.n.tt.ma/(annot.clip.dur.ms/60000),
    ttr.fa = tot.n.tt.fa/(annot.clip.dur.ms/60000),
    ttr.all = tot.n.tt.all/(annot.clip.dur.ms/60000))

tsi.ttr.plot <- ggplot(data = tsi.real.ttr.summary,
                       aes(x = language, y = ttr.all,
                           fill = rec_type, color = rec_type)) +
  geom_flat_violin(position = position_nudge(x = 0.15), alpha = 0.5) +
  geom_point(position = position_jitter(width = .1), alpha = 0.5) +
  geom_boxplot(width = 0.1, outlier.shape = NA, alpha = 0.5, color = "black") +
  theme_bw() +
  labs(y = "Turn transitions/min", x = "") +
  expand_limits(x = 1.75) +
  scale_fill_manual(values = c("steelblue4", "steelblue1")) +
  scale_color_manual(values = c("steelblue4", "steelblue1")) +
  guides(color = guide_legend(title = "Version"),
         fill = guide_legend(title = "Version")) +
  geom_pointrange(data = tsi.rand,
                  aes(x = language, y = avg.ttr,
                      ymin = ymin, ymax = ymax), color = "black",
                  position = position_dodge(0.1)) +
  theme(legend.position = "bottom")
ggexport(tsi.ttr.plot,
         filename = paste0("figs/","ttr-tsi.png"),
         width = 1250, height = 1250, res = 300)

tsi.real.is.summary.byclip <- tsi.real %>%
  filter(!is.na(intseq.num)) %>%
  mutate(
    prompt.cx = ifelse(grepl("(^CX)|(^UC)", prompt.spkr), 1, 0),
    response.cx = ifelse(grepl("(^CX)|(^UC)", response.spkr), 1, 0),
    prompt.ma = ifelse(grepl("^MA", prompt.spkr), 1, 0),
    response.ma = ifelse(grepl("^MA", response.spkr), 1, 0),
    prompt.fa = ifelse(grepl("^FA", prompt.spkr), 1, 0),
    response.fa = ifelse(grepl("^FA", response.spkr), 1, 0)) %>%
  group_by(filename, annot.clip, intseq.num, rec_type, language, age_mo_round) %>%
  summarize(
    is.has.cx = sum(prompt.cx) + sum(response.cx) > 0,
    is.has.ma = sum(prompt.ma) + sum(response.ma) > 0,
    is.has.fa = sum(prompt.fa) + sum(response.fa) > 0) %>%
  group_by(filename, annot.clip, rec_type, language, age_mo_round) %>%
  summarize(
    prop.is.cx = mean(is.has.cx),
    prop.is.ma = mean(is.has.ma),
    prop.is.fa = mean(is.has.fa))


tsi.real.is.summary <- tsi.real %>%
  filter(!is.na(intseq.num)) %>%
  mutate(
    prompt.cx = ifelse(grepl("(^CX)|(^UC)", prompt.spkr), 1, 0),
    response.cx = ifelse(grepl("(^CX)|(^UC)", response.spkr), 1, 0),
    prompt.ma = ifelse(grepl("^MA", prompt.spkr), 1, 0),
    response.ma = ifelse(grepl("^MA", response.spkr), 1, 0),
    prompt.fa = ifelse(grepl("^FA", prompt.spkr), 1, 0),
    response.fa = ifelse(grepl("^FA", response.spkr), 1, 0)) %>%
  group_by(filename, intseq.num, rec_type, language, age_mo_round) %>%
  summarize(
    is.has.cx = sum(prompt.cx) + sum(response.cx) > 0,
    is.has.ma = sum(prompt.ma) + sum(response.ma) > 0,
    is.has.fa = sum(prompt.fa) + sum(response.fa) > 0) %>%
  group_by(filename, rec_type, language, age_mo_round) %>%
  summarize(
    prop.is.cx = mean(is.has.cx),
    prop.is.ma = mean(is.has.ma),
    prop.is.fa = mean(is.has.fa))

tsi.is.plot <- ggplot(data = tsi.real.is.summary,
                      aes(x = age_mo_round, y = prop.is.cx,
                          color = rec_type, fill = rec_type,
                          linetype = rec_type)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm") +
  theme_bw() +
  labs(y = "Prop.seqs. w/ other children", x = "Age (months)") +
  guides(color = guide_legend(title = "Version"),
         fill = guide_legend(title = "Version"),
         linetype = guide_legend(title = "Version")) +
  scale_fill_manual(values = c("steelblue4", "steelblue1")) +
  scale_color_manual(values = c("steelblue4", "steelblue1")) +
  theme(legend.position = "bottom")
ggexport(tsi.is.plot,
         filename = paste0("figs/","is-tsi.png"),
         width = 1250, height = 900, res = 300)


# Write out data because latex is ????
write_csv(tseyel.real.ttr.summary, "tseyel.real.ttr.summary.csv")
write_csv(tseyel.real.is.summary, "tseyel.real.is.summary.csv")
write_csv(tseyel.real.is.summary.byclip, "tseyel.real.is.summary.byclip.csv")
write_csv(tsi.real.ttr.summary, "tsi.real.ttr.summary.csv")
write_csv(tsi.real.is.summary, "tsi.real.is.summary.csv")
write_csv(tsi.real.is.summary.byclip, "tsi.real.is.summary.byclip.csv")



# Models
ttr.tseyel.m1 <- lmer(ttr.all ~
                        `Clip type` * language + (1|filename),
                      data = tseyel.real.ttr.summary)
# summary(ttr.tseyel.m1)

is.tseyel.m2 <- lmer(prop.is.cx ~
                       age_mo_round * language + (1|filename),
                     data = tseyel.real.is.summary.byclip)
# summary(is.tseyel.m2)

ttr.tsi.m1 <- lmer(ttr.all ~
                     rec_type + (1|filename),
                      data = tsi.real.ttr.summary)
# summary(ttr.tsi.m1)

is.tsi.m2 <- lmer(prop.is.cx ~
                       age_mo_round * rec_type + (1|filename),
                     data = tsi.real.is.summary.byclip)
# summary(is.tsi.m2)


all.models <- bind_rows(
  broom.mixed::tidy(ttr.tseyel.m1) %>%
    mutate(model = "ttr_TSYD"),
  broom.mixed::tidy(is.tseyel.m2) %>%
    mutate(model = "is_TSYD"),
  broom.mixed::tidy(ttr.tsi.m1) %>%
    mutate(model = "ttr_TSI"),
  broom.mixed::tidy(is.tsi.m2) %>%
    mutate(model = "is_TSI"))

write_csv(all.models, "all.models.csv")

# A linear mixed effects regression of turn-transitions per minute with predictors of clip type, corpus, and their interaction and a random intercept for child reveals that, indeed, random clips have significantly lower turn-transition rates (B = `r stat1`, SE = `r round(coef(summary(ttr.tseyel.m1))[ , "Std. Error"][2],2)`, t = `r round(coef(summary(ttr.tseyel.m1))[ , "t value"][2],2)`) and there is no evidence for a significant difference in turn-taking rates between languages (t = `r round(coef(summary(ttr.tseyel.m1))[ , "t value"][3],2)`) and no evidence for a clip type-language interaction (t = `r round(coef(summary(ttr.tseyel.m1))[ , "t value"][4],2)`).

# A linear mixed effects regression of the proportion of interactional sequences that feature at least one non-target child with predictors of age (in months), corpus, and their interaction and a random intercept for child reveals that, as expected, there is a significant age-by-corpus interaction by which Yélî children show a larger increase in other-child interactional sequences with age compared to Tseltal children (B = `r round(coef(summary(is.tseyel.m2))[ , "Estimate"][4],2)`, SE = `r round(coef(summary(is.tseyel.m2))[ , "Std. Error"][4],2)`, t = `r round(coef(summary(is.tseyel.m2))[ , "t value"][4],2)`). There is no evidence for simple effects of age (t = `r round(coef(summary(is.tseyel.m2))[ , "t value"][2],2)`) or language (t = `r round(coef(summary(is.tseyel.m2))[ , "t value"][3],2)`).