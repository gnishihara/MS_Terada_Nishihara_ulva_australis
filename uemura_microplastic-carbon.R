# 2026_01_28
# Yuka Uemura
# 過酸化水素処理後TOC

# Loading Packages ########################################################
library(tidyverse)
library(readxl)
library(ggpubr)
library(showtext)
library(ggtext)
library(magick)
library(patchwork)
library(brms)
library(tidybayes)
library(bayestestR)
library(emmeans)
library(bayesplot)

# setting fonts ###########################################################
# font_add_google(name = "Noto Sans", family = "ns")
# font_add_google(name = "Noto Sans JP", family = "nsjp")
# theme_pubr(base_family = "ns", base_size = 10) |> theme_set()
# showtext_auto()





# Data Preparation 過酸化水素処理前 #########################################################
path1 = "~/Lab_Data/uemuray/hama_August/date/251023_hama_August.xlsx"
path2 = "~/Lab_Data/uemuray/hama_August/date/251111_hama_August_amamo.xlsx"
path3 = "~/Lab_Data/uemuray/hama/data/251211_uemura_hama_November_h2o2_filter_test.xlsx"

df1 = read_xlsx(path = path1, sheet = 1, range = "A1:P56")
df2 = read_xlsx(path = path2, sheet = 1)
df3 = read_xlsx(path = path3, sheet = 1)

# データの置換
# df2 の 56 行目全体を新しいデータフレームとして抽出・保存
data_56 = df2 |> filter(`Pos.` == 56)
data_25 = df2 |> filter(`Pos.` == 25)

# 結果の確認
print(data_56)
print(data_25)

# 25 行目の測定値の列を 56 行目の対応する列で置き換え
tmp25 = select(data_25, c(`Pos.`, `Weight  [mg]`, Name, Method))
tmp56 = select(data_56, -c(`Pos.`, `Weight  [mg]`, Name, Method))
data_25new = bind_cols(tmp25, tmp56)

df2_2 = df2 %>% filter(!(`Pos.` %in% c(25, 56))) %>% bind_rows(data_25new)

# データを結合
df_before = bind_rows(df1, df2_2, df3)

df_before2 = df_before |>
  select(
    depth = Name,
    weight = `Weight  [mg]`,
    TOC = `TOC  [%]`,
    date = `Date       Time`,
    time = `TOC400  Area`
  ) |>
  mutate(datetime = str_c(date, time, sep = " ")) |>
  mutate(datetime = as_datetime(datetime)) |>
  select(-c(date, time))

# Plotting 過酸化水素処理前 ################################################################

df_before3 =
  df_before2 |>
  filter(!depth %in% c("RunIn", "Blnk", "glucose", "caco3")) |>
  separate(depth, sep = "_",
           into = c("location", "depth", "individuals")) |>
  # location を文字列に変換
  mutate(location = as.character(location)) |>
  # 以下の方法も悪くないけど、深さの範囲が違ったら毎回変える必要がある
  # mutate(depth = str_sub(depth, 1, 2)) |>
  # mutate(depth = as.double(depth)) |>
  # mutate(depth = depth + 5) |>
  # 次の方法だと深さの範囲が変わってもコードを変える必要がない
  separate(
    depth, into = c("start", "end"), sep = "-" # 元のdepthを_で分ける
  ) |>
  mutate(start = as.numeric(start), # depthは文字列だったので数字に変える
         end = as.numeric(end)) |> # depthは文字列だったので数字に変える
  mutate(depth = (start + end) / 2) |>  # 浅い所(start)と深い所(end)で平均をとる
  # この方法だと深さの範囲が変わっても楽に修正できる
  group_by(location, depth) |>
  summarise(
    TOC_original = mean(TOC),
    .groups = "drop"
  ) |>
  mutate(weight = 20000) |>
  mutate(carbon_original = (weight * TOC_original)) |>
  select(-weight)



# Data Preparation 過酸化水素処理後 #########################################################
path4 = "~/Lab_Data/uemuray/TOC_after_H2O2/data/260122_uemura_a_h2o2_filter.xlsx"
df4 = read_xlsx(path = path4, sheet = 1)
df4 |> colnames()

# 92 ~ 104 行目全体を新しいデータフレームとして抽出・保存
data_92_104 =
  df4 |> slice(92:104)

# 結果の確認
print(data_92_104)

# 52 ~ 64 行目の測定値の列を 92 ~ 104 行目の対応する列で置き換え
df4[52:64, 5:16] = data_92_104[1:13, 5:16]

# 92 ~ 104 行目の削除
df4_2 = df4[-(92:104), ]

df4_3 = df4_2 |>
  select(
    depth = Name,
    weight = `Weight  [mg]`,
    TOC_after = `TOC  [%]`,
    date = `Date       Time`,
    time = `TOC400  Area`
  ) |>
  mutate(datetime = str_c(date, time, sep = " ")) |>
  mutate(datetime = as_datetime(datetime)) |>
  select(-c(date, time))

# Plotting 過酸化水素処理後################################################################

df_after = df4_3 |>
  filter(!depth %in% c("RunIn", "Blnk", "glucose", "caco3")) |>
  separate(depth, sep = "_",
           into = c("filter", "location", "depth", "individuals")) |>
  # depth 列に "f31_A_0-10_1" のような文字列が入っていた場合、
  # それを 「フィルター」「地点」「深さの範囲」「個体識別」 の 4 つの列に分割します。
  # location を文字列に変換
  mutate(location = as.character(location)) |>
  # 以下の方法も悪くないけど、深さの範囲が違ったら毎回変える必要がある
  # mutate(depth = str_sub(depth, 1, 2)) |>
  # mutate(depth = as.double(depth)) |>
  # mutate(depth = depth + 5) |>
  # 次の方法だと深さの範囲が変わってもコードを変える必要がない
  separate(
    depth, into = c("start", "end"), sep = "-" # 元のdepthを_で分ける
  ) |>
  mutate(start = as.numeric(start), # depthは文字列だったので数字に変える
         end = as.numeric(end)) |> # depthは文字列だったので数字に変える
  mutate(depth = (start + end) / 2) |># 浅い所(start)と深い所(end)で平均をとる
  # この方法だと深さの範囲が変わっても楽に修正できる
  mutate(carbon_weight = weight * TOC_after) |>
  group_by(location, depth) |>
  summarise(
    carbon_after = sum(carbon_weight),
    .groups = "drop"
  )

# 全データの結合 ##########################################################
df_all = df_before3 |>
  left_join(df_after, by = c("location", "depth"))# 共通の列を繋げる


# MP の寄与率計算 #########################################################
df_MP =
  df_all |>
  mutate(contribution_MP = (carbon_after * 100) / carbon_original) |>
  mutate(
    site = case_when(
      # 例：location が "A" または "B" の場合は "Group_A" にまとめる
      location %in% c("A3", "A4", "A5", "A6", "A7", "B4", "B5", "C5", "C6", "C7") ~ "Seagrass",
      # 例：それ以外（"F"、"G"、...）は "Group_B" にまとめる
      TRUE ~ "Sand"
    )) |>
  mutate(TOC_MP = (TOC_original * contribution_MP) / 100) |>
  select(-carbon_after, -carbon_original) %>%
  mutate(fdepth = factor(depth, levels = c(25, 15, 5), labels = c("Bottom", "Middle", "Top"))) |>
  mutate(TOC_adj = TOC_original - TOC_MP)

# MP 由来の TOC と MP 寄与率（オリジナル TOC の内何 % が MP 由来なのか）の平均
df_MP2 = df_MP |>
  group_by(site, depth) |>
  summarise(
    across(c(contribution_MP, TOC_MP, TOC_adj, TOC_original),
           list(m = mean, s = sd, n = length, se = ~ sd(.) / sqrt(length(.) - 1))),
    .groups = "drop"
  ) %>%
  mutate(fdepth = factor(depth, levels = c(25, 15, 5), labels = c("Bottom", "Middle", "Top")))

ylabel = "MP (%TOC)"
xlabel = "Depth (cm)"

p1 = df_MP2 %>%
  ggplot() +
  geom_col(
    aes(
      x = fdepth,
      y = TOC_MP_m,
      fill = site,
    ),
    position = position_dodge(width = 1),
    size = 3
  ) +
  geom_errorbar(
    aes(
      x = fdepth,
      ymin = TOC_MP_m - TOC_MP_se,
      ymax = TOC_MP_m + TOC_MP_se,
      color = site
    ),
    position = position_dodge(width = 1),
    width = 0,
    linewidth = 1
  ) +
  scale_x_discrete(xlabel) +
  scale_y_continuous(ylabel, limits = c(0, 0.04)) +
  scale_color_viridis_d("Site", end = 0.8, aesthetics = c("fill", "color")) +
  coord_flip()

ylabel = "Total (%TOC)"
xlabel = "Depth (cm)"

p2 = df_MP2 %>%
  ggplot() +
  geom_col(
    aes(
      x = fdepth,
      y = TOC_original_m,
      fill = site
    ),
    position = position_dodge(1),
    size = 3
  ) +
  geom_errorbar(
    aes(
      x = fdepth,
      ymin = TOC_original_m - TOC_original_se,
      ymax = TOC_original_m + TOC_original_se,
      color = site
    ),
    position = position_dodge(1),
    width = 0,
    linewidth = 1
  ) +
  scale_x_discrete(xlabel) +
  scale_y_continuous(ylabel, limits = c(0, 1.5)) +
  scale_color_viridis_d("Site", end = 0.8, aesthetics = c("fill", "color")) +
  coord_flip()

# TOC_MP の解析
# 一般化線形モデル・ガンマ分布・逆数のリンク関数
m1 = glm(TOC_MP ~ site + fdepth + site:fdepth, data = df_MP, family = Gamma("log"))
e1 = emmeans(m1, ~ site|fdepth)
contrast(e1, "pairwise")
anova(m1, "LRT")
summary(m1)
plot(m1)

df_MP |> pull(TOC_MP)       |> log() |> summary()
df_MP |> pull(TOC_original) |> log() |> summary()


scale_values = function(x) {
  # x = log(x)
  # location = mean(x)
  scale = sd(x)
  # location = median(x)
  # scale = mad(x)
  # (x - location)/scale
  (x)/scale
}

df_MP = df_MP |>
  mutate(
    sTOC_MP       = scale_values((TOC_MP)),
    sTOC_original = scale_values((TOC_original))
    )

ggplot(df_MP) + geom_histogram(aes(x = sTOC_MP))
ggplot(df_MP) + geom_histogram(aes(x = sTOC_original))

ggplot(df_MP) +
  geom_point(aes(x = sTOC_MP, y = sTOC_original, color = site), alpha = 0.9, size = 5)


lm(cbind(sTOC_MP, sTOC_original) ~ site * fdepth, data = df_MP) |> anova()

df_MP |>
  mutate(across(matches("^TOC"), log)) |>
  select(matches("^TOC")) |> cov()

manova(cbind(TOC_MP, TOC_original) ~ site + fdepth,
       data = mutate(df_MP, across(matches("TOC"), log))) |> summary(test = "Wilk")

MASS::lda(site ~ TOC_MP + TOC_original, data = df_MP)


df_MP |>
  mutate(across(matches("sTOC"), log)) |>
  reframe(across(matches("sTOC"), list(m = mean, s = sd)))

b1 = bf(sTOC_MP       ~ 0 + site : fdepth, shape ~ (1+site||fdepth)) + Gamma("log")
b2 = bf(sTOC_original ~ 0 + site : fdepth, shape ~ (1+site||fdepth)) + Gamma("log")
get_prior(b2, data = df_MP)

priorsMP = c(
  prior(normal(0, 1), class = b),
  # prior(student_t(3, -4, 1), class = Intercept),
  # prior(normal(0, 2), class = ar),
  # prior(exponential(2), class = sderr),
  # prior(gamma(0.1, 0.1), class = shape),
  prior(student_t(3,0, 2.5), class = Intercept, dpar = shape),
  prior(student_t(3,0, 1), class = sd, dpar = shape),
  NULL
)
priorsOR = c(
  prior(normal(1, 1), class = b),
  # prior(student_t(3, 0, 1), class = Intercept),
  # prior(normal(0, 2), class = ar),
  # prior(exponential(2), class = sderr),
  # prior(gamma(0.1, 0.1), class = shape),
  prior(student_t(3,0, 2.5), class = Intercept, dpar = shape),
  prior(student_t(3,0, 2.5), class = sd, dpar = shape),
  NULL
)

control = list(adapt_delta = 0.99, max_treedepth = 15)
warmup = 4000
iterations = warmup + 10000
file_refit = "always"
b1out = brm(b1, data = df_MP, seed = 2026, cores = 5, chains = 5, iter = iterations, warmup = warmup, refresh = 2000, prior = priorsMP, control = control, backend = "cmdstanr", file_refit = file_refit, file = "uemura_mp")
b2out = brm(b2, data = df_MP, seed = 2026, cores = 5, chains = 5, iter = iterations, warmup = warmup, refresh = 2000, prior = priorsOR, control = control, backend = "cmdstanr", file_refit = file_refit, file = "uemura_original")

bayesDiagnostics::automated_ppc(b1out, observed_data = df_MP$sTOC_MP)
bayesDiagnostics::automated_ppc(b2out, observed_data = df_MP$sTOC_original)

bayesDiagnostics::posterior_predictive_check(b1out, observed_data = df_MP$sTOC_MP)
bayesDiagnostics::posterior_predictive_check(b2out, observed_data = df_MP$sTOC_original)


roperange1 = df_MP$sTOC_MP       |> sd() |> print()
roperange2 = df_MP$sTOC_original |> sd() |> print()

df_MP$sTOC_MP       |> mean() |> print()
df_MP$sTOC_original |> mean() |> print()

b1out
b2out

e1 = emmeans(b1out, ~site|fdepth)
e2 = emmeans(b2out, ~site|fdepth)

contrast(e1, "pairwise") |> describe_posterior(rope_ci = 1, rope_range = c(-1,1) * roperange1 * 0.1) # TOC MP
contrast(e2, "pairwise") |> describe_posterior(rope_ci = 1, rope_range = c(-1,1) * roperange2 * 0.1) # TOC Original

# pp_check(b1out, type = "loo_pit_qq", ndraws = 50)

ppcplota1 = pp_check(b1out, type = "dens_overlay", ndraws = 50)
ppcplota2 = pp_check(b1out, type = "stat_2d", stat = c("median", "mad"), ndraws = 500)
ppcplotb1 = pp_check(b2out, type = "dens_overlay", ndraws = 50)
ppcplotb2 = pp_check(b2out, type = "stat_2d", stat = c("median", "mad"), ndraws = 500)

# pp_check(b1out, type = "dens_overlay_grouped", group = "site", ndraws = 100)
# pp_check(b1out, type = "dens_overlay_grouped", group = "fdepth", ndraws = 100)
# pp_check(b2out, type = "dens_overlay_grouped", group = "site", ndraws = 100)
# pp_check(b2out, type = "dens_overlay_grouped", group = "fdepth", ndraws = 100)

p1 + ppcplota1 + ppcplota2 + p2 + ppcplotb1 + ppcplotb2

color_scheme_set("blue")

b1out
vars = get_variables(b1out)
posteriorcp = as.array(b1out)
lpcp = log_posterior(b1out)
npcp = nuts_params(b1out)
# mcmc_parcoord(posteriorcp, np = npcp)
mcmc_pairs(posteriorcp, np = npcp, regex_pars = "^b_site", off_diag_args = list(size = 1))
mcmc_nuts_divergence(npcp, lpcp)


b2out
posteriorcp = as.array(b2out)
lpcp = log_posterior(b2out)
npcp = nuts_params(b2out)
# mcmc_parcoord(posteriorcp, np = npcp)
mcmc_pairs(posteriorcp, np = npcp, regex_pars = "^b_site", off_diag_args = list(size = 1))
mcmc_nuts_divergence(npcp, lpcp)








##################################################################################
