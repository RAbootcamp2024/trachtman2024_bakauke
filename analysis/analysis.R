# environmentをきれいにする
rm(list=ls())

# ライブラリを読み込みます
library(tidyverse)

# データを読み込みます
load(file="cleaning/data.RData")

# Table1を作るぜ！は嘘
#嫌い

# Table2を作るぜ
# female college age dailynotif med_daily_ever med_last_month nuttrack_daily_ever nuttrack_last_month rel_ben_med rel_cost_med

sub <- data %>% 
  select(day, email, female, college, age, dailynotif, med_daily_ever, med_last_month, 
         nuttrack_daily_ever, nuttrack_last_month, rel_ben_med, rel_cost_med, treatment) %>% 
  filter(day==1)#1人ごとのデータが複製されていることに注意
         
var_1 <- sub %>% 
  select(-c(day,email))
var_1

summary(sub$rel_ben_med)
sd(sub$rel_ben_med)
#importance x-yはrel_ben_medをコピー
#difficulty-fun x-yはrel_cost_medをコピー
#これを使うけど、後ほど変数を作る可能性がある

view(sub)

#平均を求める。
#treatmentごとに計算します

var_2 <- var_1 %>%
  group_by(treatment) 
var_2

# 明日続きやる
# mean_table <- stat.desc(var_2) %>% 
#   as.data.frame() %>% 
#   rownames_to_column(var = "statistics") %>% 
#   pivot_longer(-statistic,names_to = "variable",
#                values_to = "value") %>% 
#   pivot_wider(names_from = statistic,
#               values_from = value) %>% 
#   select(variable,









