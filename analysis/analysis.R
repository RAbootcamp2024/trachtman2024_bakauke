# environmentをきれいにする
rm(list=ls())

# パッケージ
# install.packages("gt")

# ライブラリを読み込みます
library(tidyverse)
library(gt)
library(haven)


# データを読み込みます
load(file="cleaning/data.RData")

# Table1を作るぜ！は嘘

# Table2を作るぜ
# female college age dailynotif med_daily_ever med_last_month nuttrack_daily_ever nuttrack_last_month rel_ben_med rel_cost_med

sub <- data %>% 
  filter(day==1) %>% #1人ごとのデータが複製されていることに注意
  select(female, college, age, dailynotif, med_daily_ever, med_last_month, 
         nuttrack_daily_ever, nuttrack_last_month, rel_ben_med, rel_cost_med, treatment)

#先にday1に限定すれば、行数が人数と一致してくれるので、emailいらなかった！
# sub <- data %>% 
#   select(day, email, female, college, age, dailynotif, med_daily_ever, med_last_month, 
#          nuttrack_daily_ever, nuttrack_last_month, rel_ben_med, rel_cost_med, treatment) %>% 
#   filter(day==1)　#1人ごとのデータが複製されていることに注意
# var_1 <- sub %>% 
#   select(-c(day,email))
# var_1
# var_2 <- var_1 %>%
#   group_by(treatment) 
# var_2

summary(sub$rel_ben_med)
sd(sub$rel_ben_med)
#importance x-yはrel_ben_medをコピー
#difficulty-fun x-yはrel_cost_medをコピー
#これを使うけど、後ほど変数を作る可能性がある

view(sub)

#平均をtreatmentごとに計算して、名前を付与
submean <- sub %>% 
  group_by(treatment) %>% 
  summarise(across(everything(),mean), .groups="drop") 

submean <- t(submean)  
submean <-submean[-1,]

label <- c("control", "mx", "my", "mx_my", "zy")
colnames(submean) <- label

variable_lab <- c(
  "Female" , "Went to college", "Age", "Daily notifications", "Meditated daily, ever",
  "Meditated daily, last month", "Logged meals, ever", "Logged meals, last month",
  "Importance, x-y", "Difficulty - fun, x-y")

submean <- submean%>%
  as.data.frame() %>% 
  mutate(stats="mean")
submean$variable <- variable_lab

#stdのデータフレーム
subsd <- sub %>% 
  group_by(treatment) %>% 
  summarise(across(everything(),sd), .groups="drop") 

subsd <- t(subsd)

subsd <- subsd[-1,]

colnames(subsd) <- label
subsd <- subsd%>%
  as.data.frame() %>% 
  mutate(stats="sd")
subsd$variable <- variable_lab

# 結合した後転置するとうまくいかない
# substat <- bind_rows(submean, subsd)
# substat <- t(substat)  

#結合
substat <- bind_rows(submean, subsd)
rownames(substat) <- NULL # 行名を空にしました
substat 

#列の順番を変えましょう
subtab <- substat %>% 
  select(variable, stats, everything()) 

#gtパッケージを用いて表作成
subtab %>% 
  group_by(variable) %>% 
  gt()　%>% #とりあえず表はできたので、以下整型
  fmt_number(decimals = 2) %>% #小数のラウンド
  tab_header(title = "TABLE 2 - ORTHOGONALITY CHECK") 
  
#F検定
# #do:
# reg `var' `treatforreg'
# 	loc pvalue = Ftail(`e(df_m)',`e(df_r)',`e(F)')

class(sub$treatment)
# sub <- sub %>% 
#   mutate(treatment=as.factor(treatment))
#stataの属性と違って変換できない。

#classをhaven::as_factor()を活用して、変更。
sub$treatment <- haven::as_factor(sub$treatment)

#Femaleについて
F1 <- sub %>% 
  aov(female ~ )









