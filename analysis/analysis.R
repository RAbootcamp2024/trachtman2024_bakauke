  # environmentをきれいにする
  rm(list=ls())
  
  # パッケージ
  # install.packages("gt")
  # install.packages("ggplot2")
  # install.packages("ragg")
  # ライブラリを読み込みます
  library(tidyverse)
  library(gt)
  library(haven)
  library(ggplot2)
  library(ragg)
  
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
  
  # #gtパッケージを用いて表作成
  # subtab %>% 
  #   group_by(variable) %>% 
  #   gt()　%>% #とりあえず表はできたので、以下整型
  #   fmt_number(decimals = 2) %>% #小数のラウンド
  #   tab_header(title = "TABLE 2 - ORTHOGONALITY CHECK") 
    
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
  
  # #Femaleについてftestのp値を抽出
  # F1 <- sub %>% 
  #    aov(female ~ treatment, .)
  # F1
  # F1sum <-  summary(F1) %>% 
  #   unlist() %>% 
  #   as_data_frame()
  # F1sum[9,]
  
  # 別の方法でp値を抜き出す
  F1 <- sub %>% 
    aov(female ~ treatment, .)
  F1
  F1sum <-  summary(F1)[[1]][["Pr(>F)"]][1]
  F1sum
  
  
  
  #summary(F1)[[1]] リストの構造の確認
  
  # treatment以外の変数の数
  # lengthvar <- length(names(sub)) - 1
  
  # 1からtreatment以外の変数の数の数列
  lengthvar <- seq(ncol(sub)-1)
  
  # からの入れ物を作る
  fvalues <- rep(NA, 10)
  
  F_sum <- list()
  #F_sum[i]にFemale以降のi番目の変数についてのF検定を実施。
  # fvaluesにp値を格納していく
  
  
  # sub[, i]がformulaの指定として不適だった。
  # for(i in lengthvar){
  #   F_sum[[i]] <- aov(sub[ ,i] ~ treatment, sub)
  #   fvalues[i] <-  summary(F_sum[i])[[1]][["Pr(>F)"]][1]
  # }
  
  
  for(i in lengthvar){
    dep <- colnames(sub)[i]
    formula <- as.formula(paste(dep,"~treatment"))
    F_sum <- aov(formula, sub)
    fvalues[i] <-  summary(F_sum)[[1]][["Pr(>F)"]][1]
  }
  fvalues
  
  # Fテストのp-valueに空の数列を追加
  ftest <- c(fvalues,rep(NA,10))
  ftest
  
  #subtabの横にftestの列とその中身を追加
  subtab$ftest <- ftest
  subtab
  
  #列名の変更
  subtab <- rename(subtab,"F-test,joint sig"="ftest")
  subtab
  

  #gtパッケージを用いて表作成
table2 <- subtab %>% 
    group_by(variable) %>% 
    gt()　%>% #整型
    cols_label(stats="") %>% 　　#statsきえてくれ～
    fmt_number(decimals = 2) %>% #小数のラウンド
    fmt_missing(columns = everything(),
                missing_text = "") %>% 
    tab_header(title = "TABLE 2 - ORTHOGONALITY CHECK") %>% 
    tab_source_note(source_note = "
                    Notes: Means and standard deviations of ten variables measured in the baseline survey. † indicates variables used in the rerandomization procedure. p-value from F-test of the joint significance of treatments is reported in last column. Daily notifications includes all notifications the participant receives across all devices and all applications, where a notification is defined as anything that generates an alert (including SMS and email). Importance, x − y is the “importance” of meditation, self-reported on a scale from 1 to 10, minus that of meal logging. Difficulty, x − y is the “difficulty” of meditation, self-reported on a scale from 1 to 10, minus that of meal logging. Fun, x − y is the “fun” of meditation, self-reported on a scale from 1 to 10, minus that of meal logging. I report only the difference between difficulty and fun
                     ") 
# %>%gtsave(filename = "analysis/Table2.png")
  # gtshotというパッケージが今不具合中。
table2

gtsave(table2, "analysis/table2.html")

#
gtsave(table2, "analysis/table2.tex")
  
  
