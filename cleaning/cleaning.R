# git ignoreにdtaフォルダを追加・記載してデータをネットに上げないようにする

# environmentをきれいにする
rm(list=ls())


#パッケージのインストール----
# install.packages("tidyverse")
# install.packages("haven")
library("haven")
library("tidyverse")


#survey データ読み込み（時間に余裕があれば繰り返し！）----
survey1_clean <- read_dta("dta/survey1_clean.dta")
survey2_clean <- read_dta("dta/survey2_clean.dta")
survey3_clean <- read_dta("dta/survey3_clean.dta")

#appdata----
appdata_x <- read_dta("dta/appdata_x_clean.dta")
appdata_y <- read_dta("dta/appdata_y_clean.dta")

#sample----
sample_random <- read_dta("dta/sample_randomized_nopii.dta")



#appdataの結合----
appdata <- left_join(appdata_x,appdata_y,
                     by=c("email","date","date_enroll","day")) 
###　fulljoinだとうまくいかなかった（datelastdownloadの中身が実は違っていた）

survey <- survey1_clean %>%  left_join(survey2_clean,
                     by=c("email")) %>% 
  left_join(survey3_clean,
            by=c("email"))

# survey <-   left_join(survey1_clean, survey2_clean,
#                                        by=c("email")) %>% 
#   left_join(survey3_clean,
#             by=c("email"))

# appdataとsurveyの結合 ----
data <- left_join(appdata, survey, by="email") %>% 
  left_join(sample_random, by=c("email","date_enroll"))


# groupbyで作ったid(email)とsurveydataのid(email)は別物でした
# trial <- appdata %>% 
#   group_by("email") %>% 
#   left_join(survey, by="email") %>% 
#   ungroup()

#havedata_xまたはhavedata_yのどちらかがゼロならばドロップ----
data <- data %>% 
  filter(!havedata_x==0) %>% 
  filter(!havedata_y==0)

#変数名に.xを含む変数を削除----
data <- data %>% 
  rename_with(~str_replace_all(.,".x",""),contains(".x"))

#yとtimeを削除
data <- data %>% 
  select(-ends_with(".y")) %>% 
  select(-starts_with("time"))　#time1-64とtimezoneが消えました

#確認
summary(dt$havedata_x)
summary(dt$havedata_y)

#マージ後のデータで同じ変数名のものを探索、"-select"でdropする----
df_x <- data %>% 
  select(ends_with(".x"))
df_y <- data %>% 
  select(ends_with(".y"))
df_x;df_y
names(df_x);names(df_y)
identical(names(df_x),names(df_y))

# * Create treatment period
# keep if day>0 これはもうやった
# gen treatmentperiod = date > date_enroll & date <= (date_enroll + 27)
data <- data %>% 
  mutate(
    treatmentperiod = if_else(date > date_enroll & date <= (date_enroll + 27),1,0)
  )


# * Treatment edits
data <- data %>% 
  mutate(
    mx = if_else(treatment==2|treatment==4 , 1 ,0) ,
    my = if_else(treatment==3|treatment==4 , 1 ,0) ,
    mxmy = if_else(treatment==4 , 1 ,0) ,
    zy = if_else(treatment==5 , 1 ,0) ,
    mxonly = if_else(treatment==2 , 1 ,0) ,
    myonly = if_else(treatment==3 , 1 ,0) ,
    mxandmy = if_else(treatment==4 , 1 ,0) 
  )

# tab treatment
# gen mx = inlist(treatment, 2, 4)
# gen my = inlist(treatment, 3, 4)
# gen mxmy = treatment==4
# gen zy = treatment==5 
# label var mx "mx"
# label var my "my"
# label var mxmy "mxmy"
# label var zy "zy"
# gen mxonly = treatment==2
# gen myonly = treatment==3
# gen mxandmy = treatment==4
# label var mxonly "mx only"
# label var myonly "my only"
# label var mxandmy "mx & my"

#データの保存----
save(data,file="cleaning/data.RData")
rm(list=ls())

