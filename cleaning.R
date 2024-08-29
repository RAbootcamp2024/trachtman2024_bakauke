# git ignoreにdtaフォルダを追加・記載してデータをネットに上げないようにする

#パッケージのインストール----
# install.packages("tidyverse")
# install.packages("haven")
library("haven");library("tidyverse")


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
#確認
summary(dt$havedata_x)
summary(dt$havedata_y)




df <- data %>% 
  select(ends_with(".x"))
df
view(df)











