library(tidyverse)
library(lubridate)

start_date <- ymd("2018-04-12")
df <- read_csv("data/test.csv")

df_1 <- df %>% 
  # 不要な変数を削除
  select(-`参照回数`) %>% 
  # 変数名を変更
  rename(UserID = `#ユーザーID`) %>% 
  # long型へ
  gather(key = ref_hist, value = date_time, -UserID) %>% 
  # 日付のみの文字列へ
  mutate(date_time = str_replace_all(date_time, "\\(.\\).*$", "")) %>% 
  # 日付の差分を算出
  mutate(delay_days = time_length(ymd(date_time) - start_date, unit = "day")) %>% 
  # NAを除去
  filter(!is.na(date_time)) %>%
  # ユーザーIDと日付がユニークなのを残す
  # -> 同一人物が同一日にアクセスしているのを1行だけにする
  distinct(UserID, date_time, delay_days) %>% 
  # ユーザーごとにグループ化
  group_by(UserID) %>% 
  # ユーザーごとに行番号(何回目の登場か)を出す
  mutate(times = row_number(date_time)) %>%
  # 並べ替え
  arrange(UserID)