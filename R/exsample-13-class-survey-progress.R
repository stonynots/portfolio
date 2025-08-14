# Description 解説 ----
# 学生アンケートの結果を集計する R プログラム
# 取り込んだデータを利用して
# 日付に分けて集計の結果を表示

# Load packages パッケージの読み込み ----
library(tidyverse)  # https://www.tidyverse.org/packages/
library(writexl)    # https://docs.ropensci.org/writexl/

# Clear all data まず全てのデータをクリアする ----
rm(list = ls())

# Set working directory 作業ディレクトリを設定 ----
# For Windows terminal ウインドウズのパスで設定
# Require to modify if running from UNIX/Linux terminal
# もしもリナックスで実行するなら調整が必要
#
# setwd("C:/r")

# Load survey data 授業アンケートデータを取り入れる ----

# Setup function here ファンクションをここで設定
function_importdata = function(file_name) {
  read_csv(file = str_c(file_name, ".csv", sep = ""),
  # read_csv(file = str_c("classsurvey/", file_name, ".csv", sep = ""),
    na = c(""),  # Set to empty if NA もしも NA であれば空白
    col_types = cols_only(
    学籍番号       = col_character(),
    学生氏名       = col_character(),
    学生カナ氏名   = col_character(),
    学部           = col_character(),
    学科           = col_character(),
    学年           = col_character(),
    指導教員       = col_character(),
    時間割コード   = col_character(),
    授業名         = col_character(),
    担当教員名     = col_character(),
    最終更新日     = col_datetime(format = "%Y/%m/%d %H:%M") # As date and time
    )
  ) %>% # Using pipe to connect パイプを利用して接続
  ## Add a date column for imported data 取り込んだデータの日付 ----
  mutate(date = as.Date(file_name)) %>%
  ## Convert to display only date 日付のみに表示を変更 ----
  mutate(最終更新日 = as.Date(
    最終更新日, format = '%y-%m-%d'
    )
  )  %>%
  ## Rename student year 学年の表示を変更 ----
  mutate(学年 = case_when(
    学年 == "学部1年" ~ "1 年生",
    学年 == "学部2年" ~ "2 年生",
    学年 == "学部3年" ~ "3 年生",
    学年 == "学部4年" ~ "4 年生"
    )
  ) %>%
  ## Add a column for student year category 学番 3 桁の情報を追加 ----
  mutate(学番三桁_studentYearCategory_name =
    substr(学籍番号, start = 1, stop = 3)
  ) %>%
  ## Add a column for curriculum category カリキュラムの情報を追加 ----
  mutate(時間割カテゴリ_curriculumCategory_name = case_when(
    substr(時間割コード, start = 1, stop = 1) == 0 ~ "10 カリ",
    substr(時間割コード, start = 1, stop = 1) == 7 ~ "17 カリ"
    )
  ) %>%
  ## Add a column for counting each entry as 1 合計の集計にそれぞれ 1 を設定----
  mutate(科目数の合計_entry_tally = case_when(
    # Even if NA is assigned as 1 もしも NA であっても 1 を設定
    !is.na(最終更新日) ~ 1,
    TRUE ~ 1
    )
  ) %>%
  ## Add a column for counting answered entry as 1 回答済みのみ 1 を設定 ----
  mutate(回答した科目数_answered_tally = case_when(
    # If NA then kept as NA もしも NA であれば NA に設定
    is.na(最終更新日) ~ NA_real_, 
    TRUE ~ 1
    )
  ) %>%
  ## Recorder columns 行の並び順を設定 ----
  relocate(any_of(c(
    "最終更新日",
    "科目数の合計_entry_tally",
    "回答した科目数_answered_tally",
    "学番三桁_studentYearCategory_name",
    "学籍番号",
    "学生氏名",
    "学生カナ氏名",
    "学年",
    "学部",
    "学科",
    "指導教員",
    "時間割カテゴリ_curriculumCategory_name",
    "時間割コード",
    "授業名",
    "担当教員名"
    ))
  ) %>%
  ## Rename columns 行の表示名を変更 ----
  rename(
  # Changed to = changed from 左側が変更後で = 右側が変更前
    "最終更新日_record_entry_date"   = "最終更新日",                          
    "学籍番号_student_id"            = "学籍番号",
    "学生氏名_student_name"          = "学生氏名",
    "学生カナ氏名_student_name_kana" = "学生カナ氏名",
    "学年_student_year_name"         = "学年",
    "学部_department_name"           = "学部",
    "学科_faculty_name"              = "学科",                               
    "指導教員_assigned_teacher_name" = "指導教員",
    "時間割コード_class_code"        = "時間割コード",
    "授業名_class_name"              = "授業名",
    "担当教員名_teacher_name"        = "担当教員名"
  )
} # Closing function_importdata = function(file_name) ファンクションの最後

# EDIT SECTION ----

# Using function to import data ファンクションを利用してデータを取り入れる ----
# Edit and add entry ここを編集して新たなデータを追加する
# File name is set to "YYYY-MM-DD" for reading as data

# Original data starts with file name
# [ CSV 授業に関するアンケート（Web版）_2023年度_前期_YYYYMMDDhhmmss.csv ]

  # day01 <- function_importdata("2023-07-21") # From 2023-07-21.csv Friday
  # day02 <- function_importdata("2023-07-22") # From 2023-07-22.csv Saturday
  # day03 <- function_importdata("2023-07-23") # From 2023-07-23.csv Sunday
  # day04 <- function_importdata("2023-07-24") # From 2023-07-24.csv Monday
  # day05 <- function_importdata("2023-07-25") # From 2023-07-25.csv Tuesday
  # day06 <- function_importdata("2023-07-26") # From 2023-07-26.csv Wednesday
  # day07 <- function_importdata("2023-07-27") # From 2023-07-27.csv Thursday
  # day08 <- function_importdata("2023-07-28") # From 2023-07-28.csv Friday
  # day09 <- function_importdata("2023-07-31") # From 2023-07-31.csv Monday
  # day10 <- function_importdata("2023-08-01") # From 2023-08-01.csv Tuesday
  # day11 <- function_importdata("2023-08-02") # From 2023-08-02.csv Wednesday
  # day12 <- function_importdata("2023-08-03") # From 2023-08-03.csv Thursday
  # day13 <- function_importdata("2023-08-04") # From 2023-08-04.csv Friday
  # day14 <- function_importdata("2023-08-06") # Downloaded on Mon, Aug 7 morning 07:26
  # day15 <- function_importdata("2023-08-13") # From 2023-08-13 Sunday at 18:50
  # day16 <- function_importdata("2023-08-18") # From 2023-08-19 Saturday at 09:20
  
# January 2024 CSV file
#  day01 <- function_importdata("2024-01-17 CSV 授業に関するアンケート（Web版）_2023年度_後期_20240117052014")
#  day02 <- function_importdata("2024-01-18 CSV 授業に関するアンケート（Web版）_2023年度_後期_20240118062850")
#  day03 <- function_importdata("2024-01-19 CSV 授業に関するアンケート（Web版）_2023年度_後期_20240119025556")
#  day04 <- function_importdata("2024-01-20 CSV 授業に関するアンケート（Web版）_2023年度_後期_20240120093532")
#  day05 <- function_importdata("2024-01-22 CSV 授業に関するアンケート（Web版）_2023年度_後期_20240122055437")
#  day06 <- function_importdata("2024-01-23 CSV 授業に関するアンケート（Web版）_2023年度_後期_20240123051715")
#  day07 <- function_importdata("2024-01-24 CSV 授業に関するアンケート（Web版）_2023年度_後期_20240124014556")
#  day08 <- function_importdata("2024-01-25 CSV 授業に関するアンケート（Web版）_2023年度_後期_20240125032932")
#  day09 <- function_importdata("2024-01-26 CSV 授業に関するアンケート（Web版）_2023年度_後期_20240126054048")
#  day10 <- function_importdata("2024-01-27 CSV 授業に関するアンケート（Web版）_2023年度_後期_20240127092743")
#  day11 <- function_importdata("2024-01-29 CSV 授業に関するアンケート（Web版）_2023年度_後期_20240129071129")
#  day12 <- function_importdata("2024-01-30 CSV 授業に関するアンケート（Web版）_2023年度_後期_20240130081043")
#  day13 <- function_importdata("2024-01-31 CSV 授業に関するアンケート（Web版）_2023年度_後期_20240131053918")
#  day14 <- function_importdata("2024-02-02 CSV 授業に関するアンケート（Web版）_2023年度_後期_20240202005041")
#  day15 <- function_importdata("2024-02-03 CSV 授業に関するアンケート（Web版）_2023年度_後期_20240203104513")
#  day16 <- function_importdata("2024-02-07 CSV 授業に関するアンケート（Web版）_2023年度_後期_20240207055637")
#  day17 <- function_importdata("2024-02-16 CSV 授業に関するアンケート（Web版）_2023年度_後期_20240216070735")

# For example for adding new data 例えば以下のように追加
#
# day03 <- function_importdata("2023-07-23")
# day04 <- function_importdata("2023-07-24")

# August 2024 CSV file
  day01 <- function_importdata("2024-08-01 CSV 授業に関するアンケート（Web版）_2024年度_前期_20240801072122")
  day02 <- function_importdata("2024-08-03 CSV 授業に関するアンケート（Web版）_2024年度_前期_20240803111358")
  day03 <- function_importdata("2024-08-06 CSV 授業に関するアンケート（Web版）_2024年度_前期_20240806072941")
  day04 <- function_importdata("2024-08-27 CSV 授業に関するアンケート（Web版）_2024年度_前期_20240827072710")
  
# Import error is present but ignoring エラーが表示されるが無視しています
#
# e.g. Example of a warning message
#
#  Warning messages:
#    1: One or more parsing issues, call `problems()` on your data frame for details, e.g.:
#    dat <- vroom(...)
#  problems(dat)
  
# To view error use command below エラー内容を確認するなら以下のコマンドを実行
#
# problems(day03) %>% print(n = 500)

# ANOTHER EDIT SECTION ----  

# Combine all together into 1 data 取り入れたデータを 1 つにまとめる ----
#
# Edit and add entry ここを編集して新たなデータを追加する

# Experimental

  data <- do.call("rbind", mget(ls(pattern="^day*")))

# Originally written as below
#    
#  data <- rbind(
#    day01, 
#    day02,
#    day03,
#    day04
    # day04    
#  )
  
## Delete joined data 統合したデータを削除
#
# This will remove all matching pattern; e.g. day01, day02, day03, etc.,
#
rm(list=ls(pattern="day"))

# rm(day01)
# rm(day02)
# rm(day03)

# Garbage collection
gc(reset = TRUE)

# ---------- ---------- rowCount ---------- ---------- ----

# Calculate by row counts ----
rowCountAnswered <- data %>%
  select(
    date,
    学籍番号_student_id,
    最終更新日_record_entry_date
  ) %>%
  filter(!is.na(最終更新日_record_entry_date)) %>%
  count(date) %>%
  rename("rowCountAnswered_num" = "n")

# Calculate by row count ----
rowCount <- data %>%
  select(
    date,
    学籍番号_student_id,
    最終更新日_record_entry_date
  ) %>%
  count(date) %>%
  rename("rowCountTotal_num" = "n") %>%
  ## Add a column ----
mutate(rowCountAnswered) %>%
  ## Add a column ----
mutate(percentage = round(
  rowCountAnswered_num / rowCountTotal_num, digits = 3)
)

# Remove data ----
rm(rowCountAnswered)

rowCount_pivot <- rowCount %>% pivot_wider(
  names_from = "date",
  values_from = c(
    "rowCountAnswered_num",
    "percentage"
  )
)

## Intentionally keeping rowCount data (Not to delete) ----
# Instead remove rowCount_pivot
rm(rowCount_pivot)

# ---------- ---------- student ---------- ---------- ----

# entryTotal ----
entryTotal <- data %>% 
  select(c(
    date,
    学籍番号_student_id, 
    科目数の合計_entry_tally
  )
) %>%
group_by(date, 学籍番号_student_id) %>%
summarize(科目数の合計_entry_tally = n(), .groups = "keep")

# answerdTotal ----
answeredTotal <- data %>% 
  select(c(
    date,
    学籍番号_student_id, 
    回答した科目数_answered_tally
  )
) %>%
## Excluding NA entries もしも NA であれば除外する ----
filter(!is.na(回答した科目数_answered_tally)) %>%
group_by(date, 学籍番号_student_id) %>%
summarize(回答した科目数_answered_tally = n(), .groups = "keep")

# List of students 学生別のデータを準備-----
student <- data %>%
  distinct(
    学番三桁_studentYearCategory_name,
    学籍番号_student_id,
    学生氏名_student_name,
    学生カナ氏名_student_name_kana,
    学年_student_year_name,
    学部_department_name,
    学科_faculty_name,
    指導教員_assigned_teacher_name
) %>% 
## Joining entryTotal データをつなげる ----
left_join(entryTotal, by = 
  "学籍番号_student_id"
) %>%
## Joining answeredTotal with data 日付も紐づけてデータをつなげる----
left_join(answeredTotal, by = c(
  "学籍番号_student_id", 
  "date"
  )
) %>%
## Calculate answered percentage 回答率の計算 ----
mutate(該当する学生の回答率_percentage_num = round(
  回答した科目数_answered_tally / 科目数の合計_entry_tally, digits = 2
)) %>%
## Add classification column based on calculation result 表示項目を追加 ----
mutate(階級_ratio_group_name = case_when(
  該当する学生の回答率_percentage_num < 0.1 ~ "< 10% 未満",
  該当する学生の回答率_percentage_num < 0.2 ~ "< 20% 未満",
  該当する学生の回答率_percentage_num < 0.3 ~ "< 30% 未満",
  該当する学生の回答率_percentage_num < 0.4 ~ "< 40% 未満",
  該当する学生の回答率_percentage_num < 0.5 ~ "< 50% 未満",
  該当する学生の回答率_percentage_num < 0.6 ~ "< 60% 未満",
  該当する学生の回答率_percentage_num < 0.7 ~ "< 70% 未満",
  該当する学生の回答率_percentage_num < 0.8 ~ "< 80% 未満",
  該当する学生の回答率_percentage_num < 0.9 ~ "< 90% 未満",
  該当する学生の回答率_percentage_num < 0.99 ~ "> 90% 以上",
  該当する学生の回答率_percentage_num == 1 ~ "= 100%",
  is.na(該当する学生の回答率_percentage_num) ~ "回答なし"
  )
) %>%
## Arrange from higher ratio データを並び替える ----
arrange(
  date,
  desc(該当する学生の回答率_percentage_num),
  学籍番号_student_id
)

# Use pivot_wider to rearrange result 結果を日付ごとに表示する
student_pivot <- student %>% pivot_wider(
  names_from = "date",
  values_from = c(
    "科目数の合計_entry_tally",
    "回答した科目数_answered_tally",
    "該当する学生の回答率_percentage_num",
    "階級_ratio_group_name"  
  )
) %>%
arrange(
  desc(学番三桁_studentYearCategory_name),
  学籍番号_student_id
)

# Remove entryTotal and answeredTotal data データを削除 ----
rm(entryTotal)
rm(answeredTotal)

# ---------- ---------- groupRatio ---------- ---------- ----

# Calculate a number of students 学生数の計算 ----
studentTotal <- data %>%
## Extract only unique record 重複しているデータを取り除く ----
distinct(学籍番号_student_id) %>%
## Counting 集計する ----
count()


# Pivot by answered percentage group ----
groupRatio <- student %>%
group_by(date, 該当する学生の回答率_percentage_num) %>%
summarize(
  回答した科目数_answered_tally = n(),
  .groups = "keep"
) %>%
## Rename columns ----
rename(
## Changed to = changed from ----
"該当する学生の人数_answered_student_tally" 
  = "回答した科目数_answered_tally"
) %>%
## Add ratio of group by total student number ----
mutate(学生数の合計で割った割合_percentage_number = round(
  該当する学生の人数_answered_student_tally / studentTotal$n, digits = 6
  )
) %>%
## Add a new column ----
mutate(回答率のグループ_percentage_group_name = case_when(
  該当する学生の回答率_percentage_num == 1 ~ "100% 回答済み",
    # 90–99%
    該当する学生の回答率_percentage_num < 1 &
      該当する学生の回答率_percentage_num >= 0.9 ~ "90–99% 回答済み",
    # 80–89%
    該当する学生の回答率_percentage_num < 0.9 &
      該当する学生の回答率_percentage_num >= 0.8 ~ "80–89% 回答済み",
    # 70–79%
    該当する学生の回答率_percentage_num < 0.8 &
      該当する学生の回答率_percentage_num >= 0.7 ~ "70–79% 回答済み",
    # 60–69%
    該当する学生の回答率_percentage_num < 0.7 &
      該当する学生の回答率_percentage_num >= 0.6 ~ "60–69% 回答済み",
    # 50–59%
    該当する学生の回答率_percentage_num < 0.6 &
      該当する学生の回答率_percentage_num >= 0.5 ~ "50–59% 回答済み",
    # 40–49%
    該当する学生の回答率_percentage_num < 0.5 &
      該当する学生の回答率_percentage_num >= 0.4 ~ "40–49% 回答済み",
    # 30–39%
    該当する学生の回答率_percentage_num < 0.4 &
      該当する学生の回答率_percentage_num >= 0.3 ~ "30–39% 回答済み",
    # 20–29%
    該当する学生の回答率_percentage_num < 0.3 &
      該当する学生の回答率_percentage_num >= 0.2 ~ "20–29% 回答済み",
    # 10–19%
    該当する学生の回答率_percentage_num < 0.2 &
      該当する学生の回答率_percentage_num >= 0.1 ~ "10–19% 回答済み",
    # 1–9%
    該当する学生の回答率_percentage_num < 0.1 &
      該当する学生の回答率_percentage_num >= 0.01 ~ "1–9% 回答済み",
    # O or NULL
    TRUE ~ "未回答"
  )
) %>%  
## Regroup by selection ----
group_by(date, 回答率のグループ_percentage_group_name) %>%
summarize(
  sum(学生数の合計で割った割合_percentage_number),
  .groups = "keep"
) %>%  
## Arrange order ----
arrange(factor(回答率のグループ_percentage_group_name, levels = c(
  '100% 回答済み', 
  '90–99% 回答済み', 
  '80–89% 回答済み', 
  '70–79% 回答済み',
  '60–69% 回答済み',
  '50–59% 回答済み',
  '40–49% 回答済み',
  '30–39% 回答済み',
  '20–29% 回答済み',
  '10–19% 回答済み',
  '1–9% 回答済み',
  '未回答'
  ))
)

# Remove studentTotal ----
rm(studentTotal)

# Remove student data ----
rm(student)

groupRatio_pivot <- groupRatio %>% pivot_wider(
  names_from = "date", 
  values_from = "sum(学生数の合計で割った割合_percentage_number)"
)

# Remove groupRatio data ----
rm(groupRatio)

# ---------- ---------- class ---------- ---------- ----

# Calculate and join entryTotal ----
classEntryTotal <- data %>% 
  select(c(
    date,
    時間割コード_class_code,
    科目数の合計_entry_tally
  )
) %>%
group_by(date, 時間割コード_class_code) %>%
summarize(科目数の合計_entry_tally = n(), .groups = "keep")

# Calculate and join answeredTotal ----
classAnsweredTotal <- data %>% 
  select(c(
    date,
    時間割コード_class_code, 
    回答した科目数_answered_tally
  )
) %>%
## Excluding NA entries
filter(!is.na(回答した科目数_answered_tally)) %>%
group_by(date, 時間割コード_class_code) %>%
summarize(回答した科目数_answered_tally = n(), .groups = "keep")

# List of classes ----
class <- data %>%
distinct(
  時間割カテゴリ_curriculumCategory_name,
  時間割コード_class_code,
  授業名_class_name,
  担当教員名_teacher_name
) %>% 
## Joining entryTotal ----
left_join(classEntryTotal, by = "時間割コード_class_code") %>%
## Joining answeredTotal ----
left_join(classAnsweredTotal, by = c("date", "時間割コード_class_code")) %>% 
## Calculate answered percentage ----
mutate(該当する学生の回答率_percentage_num = round(
  回答した科目数_answered_tally / 科目数の合計_entry_tally
  , digits = 2
  )
) %>%
## Add category ----
mutate(階級_classification = case_when(
  該当する学生の回答率_percentage_num < 0.1 ~ "< 10% 未満",
  該当する学生の回答率_percentage_num < 0.2 ~ "< 20% 未満",
  該当する学生の回答率_percentage_num < 0.3 ~ "< 30% 未満",
  該当する学生の回答率_percentage_num < 0.4 ~ "< 40% 未満",
  該当する学生の回答率_percentage_num < 0.5 ~ "< 50% 未満",
  該当する学生の回答率_percentage_num < 0.6 ~ "< 60% 未満",
  該当する学生の回答率_percentage_num < 0.7 ~ "< 70% 未満",
  該当する学生の回答率_percentage_num < 0.8 ~ "< 80% 未満",
  該当する学生の回答率_percentage_num < 0.9 ~ "< 90% 未満",
  該当する学生の回答率_percentage_num < 0.99 ~ "> 90% 以上",
  該当する学生の回答率_percentage_num == 1 ~ "= 100%",
  is.na(該当する学生の回答率_percentage_num) ~ "回答なし"
  )
) %>%
## Arrange ----
arrange(date, desc(該当する学生の回答率_percentage_num))

# Remove data ----  
rm(classAnsweredTotal)
rm(classEntryTotal)

# Class pivot data ----
class_pivot <- class %>% pivot_wider(
  names_from = "date",
  values_from = c(
    "科目数の合計_entry_tally",
    "回答した科目数_answered_tally",
    "該当する学生の回答率_percentage_num",
    "階級_classification"
  )
) %>%
arrange(
  担当教員名_teacher_name,
  時間割コード_class_code
)

## Delete class data ----
rm(class)

# ---------- ---------- yearFaculty ---------- ---------- ----

# Result by yearFaculty ----
yearFaculty <- data %>%
  select(
    date,
    学番三桁_studentYearCategory_name,
    学年_student_year_name,
    学籍番号_student_id,
    学科_faculty_name,
    科目数の合計_entry_tally,
    回答した科目数_answered_tally,
    学科_faculty_name
  ) %>%
  group_by(date, 学年_student_year_name, 学科_faculty_name) %>%
  summarize(
    履修登録数 = sum(科目数の合計_entry_tally, na.rm = TRUE),
    回答数 = sum(回答した科目数_answered_tally, na.rm = TRUE),
    .groups = "keep"
  ) %>%
  ungroup() %>%
  rename(
    "学年" = "学年_student_year_name",
    "学科" = "学科_faculty_name"
  ) %>%
  relocate(any_of(c(
    "学年",
    "学科",
    "履修登録数",
    "回答数"
  ))
  ) %>%
  arrange(
    学年,
    factor(学科, levels = c(
      '機械工学科',
      '電気電子工学科',
      '情報工学科',
      'コンピュータ応用学科',
      '総合デザイン学科',
      '人間環境学科'
    ))
  ) %>%
  mutate(回答率 = 回答数 / 履修登録数)

yearFaculty_pivot <- yearFaculty %>% pivot_wider(
  names_from = "date",
  #  names_sep = ".",
  values_from = c(
    "回答数",
    "回答率"
  )
)

# Remove yearFaculty data ----
rm(yearFaculty)

# ---------- ---------- studentYear ---------- ---------- ----

# Student year data ----
studentYear <- data %>%
select(
  date,
  学番三桁_studentYearCategory_name,
  回答した科目数_answered_tally,
) %>%
  ## Excluding NA entries
  filter(!is.na(回答した科目数_answered_tally)) %>%
  group_by(date, 学番三桁_studentYearCategory_name) %>%
  summarize(回答した科目数_answered_tally = n(), .groups = "keep"
) %>%
## Rename colums ----
rename(
  # Changed to = changed from
  "学番左3桁" = "学番三桁_studentYearCategory_name",
  "回答数" = "回答した科目数_answered_tally"
)

# StudentYearTotal data ----
studentYearTotal <- data %>%
select(
  date,
  学番三桁_studentYearCategory_name,
  回答した科目数_answered_tally
) %>%
group_by(date, 学番三桁_studentYearCategory_name) %>%
summarize(
  回答した科目数_answered_tally = n(),
  .groups = "keep"
) %>%
## Rename columns ----
rename(
  # Changed to = changed from
  "学番左3桁" = "学番三桁_studentYearCategory_name",
  "回答数合計" = "回答した科目数_answered_tally"
) %>%
## Arrange from higher ratio データを並び替える ----
arrange(
  date,
  学番左3桁
)

## Joining StudentYear data and total data データをつなげる ----
studentYear <- studentYear %>%
left_join(studentYearTotal, by = c(
  "date", 
  "学番左3桁"
  )
) %>%
## Calculate answered percentage 回答率の計算 ----
mutate(該当する学生の回答率_percentage_num = round(
  回答数 / 回答数合計, digits = 2
)) 

# Pivot studentYear data ----
studentYear_pivot <- studentYear %>% 
pivot_wider(
  names_from = "date",
 # 学番左3桁
  #  names_sep = ".",
  values_from = c(
    "回答数",
    "回答数合計",                         
    "該当する学生の回答率_percentage_num"
  )
) %>%
arrange(学番左3桁)

## Remove sudentYear data ----
rm(studentYear)
rm(studentYearTotal)

# ---------- ---------- byFaculty ---------- ---------- ----

# StudentYearTotal data ----
byFaculty <- data %>%
  select(
    date,
    学科_faculty_name,
    回答した科目数_answered_tally
  ) %>%
  ## Excluding NA entries
  filter(!is.na(回答した科目数_answered_tally)) %>%
  group_by(date, 学科_faculty_name) %>%
  summarize(
    回答した科目数_answered_tally = n(),
    .groups = "keep"
  ) %>%
  ## Rename columns ----
rename(
  # Changed to = changed from
  "学科" = "学科_faculty_name",
  "回答数" = "回答した科目数_answered_tally"
) %>%
## Arrange from higher ratio データを並び替える ----
arrange(date,
        factor(学科, levels = c(
  '機械工学科',
  '電気電子工学科',
  '情報工学科',
  'コンピュータ応用学科',
  '総合デザイン学科',
  '人間環境学科',
  '情報学科'
  ))
)

# byFacultyTotal data ----
byFacultyTotal <- data %>%
  select(
    date,
    学科_faculty_name,
    回答した科目数_answered_tally
  ) %>%
  group_by(date, 学科_faculty_name) %>%
  summarize(
    回答した科目数_answered_tally = n(),
    .groups = "keep"
  ) %>%
  ungroup() %>%
  ## Rename columns ----
rename(
  # Changed to = changed from
  "学科" = "学科_faculty_name",
  "回答数合計" = "回答した科目数_answered_tally"
)


## Joining byFaculty data and total data データをつなげる ----
byFaculty <- byFaculty %>%
  left_join(byFacultyTotal, by = c(
    "date", 
    "学科"
  )
  ) %>%
  ## Calculate answered percentage 回答率の計算 ----
mutate(該当する学生の回答率_percentage_num = round(
  回答数 / 回答数合計, digits = 2
)) 

# Pivot studentYear data ----
byFaculty_pivot <- byFaculty %>% 
  pivot_wider(
    names_from = "date",
    values_from = c(
      "回答数",
      "回答数合計",                         
      "該当する学生の回答率_percentage_num"
    )
  ) %>%
  ## Arrange from higher ratio データを並び替える ----
arrange(factor(学科, levels = c(
  '機械工学科',
  '電気電子工学科',
  '情報工学科',
  'コンピュータ応用学科',
  '総合デザイン学科',
  '人間環境学科',
  '情報学科'
  ))
)

## Remove sudentYear data ----
rm(byFaculty)
rm(byFacultyTotal)

# ---------- ---------- Output ---------- ---------- ----

# Output final result as an Excel file ----
write_xlsx(
path = "progressReport.xlsx",
# path = "classsurvey/report.xlsx",
col_names = TRUE, 
format_headers = TRUE,
list(
  '1 全体の回答率 Rows' = rowCount,
  '2 全体の回答率グループ Group' = groupRatio_pivot,
  '3 学生別のリスト Student List' = student_pivot,
  '4 授業別のリスト Class List' = class_pivot,
  '5 学年と学科別 by Year and Faculty' = yearFaculty_pivot
#  '6 回答数で学年ごと by Year' = studentYear_pivot,
#  '7 回答数で学科ごと by Faculty' = byFaculty_pivot,
#  '8 元データ Data' = data
  )
)

# End of this R program
