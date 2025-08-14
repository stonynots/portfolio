## Data Sources (Campus Square & CSV) ----
# Require 5 CSV data files to generate results
# キャンパススクエアから取り込んだファイルを使用
#  1. 学籍情報           (定形保存 23 番)
#  2. 学生出欠情報       (定形保存 13 番)
#  3. 履修情報           (定形保存 11 番)
#  4. 設定済指導教員情報 (定形保存 10 番)
#  5. 時間割情報         (定形保存 12 番)

## Joining data ----
# Data flow for joining data (Left-join)
# コード内で左外部結合 (Left-join) をおこなった順番
#  出席 ⇐ 学籍     (在籍のみを抽出)
#  出席 ⇐ 週の設定 (1 から 16 までを設定)
#  出席 ⇐ 履修     (15 Keyword 用のカウント数)
#  出席 ⇐ 指導教員 (CC や CCE などの割り当て)

# An Excel file output Contain 9 sheet tabs
# Excel ファイルに 8 つのシートタブを出力
#  1. 1 Faculty Count 出席不足者数
#  2. 2 Faculty % 出席率
#  3. 3 Weekly Total % 全体出席率
#  4. 4 Faculty Total % 合計出席率
#  5. 5 Under 50 List 出席不足者のリスト
#  6. 6 Attendance Count 出席不足者数
#  7. Student Total Numbers 学生数
#  8. Low Attendance 出席不足者の割合
#  9. 各科目の出席率とデータ入力状況の確認

# Reference: Uniform suffixes for column names
# https://www.sqlstyle.guide/#uniform-suffixes
#
# _id     = a unique identifier such as a primary key
# _status = flag value or other status
# _total  = the total or sum
# _num    = any kind of number
# _name   = signifies a name
# _seq    = sequence of values
# _date   = contains the date
# _tally  = a count
# _size   = the size of something
# _addr   = and address for the record
#
# (Custom and adjusted to Campus Square data)
# _code   = mentioned as code

# ────────────────────────────────────────────────── ----
# Load required packages 必要なパッケージの読み込み ----

## tidyverse ----
# tidyverse, a package collection include
# ggplot2, dplyr, tidyr, readr, purrr, tibble, stringr and forcats
# https://www.tidyverse.org/packages/
# tidyverse コレクションを使用
library(tidyverse)

## writexl ----
# writexl package for exporting result as Excel file
# https://docs.ropensci.org/writexl/index.html
# Excel に保存するために使用
library(writexl)

# Clear all data ----
rm(list = ls())

# DISABLED
# Set working directory under Windows terminal
# This needs to be modified if sunning from UNIX/Linux terminal
# setwd("C:/r")
setwd("C:/attendance")

# DISABLED
# Set current directory as working directory
# Require install.packages("rstudioapi")
# library(rstudioapi)
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# DISABLED
# Get current directory
# getwd()

# ────────────────────────────────────────────────── ----
# STUDENT 学籍データ ----

## Description 解説 ----
# データはキャンパススクエアの
# 学籍情報 (学籍サブシステム)
# の項目を全て取り込んだもの

## CSV 学籍 UTF-8 ----
student <- read_csv(file = "csv/23 学籍.csv",

# DISABLED
# Reference if saved in specific directory                    
# file = "data/学籍情報.csv",

# DISABLED
# Reading Shift-JIS text file
# locale = locale(encoding = "cp932"),

# UTF-8 で保存されたキャンパススクエアのデータの読み込み
locale = locale(encoding = "UTF-8"),

# Selecting columns to use
# 使用するフィールド (列) の指定
col_types = cols_only(
  学籍番号     = col_character(),  # Student identification
  所属         = col_character(),  # Student faculty
# Student name with standard characters
# 学生氏名とは別に依存文字を含まないフィールドを使用
  学生氏名WEB	 = col_character(), 
  学生氏名カナ = col_character(),  # Student name in kana
  学年         = col_integer(),    # Student year
  在籍区分     = col_character(),  # Student status
  現況区分     = col_character()   # Student registration status
 )
) %>% 
# %>% is piping and continuing
# %>% は引き続き実行するためのもの
  
## Rename columns フィールド名の変更 ----
# 学生氏名WEB field contains standard Japanese characters
rename(
# Changed to                           = changed from
"学籍番号_student_id"                  = "学籍番号",
"学生所属名_faculty_name"              = "所属",
"学生氏名カナ_student_kana_name"       = "学生氏名カナ",
"学生氏名_student_name"                = "学生氏名WEB",
"学年_student_year_num"                = "学年",
"在籍区分_student_registration_status" = "在籍区分",
"現況区分_student_current_status"      = "現況区分"
) %>%

## Arrange by student identification 学籍番号順に並び替え ----
arrange(学籍番号_student_id)

# ────────────────────────────────────────────────── ----
# ATTENDANCE 出席情報のデータ ----
# データはキャンパススクエアの学生出欠情報 (学籍サブシステム)

attendance <- read_csv(
file = "csv/13 出欠.csv",
# UTF-8 で保存されたキャンパススクエアのデータを読み込む

locale = locale(encoding = "UTF-8"),
  
# Select columns
# 使用する列の指定
  col_types = cols_only(
   学籍番号     = col_character(),        # Student identification
   出欠区分名   = col_character(),        # Attendance record
   時間割コード = col_character(),        # Class code
   授業日       = col_date(format = ""),  # Class date
   曜日         = col_character(),        # Class weekday
   時限         = col_integer(),          # Class periods
   授業回数     = col_integer()           # Class counts
  )
) %>%

rename(
# Changed to                      = changed from
 "学籍番号_student_id"             = "学籍番号",
 "出欠区分名_attendance_status"    = "出欠区分名",
 "時間割コード_class_code_id"      = "時間割コード",
 "授業日_class_date_id"            = "授業日",
 "曜日_class_weekday_status"       = "曜日",
 "時限_class_period_num"           = "時限",
 "授業回数_class_count_num"        = "授業回数"
)
  
# ────────────────────────────────────────────────── ----
# JOIN student and attendance 学生と出席の左外部結合 ----

# attendance ⇐ student
attendance <- left_join(attendance, student,
# Matched fields
# 共通するフィールド名
 by = c("学籍番号_student_id" = "学籍番号_student_id")
) %>%
  
## Keep only current students 在籍の学生のみを抽出 ----
filter(在籍区分_student_registration_status == '在籍')

# ────────────────────────────────────────────────── ----
# CLASS 履修データ ----

class <- read_csv(

# キャンパススクエアの履修情報 (学籍サブシステム)
# file = "data/Step 2 履修情報.csv",
 file = "csv/11 履修.csv",
 locale = locale(encoding = "UTF-8"),

# Select columns
# 利用するフィールドを選択
 col_types = cols_only(
  開講区分         = col_character(),  # Semester
  時間割所属コード = col_character(),  # Faculty code
  時間割所属       = col_character(),  # Faculty name
  時間割コード     = col_character(),  # Class code
  科目コード       = col_character(),  # Class subject code
  科目名           = col_character(),  # Class name 
# Class period, also used for counting keyword
# キーワード作成用に利用しているフィールド
  曜限             = col_character(), 
  教室             = col_character(),  # Class room
  単位数           = col_integer()     # Class units
 )
) %>% 

rename(
# Changed to                          = changed from
 "開講区分_semester_status"            = "開講区分",
 "時間割所属コード_class_faculty_code" = "時間割所属コード",
 "時間割所属_class_faculty_name"       = "時間割所属",
 "時間割コード_class_code_id"          = "時間割コード",
 "科目コード_class_subject_code"       = "科目コード",
 "科目名_class_subject_name"           = "科目名",
 "曜限_class_period_id"                = "曜限",
 "教室_class_room_status"              = "教室",
 "単位数_class_unit_num"               = "単位数"
) %>%
  
## Distinct class record 重複しない履修データに仕上げる ----
# Keep only unique information per each class
# 各クラスごとに選別
distinct(
 開講区分_semester_status,             # Available term
 時間割所属コード_class_faculty_code,  # Assigned faculty code
 時間割所属_class_faculty_name,        # Assigned faculty
 時間割コード_class_code_id,           # Class code
 科目コード_class_subject_code,        # Subject code
 科目名_class_subject_name,            # Subject 
 曜限_class_period_id,                 # Class period
 教室_class_room_status,               # Class room
 単位数_class_unit_num,                # Units
# Return only key values
# キー項目のみの戻り値を返す
 .keep_all = FALSE
) %>%

## Sort by class code 時間割コード順に並べる ----
arrange(時間割コード_class_code_id) %>%

## Count class length is required for calculating weeks キーワード用の計算 ----
# Count characters in 時限 field
# キーワードのカウントを行う
mutate( class_period_count = nchar(曜限_class_period_id))

# ────────────────────────────────────────────────── ----
# JOIN attendance and class 出席と履修の左外部結合 ----
# attendance ⇐ class
attendance <- left_join(attendance, class, 
# Matched fields
# 共通するフィールド
 by = c("時間割コード_class_code_id" = "時間割コード_class_code_id") 
) 

# ────────────────────────────────────────────────── ----
# TEACHER 指導教員のデータ ----
teacher <- read_csv(
 file = "csv/14 設定指導.csv",
 locale = locale(encoding = "UTF-8"),
 col_types = cols_only(
 入学年度           = col_integer(),    # Entered year
 学籍番号           = col_character(),  # Student identification 
 指導教員区分コード = col_character(),  # Assigned teacher type code 
 指導教員区分名     = col_character(),  # Assigned assigned type 
 指導教員コード     = col_character(),  # Teacher code
 指導教員名         = col_character()   # Teacher
 )
) %>%  

rename(
# Changed to                                       = changed from
 "入学年度_entry_year_num"                          = "入学年度",
 "学籍番号_student_id"                              = "学籍番号",
 "指導教員区分コード_assigned_teacher_type_code_id" = "指導教員区分コード",
 "指導教員区分名_assigned_teacher_type_name"        = "指導教員区分名",
 "指導教員コード_assigned_teacher_code"             = "指導教員コード",
 "指導教員名_assigned_teacher_name"                 = "指導教員名"
) %>%

## Select after year 2015 students 2015 年以降に入学した学生のみを選択 ----  
filter(入学年度_entry_year_num >= 2015) %>%

## Remove (exclude) a selected column 特定のフィールドを取り除く
select(-c(入学年度_entry_year_num)) %>%
  
## Sort by identification 学籍番号順に並べる ----
arrange(学籍番号_student_id)

# ────────────────────────────────────────────────── ----
# FIND MOST RECENT ASSIGNED TEACHER 最新の指導教員を求める ----

# Find and set unique record for 1 assigned teacher per 1 student
# 1 人の学生につき 1 人の教員のデータを準備する

teacher <- teacher %>%
## Group by identification 学籍番号のでまとめる ----
  
group_by(学籍番号_student_id) %>%
## Select only highest code number 指導教員区分コードが一番高いものを残す ----

# Select only 1 from some duplicate identifications 
# 重複している学籍番号から 1 つのみに絞る
slice(which.max(指導教員区分コード_assigned_teacher_type_code_id)) %>%
  
## Remove selected columns 特定のフィールドを取り除く----
select(
 -c(
  指導教員区分コード_assigned_teacher_type_code_id, 
  指導教員コード_assigned_teacher_code
 )
)

# ────────────────────────────────────────────────── ----
# JOIN teacher to attendance 出席と指導教員の左外部結合----

# attendance ⇐ teacher 
attendance <- left_join(attendance, teacher,
# Matched field
# 共通するフィールド
 by = "学籍番号_student_id"
) 

# ────────────────────────────────────────────────── ----
# CONDITIONING ATTENDANCE TYPES 出席状況の判定をおこなう ----

attendance <- attendance %>%
# Add a column for result
# 新しいフィールドに判定結果を表示
mutate(attendanceType_status = case_when(
# field name == original data ' then ~ ' output result,
 出欠区分名_attendance_status == "出席" ~ "出席",    # If attended
 出欠区分名_attendance_status == "欠席" ~ "欠席",    # If did not attend
 出欠区分名_attendance_status == "公欠" ~ "その他",  # If other
 出欠区分名_attendance_status == "遅刻" ~ "その他",  # If other
 出欠区分名_attendance_status == "補習" ~ "その他",  # If other
 TRUE ~ NA_character_  # Set to NA character for anything else
 )
) 

# ────────────────────────────────────────────────── ----
# COUNT 1 FOR EACH ENTRY 全てのエントリーを 1 としてカウント ----

# Set for count as 1 for all entries
attendance <- attendance %>%
  
mutate(entryCount_num = case_when(
 出欠区分名_attendance_status == "出席" ~ 1,  # Attended is 1
 出欠区分名_attendance_status == "公欠" ~ 1,  # Did not attended if reason is 1
 出欠区分名_attendance_status == "遅刻" ~ 1,  # Late attendance is 1
 出欠区分名_attendance_status == "補習" ~ 1,  # Covered class is 1
 出欠区分名_attendance_status == "欠席" ~ 1,  # Covered class is 1
 TRUE ~ 1                   # Include empty text as 1
 )
) 

# ────────────────────────────────────────────────── ----
# REQUIRED KEWORD FOR CALCULATION 計算に必要なキーワードの作成 ----

# Combining 2 columns with "-" character
# 2 つのフィールドを "-" でつなげる

# 2, 4, 8 are character count of 曜限 field (From 履修情報 data)
# 2 または 4 または 8 がカウント数の種類

attendance <- attendance %>%
mutate(class_keyword_id = paste(!!!rlang::syms(c(
 "class_period_count", 
 "授業回数_class_count_num"
)), sep = "-"))

# ────────────────────────────────────────────────── ----
# Select weeks to include for calculation 設定した週のみを選別する----

# This rule was implemented from year 2022 last semester (September 2022)
# この定義は 2022 年度後期から適応

# Classes are scheduled in 1 semester for
# 16 weeks or 
# 32 weeks or
# 64 weeks 

# 1 学期につき
# 16 または
# 32 または 
# 64 回のクラス回数がデータに存在する

# Select for 16 weeks per semester
  
# This is standard case as picking 1 period from week 1 to 16
# 基本的に 1 学期に 16 回ある場合
  
attendance <- attendance %>%
mutate(week_num = case_when(
 class_keyword_id == "2-1" ~ 1,  # Starting from Week 1
 class_keyword_id == "2-2" ~ 2,
 class_keyword_id == "2-3" ~ 3, 
 class_keyword_id == "2-4" ~ 4,
 class_keyword_id == "2-5" ~ 5,
 class_keyword_id == "2-6" ~ 6,
 class_keyword_id == "2-7" ~ 7,
 class_keyword_id == "2-8" ~ 8,
 class_keyword_id == "2-9" ~ 9,
 class_keyword_id == "2-10" ~ 10,
 class_keyword_id == "2-11" ~ 11,
 class_keyword_id == "2-12" ~ 12,
 class_keyword_id == "2-13" ~ 13,
 class_keyword_id == "2-14" ~ 14,
 class_keyword_id == "2-15" ~ 15,
 class_keyword_id == "2-16" ~ 16,  # Last is week 16
    
# Select 32 weeks per semester
# 1 学期に 32 回ある場合
    
# This is picking only 1 period from 2 periods per week
# 特定の回のみ計算に含んでいる
    
# For example, ○ Include, × Not to Include
# 例えば ○ は含み × は部いている回
    
# ○ 4-1 × 4-2, 
# ○ 4-3 × 4-4, 
# ○ 4-31 × 4-32

 class_keyword_id == "4-1" ~ 1,  # Starting from Week 1
 class_keyword_id == "4-3" ~ 2,
 class_keyword_id == "4-5" ~ 3,
 class_keyword_id == "4-7" ~ 4,
 class_keyword_id == "4-9" ~ 5,
 class_keyword_id == "4-11" ~ 6,
 class_keyword_id == "4-13" ~ 7,
 class_keyword_id == "4-15" ~ 8,
 class_keyword_id == "4-17" ~ 9,
 class_keyword_id == "4-19" ~ 10,
 class_keyword_id == "4-21" ~ 11,
 class_keyword_id == "4-23" ~ 12,
 class_keyword_id == "4-25" ~ 13,
 class_keyword_id == "4-27" ~ 14,
 class_keyword_id == "4-29" ~ 15,
 class_keyword_id == "4-31" ~ 16,  # Last is week 16

# Select for 64 weeks per semester
# 1 学期に 64 回ある場合
  
# This is picking only 1 period from 4 periods per week
# こちらも特定の回のみ計算に含んでいる

# For example, ○ Include, × Not to Include
# 例えば ○ は含み × は部いている回

# ○ 8-1 × 8-2 × 8-3 × 8-4, 
# ○ 8-61 × 8-62 × 8-63 × 8-64

 class_keyword_id == "8-1" ~ 1,  # Starting from week 1
 class_keyword_id == "8-5" ~ 2,
 class_keyword_id == "8-9" ~ 3,
 class_keyword_id == "8-13" ~ 4,
 class_keyword_id == "8-17" ~ 5,
 class_keyword_id == "8-21" ~ 6,
 class_keyword_id == "8-25" ~ 7,
 class_keyword_id == "8-29" ~ 8,
 class_keyword_id == "8-33" ~ 9,
 class_keyword_id == "8-37" ~ 10,
 class_keyword_id == "8-41" ~ 11,
 class_keyword_id == "8-45" ~ 12,
 class_keyword_id == "8-49" ~ 13,
 class_keyword_id == "8-53" ~ 14,
 class_keyword_id == "8-57" ~ 15,
 class_keyword_id == "8-61" ~ 16,  # Last is week 16
 TRUE ~ NA_real_
)) %>%
  
## Excluding NA entries
filter(!is.na(week_num))


# ────────────────────────────────────────────────── ----
# PIVOT TO ROW BASED DATA データの並び替え ----

pivot <- attendance %>%
## Grouping with selected columns グループ別にまとめる ----
group_by(
 学生所属名_faculty_name, 
 学年_student_year_num, 
 学籍番号_student_id, 
 学生氏名_student_name, 
 指導教員区分名_assigned_teacher_type_name, 
 指導教員名_assigned_teacher_name, 
 week_num, 
 attendanceType_status
) %>%

## Count every entries 全てをカウントする ----

summarize(
 entryCount_num = n(), 
# Keep its group
# グループ化を維持する
 .groups = "keep"
) %>% 

## WIDEN based on attendanceType 並び替える ----
pivot_wider(
# Switch attendanceType data to columns
# attendanceType のデータを横に並べる
 names_from = attendanceType_status, 
 values_from = entryCount_num
) %>% 

## SORT by identification 学籍番号で並べる ----
arrange(学籍番号_student_id, week_num) %>%
  
## ADD total column 講義日数の合計を追加する ----
mutate(講義日数_class_day_total_num = sum(
 出席, 
 その他, 
 欠席, 
# Ignore if NA
# NA であれば無視する
 na.rm = TRUE
)) %>%

## ADD attended total column 出席日数の合計を追加する ----
mutate(出席日数_attended_day_total_num = sum(
 出席, 
 その他, 
# Igrnore if NA
# NA であれば無視する
 na.rm = TRUE
)) %>%

## ADD percentage for attended 講義日数の合計を追加する ----
mutate(出席率_percentage_num =
# If condition for correcting when total is 0 day
# もしも合計が 0 であった場合の処理
 ifelse(講義日数_class_day_total_num == 0, 0, 
  round(
   出席日数_attended_day_total_num / 講義日数_class_day_total_num, 
   digits = 3
  )
 )
) %>%

## ADD flag if percentage is under 50% もしも 50% 未満の場合 ----
mutate(underFifty_status = 
 case_when(
# Flag as 1 if under 50%
# 50% 未満であれば 1 とする
  出席率_percentage_num < 0.5 ~ 1, 
# Flag as NA if not under 50%
# それ以外は NA とする
  TRUE ~ NA_real_
 )
) %>%
  
## Renaming columns フィールド名の変更 ----
rename(
# Rename to = from
# 変更後と変更前
 "学科_faculty_name"     = "学生所属名_faculty_name",
 "担当区分_type_name"    = "指導教員区分名_assigned_teacher_type_name",
 "担当教員_teacher_name" = "指導教員名_assigned_teacher_name",
 "週_week_num"           = "week_num",
 "出席_attended_tally"   = "出席",
 "欠席_missed_tally"     = "欠席",
 "その他_other_tally"    = "その他",
 "未記入_NA_tally"       = "NA",
) %>%

## Rearrange order of columns and rows フィールド名の並び変え----
# From left to right columns
# 左から右への並び
relocate(any_of(c(
 "学科_faculty_name",
 "学籍番号_student_id",
 "学生氏名_student_name",
 "学年_student_year_num",
 "担当区分_type_name",
 "担当教員_teacher_name",
 "週_week_num",
 "出席_attended_tally",
 "その他_other_tally",
 "欠席_missed_tally",
 "未記入_NA_tally",
 "講義日数_class_day_total_num",
 "出席日数_attended_day_total_num",
 "出席率_percentage_num",
 "underFifty_status"
))) %>%
  
## Set sort order for faculty 学科の並び替えを設定する ----
# From top to bottom
# 上から下への並び
arrange(factor(学科_faculty_name, levels = c(
 '工学部機械工学科',
 '工学部電気電子工学科',
 '工学部情報工学科',
 '工学部コンピュータ応用学科',
 '工学部総合デザイン学科',
 '工学部人間環境学科'
 )),
 学籍番号_student_id
)

# ────────────────────────────────────────────────── ----
# UNDER 50% STUDENT LIST 出席率 50% 未満の学生のリスト ----

# Use underFifty field to select data
# underFifty を利用して選択
underFifty <- filter(pivot, underFifty_status == 1) %>%
  
## Order by selected faculty order 学科の並び替えを指定 ----
arrange(factor(学科_faculty_name, levels = c(
 '工学部機械工学科',
 '工学部電気電子工学科',
 '工学部情報工学科',
 '工学部コンピュータ応用学科',
 '工学部総合デザイン学科',
 '工学部人間環境学科'
 )), 
 学籍番号_student_id
)

# ────────────────────────────────────────────────── ----
# FACULTY LIST 学科別のリスト ----

# Keep only < 50% counts
# 50% 未満のみを選択
faculty <- filter(pivot, underFifty_status == 1) %>%
  
## Group by selected columns グループにまとめる ----
group_by(学科_faculty_name, 学年_student_year_num, 週_week_num) %>%
summarize(underFifty_status = n(), .groups = "keep") %>%
  
## Sort result その結果を並べる ----
arrange(学科_faculty_name, 学年_student_year_num, 週_week_num) %>%
  
## Arrange by faculty order 学科別に並べる ----

arrange(factor(学科_faculty_name, levels = c(
 '工学部機械工学科',
 '工学部電気電子工学科',
 '工学部情報工学科',
 '工学部コンピュータ応用学科',
 '工学部総合デザイン学科',
 '工学部人間環境学科'
 ))
) %>%
  
## Final result for faculty 結果を出す ----
pivot_wider(
# Extend it to right with each week numbers
# 各週を横並びにする
 names_from = 週_week_num, 
# Display numbers from underFifty
# underFifty の結果を表示する
 values_from = underFifty_status,
# Fill in NA with 0
# もしも NA であれば 0 にする
 values_fill = 0
) %>%

relocate(any_of(c(
 "学科_faculty_name",
 "学年_student_year_num",
 "1", "2", "3", "4", 
 "5", "6", "7", "8",
 "9", "10", "11", "12", 
 "13", "14", "15", "16"
)))

# ────────────────────────────────────────────────── ----
# FACULTY PERCENTAGES 学科別の割合 ----

facultyPercentage <- pivot %>%

## Group by selected fields グループ化する ----
group_by(
 学科_faculty_name, 
 学年_student_year_num, 
 週_week_num
) %>%

summarize(
 出席率_percentage_num = round(
 mean(出席率_percentage_num), digits = 3
 ), .groups = "keep"
) %>%
  
## Sort by selected fields 並び替える ----
arrange(学科_faculty_name, 学年_student_year_num, 週_week_num) %>%

## Arrange and custom sort by faculty order
# 学科別に並べ順を設定する
arrange(factor(学科_faculty_name, levels = c(
 '工学部機械工学科', 
 '工学部電気電子工学科', 
 '工学部情報工学科', 
 '工学部コンピュータ応用学科', 
 '工学部総合デザイン学科',
 '工学部人間環境学科'
 ))
) %>%

## Widen the result for faculty percentage 結果を並び替える ----
pivot_wider(
 names_from = 週_week_num, 
 values_from = 出席率_percentage_num
)

# ────────────────────────────────────────────────── ----
# CALCULATE TOTAL NUMBER OF STUDENTS 学生の合計数を計算する ----

## Removing week column 特定のフィールドを省く ----
studentTotal <- attendance[,!names(attendance) %in% c("week_num")] %>%

## Calculate student total per faculty 学科別に計算する ----
distinct(
 学籍番号_student_id, 
 学生所属名_faculty_name, 
 学年_student_year_num, 
 指導教員区分名_assigned_teacher_type_name, 
 entryCount_num
) %>%

## Group and arrange グループにする ----
group_by(学生所属名_faculty_name, 学年_student_year_num) %>%
summarize(entryCount_num = n(), .groups = "keep") %>%

## Set sort order for faculty 学科別の並び順を設定する ----
arrange(
 factor(学生所属名_faculty_name, 
  levels = c(
   '工学部機械工学科', 
   '工学部電気電子工学科', 
   '工学部情報工学科', 
   '工学部コンピュータ応用学科', 
   '工学部総合デザイン学科',
   '工学部人間環境学科'
   )
  ),
 学年_student_year_num
)

# ────────────────────────────────────────────────── ----
# DATA UNDER FIFTY PERCENTAGE 50% 未満の割合データ ----

underFiftyPercentage <- underFifty %>%

group_by(
 学科_faculty_name, 
 学年_student_year_num, 
 週_week_num
) %>%

summarize(underFifty_status = n(), .groups = "keep") %>%

rename(
# Rename to = from
# 変更後と変更前
 "underFifty_status_total_num" = "underFifty_status"
) %>%
  
arrange(
 学科_faculty_name, 
 学年_student_year_num, 
 週_week_num
) %>% 

arrange(
 factor(学科_faculty_name, 
  levels = c(
   '工学部機械工学科', 
   '工学部電気電子工学科', 
   '工学部情報工学科', 
   '工学部コンピュータ応用学科', 
   '工学部総合デザイン学科',
   '工学部人間環境学科'
  )
 )
)

# ────────────────────────────────────────────────── ----
# JOIN student total 学籍と 50% 未満の情報を左外部結合----

underFiftyPercentage <- left_join(underFiftyPercentage, studentTotal, 
 by = c(
  "学科_faculty_name" = "学生所属名_faculty_name", 
  "学年_student_year_num" = "学年_student_year_num"
 )
) %>%
  
## Add percentage column 割合のフィールドを追加 ----
mutate(出席不足者率_missed_percentage_num = 
 round(
  underFifty_status_total_num / entryCount_num, digits = 3
 )
) %>%
  
## Rename columns フィールド名を変更 ----
rename(
 "学科_faculty_name"      = "学科_faculty_name",
 "学年_student_year_num"  = "学年_student_year_num",
 "週_week_num"            = "週_week_num",
 "50% 未満の人数_underFifty_total_num"
                          = "underFifty_status_total_num",
 "学科学年別の合計人数_faculty_student_total_num" 
                          = "entryCount_num",
 "出席不足者の割合_missed_percentage_num" 
                          = "出席不足者率_missed_percentage_num"
) %>% 
  
## Remove columns 特定のフィールドを外す----
select(
 -c(
  "50% 未満の人数_underFifty_total_num", 
  学科学年別の合計人数_faculty_student_total_num
 )
) %>%

## Widen the result 結果を横並びにする ----
# values_fill for NA
pivot_wider(
 names_from  = 週_week_num, 
 values_from = 出席不足者の割合_missed_percentage_num, 
 values_fill = 0
) %>%

relocate(any_of(c(
 "学科_faculty_name",
 "学年_student_year_num",
 "1", "2", "3", "4", 
 "5", "6", "7", "8",
 "9", "10", "11", "12", 
 "13", "14", "15", "16"
)))

# FACULTY TOTAL PERCENTAGE 学科別合計の割合 ----
# Group by faculty
facultyTotalPercentage <- pivot %>%
 group_by(
  学科_faculty_name, 
  週_week_num
 ) %>%

summarize(
 出席率_percentage_num = 
  round(
   mean(出席率_percentage_num), digits = 3
  ), 
 .groups = "keep"
) %>%

# Arrange data
arrange(学科_faculty_name, 週_week_num) %>%

## Arrange and custom sort by faculty order 学科別に並べる----
arrange(factor(学科_faculty_name, levels = c(
 '工学部機械工学科', 
 '工学部電気電子工学科', 
 '工学部情報工学科', 
 '工学部コンピュータ応用学科', 
 '工学部総合デザイン学科',
 '工学部人間環境学科'
))) %>%

## Widen the result by week number 結果を各週ごとで横並びにする ----
pivot_wider(names_from  = 週_week_num, values_from = 出席率_percentage_num)

# ────────────────────────────────────────────────── ----
# TOTAL PERCENTAGE EACH WEEK 各週の合計を出す ----

# Group by week
# 週ごとにまとめる
weeklyTotalPercentage <- pivot %>%
 group_by(週_week_num) %>%

summarize(出席率_percentage_num = round(
 mean(出席率_percentage_num), digits = 3), .groups = "keep") %>%
  
# Arrange data
arrange(週_week_num) %>%

## Widen the result 横並びにする ----
pivot_wider(names_from = 週_week_num, values_from = 出席率_percentage_num)

# ────────────────────────────────────────────────── ----
# List for under 50% students

underFiftyResult <- pivot %>%

ungroup() %>%

select(
 学科_faculty_name, 
 学籍番号_student_id,
 学生氏名_student_name,
 学年_student_year_num,
 担当区分_type_name,
 担当教員_teacher_name,
 週_week_num, 
underFifty_status) %>%

filter(underFifty_status == 1) %>%

pivot_wider(
 names_from = 週_week_num, 
 values_from = underFifty_status) %>%

arrange(学籍番号_student_id) %>%

relocate(any_of(c(
 "学科_faculty_name",
 "学籍番号_student_id",
 "学生氏名_student_name",
 "学年_student_year_num",
 "担当区分_type_name",
 "担当教員_teacher_name",
 "1", "2", "3", "4", "5", "6", "7", "8",
 "9", "10", "11", "12", "13", "14", "15", "16"))) %>%

rename(any_of(c(
# Added any_of() function with c() to ignore if does not exit
 "w1" = "1", "w2" = "2", "w3" = "3", "w4" = "4",
 "w5" = "5", "w6" = "6", "w7" = "7", "w8" = "8",
 "w9" = "9", "w10" = "10", "w11" = "11", "w12" = "12",
 "w13" = "13", "w14" = "14", "w15" = "15", "w16" = "16"
))) %>%

arrange(
 factor(学科_faculty_name,
  levels = c(
   '工学部機械工学科', 
   '工学部電気電子工学科', 
   '工学部情報工学科', 
   '工学部コンピュータ応用学科', 
   '工学部総合デザイン学科',
   '工学部人間環境学科',
# Added for year 2023
   '情報学部情報学科'
 )),
 担当教員_teacher_name,
 学籍番号_student_id
)

# ────────────────────────────────────────────────── ----
## CSV 履修 UTF-8 ----
checkTeacherData <- read_csv(
file = "csv/11 履修.csv",
locale = locale(encoding = "UTF-8"),
col_types = cols_only(
# 主担当教員コード = col_character(), 
 主担当教員名      = col_character(),
 年度              = col_integer(), 
# 学期コード       = col_character(),
# 学期             = col_character(), 
# 開講区分コード   = col_character(),
# 開講区分         = col_character(), 
# 時間割所属コード = col_character(),
# 時間割所属       = col_character(), 
 時間割コード      = col_character(),
 開講科目名        = col_character(), 
 科目コード        = col_character(),
 科目名            = col_character(), 
# 科目名カナ       = col_character(),
 曜限              = col_character(), 
 教室              = col_character(),
 学籍番号          = col_character(), 
 単位数            = col_integer() 
))

registeredTotal <- checkTeacherData %>%

distinct(時間割コード, 学籍番号 ) %>%

group_by(時間割コード) %>%

summarize(学籍番号 = n(), .groups = "keep") %>%

rename("登録者数" = "学籍番号")

checkTeacher <- checkTeacherData %>%

select(-c(学籍番号)) %>%

distinct() %>%

arrange(主担当教員名) %>%

left_join(registeredTotal, by = c("時間割コード"))

rm(registeredTotal)

# ────────────────────────────────────────────────── ----
## CSV 時間割 UTF-8 ----
checkTeacherSchedule <- read_csv(
# file = "data/Step 3 時間割情報.csv",
 file = "csv/12 時間割.csv",
 locale = locale(encoding = "UTF-8"),
 col_types = cols_only(
  年度 = col_integer(),
  時間割所属コード = col_character(),
  時間割所属名称 = col_character(),
  時間割コード = col_character(),
# 開講区分コード,
  開講区分名 = col_character(),
# 学期区分コード,
  学期区分名 = col_character(),
# 科目コード,
# 要件科目名,
  開講科目名称 = col_character(),
# 開講科目名カナ,
# 開講科目名英字,
  単位数 = col_integer(),
# 対象学年＿１年,
# 対象学年＿２年,
# 対象学年＿３年,
# 対象学年＿４年,
# 主担当教員コード,
  主担当教員 = col_character(),
# 授業開始年月日,
# 授業終了年月日,
# 曜日重複可否フラグ,
# 曜日重複可否,
# 曜日,
  曜日名 = col_character(),
  時限 = col_character(),
# 開始時間,
# 終了時間,
# 担当教員所属コード,
# 担当教員所属名 = col_character(),
# 担当教員コード,
# 担当教員 = col_character(),
# 担当教員勤務区分コード,
# 担当教員勤務区分名 = col_character(),
# 施設コード,
  施設 = col_character(),
# 時間割設定状態コード,
  時間割設定状態 = col_character()
)) %>%

filter(str_detect(時間割所属名称, "^工学部")) %>%

distinct() %>%

left_join(checkTeacher, by = c("時間割コード", "単位数", "年度")) %>%

arrange(主担当教員, 開講区分名, 時間割コード)

## Initiate checkTeacherStudent ----
checkTeacherStudent <- attendance %>%

select(c(
 時間割コード_class_code_id,
# 授業日_class_date_id,
 学籍番号_student_id,
 出欠区分名_attendance_status,
# 授業回数_class_count_num,
# 現況区分_student_current_status,          
 week_num,
# class_period_count,
# attendanceType_status,
# entryCount_num,
# class_keyword_id    
)) %>%

arrange(学籍番号_student_id, week_num, 時間割コード_class_code_id) %>%

### Excluding NA entries ----
filter(!is.na(出欠区分名_attendance_status)) %>%

group_by(時間割コード_class_code_id, week_num) %>%

summarize(
 total = n(),
 .groups = "keep"
) %>%

pivot_wider(
 names_from = week_num, 
 values_from = total,
 values_fill = 0
) %>%

rename(
# Added any_of() function with c() to ignore is does not exist
 any_of(c(
# Changed to = changed from
  "w1_データ入力数" = "1",
  "w2_データ入力数" = "2",
  "w3_データ入力数" = "3",
  "w4_データ入力数" = "4",
  "w5_データ入力数" = "5",
  "w6_データ入力数" = "6",
  "w7_データ入力数" = "7",
  "w8_データ入力数" = "8",
  "w9_データ入力数" = "9",
  "w10_データ入力数" = "10",
  "w11_データ入力数" = "11",
  "w12_データ入力数" = "12",
  "w13_データ入力数" = "13",
  "w14_データ入力数" = "14",
  "w15_データ入力数" = "15",
  "w16_データ入力数" = "16"
))) %>%

relocate(any_of(c(
 "時間割コード_class_code_id",
 "w1_データ入力数",
 "w2_データ入力数",
 "w3_データ入力数",
 "w4_データ入力数",
 "w5_データ入力数",
 "w6_データ入力数",
 "w7_データ入力数",
 "w8_データ入力数",
 "w9_データ入力数",
 "w10_データ入力数",
 "w11_データ入力数",
 "w12_データ入力数",
 "w13_データ入力数",
 "w14_データ入力数",
 "w15_データ入力数",
 "w16_データ入力数"
)))

## Initiate checkTeacherSchedule ----
checkTeacherSchedule <- checkTeacherSchedule %>%

left_join(checkTeacherStudent, 
 by = c("時間割コード"="時間割コード_class_code_id"))

# Show attendance percentage by classes
checkClassAttendance <- attendance %>%
 select(c(
  時間割コード_class_code_id,
# 授業日_class_date_id,
  学籍番号_student_id,
  出欠区分名_attendance_status,
# 授業回数_class_count_num,
# 現況区分_student_current_status,          
  week_num,
# class_period_count,
# attendanceType_status,
# entryCount_num,
# class_keyword_id    
)) %>%

arrange(学籍番号_student_id, week_num, 時間割コード_class_code_id) %>%

### Excluding NA entries ----
filter(!is.na(出欠区分名_attendance_status)) %>%

group_by(時間割コード_class_code_id, 
 出欠区分名_attendance_status, week_num) %>%

summarize(
 total = n(),
 .groups = "keep"
) %>%

pivot_wider(
 names_from = 出欠区分名_attendance_status, 
 values_from = total,
 values_fill = 0
) %>%

## ADD total column 合計を追加する ----
mutate(total_num = sum(
 出席, 
 欠席,
 遅刻,
 補習,
 公欠,
# Ignore if NA
# NA であれば無視する
 na.rm = TRUE
)) %>%

mutate(attended_percentage =
 round(出席 / total_num, digits = 3)
) %>%

## Remove columns 特定のフィールドを外す----
 select(
  -c(
   出席, 
   欠席,
   遅刻,
   補習,
   公欠,
   total_num
  )
) %>%

pivot_wider(
 names_from = week_num, 
 values_from = attended_percentage,
 values_fill = 0
) %>%

rename(
# Added any_of() function with c() to ignore is does not exist
 any_of(c(
# Changed to = changed from
  "w1_出席率" = "1",
  "w2_出席率" = "2",
  "w3_出席率" = "3",
  "w4_出席率" = "4",
  "w5_出席率" = "5",
  "w6_出席率" = "6",
  "w7_出席率" = "7",
  "w8_出席率" = "8",
  "w9_出席率" = "9",
  "w10_出席率" = "10",
  "w11_出席率" = "11",
  "w12_出席率" = "12",
  "w13_出席率" = "13",
  "w14_出席率" = "14",
  "w15_出席率" = "15",
  "w16_出席率" = "16"
))) %>%

relocate(any_of(c(
 "時間割コード_class_code_id",
 "w1_出席率",
 "w2_出席率",
 "w3_出席率",
 "w4_出席率",
 "w5_出席率",
 "w6_出席率",
 "w7_出席率",
 "w8_出席率",
 "w9_出席率",
 "w10_出席率",
 "w11_出席率",
 "w12_出席率",
 "w13_出席率",
 "w14_出席率",
 "w15_出席率",
 "w16_出席率"
)))

checkTeacherSchedule <- checkTeacherSchedule %>%

left_join(checkClassAttendance, 
 by = c("時間割コード"="時間割コード_class_code_id"))

# Remove data ----
rm(checkTeacher)
rm(checkTeacherData)
rm(checkTeacherStudent)


# Additional columns ----

# 欠席 3 - 4 回の科目数
# 欠席 5 回以上の科目数

# missedClasses ----
missedClasses <- attendance %>%

filter(出欠区分名_attendance_status == '欠席') %>%

select(
 c(
  学籍番号_student_id,
  時間割所属コード_class_faculty_code,
  時間割コード_class_code_id,
  時間割所属_class_faculty_name,
  科目コード_class_subject_code,
  科目名_class_subject_name,
  曜限_class_period_id,
  entryCount_num,
  week_num
 )
)

# missedClasses2 ----
missedClasses2 <- missedClasses %>%

group_by(
  学籍番号_student_id,
  時間割所属コード_class_faculty_code,
  時間割コード_class_code_id,
  時間割所属_class_faculty_name,
  科目コード_class_subject_code,
  科目名_class_subject_name,
  曜限_class_period_id,
# week_num
) %>%

summarize(
 total = sum(entryCount_num),
 .groups = "keep"
) 

# %>%
#  pivot_wider(
#   names_from = week_num, 
#   values_from = total,
#   values_fill = 0
#  )

# missedClasses3 ----
missedClasses3 <- missedClasses2 %>%

filter(total == 3 | total == 4) %>% # Filter only 3 OR 4

arrange(学籍番号_student_id, 科目コード_class_subject_code)

# missedClasses4 ----
# Count result for 3 or 4
missedClasses4 <- missedClasses3 %>%

group_by(
 学籍番号_student_id,
) %>%

summarize(
 count = n(),
 .groups = "keep"
) %>%

arrange(学籍番号_student_id)

# Another 

# missedClasses5 ----
missedClasses5 <- missedClasses2 %>%

## Filter if more then 5 ----
filter(total >= 5) %>%

arrange(学籍番号_student_id, 科目コード_class_subject_code)

# missedClasses6 ----
## Count result for more then 5 ----
missedClasses6 <- missedClasses5 %>%

group_by(
 学籍番号_student_id,
) %>%

summarize(
 count = n(),
 .groups = "keep"
) %>%

arrange(学籍番号_student_id)

## Add a column ----
underFiftyResult <- underFiftyResult %>%

left_join(missedClasses4, by = "学籍番号_student_id") %>%

rename("欠席3～4回の科目数" = "count")

## Add another column ----
underFiftyResult <- underFiftyResult %>%

left_join(missedClasses6, by = "学籍番号_student_id") %>%

rename("欠席5回以上の科目数" = "count")

# Add 2 columns to "pivot" data ----
pivot <- pivot %>%

left_join(missedClasses4, by = "学籍番号_student_id") %>%

rename("欠席3～4回の科目数" = "count")

pivot <- pivot %>%

left_join(missedClasses6, by = "学籍番号_student_id") %>%

rename("欠席5回以上の科目数" = "count")

# Add 2 columns to "underFifty" data ----
underFifty <- underFifty %>%

left_join(missedClasses4, by = "学籍番号_student_id") %>%

rename("欠席3～4回の科目数" = "count")

underFifty <- underFifty %>%

left_join(missedClasses6, by = "学籍番号_student_id") %>%

rename("欠席5回以上の科目数" = "count")

# missedClasses7 ----
# Additional columns for by classes data set ----
## Count result for 3 or 4 ----
missedClasses7 <- missedClasses3 %>%

group_by(
 時間割コード_class_code_id,
) %>%

summarize(
 count = n(),
 .groups = "keep"
) %>%

arrange(時間割コード_class_code_id)

checkTeacherSchedule <- checkTeacherSchedule %>%

left_join(missedClasses7, by = 
 c("時間割コード" = "時間割コード_class_code_id")) %>%

rename("欠席3～4回の科目数" = "count")

# missedClasses8 ----
## Count result for more then 5 ----
missedClasses8 <- missedClasses5 %>%
 group_by(
  時間割コード_class_code_id,
) %>%

summarize(
 count = n(),
 .groups = "keep"
) %>%

arrange(時間割コード_class_code_id)

checkTeacherSchedule <- checkTeacherSchedule %>%

left_join(missedClasses8, by = 
 c("時間割コード" = "時間割コード_class_code_id")) %>%

rename("欠席5回以上の科目数" = "count")

# missedClasses9 ----
# Experiment for additional properties ----
missedClasses9 <- missedClasses2 %>%

## Filter Not NULL ----
filter(!is.na(total)) %>%

arrange(学籍番号_student_id) %>%

group_by(
 学籍番号_student_id
) %>%

summarize(
 count = n(),
 .groups = "keep"
) %>%

arrange(学籍番号_student_id) %>%

rename("集計値データ確認用 - 
 1 回でも欠席をした科目の合計が NOT NULL である回数" = "count")

pivot <- pivot %>%
  left_join(missedClasses9, by = "学籍番号_student_id")

# missedClasses10 ----
missedClasses10 <- missedClasses2 %>%

## Filter IS NULL ----
filter(is.na(total)) %>%
  
arrange(学籍番号_student_id) %>%

group_by(
 学籍番号_student_id
) %>%

summarize(
 count = n(),
 .groups = "keep"
) %>%

arrange(学籍番号_student_id) %>%

rename("集計値データ確認用 - 
 1 回でも欠席をした科目の合計が IS NULL である回数" = "count")

# missedClasses11 ----
# DISABLED
# pivot <- pivot %>%
# left_join(missedClasses10, by = "学籍番号_student_id")

missedClasses11 <- missedClasses2 %>%

## Filter IS NULL ----
filter(total==1) %>%
  
arrange(学籍番号_student_id) %>%

group_by(
 学籍番号_student_id
) %>%

summarize(
 count = n(),
 .groups = "keep"
) %>%

arrange(学籍番号_student_id) %>%

rename("集計値データ確認用 - 1 回欠席をした科目の合計" = "count")

pivot <- pivot %>%

left_join(missedClasses11, by = "学籍番号_student_id")

# missedClasses12 ----
missedClasses12 <- missedClasses2 %>%

## Filter IS NULL ----
filter(total==2) %>%
  
arrange(学籍番号_student_id) %>%

group_by(
 学籍番号_student_id
) %>%

summarize(
 count = n(),
 .groups = "keep"
) %>%

arrange(学籍番号_student_id) %>%

rename("集計値データ確認用 - 2 回欠席をした科目の合計" = "count")

pivot <- pivot %>%

left_join(missedClasses12, by = "学籍番号_student_id")

# missedClasses13 ----
missedClasses13 <- missedClasses2 %>%

filter(total==3) %>% # Filter IS NULL

arrange(学籍番号_student_id) %>%

group_by(
 学籍番号_student_id
) %>%

summarize(
 count = n(),
 .groups = "keep"
) %>%

arrange(学籍番号_student_id) %>%

rename("集計値データ確認用 - 3 回欠席をした科目の合計" = "count")

pivot <- pivot %>%

left_join(missedClasses13, by = "学籍番号_student_id")

# missedClasses14 ----
missedClasses14 <- missedClasses2 %>%
  
filter(total==4) %>% # Filter IS NULL

arrange(学籍番号_student_id) %>%

group_by(
 学籍番号_student_id
) %>%

summarize(
 count = n(),
 .groups = "keep"
) %>%

arrange(学籍番号_student_id) %>%

rename("集計値データ確認用 - 4 回欠席をした科目の合計" = "count")

pivot <- pivot %>%

left_join(missedClasses14, by = "学籍番号_student_id")

# missedClasses15 ----
missedClasses15 <- missedClasses2 %>%

filter(total==5) %>% # Filter IS NULL

arrange(学籍番号_student_id) %>%

group_by(
 学籍番号_student_id
) %>%

summarize(
 count = n(),
 .groups = "keep"
) %>%

arrange(学籍番号_student_id) %>%

rename("集計値データ確認用 - 5 回欠席をした科目の合計" = "count")

pivot <- pivot %>%

left_join(missedClasses15, by = "学籍番号_student_id")

# missedClasses16 ----
missedClasses16 <- missedClasses2 %>%

filter(total==6) %>% # Filter IS NULL

arrange(学籍番号_student_id) %>%

group_by(
 学籍番号_student_id
) %>%

summarize(
 count = n(),
 .groups = "keep"
) %>%

arrange(学籍番号_student_id) %>%

rename("集計値データ確認用 - 6 回欠席をした科目の合計" = "count")

pivot <- pivot %>%

left_join(missedClasses16, by = "学籍番号_student_id")

# missedClasses17 ----
missedClasses17 <- missedClasses2 %>%

filter(total==7) %>% # Filter IS NULL

arrange(学籍番号_student_id) %>%

group_by(
 学籍番号_student_id
) %>%

summarize(
 count = n(),
 .groups = "keep"
) %>%

arrange(学籍番号_student_id) %>%

rename("集計値データ確認用 - 7 回欠席をした科目の合計" = "count")

pivot <- pivot %>%

left_join(missedClasses17, by = "学籍番号_student_id")

# missedClasses18 ----
missedClasses18 <- missedClasses2 %>%

filter(total==8) %>% # Filter IS NULL

arrange(学籍番号_student_id) %>%

group_by(
 学籍番号_student_id
) %>%

summarize(
 count = n(),
 .groups = "keep"
) %>%

arrange(学籍番号_student_id) %>%

rename("集計値データ確認用 - 8 回欠席をした科目の合計" = "count")

pivot <- pivot %>%

left_join(missedClasses18, by = "学籍番号_student_id")

# ────────────────────────────────────────────────── ----
# EXPORT ALL TO 1 EXCEL FILE 結果を Excel ファイルに保存に保存する ----

write_xlsx(
 path           = "attendance.xlsx",
 col_names      = TRUE, 
 format_headers = TRUE,
 list(
  '1 Faculty Count 出席不足者数'         = faculty,
  '2 Faculty % 出席率'                   = facultyPercentage,
  '3 Weekly Total % 全体出席率'          = weeklyTotalPercentage,
  '4 Faculty Total % 合計出席率'         = facultyTotalPercentage,
  '5 Under 50 List 出席不足者のリスト'   = underFifty,
  '6 Under 50 Result まとめたリスト'     = underFiftyResult,
  '7 Attendance Count 出席不足者数'      = pivot,
  'Student Total Numbers 学生数'         = studentTotal,
  'Low Attendance 出席不足者の割合'      = underFiftyPercentage,
  '各科目の出席率とデータ入力状況の確認' = checkTeacherSchedule
 )
)

# Example1 ----
example1 <- read_csv(
 file = "csv/13 出欠.csv",
 locale = locale(encoding = "UTF-8"),
# Select columns
 col_types = cols_only(
  学籍番号     = col_character(),        # Student identification
  出欠区分名   = col_character(),        # Attendance record
  時間割コード = col_character(),        # Class code
  授業日       = col_date(format = ""),  # Class date
  曜日         = col_character(),        # Class weekday
  時限         = col_integer(),          # Class periods
  授業回数     = col_integer()           # Class counts
 )
) %>%

distinct(
 時間割コード,
 授業回数
) %>%
  
group_by(時間割コード) %>%
  
# Select only 1 from some duplicate identifications 
slice(which.max(授業回数)) %>%

# OR SAME RESULT
# filter(授業回数 == max(授業回数, na.rm=TRUE)) %>%
arrange(
 時間割コード,
 授業回数
)

# Delete source files

# file.remove("学籍_2 学籍 12A-22A (在籍や入試区分や学位) 2023-03-08 Shift_JIS.csv")
# rm(gakuseki_csv)

# file.remove("Step 4 学生出欠情報.csv")
# rm(attendance_csv)

# file.remove("Step 2 履修情報.csv")
# rm(registration_csv)

# file.remove("学籍_7  設定済指導教員 在籍中 (指導員の情報) 2023-02-08 Shift_JIS.csv")
# rm(assignedTeacher_csv)

# file.remove("教務_2 時間割 2015–2022 (学期ごとの科目と教員) 2023-02-08 Shift_JIS.csv")
# rm(classSchedule_csv)

gc()

# ────────────────────────────────────────────────── ----
# LAST CONFIRMATION MESSAGE 完了後に確認用メッセージの表示 ----

# Only available when running from RStudio application
# RStudio 上でのみ機能する

rstudioapi::showDialog("Confirmation message", "Complete")

# END OF THIS R CODE
