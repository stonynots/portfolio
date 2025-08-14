# 日本語 Shift_JIS の CSV ファイルを読み込むとき
  #  locale = locale(encoding = "cp932"),
  # 以前の CampusSquare システムは Shift_JIS でした
  # 他部署から受け取るファイルが、Shift_JIS の場合があります

# 日本語 Unicode UTF-8 の CSV ファイルを読み込むとき
  #  locale = locale(encoding = "UTF-8"),
  # 現行の CampusSquare が UTF-8 に変更されました

# %>% の利用
  # R のコードを続けて実行する場合の、パイプ機能です

## < List of CampusSquare Data > ---

# Student data 学籍情報
  ### 23 学籍.csv ----

# Attendance data 学生出欠情報
  ### 出席 e.g., 2012L.csv (2013F, 2013L, YYYYF, YYYYL, 2024F, 2024L) ----

# Class data 履修情報
  ### 21 履修.csv ----

# Applicants High School data 出身校
  ### 30 入学手続者出身校.csv ----

# GPA data 成績
  ### 24 GPA.csv ----

## 他部署からのデータ
# "csv/25卒_進路報告登録202503201034未確定.csv",
# "csv/OLD Shift_JIS 入試課 高校の偏差値データ high school rawdata 2021-10-28.csv",
# "csv/OLD Shift_JIS 数学プレイスメントテスト math placement test 2022-05-28.csv",

# < Load packages > ----

## Tidayverse collection
library(tidyverse)
# https://www.tidyverse.org/packages/

## Export as Excel file
library(writexl)

# Prepare environment ----

## Clear all environment data
rm(list = ls())

## Set working directory under Windows
setwd("C:/history")

# Student 学籍 ----

student <- read_csv(
  file = 
    #    "csv/23 学籍 12A-23AB - 学籍 - 在籍や入試区分や学位 2024-01-25 Shift_JIS.csv",
    #  locale    = locale(encoding = "cp932"),  # Shift_JIS
    "csv/23 学籍.csv",
  # locale = locale(encoding = "UTF-8"),
  
  col_types = cols_only(
    学籍番号 = col_character(),
    所属     = col_character(),
    学年     = col_integer(),
    在籍区分 = col_character(),
    現況区分 = col_character()
  )) %>%
  
    ## Renaming columns ----
rename(
  "学籍番号_student_id"                  = "学籍番号",
  "学生所属名_faculty_name"              = "所属",
  "学年_student_year_num"                = "学年",
  "在籍区分_student_registration_status" = "在籍区分",
  "現況区分_student_current_status"      = "現況区分")

# Attendance data 出席データ ----

# Define file names to import ----
csv_files <- c(

  # {YYYYF} First semester - F は前期のデータ
  # {YYYYL} Last semester - L は後期のデータ
  
  # 2012F の対象データは、キャンパススクエアに存在しない期間
  
  # Starting from 2012 last semester
  "2012L", 

  "2013F", 
  "2013L", 
  
  "2014F",
  # "2014L",
  
  "2015F", 
  "2015L",
  
  "2016F",
  "2016L",
  
  "2017F",
  # "2017L",
  
  # "2018F",
  # "2018L",
  
  "2019F",
  "2019L",
  
  # "2020F",
  "2020L",
  
  "2021F",
  # "2021L",
  
  # "2022F",
  "2022L",

  "2023F",
  "2023L",
  
  "2024F", 
  "2024L"
  
  ## For the future ----
  ## 今後の追加例 ( 最後は必ず [ , ] を取ってください ) ----
  
  # "2025F",
  # "2025L",
  
  # "2026F",
  # "2026L",
  
  # "2027F",
  # "2027L
  
  )

# HERE IS THE FUNCTION

# Import data with defined name [ csv_files ]

attendance <- read_csv(
  file      = str_c("csv/", csv_files, ".csv", sep = ""),

  col_types = cols_only(
    年度         = col_integer(),
    学期         = col_character(),
    時間割コード = col_character(),
    授業回数     = col_integer(),
    学籍番号     = col_character(),
    出欠区分名   = col_character() ))

## Rename columns ----

attendance <- attendance %>%
  rename(
    
    # Renamed to                    = renamed from
    
    "年度_year_id"                  = "年度",
    "学期_semester_id"              = "学期",
    "時間割コード_class_code_id"    = "時間割コード",
    "授業回数_class_count_num"      = "授業回数",
    "学籍番号_student_id"           = "学籍番号"
  ) %>%  
  
## Reorder columns ----

relocate(any_of(c(
  "年度_year_id",
  "学期_semester_id",
  "時間割コード_class_code_id",
  "授業回数_class_count_num",
  "学籍番号_student_id"
)))

# Join data ----

attendance <- left_join(attendance, student,
                  by = c("学籍番号_student_id" = "学籍番号_student_id")
) %>%  
  
## Filter data ----

filter(str_detect(学生所属名_faculty_name, "^工学部")
) %>%
    
## Add a column for first 3 letters of student ID

mutate(学番三桁_studentYearCategory_id =
         substr(学籍番号_student_id, start = 1, stop = 3)
) %>%
  
## Filter data ----

filter(
  !学番三桁_studentYearCategory_id %in% 
    c('08A', '09A', '10A', '11A')
) %>%
  
## Calculate current year value ----
mutate(yearMark_id = 
         str_c(
           (年度_year_id) + 1 
           - as.numeric(
             str_c('20', (substr(学籍番号_student_id, start = 1, stop = 2))))
           , "年目", sep = " "
         ))

# Assign category for attendance ----

attendance <- attendance %>%
  mutate(出欠区分名_attendance_status = case_when(
    出欠区分名 == "出席" ~ "出席",
    出欠区分名 == "欠席" ~ "欠席", 
    出欠区分名 == "公欠" ~ "その他",
    出欠区分名 == "遅刻" ~ "その他",
    出欠区分名 == "補習" ~ "その他",
    # Added these two cases below
    出欠区分名 == "病欠" ~ "その他",
    出欠区分名 == "忌引" ~ "その他",
    TRUE                 ~ NA_character_
  ))

# Class ----

class <- read_csv(
  
  file = "csv/21 履修.csv",
  
  col_types = cols_only(
    年度             = col_integer(), 
    学期             = col_character(), 
    開講区分         = col_character(),
    時間割所属コード = col_character(),
    時間割所属       = col_character(),
    時間割コード     = col_character(),
    科目コード       = col_character(),
    科目名           = col_character(),
    曜限             = col_character(),
    
    # 教室          = col_character(),
    
    単位数           = col_integer()
  )) %>%
  
  rename(
    "年度_year_id"                        = "年度",
    "学期_semester_id"                    = "学期",
    "開講区分_semester_status"            = "開講区分",
    #  "時間割所属コード_class_faculty_code" = "時間割所属コード",
    "時間割所属_class_faculty_name"       = "時間割所属",
    "時間割コード_class_code_id"          = "時間割コード",
    "科目コード_class_subject_code"       = "科目コード",
    "科目名_class_subject_name"           = "科目名",
    "曜限_class_period_id"                = "曜限",
    #  "教室_class_room_status"              = "教室",
    "単位数_class_unit_num"               = "単位数"
  )

# ) %>%
# mutate( class_period_count = nchar(曜限_class_period_id))

class <- class %>%
  distinct(
    年度_year_id,
    学期_semester_id,
    
    # 開講区分_semester_status,
    # 時間割所属コード_class_faculty_code,
    # 時間割所属_class_faculty_name,
    
    時間割コード_class_code_id,
    
    # 科目コード_class_subject_code,
    # 科目名_class_subject_name,
    # 曜限_class_period_id,
    #  教室_class_room_status,
    # 単位数_class_unit_num,
    
    .keep_all = TRUE
  ) %>%
  
mutate( class_period_count = nchar(曜限_class_period_id))

# Joining Class data ----

attendance <- left_join(attendance, class,
                  by = c(
                    "年度_year_id",
                    "学期_semester_id",
                    "時間割コード_class_code_id"
                  )
)

# Preparing a key field for attendance calculation ----

attendance <- attendance %>%
  mutate(class_keyword_id = str_c(
    class_period_count, 
    授業回数_class_count_num,
    sep = "-"))

# Defining each data for calculating weeks (VERY IMPORTANT) ----

attendance <- attendance %>%
  mutate(week_num = case_when(
    class_keyword_id == "2-1" ~ 1,  
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
    class_keyword_id == "2-16" ~ 16,  
    
    class_keyword_id == "4-1" ~ 1,
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
    class_keyword_id == "4-31" ~ 16,  
    
    class_keyword_id == "8-1" ~ 1,  
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
    class_keyword_id == "8-61" ~ 16,
    TRUE ~ NA_real_
  )) %>%
  
  filter(!is.na(week_num))

# Pivot ----

attendance_pivot <- attendance %>%

## Grouping with selected columns グループ別にまとめる ----

group_by(
  年度_year_id,
  学期_semester_id,
  在籍区分_student_registration_status,
  現況区分_student_current_status,
  学生所属名_faculty_name,
  学年_student_year_num, 
  学籍番号_student_id,
  学番三桁_studentYearCategory_id,
  week_num,
  出欠区分名_attendance_status,
  yearMark_id,
  開講区分_semester_status
) %>%
  
## Count every entries 全てをカウントする ----

summarize(
  entryCount_num = n(),
  
  # Keep its group
  # グループ化を維持する
  .groups = "keep"
) %>%
  
  pivot_wider(
    
    # Switch attendanceType data to columns
    # attendanceType のデータを横に並べる
    names_from = 出欠区分名_attendance_status, 
    values_from = entryCount_num
  ) %>%
  
  rename("入力なし" = "NA") %>%

  mutate(講義日数_class_day_total_num = sum(
    出席, 
    欠席,
    その他,
    
    # Ignore this field by request
    # 入力なし,
    # Ignore if NA
    # NA であれば無視する
    na.rm = TRUE
    
  )) %>%
  
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

## NO LONGER USED OR APPLIED  

## ADD flag if percentage is under 50% もしも 50% 未満の場合 ----

# mutate(underFifty_status = 
#  case_when(
#    # Flag as 1 if under 50%
#    # 50% 未満であれば 1 とする
#    出席率_percentage_num < 0.5 ~ 1, 
#    # Flas as NA if not under 50%
#    # それ以外は NA とする
#    TRUE ~ NA_real_
#  )
#) %>%

# Replaced with this function

mutate(underFifty_status = ifelse(
  出席率_percentage_num < 0.5, 1,

  # Else
  NA_real_
  
)) %>%
  
relocate(any_of(c(
  "学生所属名_faculty_name",
  "学番三桁_studentYearCategory_id",
  "学籍番号_student_id",
  "学年_student_year_num",
    "在籍区分_student_registration_status",
    "現況区分_student_current_status",
    "年度_year_id",
    "学期_semester_id",
    "week_num",
    "出席率_percentage_num",
    "underFifty_status",
    "出席",
    "欠席",
    "その他",
    "出席日数_attended_day_total_num",
    "講義日数_class_day_total_num",
    "入力なし"             
  ))) %>%
  
  arrange(factor(学生所属名_faculty_name, levels = c(
    '工学部機械工学科',
    '工学部電気電子工学科',
    '工学部情報工学科',
    '工学部コンピュータ応用学科',
    '工学部総合デザイン学科',
    '工学部人間環境学科',
    '工学研究科博士前期課程機械工学専攻',
    '工学研究科博士前期課程電気情報工学専攻'
  )), 
  
  学籍番号_student_id,
  年度_year_id,
  学期_semester_id,
  week_num
  
  )

# LIST ----

# Faculty of Engineering                      工学部

# Mechanical Engineering                      機械工学科
# Electrical and Electronic Engineering       電気電子工学科
# Information Science                         情報工学科
# Applied Computer Sciences                   コンピュータ応用学科
# Multidisciplinary Design Science            総合デザイン学科
# Materials and Human Environmental Sciences  人間環境学科

# 現況区分_student_current_status

# In Next section
# KEEP or EXCLUDE

# 卒業 
# 在学

# 休学
# 除籍                           
# 退学                           
# 転出

# FILTER from 1st until 4th years

attendance_pivot <- attendance_pivot %>%
  filter(
    yearMark_id == "1 年目" & 開講区分_semester_status == "前学期" | 
      yearMark_id == "1 年目" & 開講区分_semester_status == "後学期" |
      yearMark_id == "2 年目" & 開講区分_semester_status == "前学期" | 
      yearMark_id == "2 年目" & 開講区分_semester_status == "後学期" |
      yearMark_id == "3 年目" & 開講区分_semester_status == "前学期" | 
      yearMark_id == "3 年目" & 開講区分_semester_status == "後学期" |
      yearMark_id == "4 年目" & 開講区分_semester_status == "前学期" | 
      yearMark_id == "4 年目" & 開講区分_semester_status == "後学期" 
  )

# NO LONGER USED OR APPLIED ----

# CASE A Conditions ----

  # pivot_a <- pivot_until4th %>%
  # filter(!現況区分_student_current_status %in% c('休学', '除籍', '退学', '転出'))

# CASE B Conditions ----

  # pivot_b <- pivot_until4th %>%
  # filter(!現況区分_student_current_status %in% c('卒業', '在学'))

# ALL 6 faculties ----

# Garbage collection ----

gc(reset = TRUE)

# Export as Excel file ----

write_xlsx(

  path = "xlsx/attendance_pivot.xlsx",
  
  col_names = TRUE, 
  format_headers = TRUE,
  list(

    'Attendance Pivot' = attendance_pivot,

    'student Data' = student,
    
    'class Data' = class
      
  # DISABLED DUE TO Excel OVER LIMIT
  # 'Attendance - No filter' = attendance,

  # DISABLED - NOT IMPREMENTD AND NO LONGER USED
  # 'Pivot Data - Selected 卒業 在学' = pivot_a,
  
  # DISABLED - NOT IMPREMENTD AND NO LONGER USED
  # 'Pivot Data - Selected 休学 除籍 退学 転出' = pivot_b,
  
  )
)

# NO LONGER APPLIED OR USED

# BEFORE, IT USED TO convert "attendance_pivot" into parquet

# library(arrow) # For Parquet

# write_parquet(pivot, "parquet/pivot.parquet")

# THEN READ that parquet data on other R programs


# Until this section was related to weekly attendance data

# ここまでが、学生出欠データの集計に関わるコードでした

# Stating from this section as STEP 2 ----

# To gather students' entrance, GPA and other data

# ここからが入試から就職までの学生データの集計などのセクションです


# student のデータを再設定するため、ここでいったん消去します

rm(student)


# ▀▄▀▄▀▄▀▄▀▄ Student 学籍 ▀▄▀▄▀▄▀▄▀▄ ----

## Load CSV data ----

student <- read_csv(

    file = "csv/23 学籍.csv",  

  col_types = cols_only(
    学籍番号     = col_character(),
    学生氏名WEB  = col_character(),
    学生氏名カナ = col_character(),
    所属         = col_character(), 
    所属コード   = col_character(), 
    学年         = col_integer(), 
    在籍区分     = col_character(),
    現況区分     = col_character()
  )
) %>%  # This line is a [ Pipe ] and it is for to continue
    
# DISABLED

# Only current and undergrad students ----
# filter(所属コード <= 2000, 在籍区分 == '在籍') %>%

## Only current student ----

filter(所属コード <= 2000) %>%
  
## Remove column for department code ----

select(-c(所属コード) ) %>%
  
  ## Rearrange ----
arrange(factor(所属, levels = c(
  
  '工学部機械工学科', 
  '工学部電気電子工学科', 
  '工学部情報工学科', 
  '工学部コンピュータ応用学科', 
  '工学部総合デザイン学科',
  '工学部人間環境学科',
  '情報学部情報学科'
)),

在籍区分,

factor(現況区分, levels = c(
  
  '在学', 
  '休学', 
  '転出', 
  '卒業', 
  '退学', 
  '除籍', 
  '入学辞退', 
  '修了', 
  '満期退学'
  
)), 

学籍番号

) %>%
  
## Add a column ----

# Add column to show student year; e.g. 19A, 20A, 21A, 22A

mutate(yearCategory = substr(学籍番号, 1, 3)) %>%

# Rename columns to English from Japanese

    rename(
      
      # Renamed to         = rename from
    
      "identification"     = "学籍番号",
    "name"               = "学生氏名WEB",
    "nameKana"           = "学生氏名カナ",
    "studentYear"        = "学年",
    "faculty"            = "所属",
    "registrationStatus" = "在籍区分",
    "studentStatus"      = "現況区分"
    
  ) %>%
  
  # Reorder columns
  
  relocate(any_of(c(
    
    "registrationStatus", 
    "studentStatus", 
    "studentYear",
    "identification",
    "faculty",
    "name",
    "nameKana",
    "yearCategory"
    
  )))

# ▀▄▀▄▀▄▀▄▀▄ Entry 入試 ▀▄▀▄▀▄▀▄▀▄ ----

## Load CSV data ----

entryType <- read_csv(

    file = "csv/30 入学手続者出身校.csv",

    col_types = cols_only(
    入試試験年度       = col_integer(),
    学籍番号           = col_character(),
    入試試験区分名       = col_character(),
    入試入学区分           = col_character(),
    入試試験区分コード = col_character(),
    入試入学区分コード     = col_character(),
    入学所属コード     = col_character()
  )
) %>%
  
  ## Filter after year 2012 entry with undergrad students ----

filter(入試試験年度 >= 2012, 入学所属コード <= 2000 ) %>%
  
  ## Exclude these 2 columns ----

select(-c(入学所属コード, 入試試験年度)) %>%

## Rename column names ----

rename(

    "identification"    = "学籍番号",
  "entryTestType"     = "入試試験区分名",
  "entryType"         = "入試入学区分",
  "entryTestTypeCode" = "入試試験区分コード",
  "entryTypeCode"     = "入試入学区分コード"

  ) %>%
  
  ## Reorder columns ----

relocate(any_of(c(
  
  "identification", 
  "entryType", 
  "entryTestType",
  "entryTypeCode",
  "entryTestTypeCode"

  ))) %>%
  
  ## Arrange by student identification ----

arrange(identification)

# ▀▄▀▄▀▄▀▄▀▄ Join data ▀▄▀▄▀▄▀▄▀▄ ----

## Join data, student ⇐ mathScore ----

student <- left_join(student, entryType, by = "identification")

# ▀▄▀▄▀▄▀▄▀▄ High school ▀▄▀▄▀▄▀▄▀▄ ----

## Load CSV data ----

highschool <- read_csv(

  file = "csv/30 入学手続者出身校.csv",

  col_types          = cols_only(
    学籍番号         = col_character(),
    入学年度         = col_integer(),
    入学所属コード   = col_character(),
    出身校区分名       = col_character(),
    出身校県名       = col_character(),
    出身校コード     = col_character(),
    出身校名         = col_character(),
    出身校名カナ     = col_character(),
    出身校課程名称       = col_character(),
    出身校学科名       = col_character()

      )
) %>%
  
  ## filter after year 2012 with only undergrad students ----

filter(入学年度 >= 2012, 入学所属コード <= 2000 ) %>%
  
  ## Exclude these columns ----

select(-c(入学所属コード, 入学年度)) %>%
    
  ## Rename column names ----

rename(
  "identification"              = "学籍番号",
  "origineSchool"               = "出身校名",
  "origineSchoolKana"           = "出身校名カナ",
  "origineSchoolCode"           = "出身校コード",
  "origineSchoolType"           = "出身校区分名",
  "origineSchoolPrefecture"     = "出身校県名",
  "origineSchoolCategory"       = "出身校課程名称",
  "origineSchoolFaculty"        = "出身校学科名"
) %>%

  ## Reorder columns ----

relocate(any_of(c(
  
  "identification", 
  "origineSchool", 
  "origineSchoolKana",
  "origineSchoolCode",
  "origineSchoolType",
  "origineSchoolPrefecture",
  "origineSchoolCategory",
  "origineSchoolFaculty"
  
))) %>%
  
  ## Arrange by student identification ----

arrange(identification)


# ▀▄▀▄▀▄▀▄▀▄ Join data ▀▄▀▄▀▄▀▄▀▄ ----


## Join data, student ⇐ highschool ----

student <- left_join(student, highschool, by = "identification")


# ▀▄▀▄▀▄▀▄▀▄ Class status ▀▄▀▄▀▄▀▄▀▄ ----

# Additonal class status ----

## Load CSV ----

crossClass <- read_csv(

    file = "csv/21 履修.csv",

      col_types          = cols_only(
    年度             = col_integer(),
    学期             = col_character(),
    開講区分         = col_character(),
    時間割コード     = col_character(),
    開講科目名       = col_character(),
    科目コード       = col_character(),
    科目名           = col_character(),
    主担当教員コード = col_character(),
    主担当教員名     = col_character(),
    曜限             = col_character(),
    学籍番号         = col_character(),
    履修時学年             = col_character(),
    評価             = col_character(),
    評語コード       = col_character(),
    評語             = col_character(),
    評点             = col_character(),
    合否区分         = col_character(),
    単位数           = col_character()
    
  )
) %>%
  
  ## Filter only related to 学科横断 ----

# Specifing a keyword with [ ^ ] here

filter(str_detect(科目名, "^学科横断")) %>%
  
  distinct(
    科目名,
    学籍番号,
    評語,
    .keep_all = FALSE
    
  ) %>%
  
  
  ## Rename columns ----

rename(
  
  # Renamed to     = rename from
  
  "identification" = "学籍番号",
  "className"      = "科目名",
  "score"          = "評語"

  ) %>%
  
  ## Reorder columns ----

relocate(any_of(c(

    "identification", 
  "className", 
  "score"
  
))) %>%
  
  ## Arrange order ----

arrange(identification, className) %>%
  
  ## Widen the result ----

pivot_wider(names_from = className, values_from = score)

# ▀▄▀▄▀▄▀▄▀▄ Join data ▀▄▀▄▀▄▀▄▀▄ ----

## Join data, student ⇐ crossClass ----

student <- left_join(student, crossClass, by = "identification")

# ▀▄▀▄▀▄▀▄▀▄ GPA (Grade Point Average) data ▀▄▀▄▀▄▀▄▀▄ ----

## Load CSV data ----

gpa <- read_csv(

  file = "csv/24 GPA.csv",

    col_types = cols_only(
    入学年度               = col_integer(),
    学籍番号               = col_character(), 
    処理対象年度           = col_integer(), 
    処理対象学期区分       = col_character(), 
    処理対象学期区分コード = col_integer(),
    累積GPA        = col_double(),
    学期GPA        = col_double(), 
    累積関連GPA対象総履修単位数 
    = col_integer(), 
    総修得単位数           = col_integer()
    
  )
) %>%
  
  ## Filter from year 2012 ----

filter(入学年度 >= '2012', 処理対象年度 >= '2012') %>%
  
  ## Arrange data ----

arrange(入学年度, 学籍番号, 処理対象年度, 処理対象学期区分) %>%
  
  ## Calculate student year ----

mutate(studentYearMarked = 処理対象年度 + 1 - 入学年度) %>%
  
  ## From left to right columns ----

relocate(any_of(c(
  
  "入学年度",
  "学籍番号",
  "studentYear",
  "処理対象年度",
  "処理対象学期区分",
  "累積GPA",
  "学期GPA",
  "累積関連GPA対象総履修単位数",
  "総修得単位数"
  
))) %>%
  
  ## Rename columns ----

rename(
  
  # Renamed to         = renamed from
  "entranceYear"       = "入学年度",
  "identification"     = "学籍番号",
  "year"               = "処理対象年度",
  "semester"           = "処理対象学期区分",
  "semesterCode"       = "処理対象学期区分コード",
  "accumulateGpa"      = "累積GPA",
  "semesterGpa"        = "学期GPA",
  "accumulateGpaUnits" = "累積関連GPA対象総履修単位数",
  "units"              = "総修得単位数"
  
) %>%
  
  ## Add a column for keyword ----

mutate(period = paste(!!!rlang::syms(c(
  
  "studentYearMarked", 
  "semester"
  
)), 
sep="-"
)) %>%  
  
  ## Add a column for current year with semester ----

mutate(studentYearSemester =
         
         # Convert to number from character
         as.numeric(
           
           # Create keyword by combining
           paste(!!!rlang::syms(c(
             
             "studentYearMarked", 
             "semesterCode"
             
           )), 
           sep="."
           )
         )
)

# ▀▄▀▄▀▄▀▄▀▄ Filter period selections ▀▄▀▄▀▄▀▄▀▄ ----

## Filter selected periods ----

gpa <- gpa %>%
  
  filter(
    period == "1-前学期" | 
      period == "1-後学期" |
      period == "2-前学期" | 
      period == "2-後学期" |
      period == "3-前学期" | 
      period == "3-後学期" |
      period == "4-前学期" | 
      period == "4-後学期" |
      period == "5-前学期" | 
      period == "5-後学期"
  )

# ▀▄▀▄▀▄▀▄▀▄ Trim student ID to show only year ▀▄▀▄▀▄▀▄▀▄ ----

## Using substr ----

gpa <- gpa %>%

mutate(yearCategory = substr(identification, 1, 3))

# ▀▄▀▄▀▄▀▄▀▄ Filter out grad students ▀▄▀▄▀▄▀▄▀▄ ----

## Filter data ----

gpa <- gpa %>%
  
  filter(!yearCategory %in% c(
    '21T', '22T', '23T', '24T', '25T', '26T', '27T'
    )) %>%
  
  select(-c(yearCategory)) 

# ▀▄▀▄▀▄▀▄▀▄ Join data ▀▄▀▄▀▄▀▄▀▄ ----

## Join data, student ⇐ gpa ----

studentGpa <- left_join(gpa, student, by = "identification") %>%
  
  ## Rename columns to English from Japanese ----

rename(
  
  "在籍区分"           = "registrationStatus",
  "現況区分"           = "studentStatus",
  
  "学年"               = "studentYear",
  "学籍番号"           = "identification",
  "所属"               = "faculty",
  
  "学生氏名WEB"        = "name",
  "学生氏名カナ"       = "nameKana",
  
  "学年別"             = "yearCategory",
  "入学区分"           = "entryType",
  "入試試験区分"       = "entryTestType",
  "入学区分コード"     = "entryTypeCode",
  "入試試験区分コード" = "entryTestTypeCode",
  
  "出身校名"           = "origineSchool",
  "出身校名カナ"       = "origineSchoolKana",
  "出身校コード"       = "origineSchoolCode",
  "出身校区分"         = "origineSchoolType",
  "出身校県名"         = "origineSchoolPrefecture",
  "出身校課程"         = "origineSchoolCategory",
  "出身校学科"         = "origineSchoolFaculty",
  
  "学科横断2A"         = "学科横断プログラム２Ａ",
  "学科横断2B"         = "学科横断プログラム２Ｂ",
  "学科横断3A"         = "学科横断プログラム３Ａ",
  "学科横断3B"         = "学科横断プログラム３Ｂ",
  
  "入学年度"           = "entranceYear",
  "処理対象年度"       = "year",
  "処理対象学期区分"   = "semester",
  
  "累積GPA"    = "accumulateGpa",
  "学期関連学期GPA"    = "semesterGpa",
  "累積GPA対象単位数"  = "accumulateGpaUnits",
  
  "総修得単位数"       = "units",
  "数え年"             = "studentYearMarked",
  "数え年学期"         = "period"

  )

# ▀▄▀▄▀▄▀▄▀▄ Organize ▀▄▀▄▀▄▀▄▀▄ ----

# Organize by semester GPA ----

studentGpaSemester <- studentGpa %>%

  ## Remove NA from period column ----
filter(!is.na(数え年学期)) %>%
  
  ## Remove selected columns ----

select(-c(
  
  累積GPA, 
  累積GPA対象単位数, 
  総修得単位数, 
  数え年, 
  処理対象年度, 
  処理対象学期区分,
  semesterCode,
  studentYearSemester

  )) %>%
 
  ## Widen the result ----
pivot_wider(names_from = 数え年学期, values_from = 学期関連学期GPA)

## Organize by accumulate GPA ----

studentGpaAccumulate <- studentGpa %>%
  
  ## Remove NA from period column ----

filter(!is.na(数え年学期)) %>%
    
  ## Remove selected columns ----

select(-c(
  
  学期関連学期GPA, 
  累積GPA対象単位数, 
  総修得単位数, 
  数え年, 
  処理対象年度, 
  処理対象学期区分,
  semesterCode,
  studentYearSemester
  
)) %>%
  
  ## Widen the result ----

pivot_wider(names_from = 数え年学期, values_from = 累積GPA)

## Organize by units ----

studentUnits <- studentGpa %>%

  ## Remove NA from period column ----

filter(!is.na(数え年学期)) %>%
    
  ## Remove selected columns ----

select(-c(
  
  学期関連学期GPA, 
  累積GPA対象単位数, 
  累積GPA, 
  数え年, 
  処理対象年度, 
  処理対象学期区分,
  semesterCode,
  studentYearSemester
  
)) %>%
    
  # Widen the result ----

pivot_wider(names_from = 数え年学期, values_from = 総修得単位数)

# ▀▄▀▄▀▄▀▄▀▄ Determine which data to use ▀▄▀▄▀▄▀▄▀▄ ----

## Display most recent record for each student ----

recentRecord <- gpa %>%
  
  group_by(identification) %>%

    slice(which.max(studentYearSemester))

# ▀▄▀▄▀▄▀▄▀▄ Join data ▀▄▀▄▀▄▀▄▀▄ ----

## Join data, student ⇐ mathScore ----

recentRecord <- left_join(student, recentRecord, by = "identification")

# ▀▄▀▄▀▄▀▄▀▄ Garbage collection ▀▄▀▄▀▄▀▄▀▄ ----

## Execute ----

gc()

# ▀▄▀▄▀▄▀▄▀▄ SECOND SET ▀▄▀▄▀▄▀▄▀▄ ----

## Result data ----

result <- studentGpaSemester

result <- result %>%
  
  ## Rename columns to English from Japanese ----

rename(

    # Renamed to   = rename from
  
  "学期 GPA 1 年目 前学期" = "1-前学期",
  "学期 GPA 1 年目 後学期" = "1-後学期",
  "学期 GPA 2 年目 前学期" = "2-前学期",
  "学期 GPA 2 年目 後学期" = "2-後学期",
  "学期 GPA 3 年目 前学期" = "3-前学期",
  "学期 GPA 3 年目 後学期" = "3-後学期",
  "学期 GPA 4 年目 前学期" = "4-前学期",
  "学期 GPA 4 年目 後学期" = "4-後学期",
  "学期 GPA 5 年目 前学期" = "5-前学期",
  "学期 GPA 5 年目 後学期" = "5-後学期"

  )

# ▀▄▀▄▀▄▀▄▀▄ accumulate data ▀▄▀▄▀▄▀▄▀▄ ----

## Prepare ----

accumulate <- studentGpaAccumulate

accumulate <- accumulate %>%
  
  ## Rename columns to English from Japanese ----

rename(

    # Renamed to   = rename from
  
  "累計 GPA 1 年目 前学期" = "1-前学期",
  "累計 GPA 1 年目 後学期" = "1-後学期",
  "累計 GPA 2 年目 前学期" = "2-前学期",
  "累計 GPA 2 年目 後学期" = "2-後学期",
  "累計 GPA 3 年目 前学期" = "3-前学期",
  "累計 GPA 3 年目 後学期" = "3-後学期",
  "累計 GPA 4 年目 前学期" = "4-前学期",
  "累計 GPA 4 年目 後学期" = "4-後学期",
  "累計 GPA 5 年目 前学期" = "5-前学期",
  "累計 GPA 5 年目 後学期" = "5-後学期"
  
)

# ▀▄▀▄▀▄▀▄▀▄ unites data ▀▄▀▄▀▄▀▄▀▄ ----

## Prepare ----

units <- studentUnits

units <- units %>%

## Rename columns to English from Japanese ----

rename(
  
  # Renamed to   = rename from
  
  "単位数 GPA 1 年目 前学期" = "1-前学期",
  "単位数 GPA 1 年目 後学期" = "1-後学期",
  "単位数 GPA 2 年目 前学期" = "2-前学期",
  "単位数 GPA 2 年目 後学期" = "2-後学期",
  "単位数 GPA 3 年目 前学期" = "3-前学期",
  "単位数 GPA 3 年目 後学期" = "3-後学期",
  "単位数 GPA 4 年目 前学期" = "4-前学期",
  "単位数 GPA 4 年目 後学期" = "4-後学期",
  "単位数 GPA 5 年目 前学期" = "5-前学期",
  "単位数 GPA 5 年目 後学期" = "5-後学期"

  )

# ▀▄▀▄▀▄▀▄▀▄ accumulate data ▀▄▀▄▀▄▀▄▀▄ ----

accumulate <- accumulate %>%

    ## Filter out selected columns ----

select(-c(
  
  入学年度,
  在籍区分,
  現況区分,
  学年,
  所属,
  学生氏名WEB,
  学生氏名カナ,
  学年別,
  入学区分,
  入試試験区分,
  入学区分コード,
  入試試験区分コード,
  出身校名,
  出身校名カナ,
  出身校コード,
  出身校区分,
  出身校県名,
  出身校課程,
  出身校学科,
  学科横断2A,
  学科横断2B,
  学科横断3A,
  学科横断3B
  
))

# ▀▄▀▄▀▄▀▄▀▄ units data ▀▄▀▄▀▄▀▄▀▄ ----

units <- units %>%

    ## Filter out selected columns ----

select(-c(

    入学年度,
  在籍区分,
  現況区分,
  学年,
  所属,
  学生氏名WEB,
  学生氏名カナ,
  学年別,
  入学区分,
  入試試験区分,
  入学区分コード,
  入試試験区分コード,
  出身校名,
  出身校名カナ,
  出身校コード,
  出身校区分,
  出身校県名,
  出身校課程,
  出身校学科,
  学科横断2A,
  学科横断2B,
  学科横断3A,
  学科横断3B
  
))

# ▀▄▀▄▀▄▀▄▀▄ Join data ▀▄▀▄▀▄▀▄▀▄ ----

## Joining accumulate data ----

result <- left_join(result, accumulate, by = "学籍番号")

# ▀▄▀▄▀▄▀▄▀▄ Join data ▀▄▀▄▀▄▀▄▀▄ ----

## Joining result data ----

result <- left_join(result, units, by = "学籍番号")

# ▀▄▀▄▀▄▀▄▀▄ Remove data from environment ▀▄▀▄▀▄▀▄▀▄ ----

## List of data to remove ----

# DISABLED FOR NOW

# rm(

#  accumulate, 
#  units,
#  crossClass,
#  entryType,
#  gpa,
#  highschool,
#  recentRecord,
#  student,
#  studentGpa,
#  studentGpaAccumulate,
#  studentGpaSemester,
#  studentUnits
#)

# ▀▄▀▄▀▄▀▄▀▄ Garbase collection ▀▄▀▄▀▄▀▄▀▄ ----

## Execute ----

gc()

# ▀▄▀▄▀▄▀▄▀▄ Load parquet data for past attendance ▀▄▀▄▀▄▀▄▀▄ ----

# NO LONGER USERD OR APPLIED

## Load arrow package ----

##### library(arrow) # For Parquet

# Example of saving as .parquet file
# write_parquet(result, "result.parquet")

# ▀▄▀▄▀▄▀▄▀▄ Load .parquet data ▀▄▀▄▀▄▀▄▀▄ ----

# Containing all past attendance data ----

# Generated by another R code

# attendance_data <- read_parquet("parquet/pivot.parquet")

##### attendance_data <- read_parquet("parquet/pivot.parquet")

# Assign data ---

attendance_data <- attendance_pivot

# Combining 2 fields into 1 field ----

attendance_data <- attendance_data %>%
  
    mutate(class_period_name = paste(yearMark_id, 開講区分_semester_status))

# Clear atteandance_pivot for recreation

rm(attendance_pivot)

# Preparing data ----

attendance_pivot <- attendance_data %>%
  
group_by(
    学籍番号_student_id,
    class_period_name
    
    ) %>%
  
summarize(
    mean(出席率_percentage_num),
    .groups = "keep"
  ) %>%

  rename("calculated" = "mean(出席率_percentage_num)")

## Pivot data ----

attendance_pivot <- attendance_pivot %>%
  
  pivot_wider(
    names_from = class_period_name,
    values_from = calculated)

## Renaming columns ----

attendance_pivot <- attendance_pivot %>%
  
    rename(
      
    "出席率 1 年目 前学期" = "1 年目 前学期",
    "出席率 1 年目 後学期" = "1 年目 後学期",
    "出席率 2 年目 前学期" = "2 年目 前学期",
    "出席率 2 年目 後学期" = "2 年目 後学期",
    "出席率 3 年目 前学期" = "3 年目 前学期",
    "出席率 3 年目 後学期" = "3 年目 後学期",
    "出席率 4 年目 前学期" = "4 年目 前学期",
    "出席率 4 年目 後学期" = "4 年目 後学期"
#    "出席率 5 年目 前学期" = "5 年目 前学期",
#    "出席率 5 年目 後学期" = "5 年目 後学期"

  ) %>%
  
  ## Select these columns ----

select(c(
  
  学籍番号_student_id,
  "出席率 1 年目 前学期",
  "出席率 1 年目 後学期",
  "出席率 2 年目 前学期",
  "出席率 2 年目 後学期",
  "出席率 3 年目 前学期",
  "出席率 3 年目 後学期",
  "出席率 4 年目 前学期",
  "出席率 4 年目 後学期"
#  "出席率 5 年目 前学期",
#  "出席率 5 年目 後学期"

))

# ▀▄▀▄▀▄▀▄▀▄ Joining data ▀▄▀▄▀▄▀▄▀▄ ----

## Joining attendance_pivot data ----

result <- left_join(result, attendance_pivot, by = c("学籍番号" = "学籍番号_student_id"))

# STEP 3 ----

employment <- read_csv(
  
  file = "csv/25卒_進路報告登録202503201034未確定.csv",
  
  locale = locale(encoding = "cp932"),  # これは Shift-JIS 日本語のため
  
  col_types = cols_only(
    
    # 卒業年                      = col_skip(),
    # 卒業予定月                  = col_skip(),
    
    学籍番号                      = col_character(),
    名前                          = col_character(),
    フリガナ                      = col_character(),
    
    # 学部学科区分1コード         = col_skip(),
    # 学部学科区分1               = col_skip(),
    # 学部学科区分2コード         = col_skip(),
    # 学部学科区分2               = col_skip(),
    # 学部学科コード              = col_skip(),
    # 学部学科                    = col_skip(),
    
    # 在籍区分                    = col_skip(),
    # 留学生                      = col_skip(),
    # 卒業年月日                  = col_skip(),
    # 学年                        = col_skip(),
    # 入学年度                    = col_skip(),
    
    # 性別                        = col_skip(),
    # 生年月日                    = col_skip(),
    
    # "現住所(〒)"                = col_skip(),
    #"現住所(住所1)"              = col_skip(),
    # "現住所(住所2)"             = col_skip(),
    # "現住所(TEL)"               = col_skip(),
    # 携帯電話                    = col_skip(),
    
    # "休暇中の連絡先(〒)"        = col_skip(),
    # "休暇中の連絡先(住所1)"     = col_skip(),
    # "休暇中の連絡先(住所2)"     = col_skip(),
    # "休暇中の連絡先(TEL)"       = col_skip(),
    
    # メールアドレス              = col_skip(),
    
    # 企業登録区分                = col_skip(),
    # 進路コード                  = col_skip(),
    
    進路                          = col_character(),
    # 進路2コード                 = col_skip(),
    進路2                         = col_character(),
    # 進路区分コード              = col_skip(),
    進路区分                      = col_character(),
    # 文科省進路コード            = col_skip(),
    文科省進路名                  = col_character(),
    # 再掲                        = col_skip(),
    # ポスドク再掲                = col_skip(),
    
    JNET企業コード                = col_character(),
    "進路先　名称"                = col_character(),
    "進路先　フリガナ"            = col_character(),
    "採用支社名･所属部署等"       = col_character(),
    "本社所在地(〒)"              = col_character(),
    #"本社所在地(都道府県コード)" = col_skip(),
    "本社所在地(都道府県)"        = col_character(),
    # "本社所在地(住所1)"         = col_skip(),
    # "本社所在地(住所2)"         = col_skip(),
    # "本社所在地(TEL)"           = col_skip(),
    URL                           = col_character(),
    
    # 業種コード                  = col_skip(),
    業種分類                      = col_character(),
    # 文科省業種コード            = col_skip(),
    文科省業種分類                = col_character(),
    # 上場コード                  = col_skip(),
    上場                          = col_character(),
    "資本金(億)"                  = col_character(),
    "資本金(万)"                  = col_character(),
    従業員                        = col_character(),
    
    # "勤務(予定)地コード"        = col_skip(),
    "勤務(予定)地"                = col_character(),
    # "勤務(予定)地(市区町村名)"  = col_skip(),
    # 職種コード                  = col_skip(),
    職種                          = col_character(),
    # 文科省職種コード            = col_skip(),
    文科省職種                    = col_character(),
    # 雇用形態コード1             = col_skip(),
    雇用形態1                     = col_character(),
    # 雇用形態コード2             = col_skip(),
    # 雇用形態2                   = col_skip(),
    # 雇用形態コード3             = col_skip(),
    # 雇用形態3                   = col_skip()
    
  )
)

# Renaming columns

employment <- employment %>%
  rename(
    "就職先_学籍番号"              = "学籍番号",
    "就職先_学生氏名"              = "名前",
    "就職先_学生氏名_フリガナ"     = "フリガナ",
    "就職先_進路"                  = "進路",
    "就職先_進路_追加情報"         = "進路2",
    "就職先_進路区分"              = "進路区分",
    "就職先_JNET_企業コード"       = "JNET企業コード",
    "就職先_企業名"                = "進路先　名称",
    "就職先_企業名_フリガナ"       = "進路先　フリガナ",
    "就職先_採用支社名･所属部署等" = "採用支社名･所属部署等",
    "就職先_就職先郵便番号"        = "本社所在地(〒)",
    "就職先_本社所在地(都道府県)"  = "本社所在地(都道府県)",
    "就職先_URL"                   = "URL",
    "就職先_業種分類"              = "業種分類",
    "就職先_文科省業種分類"        = "文科省業種分類",
    "就職先_上場"                  = "上場",
    "就職先_資本金(億)"            = "資本金(億)",
    "就職先_資本金(万)"            = "資本金(万)",
    "就職先_従業員"                = "従業員",
    "就職先_勤務(予定)地"          = "勤務(予定)地",
    "就職先_職種"                  = "職種",
    "就職先_文科省職種"            = "文科省職種",
    "就職先_雇用形態名"            = "雇用形態1"
  )

result <- left_join(result, employment, by = c("学籍番号" = "就職先_学籍番号"))

# ▀▄▀▄▀▄▀▄▀▄ High school score data ▀▄▀▄▀▄▀▄▀▄ ----

## Load CSV data ----

high_school_score <- read_csv(

    file = 
  
        "csv/OLD Shift_JIS 入試課 高校の偏差値データ high school rawdata 2021-10-28.csv",
  
    locale = locale(encoding = "cp932"),  # Shift-JIS
  
    col_types = cols_only(
    高校コード = col_character(), 
    最少偏差値 = col_integer()
  )
) %>%
  
  distinct()

## Rename columns ----

high_school_score <- high_school_score %>%

    rename(
  
        # Renamed to   = rename from
    
      "出身校コード" = "高校コード",
    "高校の偏差値" = "最少偏差値"
  ) 

# These are not Campus Square data


result <- left_join(result, high_school_score, by = "出身校コード")

# ▀▄▀▄▀▄▀▄▀▄ DISABLED Joining high school data ▀▄▀▄▀▄▀▄▀▄ ----

# result <- left_join(result, high_school_score, by = "出身校コード")

## Removing data ----
# rm(high_school_score)

# ▀▄▀▄▀▄▀▄▀▄ Placement test data ▀▄▀▄▀▄▀▄▀▄ ----

## Read CSV data ----

mathtest_score <- read_csv(

    file = 
  
        "csv/OLD Shift_JIS 数学プレイスメントテスト math placement test 2022-05-28.csv",
  
    # locale = locale(encoding = "cp932"),  # Shift-JIS

      col_types = cols_only(
    学籍番号 = col_character(), 
    合計点数 = col_double()
    
  )
)

## Rename column ----
mathtest_score <- mathtest_score %>%
  
  rename(
    
        # Renamed to   = rename from
    "数学プレイスメントテストの点数" = "合計点数"
  
    ) 

# ▀▄▀▄▀▄▀▄▀▄ Join data ▀▄▀▄▀▄▀▄▀▄ ----

## Joining mathtest score data ----

result <- left_join(result, mathtest_score, by = "学籍番号")

# ▀▄▀▄▀▄▀▄▀▄ Export as Excel file ▀▄▀▄▀▄▀▄▀▄ ----

## Specified as this output filename ----

write_xlsx(
  
  path = "xlsx/result.xlsx",
  
  col_names = TRUE, 
  format_headers = TRUE,
  list('Result' = result))

# END OF LINE

gc()

# LAST CONFIRMATION MESSAGE 完了後に確認用メッセージの表示 ----

# Only available when running from RStudio application

# これは、RStudio 上でのみ機能します

rstudioapi::showDialog("Confirmation message", "Complete")

# END OF THIS R CODE
