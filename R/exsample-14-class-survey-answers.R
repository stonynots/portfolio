# Description 解説 ----
# 授業アンケートの結果を集計する R プログラム

# Load packages パッケージの読み込み ----
library(tidyverse) # https://www.tidyverse.org/packages/
library(writexl) # https://docs.ropensci.org/writexl/
library(ggplot2)

rm(list = ls())

# Set for Windows environment
#
# setwd("C:/r")

# Load CSV data ----
#
## <Memo> "Please ignore warning message" after this CSV import ----
#
# Original file name e.g. "授業に関するアンケート（Web版）_2023年度_前期 ..."
#
# data <- read_csv(file = "2023-08-18.csv",

# January 2024
# data <- read_csv(file = "2024-02-16 CSV 授業に関するアンケート（Web版）_2023年度_後期_20240216070735.csv", 
                 
# August 2024
data <- read_csv(file = "2024-08-27 CSV 授業に関するアンケート（Web版）_2024年度_前期_20240827072710.csv",

# data <- read_csv(file = "classsurvey/2023-07-25.csv",
  na = c(""),  # Set to empty if NA

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

# Q1 class (6 questions) ----
#
# 授業方法について

  ## Q1_1 environment ----
  #
  #   学生の主体的な学びや授業への積極的な参加を促す工夫や
  #   雰囲気作りがなされていますか？
  #
  設問1回答  = col_character(),

  ## Q1_2 materials ----
  #
  #   教科書・配布資料・視聴覚機器・板書などは
  #   効果的に使われて授業理解に役立っていますか？
  #
  設問2回答  = col_character(),

  ## Q1_3 process ----
  #
  #   教員は学生の反応や理解度を確認しつつ授業を進めていますか？
  #
  設問3回答  = col_character(),

  ## Q1_4 instruction ----
  #
  #   予習_復習をおこなうための適切な指示や
  #   課題提示がなされていますか？
  #
  設問4回答  = col_character(),

  ## Q1_5 describing ----
  #
  #   提出した課題やレポート_質問などに対して
  #   適切な説明や指導がおこなわれていますか？
  #
  設問5回答  = col_character(),

  ## Q1_6 sharing ----
  #
  #   学生同士が意見交換したり質問などを
  #   共有したりする機会や場がありますか？
  #
  設問6回答  = col_character(),

# Q2 contents (4 questions) ----
#
# 授業内容について

  ## Q2_1 description ----
  #
  #   授業の内容は授業ガイダンス等で
  #   事前に説明され理解したものと合っていますか？
  #
  設問7回答  = col_character(),

  ## Q2_2 interest ----
  #
  #   好奇心を刺激したり_意義や必要性を感じさせたりして
  #   学ぶ意欲を高める内容になっていますか？
  #
  設問8回答  = col_character(),

  ## Q2_3 acquire ----
  #
  #   この授業の到達目標となっている知識や技能を
  #   しっかり学べる内容になっていますか？
  #
  設問9回答  = col_character(),

  # Q2_4 feeling ----
  #
  #   この授業を受けてよかったと感じていますか？
  #
  設問10回答 = col_character(),

# Q3 ability (7 choices) ----
#
# あらたに身についたと感じる能力について複数回答

# << This section contains multiple answers with for a 1 question >>
# << こちらは 1 つの問いに対して複数回答のセクション >>

  # Q3 Original text
  # 以下の選択肢の中でこの授業であらたに身についたり
  # レベルアップしたりしたと感じる能力があれば
  # チェックしてください(複数回答可)

    # Q3_1 collect ----
    #
    #   必要な情報を収集する力
    #
    設問11回答 = col_character(),

    # Q3_2 use ----
    #
    #   学んだ知識や技能を役立てる力
    #
    設問12回答 = col_character(),

    # Q3_3 expand ----
    #
    #   興味や関心の範囲を広げる力
    #
    設問13回答 = col_character(),

    # Q3_4 improve ----
    #
    #   学びや作業を振り返り改善する力
    #
    設問14回答 = col_character(),

    # Q3_5 present ----
    #
    #   作文やプレゼンテーションなど表現する力
    #
    設問15回答 = col_character(),

    # Q3_6 collaborate ----
    #
    #   他者との対話や協働作業をおこなう力
    #
    設問16回答 = col_character(),

    # Q3_7 knowledge ----
    #
    #   科目に関わる知識や技能
    #
    設問17回答 = col_character(),

# Q4 select (2 questions) ----
# 履修動機と授業への取り組み状況について

  # Q4_1 reason ----
  #
  #   この授業を履修した理由として
  #   もっとも当てはまるものはどれですか？
  #
  設問18回答 = col_character(),

  # Q4_2 invest ----
  #
  #   この授業1回あたりの総学習時間
  #   授業_予習_復習_は平均してどのくらいですか？
  #
  設問19回答 = col_character(),

  # Q5 comment ----
  #
  #   自由記述
  #
  設問20回答 = col_character(),

  最終更新日 = col_datetime(format = "%Y/%m/%d %H:%M")
  )
) %>%  # %>% is a Forward Pipe Operator

## Excluding NA entries ----
filter(!is.na(最終更新日)) %>%
  
## Rename columns ----
rename(
  
  # Changed TO (Upper)
  # = changed FROM (Below)
  
  # Q1 (Total of 6)
  #
  "Q1_1_学生の主体的な学びや授業への積極的な参加を促す工夫や雰囲気作りがある"
  # "01:授業方法について質問します。｜01:学生の主体的な学びや授業への積極的な参加を促す工夫や雰囲気作りがなされていますか"
  = "設問1回答",
  #
  "Q1_2_教科書_配布資料_視聴覚機器_板書_などは効果的で理解に役立っている"
  # 01:授業方法について質問します。｜02:教科書、配布資料、視聴覚機器、板書 などは効果的に使われて、授業理解に役立っていますか
  = "設問2回答",
  #
  "Q1_3_教員は学生の反応や理解度を確認しつつ授業を進めている"
  # 01:授業方法について質問します。｜03:教員は学生の反応や理解度を確認しつつ授業を進めていますか
    = "設問3回答",
  #
  "Q1_4_予習_復習をおこなうための適切な指示や課題提示がなされている"
  # 01:授業方法について質問します。｜04:予習・復習をおこなうための適切な指示や、課題提示がなされていますか
  = "設問4回答",
  #
  "Q1_5_提出した課題や質問などに対して適切な説明や指導がおこなわれている"
  # 01:授業方法について質問します。｜05:提出した課題やレポート、質問などに対して、適切な説明や指導がおこなわれていますか
  = "設問5回答",
  #
  "Q1_6_学生同士が意見交換したり質問などを共有したりする機会や場がある"
  # 01:授業方法について質問します。｜06:学生同士が意見交換したり質問などを共有したりする機会や場がありますか
  = "設問6回答",
  
  # Q2 (Total of 4)
  #
  "Q2_1_授業の内容は授業ガイダンス等で事前に説明され理解したものと合っている"
  # 02:授業内容について質問します｜01:授業の内容は授業ガイダンス等で事前に説明され理解したものと合っていますか
  = "設問7回答",
  #
  "Q2_2_好奇心を刺激したり意義や必要性を感じ学ぶ意欲を高める内容になっている"
  # 02:授業内容について質問します｜02:好奇心を刺激したり、意義や必要性を感じさせたりして、学ぶ意欲を高める内容になっていますか
  = "設問8回答",
  #
  "Q2_3_授業の到達目標となっている知識や技能をしっかり学べる内容になっている"
  # 02:授業内容について質問します｜03:この授業の到達目標となっている知識や技能を、しっかり学べる内容になっていますか
  = "設問9回答",
  #
  "Q2_4_授業を受けてよかったと感じている"
  # 02:授業内容について質問します｜04:この授業を受けてよかったと感じていますか
  = "設問10回答",
  
  # Q3 (Total of 7)
  #
  "Q3_1_必要な情報を収集する力" 
  # 03:以下の選択肢の中で、この授業であらたに身についたり、レベルアップしたりしたと感じる能力があれば、チェックしてください(複数回答可)｜必要な情報を収集する力
  = "設問11回答",
  #
  "Q3_2_学んだ知識や技能を役立てる力"
  # 03:以下の選択肢の中で、この授業であらたに身についたり、レベルアップしたりしたと感じる能力があれば、チェックしてください(複数回答可)｜学んだ知識や技能を役立てる力
  = "設問12回答",
  #
  "Q3_3_興味や関心の範囲を広げる力"
  # 03:以下の選択肢の中で、この授業であらたに身についたり、レベルアップしたりしたと感じる能力があれば、チェックしてください(複数回答可)｜興味や関心の範囲を広げる力
  = "設問13回答",
  #
  "Q3_4_学びや作業を振り返り改善する力"
  # 03:以下の選択肢の中で、この授業であらたに身についたり、レベルアップしたりしたと感じる能力があれば、チェックしてください(複数回答可)｜学びや作業を振り返り改善する力
  = "設問14回答",
  #
  "Q3_5_作文やプレゼンテーションなど表現する力"
  # 03:以下の選択肢の中で、この授業であらたに身についたり、レベルアップしたりしたと感じる能力があれば、チェックしてください(複数回答可)｜作文やプレゼンテーションなど表現する力
    = "設問15回答",
  #
  "Q3_6_他者との対話や協働作業をおこなう力" 
  # 03:以下の選択肢の中で、この授業であらたに身についたり、レベルアップしたりしたと感じる能力があれば、チェックしてください(複数回答可)｜他者との対話や協働作業をおこなう力
  = "設問16回答",
  #
  "Q3_7_科目に関わる知識や技能" 
  # 03:以下の選択肢の中で、この授業であらたに身についたり、レベルアップしたりしたと感じる能力があれば、チェックしてください(複数回答可)｜科目に関わる知識や技能
  = "設問17回答",
  
  # Q4 (Total of 2)
  # 
  "Q4_1_この授業を履修した理由としてもっとも当てはまるもの" 
  # 04:あなたの履修動機と授業への取り組み状況について｜01:この授業を履修した理由としてもっとも当てはまるものはどれですか
  = "設問18回答",
  #
  "Q4_2_この授業1回あたりの総学習時間_授業_予習_復習_は平均してどのくらい" 
  # 04:あなたの履修動機と授業への取り組み状況について｜02:この授業1回に対して授業外の学修に使う時間は、平均してどのくらいですか
  = "設問19回答",
  
  # Q5 (Only 1)
  #
  "Q5_自由記述"
  # こちらは自由記述です
  = "設問20回答"
  
) %>%
  
## Display only date ----
mutate(最終更新日 = as.Date(
  最終更新日, format = '%y-%m-%d'
  )
) %>%
  
## Add a column ----
#
# For converted text
#
mutate(学年 = case_when(
  学年 == "学部1年" ~ "1 年生",
  学年 == "学部2年" ~ "2 年生",
  学年 == "学部3年" ~ "3 年生",
  学年 == "学部4年" ~ "4 年生",
  )
) %>%
  
## Add a column ----
#
# For student year category
#
mutate(学番三桁_studentYearCategory_name =
         substr(学籍番号, start = 1, stop = 3)
) %>%
  
## Add a column ----
#
# For curriculum category
#
mutate(時間割カテゴリ_curriculumCategory_name = case_when(
  substr(時間割コード, start = 1, stop = 1) == 0 ~ "10 カリ",
  substr(時間割コード, start = 1, stop = 1) == 7 ~ "17 カリ"
  )
) %>%
  
## Recorder columns ----
relocate(any_of(c(
  "最終更新日",
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
  
## Rename columns ----
#
rename(
  # Changed TO (Left side) = changed FROM (Right side)
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
# End of forward pipe operator

# Q1 ----
q1 <- data %>% 
select(
  # 時間割カテゴリ_curriculumCategory_name,
    # (e.g. 17 カリ; removed because data only contains 17 カリ and no 10 カリ)
  時間割コード_class_code,
  授業名_class_name,
  担当教員名_teacher_name,
  Q1_1_学生の主体的な学びや授業への積極的な参加を促す工夫や雰囲気作りがある,
  Q1_2_教科書_配布資料_視聴覚機器_板書_などは効果的で理解に役立っている,
  Q1_3_教員は学生の反応や理解度を確認しつつ授業を進めている,
  Q1_4_予習_復習をおこなうための適切な指示や課題提示がなされている,
  Q1_5_提出した課題や質問などに対して適切な説明や指導がおこなわれている,
  Q1_6_学生同士が意見交換したり質問などを共有したりする機会や場がある
) %>%
  
## Pivot longer ----
#
pivot_longer(cols = c(
  'Q1_1_学生の主体的な学びや授業への積極的な参加を促す工夫や雰囲気作りがある',
  'Q1_2_教科書_配布資料_視聴覚機器_板書_などは効果的で理解に役立っている',
  'Q1_3_教員は学生の反応や理解度を確認しつつ授業を進めている',
  'Q1_4_予習_復習をおこなうための適切な指示や課題提示がなされている',
  'Q1_5_提出した課題や質問などに対して適切な説明や指導がおこなわれている',
  'Q1_6_学生同士が意見交換したり質問などを共有したりする機会や場がある'
  )
) %>%
  
## Assign count for each row ----
#
mutate(count = case_when(
  # If NA then kept as NA
  is.na(value) ~ NA_real_, 
  TRUE ~ 1
  )
) %>%
  
## Pivot wider ----
#
pivot_wider(
  names_from  = value,
  values_from = count,
  values_fn=sum,
  values_fill = 0  # Fill in NA with 0
) %>%
  
## Add total row ----
#
mutate(total = rowSums(select(.,いいえ, はい, どちらともいえない))) %>%
  
## Add 3 calculated rows in percentages ( % ) ----
#
mutate(はい = はい / total) %>%
mutate(いいえ = いいえ / total) %>%
mutate(どちらともいえない = どちらともいえない / total) %>%
  
## Rename columns ----
#
rename(
  # Changed to = changed from
  "時間割コード" = "時間割コード_class_code",
  "授業名"       = "授業名_class_name",
  "担当教員名"   = "担当教員名_teacher_name",
  "設問"         = "name",
  "回答人数"     = "total"
) %>%
  
## Reorder columns ----
#
relocate(any_of(c(
  "時間割コード",
  "授業名",
  "担当教員名",
  "設問",
  "はい",
  "いいえ",
  "どちらともいえない"
  ))
) %>%
  
## Arrange order ----
#
arrange(担当教員名, 時間割コード, 設問)

# Q2 ----
q2 <- data %>%
select(
  # 時間割カテゴリ_curriculumCategory_name,
    # (e.g. 17 カリ; removed because data only contains 17 カリ and no 10 カリ)
  時間割コード_class_code,
  授業名_class_name,
  担当教員名_teacher_name,
  Q2_1_授業の内容は授業ガイダンス等で事前に説明され理解したものと合っている,
  Q2_2_好奇心を刺激したり意義や必要性を感じ学ぶ意欲を高める内容になっている,
  Q2_3_授業の到達目標となっている知識や技能をしっかり学べる内容になっている,
  Q2_4_授業を受けてよかったと感じている
) %>%
  
## Do pivot_longer ----
#
pivot_longer(cols = c(
  'Q2_1_授業の内容は授業ガイダンス等で事前に説明され理解したものと合っている',
  'Q2_2_好奇心を刺激したり意義や必要性を感じ学ぶ意欲を高める内容になっている',
  'Q2_3_授業の到達目標となっている知識や技能をしっかり学べる内容になっている',
  'Q2_4_授業を受けてよかったと感じている'
  )
) %>%
  
## Add count for each row ----
#
mutate(count = case_when(
  # If NA then kept as NA
  is.na(name) ~ NA_real_, 
  TRUE ~ 1
)) %>%
  
## Do pivot_wider ----
#
pivot_wider(
  names_from  = value,
  values_from = count,
  values_fn=sum,
  # Fill in NA with 0
  values_fill = 0
) %>%
  
## Add total row ----
#
mutate(total = rowSums(select(.,いいえ, はい, どちらともいえない))) %>%
  
## Add 3 calculated row in pacentages ( % ) ----
#
mutate(はい = はい / total) %>%
mutate(いいえ = いいえ / total) %>%
mutate(どちらともいえない = どちらともいえない / total) %>%
  
## Rename columns ----
#
rename(
  # Changed TO (Left side) = changed FROM (Right side)
  "時間割コード" = "時間割コード_class_code",
  "授業名"       = "授業名_class_name",
  "担当教員名"   = "担当教員名_teacher_name",
  "設問"         = "name",
  "回答人数"     = "total"
) %>%
  
## Reorder columns ----
#
relocate(any_of(c(
  "時間割コード",
  "授業名",
  "担当教員名",
  "設問",
  "はい",
  "いいえ",
  "どちらともいえない"
  ))
) %>%
  
## Arrange order ----
arrange(担当教員名, 時間割コード, 設問)

# Total number list (for Q3) ----
#
# Get total number for each class
total <- q1 %>%
distinct(
  時間割コード,
  回答人数
)

# Q3 ----
q3 <- data %>%
select(
  # 時間割カテゴリ_curriculumCategory_name,
    # (e.g. 17 カリ; removed because data only contains 17 カリ and no 10 カリ)
  時間割コード_class_code,
  授業名_class_name,
  担当教員名_teacher_name,
  Q3_1_必要な情報を収集する力,
  Q3_2_学んだ知識や技能を役立てる力,
  Q3_3_興味や関心の範囲を広げる力,
  Q3_4_学びや作業を振り返り改善する力,
  Q3_5_作文やプレゼンテーションなど表現する力,
  Q3_6_他者との対話や協働作業をおこなう力,
  Q3_7_科目に関わる知識や技能
) %>%
  
## Rename columns ----
#
rename(
  # Changed TO (Left side) = changed FROM (Right side)
  "時間割コード" = "時間割コード_class_code",
  "授業名"       = "授業名_class_name",
  "担当教員名"   = "担当教員名_teacher_name",
) %>%
  
## Do pviot_longer ----
#
pivot_longer(cols = c(
  'Q3_1_必要な情報を収集する力',
  'Q3_2_学んだ知識や技能を役立てる力',
  'Q3_3_興味や関心の範囲を広げる力',
  'Q3_4_学びや作業を振り返り改善する力',
  'Q3_5_作文やプレゼンテーションなど表現する力',
  'Q3_6_他者との対話や協働作業をおこなう力',
  'Q3_7_科目に関わる知識や技能'
  )
) %>%
  
## Exclude NA entries ----
#
filter(!is.na(value)) %>%
  
## Remove a column ----
#
select(-c(value)) %>%
  
## Assign count number ----
#
# Counting each row as 1
#
mutate(count = case_when(
  is.na(name) ~ NA_real_,  # If NA then kept as NA
  TRUE ~ 1
  )
) %>%
  
## Do pivot_wider ----
#
pivot_wider(
  names_from  = name,
  values_from = count,
  values_fn   = sum,
  values_fill = 0  # Fill in NA with 0
) %>%
  
## Do left_join total number ----
#
left_join(total, by = "時間割コード") %>%
  
## Add columns ----
#
# For calculated results in percentages ( % )
#
mutate(Q3_1_必要な情報を収集する力
  = Q3_1_必要な情報を収集する力 / 回答人数) %>%
#
mutate(Q3_2_学んだ知識や技能を役立てる力
  = Q3_2_学んだ知識や技能を役立てる力 / 回答人数) %>%
#
mutate(Q3_3_興味や関心の範囲を広げる力
  = Q3_3_興味や関心の範囲を広げる力 / 回答人数) %>%
#
mutate(Q3_4_学びや作業を振り返り改善する力
  = Q3_4_学びや作業を振り返り改善する力 / 回答人数) %>%
#
mutate(Q3_5_作文やプレゼンテーションなど表現する力
  = Q3_5_作文やプレゼンテーションなど表現する力 / 回答人数) %>%
#
mutate(Q3_6_他者との対話や協働作業をおこなう力
  = Q3_6_他者との対話や協働作業をおこなう力 / 回答人数) %>%
#
mutate(Q3_7_科目に関わる知識や技能
  = Q3_7_科目に関わる知識や技能 / 回答人数)

q3 <- q3 %>%
# Arrange order ----
arrange(担当教員名, 時間割コード) %>%

## Reorder columns ----
#
relocate(any_of(c(
  "時間割コード",
  "授業名",
  "担当教員名",
  "Q3_1_必要な情報を収集する力",
  "Q3_2_学んだ知識や技能を役立てる力",
  "Q3_3_興味や関心の範囲を広げる力",
  "Q3_4_学びや作業を振り返り改善する力",
  "Q3_5_作文やプレゼンテーションなど表現する力",
  "Q3_6_他者との対話や協働作業をおこなう力",
  "Q3_7_科目に関わる知識や技能"
))
)

# Q4_1 ----
q4_1 <- data %>%
select(
  # 時間割カテゴリ_curriculumCategory_name,
    # (e.g. 17 カリ; removed because data only contains 17 カリ and no 10 カリ)
  時間割コード_class_code,
  授業名_class_name,
  担当教員名_teacher_name,
  Q4_1_この授業を履修した理由としてもっとも当てはまるもの,
) %>%
  
## Rename columns ----
#
rename(
  # Changed to   = changed from
  "時間割コード" = "時間割コード_class_code",
  "授業名"       = "授業名_class_name",
  "担当教員名"   = "担当教員名_teacher_name",
) %>%
  
## Do pivot_longer ----
#
pivot_longer(cols = c(
  'Q4_1_この授業を履修した理由としてもっとも当てはまるもの'
  )
) %>%
  
## Assign count for each row ----
#
mutate(count = case_when(
  is.na(value) ~ NA_real_,  # If NA then kept as NA
  TRUE ~ 1
  )
) %>%
  
## Do pivot_wider ----
#
pivot_wider(
  names_from = value,
  values_from = count,
  values_fn=sum,
  values_fill = 0  # Fill in NA with 0
) %>%
  
## Join total number ----
#
left_join(total, by = "時間割コード") %>%
  
## Remove a column ----
#
select(-c(name)) %>%
  
## Rename columns ----
#
rename(
# Changed TO (Upper)
#  = changed FROM (Bottom)
#
  "Q4_1_履修した理由_1_時間割の都合" 
  = "時間割の都合",
  #
  "Q4_1_履修した理由_2_必修などカリキュラムの都合" 
  = "必修などカリキュラムの都合",
  #
  "Q4_1_履修した理由_3_内容に関心がある" 
  = "内容に関心がある",
  #
  "Q4_1_履修した理由_4_必要な能力が身につく" 
  = "必要な能力が身につく",
  #
  "Q4_1_履修した理由_5_単位が取りやすそう" 
  = "単位が取りやすそう"
) %>%
  
## Add columns ----
#
# With calculated results in percentages ( % )
#
mutate(Q4_1_履修した理由_1_時間割の都合
  = Q4_1_履修した理由_1_時間割の都合 / 回答人数) %>%

mutate(Q4_1_履修した理由_2_必修などカリキュラムの都合
  = Q4_1_履修した理由_2_必修などカリキュラムの都合 / 回答人数) %>%

mutate(Q4_1_履修した理由_3_内容に関心がある
  = Q4_1_履修した理由_3_内容に関心がある / 回答人数) %>%

mutate(Q4_1_履修した理由_4_必要な能力が身につく
  = Q4_1_履修した理由_4_必要な能力が身につく / 回答人数) %>%

mutate(Q4_1_履修した理由_5_単位が取りやすそう
  = Q4_1_履修した理由_5_単位が取りやすそう / 回答人数)

q4_1 <- q4_1 %>%
  # Arrange order ----
arrange(担当教員名, 時間割コード) %>%

  ## Reorder columns ----
#
relocate(any_of(c(
  "時間割コード",
  "授業名",
  "担当教員名",
  "Q4_1_履修した理由_1_時間割の都合",
  "Q4_1_履修した理由_2_必修などカリキュラムの都合",
  "Q4_1_履修した理由_3_内容に関心がある",
  "Q4_1_履修した理由_4_必要な能力が身につく",
  "Q4_1_履修した理由_5_単位が取りやすそう"
))
)

# Q4_2 ----
q4_2 <- data %>%
  select(
    # 時間割カテゴリ_curriculumCategory_name,
    時間割コード_class_code,
    授業名_class_name,
    担当教員名_teacher_name,
    Q4_2_この授業1回あたりの総学習時間_授業_予習_復習_は平均してどのくらい
  ) %>%
  
  ## Rename columns ----
rename(
  # Changed to = changed from
  "時間割コード" = "時間割コード_class_code",
  "授業名"       = "授業名_class_name",
  "担当教員名"   = "担当教員名_teacher_name",
) %>%
  
  ## Do pivot_longer ----
pivot_longer(cols = c(
  'Q4_2_この授業1回あたりの総学習時間_授業_予習_復習_は平均してどのくらい'
)) %>%
  
  ## Add count for each row ----
mutate(count = case_when(
  # If NA then kept as NA
  is.na(name) ~ NA_real_, 
  TRUE ~ 1
)) %>%
  
  ## Do pivot_wider ----
pivot_wider(
  names_from  = value,
  values_from = count,
  values_fn=sum,
  # Fill in NA with 0
  values_fill = 0
) %>%
  
  ## Remove a column ----
select(-c(name)) %>%
  
  ## Join total number ----
left_join(total, by = "時間割コード") %>%

  ## Rename columns ----

# Question
# この授業1回に対して授業外の学修に使う時間は、平均してどのくらいですか

rename(
  # Changed to = changed from
  "Q4_2_総学習時間の平均_ほとんどなし" = "ほとんどなし",
  "Q4_2_総学習時間の平均_30分程度" = "３０分程度",
  "Q4_2_総学習時間の平均_1時間程度" = "１時間程度",
  "Q4_2_総学習時間の平均_2時間程度" = "２時間程度",
  "Q4_2_総学習時間の平均_それ以上" = "それ以上",
) %>%
  


# Previous selection in early 2022 年の後期版
  ## Rename columns
# rename(
  # Changed to = changed from
#   "Q4_2_総学習時間の平均_1時間未満" = "1時間未満",
#   "Q4_2_総学習時間の平均_1から2時間" = "1～2時間",
#   "Q4_2_総学習時間の平均_2から3時間" = "2～3時間",
#   "Q4_2_総学習時間の平均_3から4時間" = "3～4時間",
#   "Q4_2_総学習時間の平均_4時間以上" = "4時間以上",
# ) %>%
  
  ## Add columns ----
# For calculate results
mutate(Q4_2_総学習時間の平均_ほとんどなし
       = Q4_2_総学習時間の平均_ほとんどなし / 回答人数) %>%
  
  mutate(Q4_2_総学習時間の平均_30分程度
         = Q4_2_総学習時間の平均_30分程度 / 回答人数) %>%
  
  mutate(Q4_2_総学習時間の平均_1時間程度
         = Q4_2_総学習時間の平均_1時間程度 / 回答人数) %>%
  
  mutate(Q4_2_総学習時間の平均_2時間程度
         = Q4_2_総学習時間の平均_2時間程度 / 回答人数) %>%
  
  mutate(Q4_2_総学習時間の平均_それ以上
         = Q4_2_総学習時間の平均_それ以上/ 回答人数) %>%
  
## Reorder columns ----
relocate(any_of(c(
  "時間割コード",
  "授業名",
  "担当教員名",
  "Q4_2_総学習時間の平均_ほとんどなし",
  "Q4_2_総学習時間の平均_30分程度",
  "Q4_2_総学習時間の平均_1時間程度",
  "Q4_2_総学習時間の平均_2時間程度",
  "Q4_2_総学習時間の平均_それ以上",
  "回答人数"
)))

q4_2 <- q4_2 %>%
  # Arrange order ----
arrange(担当教員名, 時間割コード)
  

# — — — — — — — — — — ----

# Q5 ----
q5 <- data %>%
select(
  # 時間割カテゴリ_curriculumCategory_name,
    # (e.g. 17 カリ; removed because data only contains 17 カリ and no 10 カリ)
  時間割コード_class_code,
  授業名_class_name,
  担当教員名_teacher_name,
  Q5_自由記述
) %>%
  
## Rename columns ----
#
rename(
  # Changed TO (Left) = changed FROM (Right)
  "時間割コード" = "時間割コード_class_code",
  "授業名"       = "授業名_class_name",
  "担当教員名"   = "担当教員名_teacher_name",
) %>%
  
## Excluding (removing) NA entries
#
filter(!is.na(Q5_自由記述)) %>%
  
## Counting character length
#
mutate(length = str_length(Q5_自由記述))

q5 <- q5 %>%
  # Arrange order ----
arrange(担当教員名, 時間割コード, length)

# Output results as an Excel file ----
#
# Saved in different sheet tabs
#
write_xlsx(
  path           = "answersReport.xlsx",
  # path           = "classsurvey/answers.xlsx",
  col_names      = TRUE, 
  format_headers = TRUE,
  list(
    'Q1 授業方法について Process'   = q1,
    'Q2 授業内容について Contents'  = q2,
    'Q3 身についたと感じる Ability' = q3,
    'Q4_1 履修した理由 Reason'      = q4_1,
    'Q4_2 総学習時間の平均 Time'    = q4_2,
    'Q5 自由記述 Comment'           = q5
  )
)

# garbage collection
gc(reset = TRUE)

# Last line of this code
