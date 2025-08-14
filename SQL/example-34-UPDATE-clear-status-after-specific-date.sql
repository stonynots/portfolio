SELECT
  uuid,
  二回目日付,
  二回目時間帯,
  二回目接種状況
FROM vaccination.covid19
WHERE 二回目日付 >= '2021/07/31'
ORDER BY 二回目日付, 二回目時間帯;

UPDATE vaccination.covid19
  SET
    二回目日付 = NULL,
    二回目時間帯 = NULL,
    二回目接種状況 = NULL
  WHERE 二回目日付 >= '2021/07/31';
