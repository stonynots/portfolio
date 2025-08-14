SELECT 
  都道府県, 
  市区町村, 
  学籍職員番号, 
  氏名, 
  氏名かな, 
  一回目_日付, 
  一回目_時間帯, 
  クーポン券_状況, 
  uuid 
FROM vaccination.covid19 
WHERE 
  (
    一回目_接種状況 = '接種済み' 
    OR 一回目_接種状況 = '接種せず'
  ) 
  AND 都道府県 IS NULL
ORDER BY 
  一回目_日付, 
  一回目_時間帯, 
  学籍職員番号;