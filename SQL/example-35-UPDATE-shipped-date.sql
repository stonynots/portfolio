SELECT 
  * 
FROM vaccination.covid19 
WHERE ワクチン接種記録システム状況 
  IN ('登録済み　接種せず', '登録済み　一回目のみ', '登録済み　一と二回目両方') 
  AND クーポン券_状況 = 'あり';

 -- UPDATE vaccination.covid19 SET 予診票_発送日 = '2021/10/04'::date WHERE ワクチン接種記録システム状況 IN ('登録済み　接種せず', '登録済み　一回目のみ', '登録済み　一と二回目両方') AND クーポン券_状況 = 'あり';

