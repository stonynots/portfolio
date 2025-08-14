/* UPDATE vaccination.covid19 SET クーポン券状況 = NULL WHERE クーポン券状況 = ''; */

SELECT
  二回目日付,
  二回目時間帯,
  二回目接種状況,
  二回目ワクチンロット,
  二回目医師名,
  クーポン券状況,
  クーポン券受取日,
  uuid
FROM vaccination.covid19
WHERE
  二回目接種状況 = '接種済み'
  AND 二回目日付 = '2021/08/07';
  -- AND 二回目時間帯 = 'AM1';

-- ---------- ---------- ---------- ---------- ---------- ---------- ---------- ---------- ---------- ----------
/* UPDATE vaccine lot number and doctor's name based on status, date and timeslot */

UPDATE vaccination.covid19
SET
  二回目ワクチンロット = '3004232',
  二回目医師名 = 'Doctor'
WHERE
  二回目接種状況 = '接種済み' AND
  二回目日付 = '2021/08/07'::date
  AND 二回目時間帯 = 'AM3';

-- ---------- ---------- ---------- ---------- ---------- ---------- ---------- ---------- ---------- ----------
/* UPDATE coupon ticket received date based on status, date, coupon status and timeslot */

UPDATE vaccination.covid19
SET
  クーポン券受取日 = '2021/08/07'
WHERE
  二回目接種状況 = '接種済み'
  AND 二回目日付 = '2021/08/07'::date
  AND クーポン券状況 = 'あり' ;
    -- AND 二回目時間帯 = 'PM3';

/*
UPDATE vaccination.covid19
SET
  二回目接種状況 = '接種済み'
WHERE
  二回目日付 = '2021/08/06'::date
  AND クーポン券状況 IS NOT NULL ;
*/

/*
UPDATE vaccination.covid19
SET
  クーポン券状況 = 'あり'
WHERE
  二回目接種状況 = '接種済み'
  AND 二回目日付 = '2021/08/07'::date;
*/