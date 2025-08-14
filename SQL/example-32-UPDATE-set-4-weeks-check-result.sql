UPDATE vaccination.covid19 
SET 二回目_チェック_四週間後経過日 =
	CASE 
		WHEN 二回目_チェック_四週間後目安日付 > 二回目_日付 
			THEN '接種できません！四週間未満'
		WHEN 二回目_チェック_四週間後目安日付 <= 二回目_日付 
			THEN '接種 OK 四週間経過'
		ELSE '二回目日付が未定'
	END
WHERE 二回目_接種状況 IS NOT NULL;