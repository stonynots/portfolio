/* Clear a field 二回目数え順 (2nd sort order) as NULL */
UPDATE vaccination.covid19
	SET 二回目_数え順 = NULL;

/* Update using ROW_NUMBER () */
UPDATE vaccination.covid19 AS alpha
	SET 二回目_数え順 = charlie.beta
	/* Subquery named as charlie */
	FROM (
		SELECT
			uuid,
			ROW_NUMBER()
				OVER (
					PARTITION BY
						二回目_日付,
						二回目_時間帯
					ORDER BY
						二回目_日付,
						二回目_時間帯,
						-- 二回目接種状況 DESC,
						CASE
							WHEN (二回目_接種状況 = '接種済み') THEN 1
							WHEN (二回目_接種状況 = '予約') THEN 2
							WHEN (二回目_接種状況 = 'スーパーリザーブ') THEN 3
							WHEN (二回目_接種状況 = 'キャンセル') THEN 4
							ELSE 5
						END,
						CASE
            	WHEN (covid19."グループ" = '学生'::text) THEN 1
           	 WHEN (covid19."グループ" = '教職員'::text) THEN 2
            	WHEN (covid19."グループ" = 'その他'::text) THEN 3
            	ELSE 4
        		END,
        		学籍職員番号
				) AS beta
		FROM vaccination.covid19
		/*
		ORDER BY
			二回目日付,
			二回目時間帯,
			二回目接種状況
		*/
	) AS charlie
	WHERE
		alpha.uuid = charlie.uuid
		AND alpha.二回目_日付 IS NOT NULL
		AND (alpha.二回目_接種状況 <> 'キャンセル')
		AND (alpha.二回目_接種状況 <> '承認待ち')
		AND (alpha.二回目_接種状況 <> 'キャンセル済')
		AND (alpha.二回目_接種状況 <> '延期')
		AND (alpha.二回目_接種状況 <> '辞退');