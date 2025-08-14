/* Update reception time entry */

DELETE FROM vaccination.csv_timeupdate;

UPDATE vaccination.covid19 
  SET 
    二回目受付時間 = csv_timeupdate.time, 
    二回目接種状況 = csv_timeupdate.status
FROM vaccination.csv_timeupdate 
WHERE covid19.uuid = csv_timeupdate.uuid;


