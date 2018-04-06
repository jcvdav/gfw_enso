-- TEST_illegal_fishing_effort_CHN.sql
-- on GBQ: https://bigquery.cloud.google.com/savedquery/680615692052:4434695c97d3405ba9155a182ca1264c
-- This query extracts all CHINA activity, labeling points as foreign if boat flag != eez

SELECT
  mmsi,
  YEAR(timestamp) as year,
  MONTH(timestamp) as month,
  timestamp,
  flag_iso3,
  eez_iso3,
  geartype,
  IF(flag_iso3 != eez_iso3 AND eez_iso3 IS NOT NULL, "TRUE", "FALSE") as is_foreign,
  IF(nnet_score2 = 1, "TRUE", "FALSE") AS fishing,
  SUM(hours) AS hours
FROM
  [world-fishing-827:gfw_research.nn2]
WHERE
  _PARTITIONTIME >= "2012-01-01 00:00:00"
  AND _PARTITIONTIME < "2018-04-06 00:00:00"
  AND flag_iso3 = "CHN"
  AND seg_id IN (SELECT seg_id
                 FROM [world-fishing-827:gfw_research.good_segments])
  GROUP BY mmsi, year, month, timestamp, flag_iso3, eez_iso3, geartype, is_foreign, fishing