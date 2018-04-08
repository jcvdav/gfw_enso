-- TEST_illegal_fishing_effort_CHN.sql
-- on GBQ: https://bigquery.cloud.google.com/savedquery/680615692052:4434695c97d3405ba9155a182ca1264c
-- This query extracts all CHINA activity, labeling points as foreign if boat flag != eez

SELECT
  YEAR(A.timestamp) as year,
  CASE
      WHEN MONTH(timestamp) IN (12, 1, 2) THEN "winter"
      WHEN MONTH(timestamp) IN (4, 5, 6) THEN "spring"
      WHEN MONTH(timestamp) IN (7, 8, 9) THEN "summer"
      ELSE "fall"
  END AS season,
  MONTH(A.timestamp) as month,
  A.mmsi as mmsi,
  B.inferred_label as inferred_label,
  B.best_label as best_label,
  B.iso3 as iso3,
  A.eez_iso3 as eez_iso3,
  IF(B.iso3 != A.eez_iso3 AND A.eez_iso3 IS NOT NULL, "TRUE", "FALSE") as is_foreign,
  IF(A.nnet_score2 = 1, "TRUE", "FALSE") AS fishing,
  SUM(A.hours) AS hours
FROM
  [world-fishing-827:gfw_research.nn7] as A
INNER JOIN (
  SELECT mmsi,
         iso3,
         best_label,
         inferred_label
  FROM [world-fishing-827:gfw_research.vessel_info]
  WHERE
       iso3 IN ("CHN", "TWN", "PRK", "JPN", "KOR", "NZL", "AUS"))  as B
  ON A.mmsi = B.mmsi
WHERE
      A.seg_id IN (SELECT seg_id
                   FROM [world-fishing-827:gfw_research.good_segments])
  GROUP BY year, season, month, mmsi, iso3, eez_iso3, inferred_label, best_label, is_foreign, fishing
  ORDER BY year, season, month, mmsi, iso3, eez_iso3, inferred_label, best_label, is_foreign, fishing, hours