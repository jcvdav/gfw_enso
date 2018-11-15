-- !Last run on 14/11/2018 to select fishing and non-fishing hours by vessel

SELECT
  EXTRACT(YEAR FROM timestamp) AS year,
  CASE
      WHEN EXTRACT(MONTH FROM timestamp) IN (1, 2, 3) THEN "winter"
      WHEN EXTRACT(MONTH FROM timestamp) IN (4, 5, 6) THEN "spring"
      WHEN EXTRACT(MONTH FROM timestamp) IN (7, 8, 9) THEN "summer"
      ELSE "fall"
  END AS season,
  EXTRACT(MONTH FROM timestamp) as month,
  A.mmsi as mmsi,
  B.inferred_label as inferred_label,
  B.best_label as best_label,
  B.mmsi_iso3 as iso3,
  A.eez_iso3 as eez_iso3,
  IF(B.mmsi_iso3 != A.eez_iso3 AND A.eez_iso3 IS NOT NULL, "TRUE", "FALSE") as is_foreign,
  IF(A.nnet_score2 > 0.5, "TRUE", "FALSE") AS fishing,
  SUM(A.hours) AS hours
FROM
  `world-fishing-827.gfw_research.nn7` as A
INNER JOIN (
  SELECT
    mmsi,
    year,
    mmsi_iso3,
    best_label,
    inferred_label
  FROM `world-fishing-827.gfw_research.vessel_info_20181002`
  WHERE
       MMSI_iso3 IN ("CHN", "TWN", "PRK", "JPN", "KOR", "NZL", "AUS", "USA", "ESP", "GBR", "PRT", "MEX", "CHL", "PER"))  as B
  ON (A.mmsi = B.mmsi AND year = B.year)
WHERE
      A.seg_id IN (SELECT seg_id
                   FROM `world-fishing-827.gfw_research.good_segments`)
  GROUP BY year, season, month, mmsi, iso3, eez_iso3, inferred_label, best_label, is_foreign, fishing
  ORDER BY year, season, month, mmsi, iso3, eez_iso3, inferred_label, best_label, is_foreign, fishing, hours
