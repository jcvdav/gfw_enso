-- !Create girdded effort data

SELECT
year, lat_bin_center, lon_bin_center, month, iso3, eez_iso3, best_label, is_foreign, SUM(hours) as hours
FROM(
  SELECT
  FLOOR(A.lat/0.1)*0.1 + 0.5*0.1 as lat_bin_center,
  FLOOR(A.lon/0.1)*0.1 + 0.5*0.1 as lon_bin_center,
  EXTRACT(YEAR FROM timestamp) AS year,
  EXTRACT(MONTH FROM timestamp) as month,
  A.mmsi as mmsi,
  B.inferred_label_allyears as best_label,
  B.mmsi_iso3 as iso3,
  A.eez_iso3 as eez_iso3,
  IF(B.mmsi_iso3 != A.eez_iso3 AND A.eez_iso3 IS NOT NULL, "TRUE", "FALSE") as is_foreign,
  IF(A.nnet_score > 0.5, "TRUE", "FALSE") AS fishing,
  SUM(A.hours) as hours
  FROM
  `world-fishing-827.gfw_research.nn` as A
  INNER JOIN (
    SELECT
    mmsi,
    year,
    mmsi_iso3,
    inferred_label_allyears,
    inferred_label
    FROM `world-fishing-827.gfw_research.vessel_info_20181002`
    WHERE
    mmsi_iso3 IN ("CHN", "TWN", "PRK", "JPN", "KOR", "NZL", "AUS", "USA", "ESP", "GBR", "PRT", "MEX", "CHL", "PER"))  as B
    ON (A.mmsi = B.mmsi AND year = B.year)
    GROUP BY year, lat_bin_center, lon_bin_center, month, mmsi, iso3, eez_iso3, best_label, is_foreign, fishing)
  WHERE fishing = "TRUE"
  GROUP BY year, lat_bin_center, lon_bin_center, month, iso3, eez_iso3, best_label, is_foreign