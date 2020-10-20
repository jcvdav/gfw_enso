WITH
  eez_info AS (
  SELECT
    CAST(eez_id AS STRING) AS eez_id,
    territory1_iso3 AS eez_iso3
  FROM
    `world-fishing-827.gfw_research.eez_info`),
  #
  #
  #
  #
  ########
  vessel_info AS (
  SELECT
    ssvid,
    year,
    best.best_flag,
    best.best_vessel_class,
    best.best_engine_power_kw
  FROM
    `world-fishing-827.gfw_research.vi_ssvid_byyear_v20190430`
  WHERE
    best.best_vessel_class IN ("drifting_longlines",
      "tuna_purse_seines",
      "trawlers")
    AND on_fishing_list_best
    AND best.best_flag IS NOT NULL
    AND activity.offsetting IS FALSE),
  #
  #
  #
  #
  ########
  tracks AS (
  SELECT
    ssvid,
    EXTRACT(YEAR
    FROM
      _Partitiontime) AS year,
    EXTRACT(MONTH
    FROM
      _Partitiontime) AS month,
  IF
    (ARRAY_LENGTH(regions.eez) = 0,
      "0000",
      regions.eez[ORDINAL(1)]) AS eez_id,
    FLOOR(lat/0.1)*0.1 + 0.05 AS lat_bin_center,
    FLOOR(lon/0.1)*0.1 + 0.05 AS lon_bin_center,
  IF
    (nnet_score > 0.5,
      "TRUE",
      "FALSE") AS is_fishing,
    SUM(hours) AS hours
  FROM
    `world-fishing-827.gfw_research.pipe_production_b_fishing`
  WHERE
    ssvid IN (
    SELECT
      DISTINCT(ssvid)
    FROM
      vessel_info)
    AND seg_id IN (
    SELECT
      seg_id
    FROM
      `world-fishing-827.gfw_research.pipe_production_b_segs`
    WHERE
      good_seg)
    AND distance_from_shore_m > 1000
    AND distance_from_port_m > 1000
  GROUP BY
    ssvid,
    year,
    month,
    eez_id,
    lat_bin_center,
    lon_bin_center,
    is_fishing),
  #
  #
  #
  #
  ########
  gridded_by_vessel AS (
  SELECT
    year,
    month,
    ssvid,
    best_flag,
    best_vessel_class,
    eez_id,
    eez_iso3,
    ROUND(lon_bin_center, 2) AS lon_bin_center,
    ROUND(lat_bin_center, 2) AS lat_bin_center,
    best_flag != eez_iso3 AS is_foreign,
    is_fishing,
    hours,
    hours * best_engine_power_kw AS kilowats
  FROM
    tracks
  LEFT JOIN
    vessel_info
  USING
    (ssvid,
      year)
  LEFT JOIN
    eez_info
  USING
    (eez_id))
  #
  #
  #
  #
  ########
SELECT
  year,
  month,
  best_vessel_class,
  eez_id,
  eez_iso3,
  lon_bin_center,
  lat_bin_center,
  is_foreign,
  is_fishing,
  SUM(hours) AS hours,
  SUM(kilowats) AS kilowats
FROM
  gridded_by_vessel
WHERE eez_iso3 IS NOT NULL
GROUP BY
  year,
  month,
  best_vessel_class,
  eez_id,
  eez_iso3,
  lon_bin_center,
  lat_bin_center,
  is_foreign,
  is_fishing