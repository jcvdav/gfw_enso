################################################################
################    monthly_vessel_activity     ################
################################################################
# This query produces a panel showing the activity for every
# vessel. It produces a table with the following colunms:
# - year: Year, ranging from 2012 - 2019
# - month: Month in numeric format
# - ssvid: The vessel identifier
# - flag: Vessel's flag
# - gear: Gear, one of: trawlers, tuna_purse_seines, drifting_longlines
# - engine_power_kw: Power of the engine
# - is_foreign: are the flag and eez the different
# - hours: hours spent (fishing / non-fishing) in this EEZ for this month
# - fishing_hours: total hours spent fishing (nnet_score >= 0.5)
# - nonfishing_hours: total hours spent not fishing (nnet_score < 0.5)
# - kilowats: total kilowats used
# - fishing_kilowats: kilowats used while fishing
# - nonfishing_kilowats: kilowats used while NOT fishing
################################################################
#
#
#
#
######## Select EEZ information
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
  ######## Select the good segments
  good_segments AS (
  SELECT
    seg_id
  FROM
    `world-fishing-827.gfw_research.pipe_v20190502_segs`
  WHERE
    good_seg
    AND positions > 10
    AND NOT overlapping_and_short),
  #
  #
  #
  #
  #### Find nasty ssvids that spoof
  nast_ssvid AS (
  SELECT
    ssvid,
    SUM( positions) positions
  FROM
    `world-fishing-827.gfw_research.pipe_v20200805_segs` 
  WHERE
    ((dist_avg_pos_sat_vessel_km > 3000
        AND sat_positions_known > 5) )
  GROUP BY
    ssvid
  HAVING
    positions > 50),
  #
  #
  #
  #
  ######## Select relevant vessel info
  vessel_info AS (
  SELECT
    ssvid,
    year,
    best.best_flag,
    best.best_vessel_class,
    best.best_engine_power_kw
  FROM
    `world-fishing-827.gfw_research.vi_ssvid_byyear_v20200312`
  WHERE
    best.best_vessel_class IN ("drifting_longlines",
      "tuna_purse_seines")
    AND on_fishing_list_best
    AND activity.overlap_hours_multinames = 0
    AND activity.overlap_hours < 24*3
    AND activity.active_hours > 24
    AND activity.offsetting IS FALSE
    AND ssvid NOT IN (SELECT ssvid FROM nast_ssvid)
    AND CAST(ssvid AS int64) NOT IN (SELECT ssvid FROM `world-fishing-827.gfw_research.bad_mmsi` CROSS JOIN UNNEST(ssvid) AS ssvid)
    #AND best.best_engine_power_kw IS NOT NULL
    AND best.best_flag IS NOT NULL
    AND best.best_flag != "Unknown"),
  #
  #
  #
  #
  ######## Get the amount of fishing/notfishing hours that each vessel spent on any EEZ and the high seas
  position_table AS (
  SELECT
    DISTINCT ssvid,
    EXTRACT(YEAR
    FROM
      timestamp) AS year,
    EXTRACT(MONTH
    FROM
      timestamp) AS month,
  IF
    (ARRAY_LENGTH(regions.eez) = 0,
      "0000",
      regions.eez[ORDINAL(1)]) AS eez_id,
      nnet_score,
      hours
  FROM
    `world-fishing-827.gfw_research.pipe_v20200805_fishing`
  CROSS JOIN UNNEST(regions.ocean) AS ocean
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
      good_segments)
    AND ocean = "pacific"
    AND timestamp < TIMESTAMP("2019-12-31")),
      #
      #
      #
      #
      ######## Add vessel info data to the actyivity data
  activity_with_vessel_and_eez_info AS (
  SELECT
    ssvid,
    best_flag,
    best_vessel_class,
    year,
    month,
    IF (eez_iso3 is NULL, "HS_", eez_iso3) AS eez_iso3,
    eez_iso3 != best_flag AS is_foreign,
    best_engine_power_kw,
    nnet_score,
    hours
  FROM
    position_table
  LEFT JOIN
    vessel_info
  USING
    (ssvid,
      year)
  LEFT JOIN
    eez_info
  USING
    (eez_id)),
  #
  #
  #
  #
  ########
panel AS(
  SELECT
    ssvid,
    best_flag,
    best_vessel_class,
    year,
    month,
    best_engine_power_kw,
    is_foreign,
    SUM(hours) AS hours,
    SUM(IF (nnet_score >= 0.5, hours, 0)) AS fishing_hours,
    SUM(IF (nnet_score < 0.5 OR nnet_score IS NULL, hours, 0)) AS nonfishing_hours
  FROM activity_with_vessel_and_eez_info
  GROUP BY
    ssvid,
    best_flag,
    best_vessel_class,
    year,
    month,
    is_foreign,
    best_engine_power_kw)
  #
  #
  #
  #
  ########
SELECT
  year,
  month,
  ssvid,
  best_flag AS flag,
  best_vessel_class AS gear,
  best_engine_power_kw AS engine_power_kw,
  is_foreign,
  hours,
  fishing_hours,
  nonfishing_hours,
  hours * best_engine_power_kw AS kilowathours,
  fishing_hours * best_engine_power_kw AS fishing_kilowatthours,
  nonfishing_hours * best_engine_power_kw AS nonfishing_kilowatthours
FROM
  panel