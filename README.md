# GFW_ENSO


## Repository structure 

```
-- data
   |__all_indices.csv
   |__asian_countries_foreign_fishing_year_month_season_vessel.rds
   |__foreign_fishing_year_month_season_vessel.rds
   |__gridded_ff_by_gear_country.rds
   |__masked_aggregated_raster.rds
   |__month_rasters.rds
   |__sst.csv
   |__sst_df_whole.rds
   |__sst_nino3_df.csv
   |__sst_nino3_df.rds
-- desktop.ini
-- docs
   |__CHN_ff.html
   |__CHN_ff.Rmd
   |__CHN_ff_for_Cody.pdf
   |__CHN_ff_for_Cody.Rmd
   |__CHN_ff_for_Cody_top10.pdf
   |__CHN_ff_for_Cody_top10.Rmd
   |__DID.tex
   |__nar.bst.txt
   |__nature.bst
   |__Oremus_Villasenor-Derbez.pdf
   |__Oremus_Villasenor-Derbez.Rmd
   |__Oremus_Villasenor-Derbez.tex
   |__Oremus_Villasenor-Derbez_files
   |__plos2015.bst
   |__references.bib
   |__test.html
   |__test.pdf
   |__test.Rmd
-- gfw_enso.Rproj
-- git_repo
-- img
   |__unnamed-chunk-10-1.pdf
   |__unnamed-chunk-12-1.pdf
   |__unnamed-chunk-13-1.pdf
   |__unnamed-chunk-3-1.pdf
   |__unnamed-chunk-5-1.pdf
   |__unnamed-chunk-6-1.pdf
   |__unnamed-chunk-7-1.pdf
   |__unnamed-chunk-8-1.pdf
   |__unnamed-chunk-9-1.pdf
-- raw_data
   |__foreign_fishing_year_month_season_vessel.rds
   |__GFW
   |__gridded_ff_by_gear_country.rds
   |__indices
   |__spatial
-- README.md
-- scripts
   |__cor_ff_nino3.R
   |__cor_sst_nino3.R
   |__download_foreign_fishing_year_month_season_vessel.R
   |__download_gridded_ff_by_gear_country.R
   |__foreign_fishing_year_month_season_vessel.sql
   |__gridded_ff_by_gear_country.sql
   |__join_all_indices.R
   |__setup.R
   |__TEST_illegal_fishing_effort_CHN.sql
-- slides
   |__css
   |__libs
   |__meeting_with_chris.html
   |__meeting_with_chris.Rmd
   |__meeting_with_chris_files
-- writing
   |__DID.tex
   |__paper.aux
   |__paper.bbl
   |__paper.blg
   |__paper.log
   |__paper.pdf
   |__paper.synctex.gz
   |__paper.tex
   |__references.bib
```

--------- 

## Data sources

### soi_3dp

Description: SOI index with three decimal places. Con column per month, last column is yearly SOI value.

Source: https://www.esrl.noaa.gov/psd/gcos_wgsp/Timeseries/SOI/

### nino3.long.anom

Description: Nino 3 index, with mean 1951 - 2000 removed (i.e. anomaly)

Source: https://www.esrl.noaa.gov/psd/gcos_wgsp/Timeseries/Nino3/

### nino3.long

Description: Nino 3 index, with mean 1951 - 2000 NOT removed

Source: https://www.esrl.noaa.gov/psd/gcos_wgsp/Timeseries/Nino3/

### nino4.long.anom

Description: Nino 4 index, with mean 1951 - 2000 removed

Source: https://www.esrl.noaa.gov/psd/gcos_wgsp/Timeseries/Nino4/

### nino4.long

Description: Nino 4 index, with mean 1951 - 2000 NOT removed

Source: https://www.esrl.noaa.gov/psd/gcos_wgsp/Timeseries/Nino4/

### all_indices.csv

Description: Combined all indices into a tidy format. Each column is one index, and the file includes a column for year, month and date

Source: Combination of all files above

--------- 

<a href="https://orcid.org/0000-0003-1245-589X" target="orcid.widget" rel="noopener noreferrer" style="vertical-align:top;"><img src="https://orcid.org/sites/default/files/images/orcid_16x16.png" style="width:1em;margin-right:.5em;" alt="ORCID iD icon">orcid.org/0000-0003-1245-589X</a>
