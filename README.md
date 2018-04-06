# GFW_ENSO

## Repo structure

```
gfw_enso:
├───data
├───docs
├───raw_data
│   ├───GFW
│   └───indices
└───scripts
```
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

### TEST_illegal_fishing_effort_CHN.csv

Description: Obtained througj GFW, contains all CHN vessels

Source: GFW, see TEST_illegal_fishing_effort_CHN.sql for the query used
