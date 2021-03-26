# coorconvert

[![](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)

Simple coordinates converter for the area of Poland.

Applications allows to convert coordinates between different systems for Poland." Only csv and tsv input files are accepted. with the columns names in the first row:

-   label
-   lon
-   lat

where *lon* is longitude and *lat* is latitude.

Example for the option *WGS84 - World Geodetic System 1984 EPSG: 4326 Format: dms*

| label         | lon      | lat      |
|---------------|----------|----------|
| 00567-2009-01 | 162526.9 | 542649.9 |
| 00567-2009-02 | 162501.5 | 542639.1 |

Example for the option *WGS 84 / UTM zone 33N EPSG: 32633*

| label         | lon    | lat     |
|---------------|--------|---------|
| 00567-2009-01 | 592345 | 6034212 |
| 00567-2009-02 | 591894 | 6033869 |

A .csv file with converted coordinates can be downloaded as .csv.

Download is possible in .csv format.

### Acceptable formats for geographical coordinates

-   Format d: 16.4241, 54.4472
-   Format dms: 162526.9, 542649.9
