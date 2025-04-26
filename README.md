# local_housebuilding_1946_2023
Information about local authority housebuilding data in England from 1946-2023, digitised and compiled in 2024 by Centre for Cities. 

*Note - April 2025 updates add an extra year and make minor changes to housebuilding data workbooks and relevant scripts, following a decision to use MHCLG Table 123 as the source of total new build housebuilding from 2012-2023.This results in a small uplift to private and total housebuilding, mostly in urban areas, for the years affected. All historical data remains unchanged as a result of this update. See diagram in Folder 1 for clarity on data sources.*

## Description 
This repository describes the data Centre for Cities can make available following data work carried out for the 2024 Restarting Housebuilding series: 

Planning reform and the private sector: https://www.centreforcities.org/publication/restarting-housebuilding-planning-reform-and-the-private-sector/

Social housing and the public sector: https://www.centreforcities.org/publication/restarting-housebuilding-ii-social-housing-and-the-public-sector/

New towns and land value capture: https://www.centreforcities.org/publication/restarting-housebuilding-iii-new-towns-and-land-value-capture/

*Note: workbooks and scripts matched exactly to those used for the above reports are now in the Archive folder, following minor April 2025 updates*

 Data available is as follows:
 
- Folder 1: Output workbooks - Containing total, private/market, public, local authority, housing association annual housebuilding totals, as well as stock estimates and population 1946-2023. The data is available at local authority district level (separate geographies pre- and post-1974), 1981 counties, and England totals. Most users will want to start here.

- Folder 2:'raw' digitised annual housebuilding reports, which provide private and public housebuilding totals at local authority level from 1946-2000. Public housebuilding data can be separated into local authority and housing associations after 1962. 

- Folder 3: R scripts used to knit these annual reports together, and combine them with other datasets to produce stock estimates and continuous datasets to the present-day in the output workbooks.

- Folder 4: Data on housebuilding, public and private, by new town development corporations between 1950 and 1992.

- Folder 5: Geopackages containing the maps for novel geographies used in this data series, for pre-1974 local authorities and 1981 counties. They are built using original maps from the ONS Open Geography Portal and released under the Open Government Licence v3.0.

## How we got this data 
This data is mostly the product of a digitisation project led by the Centre for Cities in 2024. 

The London School of Economics Digital Library provided us with scans of housebuilding reports from 1946 through to 2000. They have now been uploaded to the LSE Online library here: 

https://lse-atom.arkivum.net/uklse-dl1eh01008 

https://lse-atom.arkivum.net/uklse-dl1eh01009

https://lse-atom.arkivum.net/uklse-dl1eh01010

We digitised this data using a combination of OCR and manual data entry and checking. The digitised data is now free of any noticeable errors. Where there were obvious errors in the source data, this was corrected as best made sense given contextual data. Small errors may have slipped through the checking process, so digitised and scanned data may not 100% match. 

We have compiled this data into useable workbooks and attached it to publicly available Government data on housebuilding, housing stock and population so that the data runs up to the present day.   

## Using this data 
### Dependencies 
R, Microsoft Excel 

### Get quickly into the data 
The easiest way to make the most of this dataset is to use the ready-made workbooks available in Folder 1. 

File names give first the geography, then the content of the workbooks. The ReadMe in the folder details the data used and notes any reasons the data may be unreliable for particular geographies. 

Users can also download geopackages to map data at pre-1974 local authority and county-level geographies. Users wanting to map 2023 local authorities can find the appropriate geopackage here: 
https://www.data.gov.uk/dataset/288458f7-7789-47d0-80d4-ffdf746c6b75/local-authority-districts-december-2023-boundaries-uk-bfe 

### Replicate our work 
If users wish, they can replicate Centre for Cities' work using the files made available here and Government data sources. To do so, users should: 
- download all the files in Folders 2 and 6.
- download the R scripts from Folder 3, and edit all file names and pathways in the scripts to match their computer and downloaded files
- download Government data sources (see below).  

We anticipate that doing this would be a lot of work - our intention in uploading raw files and R scripts is more for transparency purposes than because we think replicating the work would be worth anyone's time! 

### External data sources
External data is gathered from the Ministry of Housing, Communities and Local Government, and the Office for National Statistics. All below links are functional at the time of writing but may be updated by MHCLG and the ONS as they release new data.

Population Estimates for England and Wales: https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/estimatesofthepopulationforenglandandwales

Historic Census data on 'household spaces' and 'households': NOMIS - https://www.nomisweb.co.uk/ 

MHCLG Table 123: 
https://www.gov.uk/government/statistical-data-sets/live-tables-on-net-supply-of-housing

MHCLG Table 253: https://www.gov.uk/government/statistical-data-sets/live-tables-on-house-building 

MHCLG Affordable Housing Open Data (processed by Script 0 into useable range of formats): https://www.gov.uk/government/statistical-data-sets/live-tables-on-house-building 

MHCLG Table 125: https://www.gov.uk/government/statistical-data-sets/live-tables-on-dwelling-stock-including-vacants  

## Roadmap

Work to improve this repository & data within includes: 

- Redo 1974-1992 estimates for population and stock at the district level for North Yorkshire, Herefordshire, Worcestershire, and Humberside, which undergo locally significant internal boundary changes in the 1990s. Until then, counties are the best geography for analysing these areas

- Release Rateable Values workbooks in a similar manner on GitHub

- Edit code so it longer produces obsolete columns in the middle of the data series

## Acknowledgements

Thanks to Xuanru Lin, James Evans, Tu Minh Tri, and Addi Haran Diman for their help in building this database. 

Thanks to the LSE Digital Library team for making such high quality scans publicly available and for very helpful answers to our questions early in this research project.

If you have any questions, please contact Maurice Lange: m.lange@centreforcities.org and/or Anthony Breach a.breach@centreforcities.org

Shield: [![CC BY 4.0][cc-by-shield]][cc-by]

This work is licensed under a
[Creative Commons Attribution 4.0 International License][cc-by].

[![CC BY 4.0][cc-by-image]][cc-by]

[cc-by]: http://creativecommons.org/licenses/by/4.0/
[cc-by-image]: https://i.creativecommons.org/l/by/4.0/88x31.png
[cc-by-shield]: https://img.shields.io/badge/License-CC%20BY%204.0-lightgrey.svg
