# local_housebuilding_1946_2022
Information about local authority housebuilding data in England from 1946-2022, digitised and compiled in 2024 by Centre for Cities.   

## Description 
This repository describes the data Centre for Cities can make available following data work carried out for the 2024 Restarting Housebuilding series: 

Planning reform and the private sector: https://www.centreforcities.org/publication/restarting-housebuilding-planning-reform-and-the-private-sector/
Social housing and the public sector: https://www.centreforcities.org/event/restarting-housebuilding-social-housing-and-the-public-sector/
New towns and land value capture: https://www.centreforcities.org/publication/restarting-housebuilding-iii-new-towns-and-land-value-capture/

 Data available is as follows:
 
- Output workbooks - Containing total, private, public, local authority, housing association annual housebuilding totals, as well as stock estimates and population 1946-2022. The data is available at local authority district level (separate geographies pre- and post-1974), 1981 counties, and England totals. Most users will want to start here.

- 'raw' digitised annual housebuilding reports, which provide private and public housebuilding totals at local authority level from 1946-2000. Public housebuilding data can be separated into local authority and housing associations after 1962. 

- R scripts used to knit these annual reports together, and combine them with other datasets to produce stock estimates and continuous datasets to the present-day in the output workbooks.

- A workbook containing data on housebuilding, public and private, by new town development corporations between 1950 and 1992 is also available. 

- Geopackages containing the maps for novel geographies used in this data series, for pre-1974 local authorities and 1981 counties. They are built using original maps from the ONS Open Geography Portal and released under the Open Government Licence v3.0.

## How we got this data 
This data is mostly the product of a digitisation project led by the Centre for Cities in 2024. 

The London School of Economics Digital Library provided us with scans of housebuilding reports from 1946 through to 2000. They have now been uploaded to the LSE Online library here: 

https://lse-atom.arkivum.net/uklse-dl1eh01008 

https://lse-atom.arkivum.net/uklse-dl1eh01009

https://lse-atom.arkivum.net/uklse-dl1eh01010

We digitised this data using a combination of OCR and manual data entry and checking. The digitised data is now free of any noticeable errors. Where there were obvious errors in the source data, this was corrected as best made sense given contextual data. Small errors may have slipped through the checking process, so digitised and scanned data may not 100% match. 

We have compiled this data into useable workbooks and attached it to publicly available data on housebuilding, housing stock and population so that the data runs up to the present day.   

## Roadmap

- Redo 1974-1992 estimates for population and stock at the district level for North Yorkshire, Herefordshire, Worcestershire, and Humberside, which undergo locally significant internal boundary changes in the 1990s. Until then, counties are the best geography for analysing these areas

- Release Rateable Values workbooks in a similar manner on GitHub

## Acknowledgements

Thanks to Xuanru Lin, James Evans, and Tu Minh Tri for their help in building this database. 

Thanks to the LSE Digital Library team for making such high quality scans publicly available and for very helpful answers to our questions early in this research project.

If you have any questions, please contact Maurice Lange: m.lange@centreforcities.org and/or Anthony Breach a.breach@centreforcities.org

Shield: [![CC BY 4.0][cc-by-shield]][cc-by]

This work is licensed under a
[Creative Commons Attribution 4.0 International License][cc-by].

[![CC BY 4.0][cc-by-image]][cc-by]

[cc-by]: http://creativecommons.org/licenses/by/4.0/
[cc-by-image]: https://i.creativecommons.org/l/by/4.0/88x31.png
[cc-by-shield]: https://img.shields.io/badge/License-CC%20BY%204.0-lightgrey.svg
