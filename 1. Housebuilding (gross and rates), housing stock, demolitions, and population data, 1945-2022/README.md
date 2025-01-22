This release contains data on gross housebuilding and demolitions, housebuilding rates, population, and housing stock estimates, reported annually from 1945 to 2022. Data is available at local authority (two geographies, pre- and post-1974), county (1981 boundaries), and England level.

These workbooks are the product of R scripts (also shared on this GitHub page) which knit together the digitised housebuilding data 1945-2000, and add Government data to produce continuous estimates up to 2022.

General notes:

- Housebuilding data is reported by tenure: private and public for the whole period. Public housebuilding is separated into local authority and housing association housebuilding when available after 1962.
- Demolition data is only available from housebuilding reports 1956-1979.
- Housing stock data are estimates, not actual counts. They are constructed using Census data at each decade and net housebuilding (gross only where demolitions are not available) data, and adjust for resulting discrepancies. As stocks are mostly determined by construction and demolition, they offer a good approximation of how housing stock changed in each place annually. Changes to housing stock not caused by demolition or construction, e.g. splitting an existing dwelling, are not captured directly. Any discrepancy between Census data and estimates between years are adjusted for, with the adjustment applied to each intervening year. The estimates therefore can't capture how other causes of housing stock change vary within decades.
- Housebuilding rates are calculated by dividing gross housebuilding reported in year X by housing stock estimate for year X-1.

Notes on post-1974 data:

- Data is reported at 2023 local authority boundaries
- Digitised annual housebuilding reports are the base of our housebuilding data until 2000.
- Where housebuilding reports have consecutive years where building was not recorded between 1980 to 2000, MHCLG Table 253 data is used to plug these gaps. All other housebuilding data from 1980 to 2000 is taken from the digitised housebuilding reports. Note that these do not exactly match data in Table 253. We cannot explain the discrepancy between datasets, which should in theory report the same data. Any discrepancies result from differences between source data, not errors in the digitisation of annual housebuilding reports.
- From 1991 onward, public housebuilding at local authority level taken from Table 1011. After 2000, MHCLG Table 253 is used for private housebuilding local authority level housebuilding data. Housing stock is from MHCLG Table 125 (unrounded). Population data is from ONS after 1992.
- Where data is missing from MHCLG Table 253, we fill gaps by interpolating.
- Minor boundary changes in the 1990s are not reflected in the estimates. The effect of this will be very small everywhere, except in three places - York (which expanded outwards), Herefordshire and Worcestershire, and Boothferry in Humberside. In these three cases in particular, the 1981 county geography is the best option for local estimates.

Notes on pre-1974 data:

- Data is reported at 1971 local authority level, with minor adjustments. Where boundary changes between years are not straightforward merges, we create best-estimates by grouping local authorities together. A shapefile for the used pre-1973 geographies is also uploaded to this GitHub page.
- Where data is only available for the first 9 months (some local authorities, some years), reported data is divided by 0.75 to provide an estimate for the whole year.
- Building estimates for 1947-1949 and 1952 are interpolations, based on the difference between 1946 and 1950 data, and 1951 and 1953 respectively. The difference is not split evenly between intervening years, but distributed according to country-wide changes in building in each year (this results in all authorities reporting more building in 1948 than 1947 for example, but the total change varies between authorities because difference varies between authorities). In future, this could be improved by digitising local authority data for these years and re-writing scripts to pull this data in.

Notes on 1981 Counties geography:

1981 county boundaries are chosen as the best geography to enable analysis over the entire period. Most local authority districts at pre- and post-1974 geographies fit exactly into 1981 counties. There are some minor cases where this isn't the case before and (apart from York, Herefordshire, and Boothferry as mentioned) to a lesser degree after 1974. Users can look at these discrepancies using the two shapefiles provided.
