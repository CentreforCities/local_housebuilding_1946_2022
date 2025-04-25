These R scripts knit together local authority annual housebuilding data with Government data on housebuilding and housing stock. These produce the workbooks also published on this GitHub page.

Centre for Cities is releasing these scripts primarily for transparency and educational purposes. There is no need to use these scripts to generate usable local authority housebuilding data - we have also released the workbooks that these scripts create on this GitHub repository.

We anticipate that reading these scripts will mostly be useful for those wanting to understand how we overcame idiosyncracies in the source data, how housing stock estimates are calculated, and how Government data is added to digitised local authority data. The way these scripts do this is not the only possible way to do so (other coders would probably find more efficient ways to write the same scripts!).

Users can download and edit these scripts to use themselves, though they should be aware that to do so, they will need to:

- download digitised annual housebuilding reports (also available on this GitHub repository)
- download additional assets (also available on this GitHub repository)
- download relevant Government data sources
- update file source names and locations

Scripts should be run sequentially because they look for file names starting with today's date. For example, if users want to run script 5, but they have not already run scripts 0-4 that day, scripts 0-4 will need to be run first. Final output workbooks published elsewhere on this repository are the produced by Scripts 4, 5 and 6. 

*Note - scripts updated in April 2025 following revision to ensure best Government data sources are used*
