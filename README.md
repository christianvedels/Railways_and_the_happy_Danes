# Railways and the happy Danes

## Structure
000_Function.R contains a colleciton of funcitons used across the project. 
001_Read_data.R* reads data from various sources. 
002_Data_clean.R* rough cleaning and aggregaiton of data. 

*These scripts cannot run without access to data we cannot redistribute. You cannot run them without access to this data. 

## Data description

### Parish level population data, geo data, soil data
Files: *'Pop_reg.csv', 'Parish_soil.csv'*
Taken from Vedel (2023). Parish level demographic statistics based on digtized census data from Link Lives, available at www.rigsarkivet.dk/udforsk/link-lives-data/. This also contains HISCO level summaries. Based on Vedel & Dahl (2023). 
Soil types from Pedersen et al (2019) via Vedel (2023).

### Assmebly houses
Files: *'Huse_panel.Rdata', 'key.Rdata'*
Sourced from Bentzen et al (2023). 

### Creameries data
Files: *'MDS_DM_merge_w_add_data.csv'*
Sourced from Bentzen et al (2023) and Sharp et al (2023)

### Højskoler (Grundtvigian High Schools)
Files: *'Hoejskoler_clean_panel.csv'*

### Geographical stuff
Files: *'shape_parishes', 'Geo.csv',*
'Sogne' tranlates to parishes. This is the shape file of Danish parishes as of January 1, 1820. Which was passed on to this project from the authors of Boberg-Fazlic et al (2023). Originally this comes from www.digdag.dk. The market town of Lemvig was missing and added manually using borders downloaded directly from www.digdag.dk in Vedel (2023).

### OxRoads
Data on the distances from ox roads comes from Boberg-Fazlic et al (2023).

## References
Bentzen, J., Boberg-Fazlic, N., Sharp, P., Vedel, C. (2023) Holy Cows and Spilt Milk: A Firm Level Analysis of the Impact of Religiosity on Productivity (UNPUBLISHED MANUSCRIPT)

Boberg-Fazlic, N., Jensen, P.S., Lampe, M. et al. 'Getting to Denmark': the role of agricultural elites for development. J Econ Growth (2023). <https://doi.org/10.1007/s10887-023-09226-8>

Dahl, C. M., Vedel, C. (2023). Breaking the HISCO Barrier: AI and Occupational Data Standardization. (UNPUBLISHED MANUSCRIPT). https://sites.google.com/view/christianvedel

Klint, Thorkil, 2023, "The Danish Legislator Database (DLD)", https://doi.org/10.7910/DVN/4SFNC0, Harvard Dataverse, V1

Pedersen, S., Hermansen, B., Nathan, C., & Tougaard, L. (2019). Surface geology map of Denmark 1:200 000, version 2. GEUS. https://eng.geus.dk/products-services-facilities/data-and-maps/maps-of-denmark

Sharp, P., Henriques, S., McLaughlin, E., Tsoukli, X., & Vedel, C. (2023). A Microlevel Analysis of Danish Dairy Cooperatives: Opportunities for Large Data in Business History. Enterprise & Society, 1-29. doi:10.1017/eso.2023.5 

Vedel, C. (2023), Chapter 1: A Perfect Storm and The Natural Endowments of Infrastructure. *Natural Experiments in Geography and Institutions: Essays in the Economic History of Denmark. [PhD thesis, SDU]*. https://doi.org/10.21996/jt34-zc23



 
