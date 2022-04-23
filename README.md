# MB2_Project_SST_Response_TropicalCyclones

Project work for MB2: Introduction to Programming and Geostatistics by Larissa Gorzawski
EAGLE M.SC. Uni Würzburg

This short university project takes a closer look at tropical cyclone track data from NOAA's International Best Track Archive for Climate Stewardship (IBTrACS)[1,2] during the Atlantic Hurricane Season 2020. It was the most active atlantic hurricane season on record, with 14 hurricanes [3].
For these hurricanes, the well-known cooling of the ocean surface in a tropical cyclone's wake [4] will be investigated and visualized with daily MODIS Sea Surface Temperature (SST) data. The dataset can be accessed until June 2022 via the PODAAC drive (https://podaac-tools.jpl.nasa.gov/drive/login?dest=L2RyaXZlLw) and directly retrieved or copied from the script. For convenience, the data for this project can also be downloaded from here: https://drive.google.com/drive/folders/1hNh0-q-amZt-sN3UMoLNtblkW9OB-WrH?usp=sharing

The methods include the calculation of a temperature difference raster between pre- and post-storm conditions in a 500km radius around every track point and a following investigation of the SST response relative to the distance to the storm's center as well as the mean SST response within the maximum wind radius during the storm's progression. This was investigated exemplay for hurricane Teddy and then applied on all 2020 hurricanes. Methods are partly and freely adapted from [4,5,6].

References:
1. Knapp, K. R., M. C. Kruk, D. H. Levinson, H. J. Diamond, and C. J. Neumann, 2010: The International Best Track Archive for Climate Stewardship (IBTrACS): Unifying tropical cyclone best track data. Bulletin of the American Meteorological Society, 91, 363-376. doi:10.1175/2009BAMS2755.1
2. Knapp, K. R., H. J. Diamond, J. P. Kossin, M. C. Kruk, C. J. Schreck, 2018: International Best Track Archive for Climate Stewardship (IBTrACS) Project, Version 4. [indicate subset used]. NOAA National Centers for Environmental Information. doi:10.25921/82ty-9e16 [access date].
3. https://www.noaa.gov/media-release/record-breaking-atlantic-hurricane-season-draws-to-end
4. Dare, R. A., & McBride, J. L. (2011). Sea surface temperature response to tropical cyclones. Monthly Weather Review, 139(12), 3798-3808.
5. Monaldo, F. M., Sikora, T. D., Babin, S. M., & Sterner, R. E. (1997). Satellite imagery of sea surface temperature cooling in the wake of Hurricane Edouard (1996). Monthly Weather Review, 125(10), 2716-2721.
6. Reul, N., Chapron, B., Grodsky, S. A., Guimbard, S., Kudryavtsev, V., Foltz, G. R., & Balaguru, K. (2021). Satellite observations of the sea surface salinity response to tropical cyclones. Geophysical research letters, 48(1), e2020GL091478.
