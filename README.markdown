A ruby file that makes it possible to parse through camera trap photos for the [Biodiversity Monitoring project](http://www.lpzoo.org/conservation-science/projects/urban-wildlife-biodiversity-monitoring), as well as some R scripts to properly format those outputs. 

The goal of this program is to scrape the photo exif data to collect date, time, and tagged keywords (species) that were either collected by the camera trap or written to the files via photoshop.  We also collect the file path, picture name, and the site specific information from the file path.

The R scripts sets up most everything else so that we can easily upload these data into our access database.

This is made solely for our own purposes (the ruby and R scripts are tailor made to our naming nomenclature), but could be easily adapted to other uses.
