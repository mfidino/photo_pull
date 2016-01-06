##############################
#
#
# Utility script for format_exif_output.R
#
# By Mason Fidino
# 1/6/2015
#
#

# change working directory to photopull
setwd("C:/Users/mfidino/Documents/GitHub/photo_pull")

# Source format_exif_output.R
source("format_exif_output.R")

### STOP HERE STOP HERE STOP HERE STOP HERE STOP HERE STOP HERE
### STOP HERE STOP HERE STOP HERE STOP HERE STOP HERE STOP HERE
### STOP HERE STOP HERE STOP HERE STOP HERE STOP HERE STOP HERE


### we should now have picrec ready to be put into access, 
### but now we need to get the picatt
### ready. To do this, we need to generate the picture ID

#  merge the picatt_list
picatt_all <- rbind.fill(picatt_list)

### before running this function you need to determine what
### the starting number is. Look this up in access
picatt_all$PictureID <- create_pic_id(start = 220084, 
                                      sum(n_correct), unlist(freq_list))

# save this table
write.table(picatt_all, paste(the_path, "/../formatted_for_access/", "picatt_",dd, ".txt", sep = ""),
            append = FALSE, row.names = FALSE, col.names = TRUE, sep = "\t")