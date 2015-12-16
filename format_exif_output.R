#############################
#
#
# convert parse_photo_exif.rb output for access db
#
# By Mason Fidino
# 12/16/2015
#
#
#########################################################
# packages to load
#########################################################


#########################################################
# some objects to call in the script
#########################################################

### Assume that the output files are in the to_format folder on my computer ###

the_path <- "C:/Users/Mfidino/Documents/GitHub/photo_pull/to_format"

### transects ###

transects <- "RST|JNT|DPT|SCT"

#########################################################################
# some functions to call in the script
#########################################################################

# remove leading and trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

# remove transects in the keywords
drop_transects <- function(x) {
  return(x[-grep(transects, x)] )
}

# add empty if length of keywords in 0
add_empty <- function(x){
  ifelse(length(x) == 0, "Empty", x)
}

# remove anything with a number in the keywords
drop_nums <- function(x){
  return(x[-grep("[0-9]",x)])
}

#####################################################
# locate the files that we want to format
#####################################################

output_files <- list.files (the_path, "output_")

# vector to store number of correct files

n_correct <- rep(0, length(output_files))

# iterate through each output file in to_format
for(i in 1:length(output_files)){
  
  # read in the file, fill = TRUE because not all rows have every element
  # no factors because we are going to some character formatting
    output <- read.table(paste(the_path,"/", output_files[i], sep = ""), sep = "\t",
                       stringsAsFactors = FALSE, header = TRUE, fill = TRUE)
  
  # remove all incorrect file names and corrupt files
    incorrect <- which(output$site_season == "incorrect file name")
    corrupt <- which(output$site_season == "corrupt file")
    
  # write the file paths for incorrect names into incorrect_names subfolder 
    if(length(incorrect)>0){
      write.table(data.frame(wrong = output$path[incorrect]),paste(the_path, "/../incorrect_names/incorrect_naming_",
                output_files[i], sep = ""),row.names = FALSE,sep = "\t")
    }
  # write the file paths for corrupt files into incorrect_names subfolder
    if(length(corrupt)>0){
      write.table(data.frame(corrupt = output$path[corrupt]),paste(the_path, "/../incorrect_names/corrupt_files_",
                                                                 output_files[i], sep = ""),row.names = FALSE,sep = "\t")
    }
  # remove incorrect files now  
    output <- output[-c(incorrect, corrupt),]
  
  # pull the species info out of the keywords column
  # this should be easier if we remove everything else systematically
  # the first thing we need to do is remove the "[" and "]" from the
  # array. These are the first and last characters in each row
    output$keywords <- substr(output$keywords, 2, nchar(output$keywords)-1)
  
  # split all of the keywords by the comma, creates a list
    keys <- strsplit(output$keywords, ",")
  
  # remove the leading and trailing white space from each vector in the list
    keys <- lapply(keys, FUN = trim)
  
  # remove the transect acronyms
    keys <- lapply(keys, FUN = drop_transects)
  
  # remove anything with a number
    keys <- lapply(keys, FUN = drop_nums)
  
  # if length of keys is now 0, add "Empty"
    keys <- lapply(keys, FUN = add_empty)
  
  ### We should now have a list that contains the species in a photo, it still
  ### has multiple elements if there are multiple species.
  
  # Format date and time, but split them into two different parts first
    dtparts <- t(as.data.frame(strsplit(output$date_time, " ")))
    
  # remove the row names from dtparts because they are long and UGLY
    row.names(dtparts) <- NULL
  
  #  reformat the dates for access
    dtparts[,1] <- strftime(strptime(dtparts[,1], "%Y-%m-%d"), "%m/%d/%Y")
  
  # change the column names to the same stuff as in access
    colnames(dtparts) <- c("PictureDate", "PictureTime", "to go")
  
  # drop the 3rd column
    dtparts <- dtparts[,-3]
  
  # make a data.frame for PictureRecord 
    picrec <- data.frame(SurveyID = output$site_season,
                         dtparts,
                         concantenate = paste(output$site_season, output$name, sep = " "),
                         picturename = output$name,
                         Observer = NA)
  
  
  ### Okay, now that we have this, we need to replicate rows 
  ### if there are multiple species.
  
  # get length of character vectors for each photo, >1 = multiple species    
    freq <- sapply(keys, FUN = length)
      # make the pic_attribute data.frame
        picatt <- data.frame(Species = unlist(keys), stringsAsFactors = FALSE)
        picatt$ind <- 1 - as.numeric(picatt$Species == "Empty")
        colnames(picatt)[2] <- "NumberOfIndividuals"


  
  # store number of unique and correct photos
  
    n_correct[i] <- nrow(picrec)  
}