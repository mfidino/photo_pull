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
print("loading plyr package")
library(plyr)

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
print("loading functions")
# remove leading and trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

# remove transects in the keywords
drop_transects <- function(x) {
  if(length(grep(transects, x))==0){
    return(x)
  }else{
      return(x[-grep(transects, x)] )
    }
}

# add empty if length of keywords in 0
add_empty <- function(x){
  ifelse(length(x) == 0, "Empty", x)
}

# remove anything with a number in the keywords
drop_nums <- function(x){
  if(length(grep("[0-9]",x))==0){
    return(x)
  }else{
    return(x[-grep("[0-9]",x)])
    }
}

# remove best of tags
drop_best <- function(x){
  if(length(grep("Best", x, ignore.case = TRUE))==0){
    return(x)
  }else{
    return(x[-grep("Best", x, ignore.case = TRUE)])
  }
}

# modify the pictureid, this happens after we know what
# the ids are for picrec

# start = first id from picrec
# len = nrow of picrec
# freq = # of species in each photo, not replicated
create_pic_id <- function(start = NULL, len = NULL, freq = NULL){
  tosub <- 0
  #what number to start at and how long of a vector to make
  id <- seq(1, len, 1 ) # make the sequence
  id <- id + (start - 1) # give appropriate starting number
  # rep the numbers in freq by themselves
  freq <- rep(freq, times = freq)
  # rle those freqs
  freq <- rle(freq)
  # make a list to put all of the changes
  ans <- vector("list", length = length(freq$values))
  
  for(i in 1:length(ans)){
    # if one species, subtract twosub
    if(freq$values[i] == 1){
      ans[[i]] <-  rep(tosub, freq$lengths[i])
    }
    # if two species, subtract twosub once, then twosub + 1
    if(freq$values[i] == 2){
      # number of photos with 2 species in a row
      n_time <- freq$lengths[i]/2
      # make a list for each photo
      to_fill <- vector("list", n_time)
      # iterate through those photos
      for(j in 1 :n_time){
        to_fill[[j]] <- c(tosub, tosub + 1)
        tosub <- tosub + 1
      }
      # place those photos into ans
      ans[[i]] <- unlist(to_fill)
    }
    if(freq$values[i] == 3){
      n_time <- freq$lengths[i]/3
      to_fill <- vector("list", n_time)
      for(k in 1:n_time){
        to_fill[[k]] <- c(tosub, tosub + 1, tosub + 2)
        tosub <- tosub + 2
      }
      ans[[i]]  <- unlist(to_fill)
    }
    if(freq$values[i] == 4){
      n_time <- freq$lengths[i]/4
      to_fill <- vector("list", n_time)
      for(l in 1:n_time){
        to_fill[[l]] <- c(tosub, tosub + 1, tosub + 2, tosub + 3)
        tosub <- tosub +3
      }
      ans[[i]] <- unlist(to_fill)
    }
    if(i == length(ans)){
      tosub <- 0
      ans <- unlist(ans)
      ans <- id - ans
    }
  }
  return(ans)
}


# for moving files about
my_file_rename <- function(from, to) {
  todir <- dirname(to)
  if (!isTRUE(file.info(todir)$isdir)) dir.create(todir, recursive=TRUE)
  file.rename(from = from,  to = to)
}


#####################################################
# locate the files that we want to format
#####################################################

output_files <- list.files (the_path, "output_")

# a list to store every pic att, will turn into one massive df
picatt_list <- vector("list", length(output_files))

# a list to store frequency for every freq
freq_list <- vector("list", length(output_files))
# vector to store number of correct files

n_correct <- rep(0, length(output_files))

# iterate through each output file in to_format
print("writing files")
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
  # remove incorrect and corrupt files now
    if(length(c(incorrect, corrupt))>0){
    output <- output[-c(incorrect, corrupt),]
    }
  
  ### pull the species info out of the keywords column
  ### this should be easier if we remove everything else systematically
  ### the first thing we need to do is remove the "[" and "]" from the
  ### array. These are the first and last characters in each row
    output$keywords <- substr(output$keywords, 2, nchar(output$keywords)-1)
  
  # split all of the keywords by the comma, creates a list
    keys <- strsplit(output$keywords, ",")
  
  # remove the leading and trailing white space from each vector in the list
    keys <- lapply(keys, FUN = trim)
  
  # remove the transect acronyms
    keys <- lapply(keys, FUN = drop_transects)
  
  # remove anything with a number
    keys <- lapply(keys, FUN = drop_nums)
    
  # remove Best of keywords
    keys <- lapply(keys, FUN = drop_best)
  
  # if length of keys is now 0, add "Empty"
    keys <- lapply(keys, FUN = add_empty)
  
  # get length of character vectors for each photo, >1 = multiple species    
    freq_list[[i]] <- sapply(keys, FUN = length)
  
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
                         picturename = output$name,
                         Observer = NA)
  
  

      # make the pic_attribute data.frame
        picatt <- data.frame(PictureID = 0,
                             Species = unlist(keys), stringsAsFactors = FALSE)
        picatt$ind <- 1 - as.numeric(picatt$Species == "Empty")
        colnames(picatt)[3] <- "NumberOfIndividuals"
      
      # add picatt to picatt_list
        picatt_list[[i]] <- picatt


  
  # store number of unique and correct photos
    n_correct[i] <- nrow(picrec)
  
  # write picrec to a file
    if(i == 1){
      dd <- Sys.Date()
      write.table(picrec, paste(the_path, "/../formatted_for_access/", "picrec_",dd, ".txt", sep = ""),
                  row.names = FALSE, sep = "\t")
    }else{ # for appending
      write.table(picrec, paste(the_path, "/../formatted_for_access/", "picrec_",dd, ".txt", sep = ""),
                  append = TRUE, row.names = FALSE, col.names = FALSE, sep = "\t")
    }
    
  # move the output file from  to_format to already_formatted_outputs
  my_file_rename(paste(the_path, "/", output_files[i], sep = ""),
                 paste(the_path, "/../already_formatted_outputs/", 
                       output_files[i], sep= ""))
  print(paste(i, "of", length(output_files), "files formatted", sep = " "))
}
print("outputs moved to already_formatted_outputs sub-folder")

### we should now have picrec ready to be put into access, 
### but now we need to get the picatt
### ready. To do this, we need to generate the picture ID

#  merge the picatt_list
picatt_all <- rbind.fill(picatt_list)

### before running this function you need to determine what
### the starting number is. Look this up in access
picatt_all$PictureID <- create_pic_id(start = start_number, 
                                      sum(n_correct), unlist(freq_list))

# save this table
write.table(picatt_all, paste(the_path, "/../formatted_for_access/", "picatt_",dd, ".txt", sep = ""),
            append = FALSE, row.names = FALSE, col.names = TRUE, sep = "\t")
print("files for access in formatted_for_access sub-folder")
print("script complete!")

