library(tidyverse)
library(lubridate)
library(plotly)
library(glue)
library(data.table)
library(glue)
library(doParallel)
file_df <- read_csv("file_df.csv") # I have a local copy of file_df.csv 

#Figure out what is even in each of these folders 
#################################################

COPYofData <- "F:/hbef_fcam_images/COPY of data for script"
COPYofData_file <- data.frame(fname = list.files(path = COPYofData, recursive = T)) %>%
  mutate(photo_type = ifelse(grepl(".jpg", fname, ignore.case = T), "unlabeled photo", NA))
#this folder only contains unlabeled photos 

ImMethods <- "F:/hbef_fcam_images/Image_Methods"
list.dirs(ImMethods) 
ImMethods_file <- data.frame(fname = list.files(path = ImMethods, recursive = T)) %>%
  mutate(photo_type = ifelse(grepl(".jpg", fname, ignore.case = T), "unlabeled photo", NA)) %>%
  mutate(photo_type = ifelse(grepl("hbwtr", fname, ignore.case = T), "labeled photo", photo_type)) %>%
  mutate(imgcode = ifelse(photo_type == "labeled photo", sub('.*hbwtr_', '', fname, ignore.case = T), NA)) %>%
  mutate(ws = substr(imgcode, 1,2), 
         date = ymd(substr(imgcode, 4,11)))
#this folder has a lot of images that are labelled, and images that are inverted?

projdir <- "F:/hbef_fcam_images/project_dir"
list.dirs(projdir)
projdir_file <- data.frame(fname = list.files(path = projdir, recursive = T)) %>%
  mutate(photo_type = ifelse(grepl(".jpg", fname, ignore.case = T), "unlabeled photo", NA)) %>%
  mutate(photo_type = ifelse(grepl("hbwtr", fname, ignore.case = T), "labeled photo", photo_type)) %>%
  mutate(imgcode = ifelse(photo_type == "labeled photo", sub('.*hbwtr_', '', fname, ignore.case = T), NA)) %>%
  mutate(ws = as.factor(substr(imgcode, 1,2)), 
         date = ymd(substr(imgcode, 4,11)))
#this folder is entirely labelled images organized by watershed (end game)
#file_df <- read_csv(glue(projdir, "/labeled_image_files/file_df.csv")) #open the file_df saved on gdrive


StreamPhotos <- "F:/hbef_fcam_images/Stream Photos"
list.dirs(StreamPhotos)
StreamPhotos_file <- data.frame(fname = list.files(path = StreamPhotos, recursive = T)) %>%
  mutate(photo_type = ifelse(grepl(".jpg", fname, ignore.case = T), "unlabeled photo", NA)) %>%
  mutate(photo_type = ifelse(grepl("hbwtr", fname, ignore.case = T), "labeled photo", photo_type)) %>%
  mutate(imgcode = ifelse(photo_type == "labeled photo", sub('.*hbwtr_', '', fname, ignore.case = T), NA)) %>%
  mutate(ws = substr(imgcode, 1,2), 
         date = ymd(substr(imgcode, 4,11)))
#this folder is all unlabeled photos 

#action items
#############

# Most of the labelled photos are in proj_dir, which also contains how each photo should be labeled

# step 1a - are all the labeled photos in file_df.csv?
file_df$new_name <- ifelse(file_df$status == "renamed manually", paste0(file_df$new_name,".jpg"), file_df$new_name)
step1a <- full_join(file_df %>% mutate(imgcode = sub('.*hbwtr_', '', new_name, ignore.case = T)), projdir_file, by = "imgcode")
#all of the renamed images are in file_df! the file_df in my working directory!

#visualize how many images have been successfully renamed: 
head(step1a)
summary(step1a)

full_dates <- data.frame(date = sort(rep(seq.Date(from = dmy("01-01-2018"), to = dmy("31-12-2023"), by = "1 day"),7)), 
                         ws = rep(paste0("w",c(1:6,9)), length(seq.Date(from = dmy("01-01-2018"), to = dmy("31-12-2023"), by = "1 day"))))

labeled_data <- left_join(full_dates, step1a) %>%
  group_by(ws, date) %>%
  mutate(img_num = sum(!is.na(fname)))
labeled_data

per_complete <- labeled_data %>%
  group_by(ws, year(date)) %>%
  summarise(n = sum(is.na(fname)), 
            n_actual = sum(!is.na(fname)), 
            percent_complete = n_actual/(n+n_actual)*100)

ggplot(data = per_complete) + geom_bar(aes(x = ws, y = percent_complete, fill = factor(`year(date)`)), stat = "identity", position = "dodge") + theme_bw()
# 
p2019 <- ggplot(data = labeled_data %>% filter(year(date) %in% 2019)) + geom_tile(aes(x = date, y = ws, fill = img_num, text = imgcode))

p2020 <- ggplot(data = labeled_data %>% filter(year(date) %in% 2020)) + geom_tile(aes(x = date, y = ws, fill = img_num, text = imgcode))

p2021 <- ggplot(data = labeled_data %>% filter(year(date) %in% 2021)) + geom_tile(aes(x = date, y = ws, fill = img_num, text = imgcode))

plotly::ggplotly(p2019)
plotly::ggplotly(p2020)
plotly::ggplotly(p2021) 

#notes from looking at the images: 
# most of 2021 is labeled 
# the list_df is good 
# it looks like there are some times where there are duplicate images per date; this is to be expected 

#check if these images metadata are intact? -- this was a proof of concept to ensure that metadata were intact AND to clean the proj_dir images that were labelled incorrectly 
{

library("exiftoolr")
#install_exiftool()

n <- length(list.files(projdir, recursive = T, pattern = "*.jpg", ignore.case = T, full.names = T))
files <- list.files(projdir, recursive = T, pattern = "*.jpg", ignore.case = T, full.names = T)

exif_read(files[1])
file.exists(file_names[1])



#1725 is an error 
# error_list <- list()
#

#find similar package available in python 
#cannot use indexing within for loops 
# for(i in 467:n){
#   error_list[[i]] <- exif_read(files[i])$CreateDate
#   print(i)
# }

#do parrallel has for_each 
# type of for loop built to spread out 
# google do parallel fo reach loop 

create_dates <- function(x) {
    created = exif_read(x)$CreateDate
    df = data.frame(fname = x, created_dttm = created)
  return(df)
}

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

safe_create_dates <- function(filepath) {
  tryCatch({
    create_dates(filepath)  # Your original function
  }, error = function(e) {
    cat("Error processing file:", substrRight(filepath, 24), "--Error message:", e$message, "\n")
    NULL  # Return NULL or some indication of failure that can be handled later
  })
}


# the new creation dates
creation_dates <- list.files(projdir, recursive = TRUE, pattern = "*.jpg", ignore.case = TRUE, full.names = TRUE) %>%
  lapply(safe_create_dates) %>%
  rbindlist(fill = TRUE) 

creation_dates$created_dttm <- ymd_hms(creation_dates$created_dttm)
creation_dates$written_dttm <- ymd_hms(substr(creation_dates$fname, nchar(creation_dates$fname)-18, nchar(creation_dates$fname)-4))
creation_dates$written_dttm

creation_dates$error <- ifelse(abs(difftime(creation_dates$created_dttm, creation_dates$written_dttm, units = "mins")) <=10, "same", "diff")

## fix impossible dates 
creation_dates %>%
  filter(is.na(error))

## fix wrong dates
creation_dates %>%
  filter(error == "diff")

# write_csv(creation_dates, "projdir_metadata_files.csv")
#Error processing file: hbwtr_w3_20211025_115909.JPG --Error message: arguments imply differing number of rows: 1, 0 

#list of files to be renamed & were renamed on the flash drive AND in the shared gdrive 
#Hbwtr_w1_20201295_115918 --> 2020-12-25 fd; 
#Hbwtr_w1_20201904_115941 --> 2020-12-04 fd;
#Hbwtr_w1_20204216_115928 --> 2020-12-16 fd;

#Hbwtr_w1_20210319_115952 --> 2021-03-15 fd;
#Hbwtr_w1_20210503_115856 --> 2021-05-08 fd;
#Hbwtr_w2_20191118_115825 --> 2019-11-13 fd;
#Hbwtr_w9_20211118_115855 --> 2021-11-13 fd;

#re-upload new file_df 

}