FacebookData <- read.csv(file="~/Downloads/facebook.csv", header=TRUE, sep=",")
View(FacebookData)
colnames(FacebookData)
names(FacebookData) <- c("page_popularity", "page_checkins", "page_talking_about", "page_category", "derived_1", "derived_2", "derived_3", "derived_4", 
                         "derived_5", "derived_6", "derived_7", "derived_8", "derived_9", "derived_10", "derived_11", "derived_12", "derived_13", "derived_14","derived_15",
                         "derived_16", "derived_17", "derived_18", "derived_19", "derived_20", "derived_21", "derived_22", "derived_23", "derived_24", "derived_25",
                         "CC1", "CC2", "CC3", "CC4", "CC5", "base_time", "post_length", "post_share_count", "post_promotion_status", "h_local", "post_published_weekday_1", 
                         "post_published_weekday_2", "post_published_weekday_3", "post_published_weekday_4", "post_published_weekday_5", "post_published_weekday_6",
                         "post_published_weekday_7", "base_datetime_weekday_1", "base_datetime_weekday_2", "base_datetime_weekday_3", "base_datetime_weekday_4",
                         "base_datetime_weekday_5", "base_datetime_weekday_6", "base_datetime_weekday_7", "target_variable")
