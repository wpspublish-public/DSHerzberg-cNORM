#I tried two methods to create score conversion tables for TOD project using
#simulated data. These two methods both work well and end up with comparable
#reuslts. In Method 1, I used the Based R bulid-in function (aggregate()) to get
#all the possible combinations between the standard score (SS) and age
#(age_range). In Method 2, I used ifelse() to manually list all the possible
#combinations. So, from this perspective, I think the Method 1 wins but Method 2
#can be a good back up plan.
############################################### Method 1: Code to create score conversion tables   ####################################
# simulate some data
scale_a <- data.frame(age_range = c(rep("5.0-5.3",7),rep("6.0-6.3",2),"6.4-6.7"),
                      ss = c(128,129,rep(130,5),132,132,133),
                      raw = c('NA',12,rep(20:24),23,27,'NA')
)


scale_b <- data.frame(age_range = c(rep("5.0-5.3",7),rep("6.0-6.3",2),"6.4-6.7"),
                      ss = c(118,119,rep(120,5),121,122,123),
                      raw = c('NA',22,rep(30:34),43,'NA',37)
)

scale_c <- data.frame(age_range = c(rep("5.0-5.3",7),rep("6.0-6.3",2),"6.4-6.7"),
                      ss = c(108,109,rep(110,5),111,112,113),
                      raw = c(2,'NA',rep(5:9),'NA',7,9)
)

scale <- list(scale_a, scale_b, scale_c)
names(scale) <- c("a", "b", "c")

# write my personal function to find the range for all kids of possible
# combinations between with age_range and ss (standard score)
f <- function(x){
  raw_min <- min(na.omit(x)) # find the min value 
  raw_max <- max(na.omit(x)) # find the max value
  if (raw_min == 'NA'|raw_max == 'NA'){ # if either the min or max value is "NA", then the range is just "NA"
    raw_range <- paste("NA")
  }
  else if (raw_min == raw_max){ # if the min is equal to the max, then range can be either min or max
    raw_range <- paste(raw_max)
  }
  else{# for none of the above two conditions, then the real range can be found.
    raw_range <- paste(raw_min, raw_max , sep = "-")
  }
}

# we need to find a way to apply the function to a list of dataframe, so lappy
# is used here. The purpose of aggregrate() is to help us find all possible
# combinations between standard score (ss) and age (age_range).
scale_convert1 <- lapply(scale,function(x){aggregate(x$raw, list(age_range = x$age_range,ss = x$ss), FUN = function(z) f(z))})

scale_convert_bind <- do.call("rbind", scale_convert1)
scale_convert_bind[["scales"]] <- rep(names(scale_convert1), sapply(scale_convert1, nrow))
rownames(scale_convert_bind) <- NULL

library(reshape2)
# use the library(reshape2) to transform the long data into wide data and it can
# put the row and column in the right order
scale_convert_bind_wide <- reshape2::dcast(scale_convert_bind,formula = ss ~ age_range + scales, value.var = "x")

# split the above wide data-frame into different age group within a list
sss <- c("5.0-5.3", "6.0-6.3","6.4-6.7")
scale_convert_bind_wide_new <- lapply(setNames(sss,sss), function(x) scale_convert_bind_wide[, grep(x, colnames(scale_convert_bind_wide))])

# add the common column (ss) back to each list elements (age range)
l2 <- lapply(scale_convert_bind_wide_new, function(x) 
  cbind(ss = scale_convert_bind_wide$ss,x))

# respecify the name of list elements
new.names <-c("ss","a", "b", "c")

# assign the new name to the list
new_1 <- lapply(l2, setNames, new.names)

# write list output into the same excel with multiple sheets
writexl::write_xlsx(new_1, "method1.xlsx")
#################################################### End of code (Method 1) ######################################################

############################################### Method 2: Code to create score conversion tables  #################################
# simulate some data to play with (the same as those used in Method 1)
scale_a <- data.frame(age_range = c(rep("5.0-5.3",7),rep("6.0-6.3",2),"6.4-6.7"),
                      ss = c(128,129,rep(130,5),132,132,133),
                      raw = c('NA',12,rep(20:24),23,27,'NA')
)


scale_b <- data.frame(age_range = c(rep("5.0-5.3",7),rep("6.0-6.3",2),"6.4-6.7"),
                      ss = c(118,119,rep(120,5),121,122,123),
                      raw = c('NA',22,rep(30:34),43,'NA',37)
)

scale_c <- data.frame(age_range = c(rep("5.0-5.3",7),rep("6.0-6.3",2),"6.4-6.7"),
                      ss = c(108,109,rep(110,5),111,112,113),
                      raw = c(2,'NA',rep(5:9),'NA',7,9)
)

scale <- list(scale_a, scale_b, scale_c)
names(scale) <- c("a", "b", "c")

# specify some place holders 
raw_min = NULL
raw_max = NULL
raw_range = NULL
number = NULL
raw_range_new = NULL

#write a function to find the range from a given data (it's tested on the sigle
#data called scale. If you are interested, please try it by yourself.)

range_finder <- function(x){
  x_new <- x[(duplicated(x[,2])|duplicated(x[,2], fromLast = TRUE)),]
  for (i in 1:length(unique(x$age_range))){
    raw_min[i] <- min(na.omit(x_new[which(x_new$age_range ==unique(x_new$age_range)[i]), ][,"raw"]))
    number[i] <- nrow(x[which(x$age_range == unique(x$age_range)[i]), ])
    raw_max[i] <- x_new[which(x_new$age_range ==unique(x_new$age_range)[i]), ][,"raw"][max(which(x_new[which(x_new$age_range ==unique(x_new$age_range)[i]), ][,"raw"] != 'NA'), na.rm = TRUE)]
    raw_range[i] <- paste(raw_min[i], raw_max[i], sep="-")
    raw_range_new <- rep(raw_range, times = number)
  }
  return(raw_range_new)
}
# write a function to add the above function result into the data and chnage it into wide format for age range
convert <- function(x){
  x[ ,paste0(" ")] <- ifelse(x[,3] == "NA", "NA", 
                             ifelse(((duplicated(x[,1])|duplicated(x[,1], fromLast = TRUE))=="FALSE"|
                                       (duplicated(x[,2])|duplicated(x[,2], fromLast = TRUE))=="FALSE"), x[,3],range_finder(x)))
  return(reshape(unique(x[,-3]), idvar = "ss", timevar = "age_range", v.names=" ", direction="wide"))
}

# apply the function to the data
scale_convert2 <- lapply(scale,convert)

# combine the list elements into a dataframe (unordered)
scale_convert2_new <- do.call("rbind", scale_convert2)

# put the scale information into the above data
scale_convert2_new[["scales"]] <- rep(names(scale_convert2), sapply(scale_convert2, nrow))

# remove the column name
rownames(scale_convert2_new) <- NULL

# put the above data into wide format
scale_convert2_wide <- reshape(scale_convert2_new,idvar=c("ss"),v.names = c(" .5.0-5.3"," .6.0-6.3"," .6.4-6.7"),timevar = "scales",direction="wide")
scale_convert2_wide_order <- scale_convert2_wide[order(scale_convert2_wide$ss),] 

# split the data based on the age-range
scale_convert2_wide_new  <- sapply(c(" .5.0-5.3", " .6.0-6.3"," .6.4-6.7"),
                                   function(x) scale_convert2_wide_order[startsWith(names(scale_convert2_wide_order),x)],
                                   simplify = FALSE)

# add the common column (ss) to each list elements (age range)
l2_method <- lapply(scale_convert2_wide_new, function(x) 
  cbind(ss = scale_convert2_wide_order$ss,x))

# respecify the name of list elements
new.names <-c("ss","a", "b", "c")

# assign the new name to the list
new_2<- lapply(l2_method , setNames, new.names)

# write list output into the same excel with multiple sheets
writexl::write_xlsx(new_2, "method2.xlsx")

#################################################### End of code (Method 2) ##################################################################
