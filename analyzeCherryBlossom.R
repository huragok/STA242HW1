# UC Davis STA 242 2015 Spring Assignment 1
# Analyze the Credit Union Cherry Blossom Ten Mile Run results from 1999 to 2010.
# Author: Wenhao Wu
# Email: wenhaowu1989@hotmail.com
# Date: April 6th 2015
rm(list = ls())

library(stringr)
library(maps)
library(maptools)
library(ggplot2)
library(sp)
library(plyr)

files <- list.files(path = "./data")
n_files <- length(files)

# All possible field names and the patterns used to detect the field name line
fields_all = c("place", "divtot", "name", "number", "age", "hometown", "time_gun", "time_net", "pace", "seed", "split", "time_five_mile", "pace_five_mile", "time_ten_km", "pace_ten_km")
patterns_fieldname = c("place", "div[^:print:]*/tot", "name", "num", "ag", "hometown", "gun( tim)*", "net( tim)*|time", "pace", "s", "split", "5 mi(le)*", "pace", "10 km", "pace") # Fuck you! men10Mile_2009 has a weirdo unprintable character on line 7 col 9
names(patterns_fieldname) = fields_all

# Define the patterns of data in each field
pattern_time = "((\\s{5,8})|((([[:digit:]]:)?[[:digit:]]{2}:[[:digit:]]{2})))"
pattern_split = "((\\s{0,8})|((([[:digit:]]:)?[[:digit:]]{2}:[[:digit:]]{2})))"
pattern_pace = "((\\s{4,5})|(1?[[:digit:]]:[[:digit:]]{2}))"
pattern_time_net = "(((([[:digit:]]:)?[[:digit:]]{2}:[[:digit:]]{2})[#\\*]?\\s?)|(\\s{0,7}[#\\*]?\\s?))"
pattern_time_gun = pattern_time_net 
pattern_name = "(([[:alnum:],'&\\.\\-]+\\s{1,2})*[[:alnum:],'&`\\.\\-]*)"

patterns_field = c("([[:digit:]]+)", "(([[:digit:]]+/[[:digit:]]+)?)", pattern_name, "([[:digit:]]*)", "([[:digit:]]{0,2})", pattern_name, pattern_time_gun, pattern_time_net, pattern_pace, "([^#]?)", pattern_split, pattern_time, pattern_pace, pattern_time, pattern_pace)
names(patterns_field) = fields_all


patterns_to_scan = patterns_fieldname[c(-9, -13, -15)] # The field name we are going to search for directly using regular expression, pace/pace for 5mile/pace for 10km will be determined by contexts

# Function to extract one specific field name using RE
extractField <- function(field, fieldline) {
  pos <- regexpr(patterns_to_scan[field], fieldline)
}

# Function to get the fields name from a vecors of lines in the file
getFields <- function(filelines) {
  
  # Locate the line containing the field names
  n_line = length(filelines)
  flag_field_line = FALSE
  for (i_line in seq(n_line)) {
    if (regexpr("PLACE|Place", filelines[i_line]) == 1) {
      flag_field_line = TRUE
      break
    }
  }
  if (!flag_field_line) { # Cannot locate a field line
    return (NULL)
  }
  else { # Located a field line, now extract the field names
    fieldline_lc = tolower(filelines[i_line])
    pos <- sapply(names(patterns_to_scan), extractField, fieldline_lc) 
    pos <- pos[pos > 0] 
    pos <- sort(pos) # The positions of part of fields in increasing order, extracted directly using RE
    
    # Now extract the position for pace/pace_five_mile/pace_10_km
    if (!is.na(pos["time_five_mile"])) { # 5 mile time field exist, check whether the next field is pace
      if (str_detect(substr(fieldline_lc, pos["time_five_mile"], nchar(fieldline_lc)), paste(patterns_fieldname["time_five_mile"], " *pace", sep=""))) { #
        
        fieldname_time_five_mile = str_extract(fieldline_lc, patterns_fieldname["time_five_mile"]) # The string of the 5 mile time field
        pos_pace_five_mile = regexpr("pace", substr(fieldline_lc, pos["time_five_mile"] + nchar(fieldname_time_five_mile), nchar(fieldline_lc))) # The pos of the 5 mile pos field
        pos["pace_five_mile"] = pos["time_five_mile"] + nchar(fieldname_time_five_mile) + pos_pace_five_mile - 1# Update the pos vector
      }
    }
    if (!is.na(pos["time_ten_km"])) { # 10 km time field exist, check whether the next field is pace
      if (str_detect(substr(fieldline_lc, pos["time_ten_km"], nchar(fieldline_lc)), paste(patterns_fieldname["time_ten_km"], " *pace", sep=""))) { #
        
        fieldname_time_ten_km = str_extract(fieldline_lc, patterns_fieldname["time_ten_km"]) # The string of the 10 km time field
        pos_pace_ten_km = regexpr("pace", substr(fieldline_lc, pos["time_ten_km"] + nchar(fieldname_time_ten_km), nchar(fieldline_lc))) # The pos of the 10 km pos field
        pos["pace_ten_km"] = pos["time_ten_km"] + nchar(fieldname_time_ten_km) + pos_pace_ten_km - 1# Update the pos vector
      }
    }
    pos_pace_all <- gregexpr("pace", fieldline_lc)[[1]]
    if (pos_pace_all > 0) {
      for (pos_candidate in pos_pace_all)
      {
        if ((is.na(pos["pace_five_mile"]) || pos["pace_five_mile"] != pos_candidate) && (is.na(pos["pace_ten_km"]) || pos["pace_ten_km"] != pos_candidate)) {
          break
        }
      }
      pos["pace"] = pos_candidate
    }
    
    return(sort(pos))
  }
}

# Function to generate the gsub pattern and the replace patterns according to the given field
getFieldPatterns <- function(fieldnames) {
  pattern = "^\\s*"
  for (fieldname in fieldnames){
    pattern = paste(pattern, patterns_field[fieldname], "\\s*", sep = "")
  }
  pattern = paste(pattern, "$", sep = "")
  return (pattern)
}

# Function to trim leading and trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

# Function to clean format of the lines in the file to meet the RE. Designed based on trial and error
cleanLines <- function(filelines) {

  filelines = gsub("8illiam", "William", filelines) # The "8illiam" in men10Mile_2000
  filelines = gsub("Christopher5", "Christopher", filelines) # The "Christopher5" in men10Mile_2007


  filelines = gsub("`", "", filelines) # remove meaningless symbols (e.g."`" in "men10Mile_2001")
  filelines = gsub("([[:digit:]]{1,2}:)(\\s|$)", "\\100\\2", filelines) # Add the missed second part of the time in "men10Mile_2001")

  filelines = gsub("±", "t", filelines) # Roberto Pe±a to Peta in men10Mile_2008

  return(filelines)
}

# Function to get the range of lines to containing the tables
getBeginEnd <- function(filelines, pattern) {
  begin = 0
  for (i_row in seq(length(filelines))) {
    if (str_detect(filelines[i_row], pattern)) {
      break
    }
  }
  begin = i_row
  
  end = length(filelines)
  for (i_row in seq(length(filelines), 1)) {
    if (str_detect(filelines[i_row], pattern)) {
      break
    }
  }
  end = i_row
  return (c(begin, end))
}

# Function to test the format of the file. If UTF-8 convert to ASCII
conv2ASCII <- function(filename) {
  fmt = system(paste("file -b \"", filename, "\"", sep = "" ), intern=TRUE)
  if (str_detect(fmt, "UTF-8")) {
    system(paste("iconv -t ASCII//TRANSLIT \"", filename, "\" > \"", filename, "_new\"", sep = ""))
    system(paste("mv -f \"", filename, "_new\" \"", filename, "\"", sep = ""))
  }
}

fields_post = # After post processing the all data frames should contain these fields in this order
c("place", "div", "tot", "name", "number", "age", "hometown", "time_gun", "time_net", "pace", "seed", "split", "time_five_mile", "pace_five_mile", "time_ten_km", "pace_ten_km", "guideline", "hometown_country", "hometown_state", "hometown_city")

postProcFrame <- function(data_frame, filename = "", verbose = "FALSE") {
  # place column does not need to be postprocessed
  if (verbose) print(paste(filename, "being post-processed..."))
  
  # Decompose the div/tot into two columns
  if ("divtot" %in% colnames(data_frame)) {
    data_frame$div <- sapply(data_frame$divtot, postProcDiv)
    data_frame$tot <- sapply(data_frame$divtot, postProcTot)
    data_frame$divtot <- NULL
  } else {
    data_frame$div <- NA
    data_frame$tot <- NA
  }
  data_frame$div = as.factor(data_frame$div)
  data_frame$tot = as.factor(data_frame$tot)
  if (verbose) print("-- Div/tot processed.")

  
  # Process the number domain
  if ("number" %in% colnames(data_frame)) {
    data_frame$number <- as.factor(as.integer(data_frame$number))
  } else {
    data_frame$number <- NA
  }

  # Convert the name into lower cases, remove
  data_frame$name = as.factor(sapply(data_frame$name, postProcName))
  if (verbose) print("-- Name processed.")
  
  
  # Extract the USATF OPEN guideline (OPEN) or Under USATF Age-Group (AG) guideline or no guideline (NONE)
  field_gl = data_frame$time_net
  if ("time_gun" %in% colnames(data_frame)) {
    field_gl = cbind(field_gl, data_frame$time_gun)
  }
  if (is.vector(field_gl)) {
    data_frame$guideline <- sapply(field_gl, postProcGL)
  }
  else {
    #print(which(is.na(field_gl[,2])))
    data_frame$guideline <- apply(field_gl, 1, postProcGL)
  }
  data_frame$guideline = as.factor(data_frame$guideline)
  if (verbose) print("-- USATF guideline indicators extracted.")
  
  # Convert the time domain into seconds
  for (fieldname in c("time_gun", "time_net", "pace", "split", "time_five_mile", "pace_five_mile", "time_ten_km", "pace_ten_km")) {
    if (fieldname %in% colnames(data_frame)) {
      data_frame[,fieldname] <- sapply(data_frame[,fieldname], postProcTime)
    }
    else {
      data_frame[,fieldname] <- NA
    }
  }
  if (verbose) print("-- All time fields converted into numerical (seconds).")
  
  # Convert the seed domain into logical
  if ("seed" %in% colnames(data_frame)) {
    data_frame$seed <- (!is.na(data_frame$seed) & str_detect(data_frame$seed, "[^\\s]{1}"))
    #data_frame$seed <- (nchar(data_frame$seed) > 0)
  } else {
    data_frame$seed <- NA
  }
  data_frame$seed = as.factor(data_frame$seed)
  if (verbose) print("-- Seed processed")
  
  # Process the hometown domain
  data_frame$hometown = gsub("^[[:digit:]]+\\sBerlin.*$", "Germany", data_frame$hometown) # The idiot who put his zip on Berlin in the address field in "men10Mile_2005"
 
  data_frame$hometown= gsub("Suite\\s[[:digit:]]+\\s([A-Z]{2})\\s", "\\1", data_frame$hometown)
  data_frame$hometown = gsub("#?\\s?[[:digit:]]+\\s([A-Z]{2})\\s", "\\1", data_frame$hometown)
  data_frame$hometown = gsub("[aA][pP][tT]\\.?\\s.*([A-Z]{2})\\s", "\\1", data_frame$hometown)
  
  data_frame$hometown = gsub("[[:digit:]]+(\\s[[:alpha:]]+)*\\s[Ss][Tt](\\s[A-Z]{2}\\s)", "\\2", data_frame$hometown) # The idiots who put their street address in "men10Mile_2007"
  data_frame$hometown = gsub("[[:alpha:]]+@[[:alpha:]]+\\s", "", data_frame$hometown) # The idiots who put their email address in the address field in "men10Mile_2007"
  data_frame$hometown = gsub("(\\s[[:alpha:]]+)[[:digit:]]+\\s", "\\1 ", data_frame$hometown) # The idiot who put his zipcode in the address field in "men10Mile_2007"
  if (verbose)  print("-- Clean the hometown field initially.")
  
  mat_hometown = t(sapply(data_frame$hometown, postProcHometown))
  
  names(mat_hometown) = NULL
  data_frame$hometown = as.factor(mat_hometown[,1])
  data_frame$hometown_country = as.factor(mat_hometown[,2])
  data_frame$hometown_state = as.factor(mat_hometown[,3])
  data_frame$hometown_city = as.factor(mat_hometown[,4])
  if (verbose)  print("-- Hometown processed.")
  
  ord = match(fields_post, colnames(data_frame))
  return(data_frame[, ord])
}

postProcDiv <- function(s) {
  if (s == "") {
    return (NA)
  } else {
    return (as.integer(str_extract(s, "^[[:digit:]]+\\b")))
  }
}

postProcTot <- function(s) {
  if (s == "") {
    return (NA)
  } else {
    return (as.integer(str_extract(s, "\\b[[:digit:]]+$")))
  }
}

postProcName <- function(s) {
  s = tolower(s)
  if (str_detect(s, "unknown|unnamed")) {
    s = NA
  }
  return(s)
}

postProcGL <- function(v_s) {
  v_s[is.na(v_s)] = ""
  if (any(str_detect(v_s, "#"))) {
    return("OPEN")
  }
  else if (any(str_detect(v_s, "\\*"))) {
    return("AG")
  }
  else {
    return("NONE")
  }
}

postProcTime <- function(s) {
  if (is.na(s) | s == "") {
    return(NA)
  }
  v_s = unlist(strsplit(s, ":"))
  v_s = as.integer(gsub("[^0-9]","",v_s))  #remove all non-numeric characters 
  l_v = length(v_s)
  return (sum((60 ^ seq(l_v - 1, 0)) * v_s))
}

data(us.cities)
us_cities = gsub("\\s[A-Z]{2}", "", us.cities$name)
postProcHometown <- function(s) {
  if (is.na(s) | s == "") { # Empty hometown are considered as NA
    return(c(NA, NA, NA, NA))
  }
  hometown_state = toupper(str_extract(s, "\\b[[:alpha:]]{2}$")) # determine state
  if (is.na(hometown_state)) {
    if (s %in% us_cities) { # In 2006 the retarded organizers forgot to add the state names. Look up a table to check whether the city is in US
      hometown_state = NA
      hometown_country = "US"
      hometown_city = s
    } else if (s == "Washington") {
      hometown_state = "DC"
      hometown_country = "US"
      hometown_city ="Washington"
    } else { # We assume we encountered a country
      hometown_country = s
      hometown_state = NA
      hometown_city = NA
    }
  } else if (hometown_state %in% state.abb) {
    hometown_country = "US"
    hometown_city = trim(gsub("\\b[[:alpha:]]{2}$", "", s)) # We assume the pre hometown city
    if (hometown_city == "") {
      hometown_city = NA
    }
  } else if (hometown_state == "DC") {
    hometown_country = "US"
    hometown_city = "Washington"
  } else { # Where are you from?
    hometown_country = "NA"
    hometown_state = NA
    hometown_city = NA
  }
  
  return(c(s, hometown_country, hometown_state, hometown_city))
}

# Function to analyze each file into a data frame
parseFile <- function(filename, path, pos = NULL, verbose = FALSE) {
  if(verbose) print(paste(filename, "being parsed..."))
  if (!str_detect(filename, "^(men|women)10Mile_[[:digit:]]{4}$")) {
    return (NULL)
  }
  
  fullname = paste(path, filename, sep="")
  conv2ASCII(fullname) # If file is UTF-8, convert to ASCII
  
  filelines = readLines(fullname)
  if(verbose) print(paste("--", toString(length(filelines)), "lines read in."))
  filelines = cleanLines(filelines)
  if (is.null(pos)) {
    pos = getFields(filelines)
  }
  if (is.null(pos)) {
    warning("Can not locate the field line. Try specifying the field names manually!")
    return(NULL)
  }
  if(verbose) print("-- Data fields and their positions are:")
  if(verbose) print(pos)
  
  max_len_line = max(nchar(filelines))
  fieldwidths = c(pos[-1], max_len_line) - pos
  names(fieldwidths) = names(fieldnames)
  
  pr = getFieldPatterns(names(pos))
  be = getBeginEnd(filelines, pr[1]) # locate the first and last line to read
  if(verbose) print(paste("-- Data section in this file are between line", toString(be)))
  
  tc <- textConnection(filelines[be[1]:be[2]])
  data <- read.fwf(tc, fieldwidths, header = FALSE, strip.white=TRUE, blank.lines.skip = TRUE, fill = TRUE, col.names = names(pos), comment.char="", stringsAsFactors = FALSE)
  if(verbose) print("-- Data successfully read into data frame.")
  
  data = data[!is.na(data$place),]
}

# The wrapper function to call a function on a data frame by specifing gender and year
wrapper <- function(func_frame, index, list_frame, ...) {
  return (func_frame(list_frame[[index]], ...))
}

# Parsing and post-processing
path = "./data/"

fieldnames = c("place", "number", "name", "age", "hometown", "time_net", "time_gun") # Manually set data for women10Mile_2001
pos = c(1, 7, 13, 35, 38, 57, 65)
names(pos) = fieldnames 

list_data = list()
for (file in files) {
  gender = str_extract(file, "^(men|women)")
  if (gender == "men") {
    gender = "M"
  } else {
    gender = "F"
  }
  year = str_extract(file, "[[:digit:]]{4}$")
  filename = paste(gender, year)
  if (file == "women10Mile_2001") {
    data_raw = parseFile(file, path, pos)
  } else {
    data_raw = parseFile(file, path)
  }
  list_data[[filename]] = postProcFrame(data_raw)
  print(paste(filename, "done"))
}
 
 
# The variable derivation part
 
years = seq(1999, 2010)
index = cbind(paste("M", years), paste("F", years))
colnames(index) = c("M", "F")
rownames(index) = years

# Number of participants
num_runner_men = sapply(index[,1], wrapper, func_frame = nrow, list_frame = list_data)
num_runner_women = sapply(index[,2], wrapper, func_frame = nrow, list_frame = list_data)
num_runner = t(data.frame(num_runner_men, num_runner_women))

pdf("num_runner.pdf")
par(mar = c(5.1, 4.1, 4.1, 7.1), xpd = TRUE)
barplot(num_runner, col = heat.colors(length(rownames(num_runner))), width = 2)
legend("topright", inset = c(-0.25, 0), fill = heat.colors(length(rownames(num_runner))),legend = c("Men", "Women"))
xlab="Years"
dev.off()
print("Number of runners during 1999-2010 plotted.")
 
# The best and mean performance
getMinMeanTimenet <- function(data_frame) {
  return(c(min(data_frame$time_net, na.rm = TRUE), mean(data_frame$time_net, na.rm = TRUE)))
}
time_net_men = t(sapply(index[,1], wrapper, func_frame = getMinMeanTimenet, list_frame = list_data))
time_net_women = t(sapply(index[,2], wrapper, func_frame = getMinMeanTimenet, list_frame = list_data))
time_net = data.frame(time_net_men, time_net_women)
colnames(time_net) = c("Men min.", "Men mean", "Women min.", "Women mean")

pdf("time_net.pdf")
plot (c(1999,2010),c(0,6000),type="n", xlab="Years",ylab="Net Time/s") # adds titles to the axes ), # sets the x and y axes scales 
lines(years, time_net[,1], col="blue", lwd=2.5, lty = 1)
lines(years, time_net[,2], col="blue", lwd=2.5, lty = 2)
lines(years, time_net[,3], col="red", lwd=2.5, lty = 1)
lines(years, time_net[,4], col="red", lwd=2.5, lty = 2)
legend(2006,1500, legend = colnames(time_net), lty=c(1,2,1,2), lwd=c(2.5,2.5,2.5,2.5),col=c("blue","blue","red","red"))
dev.off()
print("Mean and best net time during 1999-2010 plotted.")

# The distribution of the US runners across different states in 2010
state2010 = table(c(as.character(list_data[["M 2010"]]$hometown_state), as.character(list_data[["F 2010"]]$hometown_state)))
state2010_frame = data.frame(as.vector(state2010), names(state2010))
names(state2010_frame) = c('values', 'state.abb')
state2010_frame$states <- tolower(state.name[match(state2010_frame$state.abb,  state.abb)])
state2010_frame$states[which(state2010_frame$state.abb == "DC")] = "district of columbia"
 
mapUSA <- map('state',  fill = TRUE,  plot = FALSE)
nms <- sapply(strsplit(mapUSA$names,  ':'),  function(x)x[1])
USApolygons <- map2SpatialPolygons(mapUSA,  IDs = nms,  CRS('+proj=longlat'))
idx <- match(unique(nms),  state2010_frame$states)
state2010_frame2 <- data.frame(value = state2010_frame$value[idx], state = unique(nms))
row.names(state2010_frame2) <- unique(nms)
USAsp <- SpatialPolygonsDataFrame(USApolygons,  data = state2010_frame2)

pdf("heatmap_usa_2010.pdf")
spplot(USAsp['value'])
dev.off()
print("Heatmap of runners' state across US plotted.")

# The age distribution of the runners in 2010
getGroupInfoByTot <- function(tot, group) {
  count_age = tot
  min_time_net = min(group[which(group[,2] == tot), 3], na.rm=TRUE)
  mean_time_net = mean(group[which(group[,2] == tot), 3], na.rm=TRUE)
  min_age = min(group[which(group[,2] == tot), 1], na.rm=TRUE)
  max_age = max(group[which(group[,2] == tot), 1], na.rm=TRUE)  
  return(list(count_age, min_time_net, mean_time_net, min_age, max_age))
}

getGroupInfo <- function(index, list_data) {
  age = list_data[[index]]$age
  tot = as.numeric(as.character(list_data[[index]]$tot))
  time_net = list_data[[index]]$time_net
  group = data.frame(age, tot, time_net)
  tot_by_group = as.vector(table(tot))

  group_info = sapply(tot_by_group, getGroupInfoByTot, group)
  rownames(group_info) = c('tot', 'min_time_net', 'mean_time_net', 'min_age', 'max_age')
  ord <- order(unlist(group_info[4,]))
  group_info <- group_info[,ord]
  return(group_info)
}

group_info_m2010= getGroupInfo('M 2010', list_data)
group_info_f2010 = getGroupInfo('F 2010', list_data)


getParRect <- function(groupInfo) {
  xleft = unlist(groupInfo[4, ])
  xright = unlist(groupInfo[5, ]) + 1
  ybottom = 0
  ytop = unlist(groupInfo[1, ]) / (xright - xleft + 1)
  return(list(xleft, xright, ybottom, ytop))
}

color_group_m2010 = rainbow(ncol(group_info_m2010))
par_rect_m2010 = getParRect(group_info_m2010)
pdf("group_m2010.pdf")
plot(c(0, 100), c(0, 500), type = 'n',  xlab="Age", ylab="Density w.r.t Age")
rect(xleft = par_rect_m2010[[1]], ybottom = par_rect_m2010[[3]], xright = par_rect_m2010[[2]], ytop = par_rect_m2010[[4]], col = color_group_m2010)
dev.off()

color_group_f2010 = rainbow(ncol(group_info_f2010))
par_rect_f2010 = getParRect(group_info_f2010)
pdf("group_f2010.pdf")
plot(c(0, 100), c(0, 500), type = 'n',  xlab="Age", ylab="Density w.r.t Age")
rect(xleft = par_rect_f2010[[1]], ybottom = par_rect_f2010[[3]], xright = par_rect_f2010[[2]], ytop = par_rect_f2010[[4]], col = color_group_f2010)
dev.off()

pdf("time_net_group_2010.pdf")
plot(c(0, 100), c(0, 8000), type = 'n',  xlab="Age", ylab="Net Time/s")
lines((unlist(group_info_m2010[4,]) + unlist(group_info_m2010[5,]) + 1) / 2, unlist(group_info_m2010[2,]), type="b",  col="blue", lwd=2.5, lty = 1, pch = 1, cex=1)
lines((unlist(group_info_m2010[4,]) + unlist(group_info_m2010[5,]) + 1) / 2, unlist(group_info_m2010[3,]), type="b",  col="blue", lwd=2.5, lty = 3, pch = 1, cex=1)
lines((unlist(group_info_f2010[4,]) + unlist(group_info_f2010[5,]) + 1) / 2, unlist(group_info_f2010[2,]), type="b",  col="red", lwd=2.5, lty = 1, pch = 1, cex=1)
lines((unlist(group_info_f2010[4,]) + unlist(group_info_f2010[5,]) + 1) / 2, unlist(group_info_f2010[3,]), type="b",  col="red", lwd=2.5, lty = 3, pch = 1, cex=1)
legend(60,1500, legend = colnames(time_net), lty=c(1,2,1,2), lwd=c(2.5,2.5,2.5,2.5),col=c("blue","blue","red","red"))
dev.off()
print("Mean and best net time during 1999-2010 plotted.")

# Match individual across years
getRecord <- function(year_target, name_base, year_base, age_base, gender_base, list_data) {# Find the record of someone according to his name, year, age at that year and gender in a target year
  index = paste(gender_base, as.character(year_target)); # The index of the target data frame
  i_row = which(list_data[[index]]$name == postProcName(name_base) & list_data[[index]]$age == age_base + year_target - year_base)
  record = list_data[[index]][i_row,]
  if (nrow(record) > 0) {
    record$year = year_target
    return(data.frame(lapply(record, as.character), stringsAsFactors=FALSE))
  } else {
    return (NULL)
  }
}

getMatching <- function(index, i_row, list_data, range) { # Find the record that matches the i_row's record in list_data[index]
  gender_base = substr(index, 1, 1)
  year_base = as.integer(substr(index, 3, 6))
  name_base = list_data[[index]]$name[i_row]
  age_base = list_data[[index]]$age[i_row]
  years_target = range
  print(years_target)
  lapply(years_target, getRecord, name_base, year_base, age_base, gender_base, list_data)
}

range = seq(1999, 2010)
list_record = getMatching('M 2003', 38, list_data, range)
record_individual = ldply(list_record, data.frame)
print(record_individual)

print("All matches of Christopher Dean (male) 's record on 2003 found.")

