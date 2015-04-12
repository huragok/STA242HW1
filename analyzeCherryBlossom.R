# UC Davis STA 242 2015 Spring Assignment 1
# Analyze the Credit Union Cherry Blossom Ten Mile Run results from 1999 to 2010.
# Author: Wenhao Wu
# Email: wenhaowu1989@hotmail.com
# Date: April 6th 2015
rm(list = ls())

library(stringr)
library(maps)
library(ggplot2)

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
  #print(n_line)
  flag_field_line = FALSE
  for (i_line in seq(n_line)) {
    #print(regexpr("PLACE|Place", filelines[i_line]) == 1)
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

  
  # Convert the name into lower cases, remove
  if ("number" %in% colnames(data_frame)) {
    data_frame$number <- as.factor(as.integer(data_frame$number))
  } else {
    data_frame$number <- NA
  }
  data_frame$name = as.factor(data_frame$name)
  if (verbose) print("-- Name processed.")
  
  # Process the number domain
  if ("name" %in% colnames(data_frame)) {
  } else {
  }
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
  
  return(data_frame)
}

postProcDiv <- function(s) {
  if (s == "") {
    #print(s)
    return (NA)
  } else {
    return (as.integer(str_extract(s, "^[[:digit:]]+\\b")))
  }
}

postProcTot <- function(s) {
  if (s == "") {
    #print(s)
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
  #s = trim(s)
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
  #filelines = trim(readLines(fullname))
  
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
  #return(list(c(gender, year)))
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
 
 #file = c("women10Mile_2002")
 #fullname = paste(path, file, sep = "")
 #data_raw = parseFile(file, path)#, pos)
 #data = postProcFrame(data_raw, file)
 
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
 num_runner = t(data.frame(num_runner_men, num_runner_women)
 
 pdf("num_runner.pdf")
 par(mar = c(5.1, 4.1, 4.1, 7.1), xpd = TRUE)
 barplot(num_runner, col = heat.colors(length(rownames(num_runner))), width = 2)
 legend("topright", inset = c(-0.25, 0), fill = heat.colors(length(rownames(num_runner))),legend = c("Women", "Men"))
 xlab="Years"
 dev.off()
 

 #conv2ASCII(fullname)
 #data_raw = parseFile(file, path)#, pos)
 #data = postProcFrame(data_raw, file)
 #print(summary(data))
 #print(data_raw[which(is.na(data_raw$age)),])
 #print(data_raw[which(is.na(data_raw$number)),])
 
 #year = seq(1999, 2010)
 #filenames = sapply()
