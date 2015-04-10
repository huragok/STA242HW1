# UC Davis STA 242 2015 Spring Assignment 1
# Analyze the Credit Union Cherry Blossom Ten Mile Run results from 1999 to 2010.
# Author: Wenhao Wu
# Email: wenhaowu1989@hotmail.com
# Date: April 6th 2015

library(stringr)
library(stringi)

files <- list.files(path = "./data")
n_files <- length(files)

# The patterns of all possible fields
# All the possible fields
fields_all = c("place", "divtot", "name", "number", "age", "hometown", "time_gun", "time_net", "pace", "seed", "split", "time_five_mile", "pace_five_mile", "time_ten_km", "pace_ten_km")
patterns_fieldname = c("place", "div[^:print:]*/tot", "name", "num", "ag", "hometown", "gun( tim)*", "net( tim)*|time", "pace", "s", "split", "5 mi(le)*", "pace", "10 km", "pace") # Fuck you! men10Mile_2009 has a weirdo unprintable character on line 7 col 9
names(patterns_fieldname) = fields_all

pattern_time = "((([[:digit:]]{1,2}:){1,2}[[:digit:]]{2})?)"
pattern_time_net = "(([[:digit:]]{1,2}:){1,2}[[:digit:]]{2}[#*]?)"
pattern_time_gun = "((([[:digit:]]{1,2}:){1,2}[[:digit:]]{2}[#*]?)?)"

#pattern_name = "(([[:alpha:] ]*[[:digit:]]?[[:alpha:]*[\\.,\\-'] ]?)*[[:alpha:][:digit:]]*)"
pattern_name = "(([[:alpha:]\\.,\\-'&]+\\s{1,2})*[[:alpha:]\\.,\\-'&]*)" #Here we assume that the name field does not contain any number and two words are separated by 1 spaces

patterns_field = c("([[:digit:]]+)", "(([[:digit:]]+/[[:digit:]]+)?)", pattern_name, "([[:digit:]]*)", "([[:digit:]]{0,2})", pattern_name, pattern_time_gun, pattern_time_net, pattern_time, "(.?)", pattern_time, pattern_time, pattern_time, pattern_time, pattern_time)
count_group = stri_count(patterns_field , fixed = "(")
names(patterns_field) = fields_all
names(count_group) = fields_all

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
    return (names(sort(pos)))
  }
}

# Function to generate the gsub pattern and the replace patterns according to the given field
getFieldPatterns <- function(fieldnames) {
  n_fields = length(fieldnames)
  if (n_fields < 1) {
    return (c("", ""))
  }
  pattern = "^"
  replace = ""
  count_field = 1
  #print(fieldnames)
  for (fieldname in fieldnames){
    pattern = paste(pattern, patterns_field[fieldname], " *", sep = "")
    replace = paste(replace, "$", count_field, ";", sep = "")
    #replace = paste(replace, "\\", count_field, ";", sep = "")
    count_field = count_field + count_group[fieldname]
  }
  pattern = paste(pattern, "$", " *", sep = "")
  pattern = substr(pattern, 1, nchar(pattern) - 2)
  replace = substr(replace, 1, nchar(replace) - 1)
  return (c(pattern, replace))

}

# Function to trim leading and trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

# Function to clean format of the lines in the file to meet the RE. Designed based on trial and error
cleanLines <- function(filelines) {
  filelines = gsub("2[nN][dD]", "second", filelines) # replace "2nd" with "second"
  filelines = gsub("3[rR][dD]", "third", filelines) # replace "2nd" with "second"

  filelines = gsub("8illiam", "William", filelines) # The "8illiam" in men10Mile_2000
  filelines = gsub("Christopher5", "Christopher", filelines) # The "Christopher5" in men10Mile_2007

  filelines = gsub("\\s\\((\\w*)\\)", "", filelines)# remove aka in the middle of the name
  filelines = gsub("\\s\\(", "", filelines)
  filelines = gsub("\\s\\)", "", filelines)

  filelines = gsub("`", "", filelines) # remove meaningless symbols (e.g."`" in "men10Mile_2001")
  filelines = gsub("([[:digit:]]{1,2}:)(\\s|$)", "\\100\\2", filelines) # Add the missed second part of the time in "men10Mile_2001")
  
  filelines = gsub("\\s+([jJsS][rR])(\\s|$)", " \\1\\2", filelines) #Remove the extra space before jr and sr in "men10Mile_2002"
  
  filelines = gsub("[[:digit:]]{5,}\\s([[:alpha:]]+)", " \\1", filelines) # The idiot who put his zip on Berlin in the address field in "men10Mile_2005"
  filelines = gsub("([aA][pP][tT]\\.?\\s?#?[[:digit:]]+\\w*)\\s", "", filelines) # The idiots who put their apt number in the address field in "men10Mile_2007"
  filelines = gsub("\\s[[:alpha:]]+@[[:alpha:]]+\\s", "", filelines) # The idiots who put their email address in the address field in "men10Mile_2007"
}

# Function to get the range of lines to containing the tables
getBeginEnd <- function(filelines, pattern) {
  begin = 0
  for (i_row in seq(length(filelines))) {
    if (stri_detect(filelines[i_row], regex = pattern)) {
      break
    }
  }
  begin = i_row
  
  end = length(filelines)
  for (i_row in seq(length(filelines), 1)) {
    if (stri_detect(filelines[i_row], regex = pattern)) {
      break
    }
  }
  end = i_row
  return (c(begin, end))
}
# Function to analyze each file
analyzeFile <- function(filename, path) {
  if (!str_detect(filename, "^(men|women)10Mile_[[:digit:]]{4}$")) {
    return (NULL)
  }
  gender = str_extract(filename, "^(men|women)")
  year = str_extract(filename, "[:digit:]{4}$")
  fullname = paste(path, filename, sep="")
  filelines = trim(readLines(fullname))
  filelines = cleanLines(filelines)
  #print(filelines[1])
  fieldnames = getFields(filelines)

  pr = getFieldPatterns(fieldnames)
  
  be = getBeginEnd(filelines, pr[1]) # locate the first line to read
  #print(be)
  #print(pr)
  #filelines = stri_replace_all_regex(filelines[2791], pr[1], pr[2]) # Add the delimeters
  filelines = stri_replace_all_regex(filelines, pr[1], pr[2]) # Add the delimeters
  #data_part = filelines[be[1]:be[2]]
  #x = stri_count(data_part, fixed=';')
  #print(x[891])
  #print(data_part[1])
  tc <- textConnection(filelines[be[1]:be[2]])
  data <- read.table(tc, sep=";", header = FALSE, strip.white=TRUE, blank.lines.skip = TRUE, fill = TRUE, col.names = fieldnames, quote = "", comment.char="")

  #str_replace(filelines[86], pr[1], pr[2])
  #return(list(c(gender, year)))
}
  
#data_raw = sapply(files, analyzeFile, "./data/")
 path = "./data/"
 file = c("men10Mile_2008")
 data_raw = analyzeFile(file, path)
 #print(data_raw[which(is.na(data_raw$age)),])
