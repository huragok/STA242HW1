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

