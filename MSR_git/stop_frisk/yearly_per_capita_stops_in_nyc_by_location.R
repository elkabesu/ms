load('frisky.Rdata')
load('output.Rdata')

library(ggplot2)
library(ggmap)
library(data.table)
library(plyr)
library(scales)

# use available sqf data from before 2013
sqf <- sqf[year != 2013]
setnames(sqf, "suspect.race", "Race")
setnames(sqf, "suspect.sex", "Sex")
sqf <- sqf[Race %in% c("white hispanic", "black hispanic") , Race:= "hispanic"]

# match up sqf and census ages
setkey(sqf, suspect.age)
sqf[suspect.age < 5, Age := 0]
sqf[suspect.age >= 5 & suspect.age <= 9, Age := 5]
sqf[suspect.age >= 10 & suspect.age <= 14, Age := 10]
sqf[suspect.age >= 15 & suspect.age <= 17, Age := 15]
sqf[suspect.age >= 18 & suspect.age <= 19, Age := 18]
sqf[suspect.age >= 22 & suspect.age <= 24, Age := 22]
sqf[suspect.age >= 25 & suspect.age <= 29, Age := 25]
sqf[suspect.age >= 30 & suspect.age <= 34, Age := 30]
sqf[suspect.age >= 35 & suspect.age <= 39, Age := 35]
sqf[suspect.age >= 40 & suspect.age <= 44, Age := 40]
sqf[suspect.age >= 45 & suspect.age <= 49, Age := 45]
sqf[suspect.age >= 50 & suspect.age <= 54, Age := 50]
sqf[suspect.age >= 55 & suspect.age <= 59, Age := 55]
sqf[suspect.age >= 60 & suspect.age <= 61, Age := 60]
sqf[suspect.age >= 62 & suspect.age <= 64, Age := 62]
sqf[suspect.age >= 65 & suspect.age <= 66, Age := 65]
sqf[suspect.age >= 67 & suspect.age <= 69, Age := 67]
sqf[suspect.age >= 70 & suspect.age <= 74, Age := 70]
sqf[suspect.age >= 75 & suspect.age <= 79, Age := 75]
sqf[suspect.age >= 80 & suspect.age <= 84, Age := 80]
sqf[suspect.age >= 85, Age := 85]

sqf <- sqf[!is.na(Age) & !is.na(Race) & !is.na(Sex) & !is.na(lon) & !is.na(lat)]

# get map of nyc
center_lon <- mean(sqf$lon, na.rm = TRUE)
center_lat <- mean(sqf$lat, na.rm = TRUE)
nyc_map <- qmap(location = c(center_lon, center_lat), zoom = 11, source = "stamen", maptype = "toner")

# find square length & make each lon,lat a part of the left-bottom most edge of the grid
min_lon <- min(sqf$lon, na.rm = TRUE)
max_lon <- max(sqf$lon, na.rm = TRUE)
min_lat <- min(sqf$lat, na.rm = TRUE)
max_lat <- max(sqf$lat, na.rm = TRUE)
square_length = (max_lon - min_lon) / 50    # square length is approximately 1km or .65 mile
sqf <- sqf[,  left_lon := floor((lon - min_lon) / square_length) * square_length + min_lon]
sqf <- sqf[,  bottom_lat := floor((lat - min_lat) / square_length) * square_length + min_lat]

group_by_stopped <- sqf[, list(num_stopped_year = .N / 7, num_arrested_year = sum(arrested) / 7, num_stopped_in_housing = sum(location.housing == "housing") / 7), by = c('Race', 'Age', 'Sex', 'left_lon', 'bottom_lat') ][order(-num_stopped_year)]



# turn counties into boroughs in census
counties <- unique(output$County)
borough <- c('Bronx', 'Brooklyn', 'Manhattan', 'Queens', 'Staten Island')
boroughs <- data.table(county = counties, borough = borough)
setnames(output, "County", "county")
output <- merge(output, boroughs, by = "county")

# merge census to shapefile table to get lon,lat of every census block
block_by_loc <- block.shapes[ , list(lon = mean(lon), lat = mean(lat)), by = c('borough', 'tract', 'block')]
setnames(output, "Tract", "tract")
setnames(output, "Block", "block")
output <- output[!is.na(Race)]
output <- output[Race == "two or more", Race := "other"]
output <- output[Race == "native hawaiian", Race := "asian"]
group_by_blocks <- merge(output, block_by_loc, by = c('borough','tract','block'))
all_people <- group_by_blocks[Count > 0 ][order(-Count)]

# make each lon,lat a part of the left-bottom most edge of the grid
all_people <- all_people[,  left_lon := floor((lon - min_lon) / square_length) * square_length + min_lon]
all_people <- all_people[,  bottom_lat := floor((lat - min_lat) / square_length) * square_length + min_lat]

# merge sqf and census tables
all_people <- all_people[ , list(num_people = sum(Count)), by = c('Race', 'Sex', 'Age', 'left_lon', 'bottom_lat')]
all_people <- all_people[Race != "white"]
all_people <- all_people[Race == "white_not_HL", Race := "white"]
census_stops <- merge(group_by_stopped, all_people, by = c('Race', 'Age', 'Sex', 'left_lon', 'bottom_lat'), all.y = TRUE)[order(-num_stopped_year)]
census_stops[is.na(num_stopped_year), num_stopped_year := 0]
census_stops[is.na(num_arrested_year), num_arrested_year := 0]

# bin the ages
census_stops[Age < 18, sAge := "<18"]
census_stops[Age >= 18 & Age <= 21, sAge := "18-21"]
census_stops[Age == 22, sAge := "22-24"]
census_stops[Age == 25, sAge := "25-29"]
census_stops[Age == 30 | Age == 35, sAge := "30-39"]
census_stops[Age == 40 | Age == 45, sAge := "40-49"]
census_stops[Age >= 50, sAge := ">50"]

# group by Race, Age, Sex, and Location
census_stops <- census_stops[ , list(num_stopped_year, num_arrested_year, num_people, num_stopped_in_housing), 
                             by = c('Race', 'sAge', 'Sex', 'left_lon', 'bottom_lat')]
census_stops <- census_stops[ , ypcs := num_stopped_year / num_people]
census_stops <- census_stops[, innocent_ypcs := (num_stopped_year - num_arrested_year) / num_people]
census_stops <- census_stops[order(-innocent_ypcs)]
census_stops[is.na(num_stopped_in_housing), num_stopped_in_housing := 0]


# create squares on nyc map to show stops & stoprate differences
young_people <- census_stops[sAge == "18-21" | sAge == "22-24" | sAge == "25-29" ]
young_men <- young_people[Sex == "male"]

young_black_men <- young_men[Race == "black"]
young_white_men <- young_men[Race == "white"]
young_hispanic_men <- young_men[Race == "hispanic"]

# map yearly per capita stops for young black men
# most stopped in Jamaica Queens (York College). Also, 14th St. & 7th Ave.  ~ 3.2
black_by_loc <- young_black_men[, list(num_people=sum(num_people), num_stopped_year=sum(num_stopped_year),
                                       num_arrested_year = sum(num_arrested_year), num_stopped_in_housing = sum(num_stopped_in_housing),
                                      ypcs = sum(num_stopped_year)/sum(num_people),
                                      innocent_ypcs=(sum(num_stopped_year) - sum(num_arrested_year))/sum(num_people)),
                               by=c('left_lon','bottom_lat')][order(-innocent_ypcs)]

black_by_loc <- black_by_loc[num_people >= 100]
black_by_loc$id <- 1:nrow(black_by_loc)
black_by_loc <- black_by_loc[rep(1:nrow(black_by_loc), each=4)]
black_by_loc <- black_by_loc[, lon := left_lon + rep(c(0,0,square_length,square_length), length.out=.N)]
black_by_loc <- black_by_loc[, lat := bottom_lat + rep(c(0,square_length,square_length,0), length.out=.N)]
nyc_map + geom_polygon(data = black_by_loc, aes(fill = innocent_ypcs, group = id), alpha = .9) + scale_fill_gradient(lim = c(.25,3.3),low = "orange", high = "dark red", "Yearly per\ncapita stops") + theme(legend.title = element_text(size = 15), legend.text = element_text(size = 15))



# map yearly per capita stops for young hispanic men
# most stopped by 34th St Herald Square   ~ 2.5
hispanic_by_loc <- young_hispanic_men[, list(num_people=sum(num_people), num_stopped_year=sum(num_stopped_year),
                                             num_arrested_year = sum(num_arrested_year), num_stopped_in_housing = sum(num_stopped_in_housing),
                                       ypcs = sum(num_stopped_year)/sum(num_people),
                                       innocent_ypcs=(sum(num_stopped_year) - sum(num_arrested_year))/sum(num_people)),
                                by=c('left_lon','bottom_lat')][order(-innocent_ypcs)]

hispanic_by_loc <- hispanic_by_loc[num_people >= 100]
hispanic_by_loc$id <- 1:nrow(hispanic_by_loc)
hispanic_by_loc <- hispanic_by_loc[rep(1:nrow(hispanic_by_loc), each=4)]
hispanic_by_loc <- hispanic_by_loc[, lon := left_lon + rep(c(0,0,square_length,square_length), length.out=.N)]
hispanic_by_loc <- hispanic_by_loc[, lat := bottom_lat + rep(c(0,square_length,square_length,0), length.out=.N)]
nyc_map + geom_polygon(data = hispanic_by_loc, aes(fill = innocent_ypcs, group = id), alpha = .9) + scale_fill_gradient(lim = c(.25,3.3),low = "orange", high = "dark red", "Yearly per\ncapita stops") + theme(legend.title = element_text(size = 15), legend.text = element_text(size = 15))

# map yearly per capita stops for young white men
# most stopped blocks from Coney Island   ~ .5
white_by_loc <- young_white_men[, list(num_people=sum(num_people), num_stopped_year=sum(num_stopped_year),
                                       num_arrested_year = sum(num_arrested_year), num_stopped_in_housing = sum(num_stopped_in_housing),
                                       ypcs = sum(num_stopped_year)/sum(num_people),
                                       innocent_ypcs=(sum(num_stopped_year) - sum(num_arrested_year))/sum(num_people)),
                                by=c('left_lon','bottom_lat')][order(-innocent_ypcs)]

white_by_loc <- white_by_loc[num_people >= 100]
white_by_loc$id <- 1:nrow(white_by_loc)
white_by_loc <- white_by_loc[rep(1:nrow(white_by_loc), each=4)]
white_by_loc <- white_by_loc[, lon := left_lon + rep(c(0,0,square_length,square_length), length.out=.N)]
white_by_loc <- white_by_loc[, lat := bottom_lat + rep(c(0,square_length,square_length,0), length.out=.N)]
nyc_map + geom_polygon(data = white_by_loc, aes(fill = innocent_ypcs, group = id), alpha = .9) + scale_fill_gradient(lim = c(.25,3.3),low = "orange", high = "dark red", "Yearly per\ncapita stops") + theme(legend.title = element_text(size = 15), legend.text = element_text(size = 15))

info_by_loc <- census_stops[, list(num_people=sum(num_people), num_stopped_year=sum(num_stopped_year),
                                       num_arrested_year = sum(num_arrested_year), num_stopped_in_housing = sum(num_stopped_in_housing),
                                       ypcs = sum(num_stopped_year)/sum(num_people),
                                       innocent_ypcs=(sum(num_stopped_year) - sum(num_arrested_year))/sum(num_people)),
                                by=c('left_lon','bottom_lat')][order(-innocent_ypcs)]

#info_by_loc <- info_by_loc[num_people >= 100]
info_by_loc$id <- 1:nrow(info_by_loc)
info_by_loc <- info_by_loc[rep(1:nrow(info_by_loc), each=4)]
info_by_loc <- info_by_loc[, lon := left_lon + rep(c(0,0,square_length,square_length), length.out=.N)]
info_by_loc <- info_by_loc[, lat := bottom_lat + rep(c(0,square_length,square_length,0), length.out=.N)]
nyc_map + geom_polygon(data = info_by_loc, size = 1, color = "red", aes(group = id), alpha = 0) + scale_fill_gradient(lim = c(.25,3.3),low = "orange", high = "dark red", "Yearly per\ncapita stops") + scale_color_discrete(guide = FALSE) + scale_size_discrete(guide = FALSE) + scale_fill_continuous(guide = FALSE)


