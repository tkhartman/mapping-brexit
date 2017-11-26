##' ---
##' title: "Who Brexited? Mapping the 2016 EU Referendum Vote"
##' author: "Todd K Hartman"
##' date: "Last updated `r format(Sys.Date())`"
##' output: "github_document"
##' ---
    
##' Housekeeping
## Load packages via 'pacman' package manager
pacman::p_load(broom, ggplot2, googleVis, rgdal)

## Set the working directory
# setwd("ENTER YOUR WORKING DIRECTORY HERE")

##' 2016 EU Referendum (Brexit) vote share
## Download and import brexit data from the web (or get from Github directly)
url.df <- "https://goo.gl/Bk1vq6"
file.df <- "EU-referendum-result-data.csv"

## Only download the file if it doesn't exist in the working directory
if (!file.exists(file.df))
    download.file(url = url.df, destfile = file.df)

## Load the brexit data
brexit <- read.csv(file.df)
head(brexit)

## Order 'leave' vote share from highest to lowest
rank <- order(-brexit$Pct_Leave)  # Create vector of rankings 
brexit <- brexit[rank, ]  # Sort the data by leave rank, highest to lowest

## Display 'Leave' vote share from highest to lowest
bar.plot <- gvisBarChart(brexit, xvar = "Area", yvar = "Pct_Leave", 
                         options = list(legend = "none",
                                        vAxes = "[{textStyle:{fontSize: '16'}}]",
                                        chartArea = "{left:250,top:10,bottom:10}",
                                        width= 800, height = 10000) )
plot(bar.plot)

## Save the bar chart
bar.plot$html$chart
cat(bar.plot$html$chart, file = "brexit.bar.plot.html")

##' UK ESRI shapefile
## Download and import from the web (or get from Github)
url.shp <- "https://t.co/GYqX2PCqJ0"
file.zip <- "esri.zip"

## Download the shape file if it's not present
if (!file.exists(file.zip)) 
    download.file(url = url.shp, destfile = file.zip, mode = "wb")

## Unzio and extract the .shp file
file.shp <- grep("shp", unzip(file.zip), 
                 ignore.case=TRUE, value=TRUE)

## Import the .shp file into R
uk.shp <- readOGR(file.shp, 
                  ogrListLayers(file.shp)[1])  # Load the shapefile

## Remove Gibraltar
brexit2 <- brexit[!(brexit$Area == "Gibraltar"), ]

## Fix levels to remove 'Gibraltar'
brexit2 <- droplevels(brexit2)

## Only keep variables needed for mapping
brexit2 <- subset(brexit2, select = c("Area_Code", "Pct_Leave"))

## Change names to match shape file for merging
names(brexit2) <- c("Area_Cd", "Pct_Leave")

## Sort the data by Area Code for merging
ac.order <- order(brexit2$Area_Cd)  # Create ordering id
brexit2 <- brexit2[ac.order, ]  # Sort the data by ordering id

## Confirm that the Area Code in the brexit data matches the shapefile
sort(brexit2$Area_Cd) == sort(uk.shp@data$Area_Cd)

## Merge the datasets
uk.merged <- merge(uk.shp, brexit2, by = "Area_Cd")

## Convert to a data.frame
uk.df <- uk.merged@data

## Convert the map data to a data.frame for ggplot using the 'broom' package
uk.map <- tidy(uk.merged)

## Adjust data 'id' so that it matches the map 'id'
uk.df$id <- uk.df$id - 1 

## Mapping Brexit vote share using ggplot
map <- ggplot() +  # Create the plot
    geom_map(data = uk.df,  # Make a map
             aes(map_id = uk.df$id, fill = uk.df$Pct_Leave),  # Fill with 'leave' vote
             map = uk.map) +  # Draw the map using converted .shp file
    expand_limits(x = uk.map$long, y = uk.map$lat) + # Needed to display map
    scale_fill_distiller(palette = "Spectral") +  # Choose colours
    # scale_fill_gradient(low = "white", high = "#000034") +  # Colours for printing
    ggtitle("Brexit Map: 2016 EU Referendum Vote Share") +  # Title
    labs(fill = "Leave (%)") +  # Legend label
    theme(plot.title = element_text(lineheight = .8, face = "bold"),  # Bold title
          axis.title.x = element_blank(),  # Remove x-axis title
          axis.text.x = element_blank(),  # Remove x-axis labels
          axis.title.y = element_blank(),  # Remove y-axis title
          axis.text.y = element_blank(),  # Remove y-axis labels
          axis.ticks = element_blank())  # Remove axis tick marks

plot(map)