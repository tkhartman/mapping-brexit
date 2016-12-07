##' ---
##' title: "Who Brexited? Mapping the 2016 EU Referendum Vote"
##' author: "Todd K. Hartman"
##' date: "`r format(Sys.Date())`"
##' output: "github_document"
##' ---

##' Housekeeping
## Load packages via 'pacman' package manager
pacman::p_load(broom, data.table, ggplot2, googleVis, rgdal)
  
## Set the working directory
# setwd("ENTER YOUR WORKING DIRECTORY HERE")

##' 2016 EU Referendum (Brexit) vote share data
## Download and import brexit data from the web (or get from Github directly)
url.df <- "http://www.electoralcommission.org.uk/__data/assets/file/0014/212135/EU-referendum-result-data.csv"
file.df <- basename(url.df)  # Extract the filename
if (!file.exists(file.df))   # Only download the file if it doesn't already exist in our working directory
    download.file(url = url.df, destfile = file.df, mode = "wb")
brexit <- fread(file.df)  # Read the dataset quickly
head(brexit)  # Check the data

## Remove Gibraltar from the dataset for mapping purposes
brexit <- brexit[!(brexit$Area == "Gibraltar"), ]

## Create a common identifier for mapping in ggplot
brexit <- brexit[order(Area_Code)]
brexit$id <- 0:380

## Display 'Leave' vote share from highest to lowest
brexit$Pct_Leave <- as.numeric(brexit$Pct_Leave)  # Percentage of leave vote share by administrative area
brexit2 <- subset(brexit, select = c("Area", "Pct_Remain", "Pct_Leave"))  # Subset the data
brexit2 <- brexit2[order(-Pct_Leave), ]  # Display the 'Leave' vote by area
print(brexit2[order(-Pct_Leave), ], nrows = 382)  # Display the 'Leave' vote by area

bar.plot <- gvisBarChart(brexit2, xvar = "Area", yvar = "Pct_Leave", 
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
if (!file.exists(file.zip)) 
    download.file(url = url.shp, destfile = file.zip, mode = "wb")
file.shp <- grep("shp", unzip(file.zip), ignore.case=TRUE, value=TRUE)  # Unzip the shapefile
uk.shp <- readOGR(file.shp, ogrListLayers(file.shp)[1])  # Load the shapefile

## Confirm that the brexit data matches the shapefile
names(uk.shp)  # Get the variables in the shapefile
sort(brexit$Area_Code) == sort(uk.shp@data$Area_Cd)  # Are the admin areas the same?

## Convert the raw data to a data.frame for ggplot
uk.shp@data$id <- rownames(uk.shp@data)  # Create an id to match the brexit data
uk.f <- tidy(uk.shp)  # Convert to a dataframe

## If desired, merge the map with brexit data
# uk.map <- merge(uk.f, brexit, by="id", all.x=TRUE)
# uk.map <- uk.map[order(uk.map$order), ]  # Order for plotting purposes
# head(uk.map)

##' Mapping Brexit vote share using ggplot
map <- ggplot() +
    geom_map(data = brexit, 
             aes(map_id = id, fill = Pct_Leave), 
             map = uk.f) + 
    expand_limits(x = uk.f$long, y = uk.f$lat) + 
    scale_fill_distiller(palette = "Spectral") +
    # scale_fill_gradient(low = "white", high = "#000034") +  Alternative colour option for printing
    ggtitle("Brexit Map: 2016 EU Referendum Vote Share") + 
    labs(fill = "Leave (%)") +
    theme(plot.title = element_text(lineheight = .8, face = "bold"),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank())

plot(map)
