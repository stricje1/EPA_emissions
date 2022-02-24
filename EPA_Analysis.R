################################ Begin Plot1.R #################################
#
# Question 1. Have total emissions from PM2.5 decreased in the United States 
# from 1999 # to 2008? Using the base plotting system, make a plot showing the 
# total PM2.5 emission from all sources for each of the years 1999, 2002, 2005,
# and 2008.

# Getting the data, clean, and manipulated for analysis

library(ggplot2)
library(RColorBrewer)

# Check the working directory
getwd()

# Download and unzip the file:
# * Set the filename to match "summarySCC_PM25.rd"
# * Check to see if already downloaded and unzipped working directory (WD)
# * If filename is missing, download to the WD from the given URL
# * Unzip the air_pollution.zip into the WD
# * summarySCC_PM25.rds & Source_Classification_Code.rds appear in the WD

filename = "summarySCC_PM25.rds"
if (!file.exists(filename)){
  urlzip <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
  download.file(urlzip, destfile = "./air_pollution.zip" )
  unzip("./air_pollution.zip", exdir = "." )
}  

# Load the data:
NEI <- readRDS("./summarySCC_PM25.rds")
SCC <- readRDS("./Source_Classification_Code.rds")

# Check NEI data structure
str(NEI)

# Check SCC data structure
str(SCC)
  
# Define dataset for analysis and plotting
# Aggregate the Emissions data from NEI by year and sum them

totalNEI <- aggregate(Emissions ~ year, NEI, sum)

# Construct a combined line-point plot showing emissions from the given years
png(file="./plot1.png", width=600, height=350)
plot(totalNEI$year, totalNEI$Emissions, # Plot variables
     type = "o",                        # Plot type is overlaid (line & points)
     col = "dodgerblue",                # Plot line color
     lwd=3,                             # Plot line thickness 
     font.main = 3,                     # Main title font size
     col.main = "darkred",              # Main title color
     main = expression("Total US "~ PM[2.5]~ "Emissions by Year"), 
                                        # Title label
     col.lab = "purple",                # Axes labels text color
     ylab = expression("Total US "~   PM[2.5] ~ "Emissions"), 
                                        # y-axis label text 
     xlab = "Year",                     # x-axis label text
     fg = "green3",                     # plot axes color
     pch = 23,                          # plot point type
     bg = "red",                        # plot point fill color
     cex = 2)                           # Plot point size

dev.off()

ggsave("plot1.png", width = 15, height = 10, units = "cm")

# Answer to Question 1:
# Total emissions from PM2.5 have decreased in the United States from 1999 
# to 2008.
################################# End Plot1.R ##################################

################################ Begin Plot2.R #################################
#
# Question 2. Have total emissions from PM2.5 decreased in the Baltimore City, 
# Maryland (fips == “24510”) from 1999 to 2008? Use the base plotting system to 
# make a plot answering this question.

# Getting the data, clean, and manipulated for analysis

library(ggplot2)
library(RColorBrewer)

# Check the working directory

getwd()

# Download and unzip the file:
# * Set the filename to match "summarySCC_PM25.rd"
# * Check to see if already downloaded and unzipped working directory (WD)
# * If filename is missing, download to the WD from the given URL
# * Unzip the air_pollution.zip into the WD
# * summarySCC_PM25.rds & Source_Classification_Code.rds appear in the WD

filename = "summarySCC_PM25.rds"
if (!file.exists(filename)){
  urlzip <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
  download.file(urlzip, destfile = "./air_pollution.zip" )
  unzip("./air_pollution.zip", exdir = "." )
}  

# Load the data:
NEI <- readRDS("./summarySCC_PM25.rds")
SCC <- readRDS("./Source_Classification_Code.rds")

# Check NEI data structure
str(NEI)

# Check SCC data structure
str(SCC)

# Extract Baltimore City (FIPS Code 24510) Emissions data for the corresponding
# years and sum them for each year
baltimore <- subset(NEI, NEI$fips == "24510")

totalBaltimore <- aggregate(Emissions ~ year, baltimore, sum)

# Construct a combined line-point plot showing emissions for Baltimore City 
# for the given years 1999 to 2008

plot(totalBaltimore$year, totalBaltimore$Emissions, 
     type = "o",                        # Plot type is overlaid (line & points)
     col = "green4",                    # Plot line color
     lwd=3,                             # Plot line thickness 
     font.main = 3,                     # Main title font size
     col.main = "darkgreen",            # Main title color
     main = expression("Total Baltimore" ~ PM[2.5] ~ "Emissions by Year"), 
     xlim = c(1998,2010),
     xlab = "Year",                     # x-axis label text
     ylab = expression("Total Baltimore "~ PM[2.5] ~ "Emissions"), 
     col.lab = "blue3",
     fg = "maroon4",                    # plot axes color
     col.axis = "maroon4",              # axis labels color
     pch = 23,                          # plot point type
     bg = "orange",                     # plot point fill color
     cex = 2)                           # Plot point size)
# Add date labels to the plot
text(Emissions ~year, labels=totalBaltimore$year,data=totalBaltimore, cex=1, font=2, pos=4) 

ggsave("plot2.png", width = 15, height = 10, units = "cm")

# Answer to Question 2:
# The total emissions from PM2.5 did decrease in the Baltimore City, Maryland 
# from 1999 to 2008. However, this is not a complete picture. Although decreasing
# from 1999 to 2002, the was a steep increase from 2002 to 2005, nearly at the 
# 1999 value. After that there was another drastic decline from 205 to 2008.
################################# End Plot2.R ##################################

################################ Begin Plot3.R #################################
#
# Question 3. Of the four types of sources indicated by the type (point, nonpoint, 
# onroad, nonroad) variable, which of these four sources have seen decreases in
# emissions from 1999-2008 for Baltimore City? Which have seen increases in 
# emissions from 1999-2008? Use the ggplot2 plotting system to make a plot 
# answer this question. library(ggplot2)

# Getting the data, clean, and manipulated for analysis

library(ggplot2)
library(hrbrthemes)
library(RColorBrewer)

# Check the working directory

getwd()

# Download and unzip the file:
# * Set the filename to match "summarySCC_PM25.rd"
# * Check to see if already downloaded and unzipped working directory (WD)
# * If filename is missing, download to the WD from the given URL
# * Unzip the air_pollution.zip into the WD
# * summarySCC_PM25.rds & Source_Classification_Code.rds appear in the WD

filename = "summarySCC_PM25.rds"
if (!file.exists(filename)){
  urlzip <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
  download.file(urlzip, destfile = "./air_pollution.zip" )
  unzip("./air_pollution.zip", exdir = "." )
}  

# Load the data:
NEI <- readRDS("./summarySCC_PM25.rds")
SCC <- readRDS("./Source_Classification_Code.rds")

# Check NEI data structure
str(NEI)

# Check SCC data structure
str(SCC)


# Description of the base graphic object
str(g)

# Construct the data setcomprised of emissions by source (type) in Baltimore 
# City (FIPS Code 24510) from 1999 to 2008

baltimore <- subset(NEI, NEI$fips == "24510")
baltimoreType <- aggregate(Emissions ~ year + type, baltimore, sum)


# Plot the four types of sources indicated by the type (point, nonpoint, 
# onroad, nonroad) variable and the corresponding emissions fro 1999 to 2008 

g <- ggplot(baltimoreType, aes(year, Emissions, col = type)) ## Base graphic object

# Add plot as a line type with 50% increase in size
g + geom_line(size = 1.5) + 
  # Plot is a line type with 50% increase in size
    geom_point(size = 3, pch =23, fill="yellow") +
  # Main title label text
    ggtitle(expression("Total Baltimore " ~ PM[2.5] ~ "Emissions:")) + #Title label
  # Main subtitle label text 
    labs(subtitle=expression("by Type and Year")) + 
  # y-axis label text
    ylab(expression("Total Baltimore " ~ PM[2.5] ~ "Emissions")) +
  # x-axis lable text
    xlab("Year") +
  # plot line colors varied by pollution sources
    scale_colour_discrete(name = "Type of sources") +
  # plot themes including: background color
    theme(plot.background = element_rect(fill = "darkblue"), 
          # colors of axis titles 
          axis.title=element_text(face = "bold.italic", color = "white"),
          # color of legend title
          legend.title = element_text(face = "bold", color="blue4"),
          # orientation, size, and color of x-axis labels
          axis.text.x = element_text(angle = 45, hjust = 1, color = "white"),
          # orientation, size, and color of x-axis labels
          axis.text.y = element_text(angle = 45, hjust = 1, color = "white"),
          # font size, color, and facing of main title
          plot.title= element_text(size=18,
                                 color="cyan",
                                 face="bold"),
          # font size, color, and facing of main subtitle
          plot.subtitle = element_text(size=14,
                                       color="red",
                                       face="bold"))

ggsave("plot3.png", width = 15, height = 10, units = "cm")

# Answer to Question 3:
# The emissions from point sources is higher in 2008 that it was in 1999, even 
# though it has risen and fell. For the sources non-point, road, and on-road the
# 2008 levels are lower that those of 1999, and the overall trend is decreasing.
################################# End Plot3.R ##################################

################################ Begin Plot4.R #################################
#
# Question 4. Across the United States, how have emissions from coal combustion-
# related sources changed from 1999-2008?

# Getting the data, clean, and manipulated for analysis

library(ggplot2)
library(hrbrthemes)
library(RColorBrewer)
library(ggformula)

# Check the working directory

getwd()

# Download and unzip the file:
# * Set the filename to match "summarySCC_PM25.rd"
# * Check to see if already downloaded and unzipped working directory (WD)
# * If filename is missing, download to the WD from the given URL
# * Unzip the air_pollution.zip into the WD
# * summarySCC_PM25.rds & Source_Classification_Code.rds appear in the WD

filename = "summarySCC_PM25.rds"
if (!file.exists(filename)){
  urlzip <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
  download.file(urlzip, destfile = "./air_pollution.zip" )
  unzip("./air_pollution.zip", exdir = "." )
}  

# Load the data:
NEI <- readRDS("./summarySCC_PM25.rds")
SCC <- readRDS("./Source_Classification_Code.rds")

# Check NEI data structure
str(NEI)

# Check SCC data structure
str(SCC)

# Construct the data set comprised of emissions from coal related combustion
# across the United States from 1999 to 2008. This requires aggregating data
# from both base data sets and use of the pattern-matching and replacement 
# function grepl() for "coal".

SCCcoal <- SCC[grepl("coal", SCC$Short.Name, ignore.case = T),]
NEIcoal <- NEI[NEI$SCC %in% SCCcoal$SCC,]
totalCoal <- aggregate(Emissions ~ year + type, NEIcoal, sum)
totalCoal2 <- aggregate(Emissions ~ year, NEIcoal, sum)

# Define a ggplot combining line-point plots for coal related emissions
g= ggplot(totalCoal2, aes(year, Emissions, col = "Emissons"))

# Description of the base graphic object
str(g)

g+geom_line(size=1.5) +
  geom_point(col = "navyblue", size = 3) +   xlim(1997, 2008) +
  geom_spline(aes(x = year, y = Emissions, color = "Spline"), size = 1.25) +
  geom_text(aes(label=round(Emissions,0)), hjust=1.25, vjust=0, col = "red") +
  ggtitle(expression("Total" ~ PM[2.5] ~ "Coal Emission by Year Across the United States")) +
  xlab("Year") +
  ylab(expression("US " ~ PM[2.5] ~ "Coal Emission")) +
  theme(legend.title = element_text(face = "bold"))
  

g+geom_line(size=1.5) +
  geom_point(col = "navyblue", size =2.5) +
  geom_spline(
    stat = "spline",
    position = "identity",
    na.rm = FALSE,
    show.legend = TRUE,
    inherit.aes = TRUE)+
  ggtitle(expression("Total US" ~ PM[2.5] ~ "Coal Emission by Year")) +
  xlab("Year") +
  ylab(expression("US " ~ PM[2.5] ~ "Coal Emission")) +
  theme(legend.title = element_text(face = "bold"))
  

ggsave("plot4.png", width = 15, height = 10, units = "cm")

# Answer to Question 4:
# Across the United States, emissions from coal combustion-related sources has
# decreased from 1999-2008, and the overall trend is negative (decreasing) and 
# seen by the spline in plot4.png.

################################# End Plot4.R ##################################

################################ Begin Plot5.R #################################
#
# Question 5. How have emissions from motor vehicle sources changed from 
# 1999-2008 in Baltimore City?

# Getting the data, clean, and manipulated for analysis

library(ggplot2)
library(hrbrthemes)
library(RColorBrewer)

# Check the working directory

getwd()

# Download and unzip the file:
# * Set the filename to match "summarySCC_PM25.rd"
# * Check to see if already downloaded and unzipped working directory (WD)
# * If filename is missing, download to the WD from the given URL
# * Unzip the air_pollution.zip into the WD
# * summarySCC_PM25.rds & Source_Classification_Code.rds appear in the WD

filename = "summarySCC_PM25.rds"
if (!file.exists(filename)){
  urlzip <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
  download.file(urlzip, destfile = "./air_pollution.zip" )
  unzip("./air_pollution.zip", exdir = "." )
}  

# Load the data:
NEI <- readRDS("./summarySCC_PM25.rds")
SCC <- readRDS("./Source_Classification_Code.rds")

# Check NEI data structure
str(NEI)

# Check SCC data structure
str(SCC)

# Construct a dataset comprised of emissions data for Baltimore City (FIPS Code
# 24510) from 199 to 2008, aggregated by year and summed

baltimoreMotor <- subset(NEI, NEI$fips == "24510" & NEI$type == "ON-ROAD")
baltimoreMotorAGG <- aggregate(Emissions ~ year, baltimoreMotor, sum)

# Construct a bar chart comprised of four years (one bar for each) with summed
# emissions data for Baltimore City
# Define a base plotting object "g" using ggplot

g <- ggplot(baltimoreMotorAGG, aes(year, Emissions))

# Description of the base graphic object
str(g)

# Add geom functions to augment the plot and add aesthetics 

g+geom_bar(stat="identity",fill="steelblue",width=2) +
  # Add theme
  theme_bw() +  
  # Set guides for scale to FALSE
  guides(fill=FALSE) +
  # Add x and y axes labels
  labs(x="year", y=expression("Total PM"[2.5]*" Emission (10^5 Tons)")) + 
  # Add main titlle
  labs(title=expression("PM"[2.5]*" Motor Vehicle Source Emissions in Baltimore from 1999-2008"))  

ggsave("plot5.png", width = 16, height = 10, units = "cm")

# Answer to Question 5:
# Total emissions from motor vehicle sources in Baltimore City have decreased 
# from 1999-2008, with a clear downward trend.

################################# End Plot6.R ##################################

################################ Begin Plot6.R #################################
#
# 6. Compare emissions from motor vehicle sources in Baltimore City with 
# emissions from motor vehicle sources in Los Angeles County, California 
# (fips == “06037”). Which city has seen greater changes over time in motor 
# vehicle emissions?

# Getting the data, clean, and manipulated for analysis

library(ggplot2)
library(hrbrthemes)
library(RColorBrewer)

# Check the working directory

getwd()

# Download and unzip the file:
# * Set the filename to match "summarySCC_PM25.rd"
# * Check to see if already downloaded and unzipped working directory (WD)
# * If filename is missing, download to the WD from the given URL
# * Unzip the air_pollution.zip into the WD
# * summarySCC_PM25.rds & Source_Classification_Code.rds appear in the WD

filename = "summarySCC_PM25.rds"
if (!file.exists(filename)){
  urlzip <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
  download.file(urlzip, destfile = "./air_pollution.zip" )
  unzip("./air_pollution.zip", exdir = "." )
}  

# Load the data:
NEI <- readRDS("./summarySCC_PM25.rds")
SCC <- readRDS("./Source_Classification_Code.rds")

# Check NEI data structure
str(NEI)

# Check SCC data structure
str(SCC)

vehiclesNEI <- subset(NEI, NEI$fips %in% c("24510","06037") & NEI$type == "ON-ROAD")

SCC_Vehicles <- SCC %>%
  filter(grepl('[Vv]ehicle', SCC.Level.Two)) %>%
  select(SCC, SCC.Level.Two)

Balt_LA_Emissions <- NEI %>%
  filter(fips == "24510" | fips == "06037") %>%
  select(fips, SCC, Emissions, year) %>%
  inner_join(SCC_Vehicles, by = "SCC") %>%
  group_by(fips, year) %>%
  summarise(Total_Emissions = sum(Emissions, na.rm = TRUE)) %>%
  select(Total_Emissions, fips, year)

Balt_LA_Emissions$fips <- gsub("24510", "Baltimore City", Balt_LA_Emissions$fips)
Balt_LA_Emissions$fips <- gsub("06037", "Los Angeles County", Balt_LA_Emissions$fips)

# Construct a set of bar charts one for Baltimore City and one for Los Angeles 
# County comprised of four years (one bar for each) with summed emissions 
# Define a base plotting object "g" using ggplot

g <- ggplot(Balt_LA_Emissions, aes(x = factor(year), y = Total_Emissions, fill = fips)) +

# Description of the base graphic object
str(g)

  g + geom_bar(stat = "identity", width = 0.7) +
  facet_grid(.~fips) + scale_y_continuous(trans='log2')
  labs(x = "Year", y = "Emissions (Tons)", title = "Comparison of Motor Vehicle Related Emissions", subtitle = "Between Baltimore City and Los Angeles From 1999 - 2008") +
  theme(plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 14),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        strip.text.x = element_text(size = 12)) +
  theme_dark()
  
ggsave("plot6.png", width = 20, height = 10, units = "cm")
ggsave("plot6_log.png", width = 20, height = 10, units = "cm")

# Answer to Question 6:
# Comparing emissions from motor vehicle sources in Baltimore City with 
# emissions from motor vehicle sources in Los Angeles County, California are as
# follows: 
# 1. Los Angeles County has seen significant change in scale that Baltimore City. 
# 2. However, while not at the same scale, Baltimore city has seen a decline in 
#    vehicle emissions emissions.
# 3. While Los Angeles County has seen greater changes over time in motor vehicle 
#    emissions, the 2008 level is high than the 1999 level.
# 4. The picture is clearer in plot6_log.png with the y-axis logarithm 
#    transformation, with Baltimore City seeing greater downward changes over
#    time.

################################# End Plot6.R ##################################
