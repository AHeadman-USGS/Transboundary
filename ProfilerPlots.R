library(dataRetrieval)
library(scales)
library(plotly)
library(zoo)
library(lubridate)


setwd("c:/ssiefken/Koocanusa/Data/Platform") #This will obviously need changing if run by someone other than me



##Data retrieval from NWIS - needs depth time series published to work
siteNum <-  "12300110" #Lake Koocanusa at boundary
pCodes <- c("00010", "00300", "00400", "00095", "63680") #List of parameter codes
#Parameter Codes:
#Temp: 00010
#DO mg/L: 00300
#DO %Sat: 00301
#pH: 00400
#Specific Conductance: 00095
#Turbidity: 63680
#Chlorophyll RFU: 32315
#BGA-PC RFU: 32321
#fDOM RFU: 32322
#Sensor Depth (m) : 72148

start.date <- as.Date("2020-01-01")
end.date <- Sys.Date()

#Request NWIS data
NWISdata <- renameNWISColumns(readNWISuv(siteNum, pCodes, startDate = start.date, endDate = end.date))
NWISdata <- complete.cases(NWISdata) #Remove missing rows







##Data retrieval from raw data file, data file should be placed in the working directory
dataFile <- "12300110_sampleData.txt" #Path to data file
LakeData <- read.csv(dataFile, skip=4, header = F, na.string="NAN")
LakeData <- LakeData[complete.cases(LakeData),] #Remove any rows with NA values
LakeData <- unique(LakeData) #Remove any duplicate rows
colnames(LakeData) <- c("dateTime", "PFL_Counter", "X2", "X3", "depth_m", "Temp_C", "ODOpercent", "ODOmg", "pH", "SC", "turb", "BGA_RFU", "Chl_RFU", "fDOM_RFU", "depth_m_sonde")
LakeData$dateTime <- as.POSIXct(LakeData$dateTime, format="%Y-%m-%d %H:%M:%S")

LakeData$depth_m <- round(LakeData$depth_m) #Round depth to nearest meter for consistent plotting

LakeData$PFL_time <- as.POSIXct(NA)
LakeData$dateTime <- round_date(LakeData$dateTime, unit="2 hours") #Round times to nearest 2 hours (based on 2 hours/profile)

#Select date range of interest
LakeData <- LakeData[LakeData$dateTime > "2020-07-01",]
LakeData <- LakeData[LakeData$dateTime <= "2020-10-23",]






##Plot generation
#Variable names will need to be modified to plot NWIS data instead data from file


#Plot Temperature
p <- plot_ly( x = LakeData$dateTime, y = LakeData$depth_m, z = round(LakeData$Temp_C, 1), type = "contour", colorscale='Jet',
              line = list(width=0), contours = list(start = 4, end = 25, size = 0.1), colorbar = list(title='Temp (C)')) %>%
  layout(title="12300110 Lake Koocanusa at International Boundary", yaxis = list(autorange = "reversed", title = 'Depth (m)'), xaxis = list(title = 'Date'))

htmlwidgets::saveWidget(as_widget(p), "./12300110_TempC_2020.html")

#Plot specific conductance
p <- plot_ly( x = LakeData$dateTime, y = LakeData$depth_m, z = round(LakeData$SC), type = "contour", colorscale='Jet',
              line = list(width=0), contours = list(start = 200, end = 300, size = 1), colorbar = list(title='Specific Conductance \n (μS/cm)')) %>%
  layout(title="12300110 Lake Koocanusa at International Boundary", yaxis = list(autorange = "reversed", title = 'Depth (m)'), xaxis = list(title = 'Date'))

htmlwidgets::saveWidget(as_widget(p), "./12300110_SC_2020.html")

#Plot ODO mg/L
p <- plot_ly( x = LakeData$dateTime, y = LakeData$depth_m, z = round(LakeData$ODOmg,2), type = "contour", colorscale='Jet',
              line = list(width=0), contours = list(start = 4, end = 12, size = 0.01), colorbar = list(title='Dissolved Oxygen \n (mg/L)')) %>%
  layout(title="12300110 Lake Koocanusa at International Boundary", yaxis = list(autorange = "reversed", title = 'Depth (m)'), xaxis = list(title = 'Date'))

htmlwidgets::saveWidget(as_widget(p), "./12300110_DOmg_2020.html")

#Plot pH
p <- plot_ly( x = LakeData$dateTime, y = LakeData$depth_m, z = round(LakeData$pH,2), type = "contour", colorscale='Jet', reversescale=TRUE,
              line = list(width=0), contours = list(start = 7.5, end = 9, size = 0.01), colorbar = list(title='pH')) %>%
  layout(title="12300110 Lake Koocanusa at International Boundary", yaxis = list(autorange = "reversed", title = 'Depth (m)'), xaxis = list(title = 'Date'))

htmlwidgets::saveWidget(as_widget(p), "./12300110_pH_2020.html")

#Plot turbidity
p <- plot_ly( x = LakeData$dateTime, y = LakeData$depth_m, z = round(LakeData$turb,2), type = "contour", colorscale='Jet',
              line = list(width=0), contours = list(start = 0, end = 20, size = 0.1), colorbar = list(title='Turbidity (FNU)')) %>%
  layout(title="12300110 Lake Koocanusa at International Boundary", yaxis = list(autorange = "reversed", title = 'Depth (m)'), xaxis = list(title = 'Date'))

htmlwidgets::saveWidget(as_widget(p), "./12300110_turb_2020.html")

#Plot fDOM
p <- plot_ly( x = LakeData$dateTime, y = LakeData$depth_m, z = round(LakeData$fDOM_RFU,2), type = "contour", colorscale='Jet',
              line = list(width=0), contours = list(start = 0, end = 8, size = 0.1), colorbar = list(title='fDOM (RFU)')) %>%
  layout(title="12300110 Lake Koocanusa at International Boundary", yaxis = list(autorange = "reversed", title = 'Depth (m)'), xaxis = list(title = 'Date'))

#Plot Chlorophyll
p <- plot_ly( x = LakeData$dateTime, y = LakeData$depth_m, z = round(LakeData$Chl_RFU,2), type = "contour", colorscale='Jet',
              line = list(width=0), contours = list(start = 0, end = 2, size = 0.1), colorbar = list(title='Chlorophyll (RFU)')) %>%
  layout(title="12300110 Lake Koocanusa at International Boundary", yaxis = list(autorange = "reversed", title = 'Depth (m)'), xaxis = list(title = 'Date'))


htmlwidgets::saveWidget(as_widget(p), "./12300110_chlorophyll_2020.html")

#Plot BGA-PC
p <- plot_ly( x = LakeData$dateTime, y = LakeData$depth_m, z = round(LakeData$BGA_RFU,2), type = "contour", colorscale='Jet',
              line = list(width=0), contours = list(start = 0, end = 0.5, size = 0.1), colorbar = list(title='BGA-PC (RFU)')) %>%
  layout(title="12300110 Lake Koocanusa at International Boundary", yaxis = list(autorange = "reversed", title = 'Depth (m)'), xaxis = list(title = 'Date'))

