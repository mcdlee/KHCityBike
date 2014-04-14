library(shiny)
library(plyr)
library(RColorBrewer)
library(XML)

# get List from KH city open Data
url = "http://www.c-bike.com.tw/xml/stationlistopendata.aspx"
ls <- xmlToList(xmlParse(url))

# get a new list for rCharts leaflet
stationList <- llply(ls[[1]], summarize, 
                     Name = StationName,
                     Address = StationAddress,
                     fillColor = cut(
                       as.numeric(StationNums1)/(as.numeric(StationNums1) + as.numeric(StationNums2)),
                       breaks = c(0, 0.20, 0.40, 0.60, 0.80,1),
                       labels = brewer.pal(5, 'RdYlGn'),
                       include.lowest = TRUE),
                     popup = iconv(sprintf("%s <br /> %s <br /> %s <br />容量： %i <br />尚餘車輛： %i ",
                                     StationName,
                                     StationAddress,
                                     StationDesc,
                                     as.numeric(StationNums1) + as.numeric(StationNums2),
                                     as.numeric(StationNums1)), to= "UTF-8"),
                     latitude = StationLat,
                     longitude = StationLon)
#get center

getCenter <- function (data){
  tmpdf <- ldply(stationList)
  lat <- mean(as.numeric(tmpdf$latitude))
  lng <- mean(as.numeric(tmpdf$longitude))
  return(list(lat = lat, lng = lng))
}

#display the map
plotMap <- function(data = stationList){
  center <- getCenter(stationList)
  L1 <- Leaflet$new()
  L1$tileLayer(provider = 'Stamen.TonerLite')
  L1$set(width=1000, height=700)
  L1$setView(c(center$lat, center$lng), 11)
  L1$geoJson(toGeoJSON(data),
    onEachFeature = '#! function(feature, layer){
      layer.bindPopup(feature.properties.popup)
      } !#',
    pointToLayer = "#! function(feature, latlng){
      return L.circleMarker(latlng, {
      radius: 4,
      fillColor: feature.properties.fillColor || 'red',
      color: '#000',
      weight: 1,
      fillOpacity: 0.8
      })
      } !#")
  L1$fullScreen(TRUE)
  return(L1)
}