Chicago <- data.frame(NEW.Chicago.Crimes)
Chicago <- na.omit(Chicago)
tail(Chicago,5)

mean(Chicago$District)
Chicago$Case.Number<-as.numeric(Chicago$Case.Number)


ChiSchools<-data.frame(CPS_School_Locations_1617)
str(ChiSchools)

ChiSchools <- ChiSchools[,-16]
ChiSchools <- ChiSchools[,-15]
ChiSchools <- ChiSchools[,-14]


install.packages("sp")
install.packages("raster")
install.packages("maptools")
install.packages("mapdata")
install.packages("mapproj")
install.packages("ggmap")
install.packages("rworldmap")
install.packages("qmap")
install.packages("ggplot2")
install.packages("data.table")
library(data.table)
library(ggplot2)
library(sp)
library(raster)
library(maptools)
library(mapdata)
library(mapproj)
library(ggmap)
library(rworldmap)
library(qmap)

##Changing date datatype from Factor to date
Chicago$Date <- as.Date(Chicago$Date,format = "%m/%d/%Y")
Chicago$Date

##put all Lat/Lon related to schools in their own Vector
SchoolLat<- ChiSchools$Lat
SchoolLon<- ChiSchools$Long

##put all Lat/Lon related to crimes in their own Vector
CrimeLat <-Chicago$Latitude
CrimeLon <-Chicago$Longitude

##Putting Assault crimes in their own Vector
ChicagoASSAULT<-Chicago[Chicago$Primary.Type=='ASSAULT',]
ChicagoASSAULT
ChicagoTotAssault<-ChicagoASSAULT 

##Filtering Assault crimes by date
ChicagoASSAULT<- ChicagoASSAULT [order(ChicagoASSAULT$Date),]
ChicagoASSAULT$Date
row.names(ChicagoASSAULT)<-c(1:41891)
ChicagoASSAULT<-ChicagoASSAULT[29228:41280,]

##put all Lat/Lon related to Assault crimes in their own Vector between school years
AssaultLat <-ChicagoASSAULT$Latitude
AssaultLon <-ChicagoASSAULT$Longitude

##Putting Arson crimes Date in order
ChicagoARSON<-Chicago[Chicago$Primary.Type=='ARSON',]
ChicagoARSON<- ChicagoARSON [order(ChicagoARSON$Date),]
ChicagoARSON
ChicagoTotArson<-ChicagoARSON

##Filtering Arson crimes by date
ChicagoARSON<- ChicagoARSON [order(ChicagoARSON$Date),]
ChicagoARSON$Date
row.names(ChicagoARSON)<-c(1:924)
ChicagoARSON<-ChicagoARSON[642:908,]

##put all Lat/Lon related to Arson crimes in their own Vector between school years
ArsonLat <-ChicagoARSON$Latitude
ArsonLon <-ChicagoARSON$Longitude

##Putting all Homicide in their own Vector
ChicagoHOMICIDE<-Chicago[Chicago$Primary.Type=='HOMICIDE',]
ChicagoHOMICIDE<- ChicagoHOMICIDE [order(ChicagoHOMICIDE$Date),]
ChicagoHOMICIDE
ChicagoTotHomicide<-ChicagoHOMICIDE

##Filtering Homicide crimes by date
ChicagoHOMICIDE<- ChicagoHOMICIDE [order(ChicagoHOMICIDE$Date),]
ChicagoHOMICIDE$Date
row.names(ChicagoHOMICIDE)<-c(1:8)

##put all Lat/Lon related to Homicide crimes in their own Vector between school years
HomicideLat <-ChicagoHOMICIDE$Latitude
HomicideLon <-ChicagoHOMICIDE$Longitude

##Putting all Narcotics in their own Vector
ChicagoNARC<-Chicago[Chicago$Primary.Type=='NARCOTICS',]
ChicagoNARC<- ChicagoNARC [order(ChicagoNARC$Date),]
ChicagoNARC
ChicagoTotNarcotics<-ChicagoNARC

##Filtering Narcotics crimes by date
ChicagoNARC<- ChicagoNARC [order(ChicagoNARC$Date),]
ChicagoNARC$Date
row.names(ChicagoNARC)<-c(1:73055)
ChicagoNARC<-ChicagoNARC[54196:72088,]

##put all Lat/Lon related to Narcotics crimes in their own Vector between school years
NarcLat <-ChicagoNARC$Latitude
NarcLon <-ChicagoNARC$Longitude

##Putting all Theft in their own Vector
ChicagoTHEFT<-Chicago[Chicago$Primary.Type=='THEFT',]
ChicagoTHEFT<- ChicagoTHEFT [order(ChicagoTHEFT$Date),]
ChicagoTHEFT
ChicagoTotTheft<- ChicagoTHEFT

##Filtering Theft crimes by date
ChicagoTHEFT<- ChicagoTHEFT [order(ChicagoTHEFT$Date),]
ChicagoTHEFT$Date
row.names(ChicagoTHEFT)<-c(1:154498)
ChicagoTHEFT<-ChicagoTHEFT[111990:152707,]

##put all Lat/Lon related to Theft crimes in their own Vector between school years
TheftLat <-ChicagoTHEFT$Latitude
TheftLon <-ChicagoTHEFT$Longitude

##Putting all Battery in their own Vector
ChicagoBATTERY<-Chicago[Chicago$Primary.Type=='BATTERY',]
ChicagoBATTERY<- ChicagoBATTERY [order(ChicagoBATTERY$Date),]
ChicagoBATTERY
ChicagoTotBattery<- ChicagoBATTERY

##Filtering Battery crimes by date
ChicagoBATTERY<- ChicagoBATTERY [order(ChicagoBATTERY$Date),]
ChicagoBATTERY$Date
row.names(ChicagoBATTERY)<-c(1:123518)
ChicagoBATTERY<-ChicagoBATTERY[87234:121739,]

##put all Lat/Lon related to Battery crimes in their own Vector between school years
BatteryLat <-ChicagoBATTERY$Latitude
BatteryLon <-ChicagoBATTERY$Longitude



##putting School Lat/Lon in one vector
df<- as.data.frame(cbind(SchoolLon, SchoolLat))

##putting Crime Lat/Lon vectors in one vector
df2<- as.data.frame(cbind(CrimeLon, CrimeLat))

##putting Assault Lat/Lon vectors in one vector 
df3<- as.data.frame(cbind(AssaultLon, AssaultLat))

##putting Arson Lat/Lon vectors in one vector 
df4<- as.data.frame(cbind(ArsonLon, ArsonLat))

##putting Homicide Lat/Lon vectors in one vector 
df5<- as.data.frame(cbind(HomicideLon, HomicideLat))
  
##putting Narcotics Lat/Lon vectors in one vector 
df6<- as.data.frame(cbind(NarcLon, NarcLat))

##putting Theft Lat/Lon vectors in one vector 
df7<- as.data.frame(cbind(TheftLon, TheftLat))

##putting Theft Lat/Lon vectors in one vector 
df8<- as.data.frame(cbind(BatteryLon, BatteryLat))

  
##getting the map for schools
mapgilbert <- get_map(location = c(lon = mean(df$SchoolLon), 
                                   lat = mean(df$SchoolLat)), zoom = 12,
                      maptype = "satellite", scale = 2)

##getting the map for crimes
mapgilbert2 <- get_map(location = c(lon = mean(df2$CrimeLon), 
                                    lat = mean(df2$CrimeLat)), zoom = 12,
                       maptype = "satellite", scale = 2)

##getting the map for Assault crimes
mapgilbert3 <- get_map(location = c(lon = mean(df3$AssaultLon), 
                                    lat = mean(df3$AssaultLat)), zoom = 12,
                       maptype = "satellite", scale = 2)

##getting the map for Arson crimes
mapgilbert4 <- get_map(location = c(lon = mean(df4$ArsonLon), 
                                    lat = mean(df4$ArsonLat)), zoom = 12,
                       maptype = "satellite", scale = 2)

##getting the map for Homicide crimes
mapgilbert5 <- get_map(location = c(lon = mean(df5$HomicideLon), 
                                    lat = mean(df5$HomicideLat)), zoom = 12,
                       maptype = "satellite", scale = 2)

##getting the map for Narcotics crimes
mapgilbert6 <- get_map(location = c(lon = mean(df6$NarcLon), 
                                    lat = mean(df6$NarcLat)), zoom = 12,
                       maptype = "satellite", scale = 2)

##getting the map for Theft crimes
mapgilbert7 <- get_map(location = c(lon = mean(df7$TheftLon), 
                                    lat = mean(df7$TheftLat)), zoom = 12,
                       maptype = "satellite", scale = 2)

##getting the map for Battery crimes
mapgilbert8 <- get_map(location = c(lon = mean(df8$BatteryLon), 
                                    lat = mean(df8$BatteryLat)), zoom = 12,
                       maptype = "satellite", scale = 2)


##map of Assault during the school year nearby schools
ggmap(mapgilbert3, extent = "device") + geom_point(aes(x = AssaultLon, y = AssaultLat), colour = "blue", 
                                                   alpha = 0.1, size = 2, data = df3) + geom_point(data = df, aes(x = SchoolLon, y = SchoolLat, alpha = 1), fill = "yellow",col="blue", size = 3, shape = 21) +
  guides(fill=FALSE, alpha=FALSE, size=FALSE)


##map of Arson during the school year nearby schools
ggmap(mapgilbert4, extent = "device") + geom_point(aes(x = ArsonLon, y = ArsonLat), colour = "darkred", 
                                                   alpha = 0.1, size = 6, data = df4) + geom_point(data = df, aes(x = SchoolLon, y = SchoolLat, alpha = 1), fill = "yellow",col="blue", size = 3, shape = 21) +
  guides(fill=FALSE, alpha=FALSE, size=FALSE)


##map of Homicide during the school year nearby schools
ggmap(mapgilbert5, extent = "device") + geom_point(aes(x = HomicideLon, y = HomicideLat), colour = "red", 
                                                   alpha = 0.1, size = 6, data = df5) + geom_point(data = df, aes(x = SchoolLon, y = SchoolLat, alpha = 1), fill = "yellow",col="blue", size = 3, shape = 21) +
  guides(fill=FALSE, alpha=FALSE, size=FALSE)


##map of Narcotics during the school year nearby schools
ggmap(mapgilbert6, extent = "device") + geom_point(aes(x = NarcLon, y = NarcLat), colour = "white", 
                                                   alpha = 0.1, size = 2, data = df6) + geom_point(data = df, aes(x = SchoolLon, y = SchoolLat, alpha = 1), fill = "yellow",col="blue", size = 3, shape = 21) +
  guides(fill=FALSE, alpha=FALSE, size=FALSE)

##map of Theft during the school year nearby schools
ggmap(mapgilbert7, extent = "device") + geom_point(aes(x = TheftLon, y = TheftLat), colour = "brown", 
                                                   alpha = 0.1, size = 2, data = df7) + geom_point(data = df, aes(x = SchoolLon, y = SchoolLat, alpha = 1), fill = "yellow",col="blue", size = 3, shape = 21) +
  guides(fill=FALSE, alpha=FALSE, size=FALSE)

##map of Battery during the school year nearby schools
ggmap(mapgilbert8, extent = "device") + geom_point(aes(x = BatteryLon, y = BatteryLat), colour = "darkgoldenrod", 
                                                   alpha = 0.1, size = 2, data = df8) + geom_point(data = df, aes(x = SchoolLon, y = SchoolLat, alpha = 1), fill = "yellow",col="blue", size = 3, shape = 21) +
guides(fill=FALSE, alpha=FALSE, size=FALSE)

##CountingTotalCrimesinChicago##

#Filter Assault Crimes by Year
ChicagoTotAssault<- ChicagoTotAssault [order(ChicagoTotAssault$Date),]
ChicagoTotAssault$Date
row.names(ChicagoTotAssault)<-c(1:41891)
ChicagoTotAssault2013<-ChicagoTotAssault[1:17920,]
ChicagoTotAssault2014<-ChicagoTotAssault[17921:34582,]
ChicagoTotAssault2015<-ChicagoTotAssault[34583:41891,]
#countingTotalAssultsPerYear
NumOfAssaultsin2013<-nrow(ChicagoTotAssault2013)
NumOfAssaultsin2014
NumOfAssaultsin2014<-nrow(ChicagoTotAssault2014)
NumOfAssaultsin2015<-nrow(ChicagoTotAssault2015)

#countingTotalAssaultPerYear
NumOfAssaultTOTAL<-data.frame((NumOfAssaultsin2013),(NumOfAssaultsin2014),(NumOfAssaultsin2015))
NumOfAssaultTOTAL

cnames <- colnames(NumOfAssaultTOTAL)
cnames[1] <-"2013"
cnames[2] <-"2014"
cnames[3] <-"2015"
colnames(NumOfAssaultTOTAL) <-cnames
colnames(NumOfAssaultTOTAL)
NumOfAssaultTOTAL


#Filter Assault Crimes by Year
ChicagoTotArson<- ChicagoTotArson [order(ChicagoTotArson$Date),]
ChicagoTotArson$Date
row.names(ChicagoTotArson)<-c(1:924)
ChicagoTotArson2013<-ChicagoTotArson[1:364,]
ChicagoTotArson2014<-ChicagoTotArson[365:752,]
ChicagoTotArson2015<-ChicagoTotArson[753:924,]
#countingTotalAssultsPerYear
ChicagoTotArson2013<-nrow(ChicagoTotArson2013)
NumOfAssaultsin2014
ChicagoTotArson2014<-nrow(ChicagoTotArson2014)
ChicagoTotArson2015<-nrow(ChicagoTotArson2015)

#countingTotalAssaultPerYear
NumOfAssaultTOTAL<-data.frame((NumOfAssaultsin2013),(NumOfAssaultsin2014),(NumOfAssaultsin2015))
NumOfAssaultTOTAL

cnames <- colnames(NumOfAssaultTOTAL)
cnames[1] <-"2013"
cnames[2] <-"2014"
cnames[3] <-"2015"
colnames(NumOfAssaultTOTAL) <-cnames
colnames(NumOfAssaultTOTAL)
NumOfAssaultTOTAL

#Filter Homicide Crimes by Year
ChicagoTotHomicide<- ChicagoTotHomicide [order(ChicagoTotHomicide$Date),]
ChicagoTotHomicide$Date
row.names(ChicagoTotHomicide)<-c(1:8)
ChicagoTotHomicide2013<-ChicagoTotHomicide[1:2,]
ChicagoTotHomicide2014<-ChicagoTotHomicide[3:4,]
ChicagoTotHomicide2015<-ChicagoTotHomicide[5:6,]

##Putting num of crimes in own vector by year
ChicagoTotHomicide2013<-nrow(ChicagoTotHomicide2013)
ChicagoTotHomicide2013
ChicagoTotHomicide2014<-nrow(ChicagoTotHomicide2014)
ChicagoTotHomicide2015<-nrow(ChicagoTotHomicide2015)

#countingTotalHomicidePerYear

#countingTotalHomicidePerYear
NumOfHomicideTOTAL<-data.frame(ChicagoTotHomicide2013,ChicagoTotHomicide2014 ,ChicagoTotHomicide2015)
NumOfHomicideTOTAL

cnames <- colnames(NumOfHomicideTOTAL)
cnames[1] <-"2013"
cnames[2] <-"2014"
cnames[3] <-"2015"
colnames(NumOfHomicideTOTAL) <-cnames
colnames(NumOfHomicideTOTAL)
NumOfHomicideTOTAL

#Filter Narcotics Crimes by Year
ChicagoTotNarcotics<- ChicagoTotNarcotics [order(ChicagoTotNarcotics$Date),]
ChicagoTotNarcotics$Date
row.names(ChicagoTotNarcotics)<-c(1:73055)
ChicagoTotNarcotics2013<-ChicagoTotNarcotics[1:34095,]
ChicagoTotNarcotics2014<-ChicagoTotNarcotics[34096:62202,]
ChicagoTotNarcotics2015<-ChicagoTotNarcotics[62203:73055,]

##Putting num of crimes in own vector by year
ChicagoTotNarcotics2013<-nrow(ChicagoTotNarcotics2013)
ChicagoTotNarcotics2013
ChicagoTotNarcotics2014<-nrow(ChicagoTotNarcotics2014)
ChicagoTotNarcotics2015<-nrow(ChicagoTotNarcotics2015)



#countingTotalNarcoticsPerYear
NumOfNarcoticsTOTAL<-data.frame(ChicagoTotNarcotics2013 , ChicagoTotNarcotics2014,ChicagoTotNarcotics2015)
NumOfNarcoticsTOTAL

cnames <- colnames(NumOfNarcoticsTOTAL)
cnames[1] <-"2013"
cnames[2] <-"2014"
cnames[3] <-"2015"
colnames(NumOfNarcoticsTOTAL) <-cnames
colnames(NumOfNarcoticsTOTAL)
NumOfNarcoticsTOTAL


#Filter Theft Crimes by Year
ChicagoTotTheft<- ChicagoTotTheft [order(ChicagoTotTheft$Date),]
ChicagoTotTheft$Date
row.names(ChicagoTotTheft)<-c(1:154498)
ChicagoTotTheft2013<-ChicagoTotTheft[1:71168,]
ChicagoTotTheft2014<-ChicagoTotTheft[71169:131879,]
ChicagoTotTheft2015<-ChicagoTotTheft[131880:154498,]

##Putting num of crimes in own vector by year
ChicagoTotTheft2013<-nrow(ChicagoTotTheft2013)
ChicagoTotTheft2013
ChicagoTotTheft2014<-nrow(ChicagoTotTheft2014)
ChicagoTotTheft2015<-nrow(ChicagoTotTheft2015)



#countingTotalTheftsPerYear
NumOfTheftTOTAL<-data.frame(ChicagoTotTheft2013,ChicagoTotTheft2014,ChicagoTotTheft2015)
NumOfTheftTOTAL

cnames <- colnames(NumOfTheftTOTAL)
cnames[1] <-"2013"
cnames[2] <-"2014"
cnames[3] <-"2015"
colnames(NumOfTheftTOTAL) <-cnames
colnames(NumOfTheftTOTAL)
NumOfTheftTOTAL


#Filter Battery Crimes by Year
ChicagoTotBattery<- ChicagoTotBattery [order(ChicagoTotBattery$Date),]
ChicagoTotBattery$Date
row.names(ChicagoTotBattery)<-c(1:123518)
ChicagoTotBattery2013<-ChicagoTotBattery[1:53860,]
ChicagoTotBattery2014<-ChicagoTotBattery[53861:102523,]
ChicagoTotBattery2015<-ChicagoTotBattery[102524:123518,]

##Putting num of crimes in own vector by year
ChicagoTotBattery2013<-nrow(ChicagoTotBattery2013)
ChicagoTotBattery2013
ChicagoTotBattery2014<-nrow(ChicagoTotBattery2014)
ChicagoTotBattery2015<-nrow(ChicagoTotBattery2015)


#countingTotalBatteryPerYear
NumOfBatteryTOTAL<-data.frame(ChicagoTotBattery2013,ChicagoTotBattery2014,ChicagoTotBattery2015)
NumOfBatteryTOTAL

cnames <- colnames(NumOfBatteryTOTAL)
cnames[1] <-"2013"
cnames[2] <-"2014"
cnames[3] <-"2015"
colnames(NumOfBatteryTOTAL) <-cnames
colnames(NumOfBatteryTOTAL)
NumOfBatteryTOTAL

##putting the total crimes of each year in one dataframe
NewTotal<-data.frame(NumOfAssaultsin2013,NumOfAssaultsin2014,NumOfAssaultsin2015, 
                     ChicagoTotArson2013,ChicagoTotArson2015,ChicagoTotArson2015, 
                     ChicagoTotHomicide2013, ChicagoTotHomicide2014, ChicagoTotHomicide2015,
                     ChicagoTotNarcotics2013,ChicagoTotNarcotics2014,ChicagoTotNarcotics2015,
                     ChicagoTotTheft2013,ChicagoTotTheft2014,ChicagoTotTheft2015,
                     ChicagoTotBattery2013,ChicagoTotBattery2014,ChicagoTotBattery2015)
NewTotal<-data.frame(t(NewTotal))
rownames(NewTotal)<- c()

cnames <- colnames(NewTotal)
cnames[1] <-"Total"
colnames(NewTotal) <-cnames
colnames(NewTotal)
NewTotal

##Creating new column and inputting the year and type of crime into those columns
NewTotal$Year<- rbind("2013","2014","2015","2013","2014","2015","2013","2014","2015","2013","2014","2015","2013","2014","2015","2013","2014","2015")
NewTotal$Crime<-rbind("Assault","Assault","Assault", "Arson","Arson","Arson","Homicide",
                      "Homicide","Homicide","Narcotics","Narcotics","Narcotics",
                      "Theft","Theft","Theft","Battery","Battery","Battery")

NewTotal<- NewTotal [order(NewTotal$Year),]
NewTotal

##Line Chart indicating the total crimes by crime type over a 3 year span

ggplot(NewTotal, aes(x = Year, y=Total, group=Crime)) + geom_line(aes(color=Crime))

##BarGraph indicating the total crimes by crime type over a 3 year span
ggplot(NewTotal, aes(x = Year, y=Total)) + geom_bar(stat="identity", position="dodge", color="black", aes(fill=Crime))


Chicago1<-Chicago
library(ggplot2)
str(Chicago1)
Chicago1$Date<-as.character(Chicago1$Date)
Chicago1$Date<-sub('...$','',Chicago1$Date)
Chicago1$Date<-as.numeric(gsub('-','',Chicago1$Date))
CrimeNumber<-as.data.frame(t(table(Chicago1$Date)))
CrimeNumber$Var2<-as.numeric(CrimeNumber$Var2)
model1<-lm(CrimeNumber$Freq~CrimeNumber$Var2)
summary(model1)
class(CrimeNumber$Var2)

Ward3<-Chicago1[Chicago$Ward==3,]
CrimeNumber3<-as.data.frame(t(table(Ward3$Date)))
CrimeNumber3$Var2<-as.numeric(CrimeNumber3$Var2)
model3<-lm(CrimeNumber3$Freq~CrimeNumber3$Var2)
summary(model3)

Ward17<-Chicago1[Chicago$Ward==17,]
CrimeNumber17<-as.data.frame(t(table(Ward17$Date)))
CrimeNumber17$Var2<-as.numeric(CrimeNumber17$Var2)
model17<-lm(CrimeNumber17$Freq~CrimeNumber17$Var2)
summary(model17)

Ward25<-Chicago1[Chicago$Ward==25,]
CrimeNumber25<-as.data.frame(t(table(Ward25$Date)))
CrimeNumber25$Var2<-as.numeric(CrimeNumber25$Var2)
model25<-lm(CrimeNumber25$Freq~CrimeNumber25$Var2)
summary(model25)

Ward34<-Chicago1[Chicago$Ward==34,]
CrimeNumber34<-as.data.frame(t(table(Ward34$Date)))
CrimeNumber34$Var2<-as.numeric(CrimeNumber34$Var2)
model34<-lm(CrimeNumber34$Freq~CrimeNumber34$Var2)
summary(model34)

Ward46<-Chicago1[Chicago$Ward==46,]
CrimeNumber46<-as.data.frame(t(table(Ward46$Date)))
CrimeNumber46$Var2<-as.numeric(CrimeNumber46$Var2)
model46<-lm(CrimeNumber46$Freq~CrimeNumber46$Var2)
summary(model46)

