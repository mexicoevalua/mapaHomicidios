# Crear mapa interactivo con la tasa de averiguaciones previas para homicidios dolosos

# Descargar datos
#====
# Diego Valle-Jones provee una excelente compilación de los datos del secretariado en su blog
# la base de datos también tiene las estimaciónes de población por municipio 
# http://crimenmexico.diegovalle.net/en/csv/
# Fuente original: http://www.incidenciadelictiva.secretariadoejecutivo.gob.mx/

require(R.utils) 
#temp <- tempfile()
#download.file("http://crimenmexico.diegovalle.net/en/csv/fuero-comun-municipios.csv.gz",temp)
#data <- read.csv(gunzip(temp, "data/fuero-comun-municipios.csvuero-comun-municipios.csv",overwrite=T))
#unlink(temp)

# Utilizar datos desde el archivo gzip para consumir menos memoria
gunzip("data/fuero-comun-municipios.csv.gz")
data  <- read.csv("data/fuero-comun-municipios.csv", as.is=T)

# Años
table(data$year) # 2011, 2012, 2013, 2014

# Total crimenes
sum(data$count, na.rm=T) #5,473,225

# Agregar datos para crímenes en los siguientes grupos: homicidios, secuestros, robos, 
# otros delitos
table(data$crime)

# Agregar DELITOS PATRIMONIALES, DELITOS SEXUALES, LESIONES, OTROS DELITOS, en una 
# sola categoría llamada OTROS DELITOS
other  <- c("DELITOS PATRIMONIALES","DELITOS SEXUALES","LESIONES", "OTROS DELITOS")
data$group  <- data$crime
for(x in other){
  data$group  <- gsub(x,"OTROS DELITOS", data$group)
}

# Agregar homicidios dolosos como grupo
data$group[data$group =="HOMICIDIOS" & data$type =="DOLOSOS"]  <- "HOMICIDIOS DOLOSOS"
data$group[data$group =="HOMICIDIOS" & data$type =="CULPOSOS"]  <- "HOMICIDIOS CULPOSOS"
table(data$group)

# Cambiar nombre secuestro
data$group  <- gsub("PRIV. DE LA LIBERTAD \\(SECUESTRO\\)","SECUESTRO",data$group)
table(data$group)
format(table(data$group)  / length(data$group)*100, digits=2)

# Para efectos del mapa interactivo se excluirán de los cálculos las observaciones con NA.
names(data)
require(plyr)

ave <- ddply(data, c("state_code","mun_code","year","group"), summarize,
                averiguaciones = sum(count,na.rm=T))
head(ave)

# Revisar si coinciden los numeros de averiguaciones
format(sum(data$count,na.rm=T),big.mark=",") # Numero total de averiguaciones 5,473,225
format(sum(ave$averiguaciones,na.rm=T), big.mark=",") # Numero total de averiguaciones 5,473,225

# Estimaciones de poblacion por municipio
population <- ddply(data, c("state_code","mun_code","year"), summarize,
                    population = max(population))

# Tasa de averiguaciones previas por cada 100 mil habitantes
head(ave)
head(population)
ave  <- merge(ave, population, by= c("state_code","mun_code","year"))
ave$rate  <- (ave$averiguaciones*100000 / ave$population)

# Cambiar formato % en rate para reducir a dos decimales
ave$rate  <- as.numeric(format(round(as.numeric(ave$rate),2), nsmall = 2))

# Agregar abreviaturas de los estados
temp  <- read.csv("data/state_names.csv",stringsAsFactors=F, encoding="utf8")
temp
ave  <- merge(ave,temp)

# Total de delitos del fuero común:
head(ave)
aveTot <- ddply(data, c("state_code","mun_code","year"), summarize,
                averiguaciones = sum(count,na.rm=T))
head(aveTot)
# Revisar si coinciden los numeros de averiguaciones
format(sum(data$count,na.rm=T),big.mark=",") # Numero total de averiguaciones 5,473,225
format(sum(aveTot$averiguaciones,na.rm=T), big.mark=",") # Numero total de averiguaciones 5,473,225

# Calcular tasa del total de delitos del fuero común
aveTot  <- merge(aveTot, population, by= c("state_code","mun_code","year"))
aveTot$rate  <- (aveTot$averiguaciones*100000 / aveTot$population)
summary(aveTot$rate)

# Cambiar formato % en rate para reducir a dos decimales
aveTot$rate  <- as.numeric(format(round(as.numeric(aveTot$rate),2), nsmall = 2))

# Agregar abreviaturas de los estados
aveTot  <- merge(aveTot,temp)
# Agregar el total de delitos al archivo de grupos
aveTot$group  <- "Total"
head(ave)
head(aveTot)
aveTot  <- aveTot[,c(1,2,3,9,4:8)]
names(ave) == names(aveTot) # Las columnas coinciden
ave  <- rbind(ave, aveTot)
names(ave)

# Cambiar los IDs para que coincidan con el shp
ave$mun_code  <- sprintf("%03d", ave$mun_code)
ave$state_code  <- sprintf("%02d", ave$state_code)
head(ave$mun_code)

# Mantener solo homicidios dolosos
table(ave$group)
ave  <- subset(ave, ave$group != "HOMICIDIOS CULPOSOS")
ave$group  <- gsub(pattern="HOMICIDIOS DOLOSOS", "HOMICIDIOS",ave$group)
#====

# Preparar datos para unir con shapefile
#====
#install.packages("reshape")
require(reshape2)

# Agregar id unico
ave$id  <- paste(ave$state_code,ave$mun_code,sep="")
length(unique(ave$id))

#Cambiar datos de formato long a wide
temp  <- ave[,c(1:4,7,10)]
names(temp)
temp  <- melt(temp, id=c("id", "state_code","mun_code","year","group"),
              measured=c("rate"))

tasas_ave  <- dcast(temp, id + state_code + mun_code ~ group + variable + year)

##################
# Unir las tasas de cada delito con el shapefile
#====
#Descargar shapefile
download.file("http://mapserver.inegi.org.mx/MGN/mgm2010v5_0a.zip", "shapefiles/municipios.zip")
unzip("shapefiles/municipios.zip",exdir="shapefiles", overwrite=T)

require(rgdal)
# Unir tasas de crimen con el shapefile del INEGI

# Exportar csv para hacer la unión en QGIS
n  <- names(tasas_ave)
n  <- gsub("_rate_","",n)
n  <- gsub("HOMICIDIOS","HOM",n)
n  <- gsub("OTROS DELITOS","OTR",n)
n  <- gsub("ROBOS","ROB",n)
n  <- gsub("SECUESTRO","SEC",n)
n  <- gsub("Total","TOT",n)
names(tasas_ave)  <- n
write.csv(tasas_ave, "data/tasas_ave.csv", fileEncoding="utf8")

# Agregar un ID al shapefile de municipios para hacer la union en QGIS
shp  <- readOGR("shapefiles", "Municipios_2010_5A", stringsAsFactors=FALSE, encoding="UTF-8")
names(shp)
shp$id  <- paste(shp$CVE_ENT,shp$CVE_MUN,sep="")
head(shp$id)
length(unique(shp$id)) # 2,456 municipios
writeOGR(shp, "shapefiles", "Municipios_2010_5A", driver="ESRI Shapefile", layer_options= c(encoding= "UTF-8"),
         overwrite_layer=T)

### Despues de hacer la union en QGIS
# Cambiar formato del shp para que todo sea numérico
require(foreign)
shp  <- read.dbf("shapefiles//tasas_ave.dbf", as.is=T)
names(shp)  <- gsub("X_","",names(shp))
names(shp)
str(shp)
names(shp[9:28])
cols  <-  c(9:28)
shp[,cols]  <-  apply(shp[,cols], 2, function(x) as.numeric(as.character(x)))
write.dbf(shp,"shapefiles//tasas_ave.dbf")

# Otra forma de hacer la unión directamente desde R, tiene la desventaja de no conservar los
# formatos de los números (e.g. número de decimales)
#check  <- merge(shp, tasas_ave, by="id")
#writeOGR(check, "shapefiles", "check", driver="ESRI Shapefile", layer_options= c(encoding= "UTF-8"),
#         overwrite_layer=T)
#head(check$HOM2011)
#c  <- read.dbf("shapefiles//check.dbf", as.is=T)

############
# Calcular quantiles para el mapa
# Tasa del total de averiguaciones previas
fillKey  <- subset(tasas_ave$TOT2013, !is.na(tasas_ave$TOT2013))
fillKey  <- cut(unique(fillKey),quantile(unique(fillKey), probs = seq(0, 1, by = 0.20), na.rm=TRUE), dig.lab = 5, include.lowest=T, right=F)
table(fillKey)
# Otra opción utilizando ggplot2
#table(cut_number(unique(tasas_ave$TOT2013), n=5))
# Tasa de homicidios
fillKey  <- subset(tasas_ave$HOM2013, !is.na(tasas_ave$HOM2013))
fillKey  <- cut(unique(fillKey),quantile(unique(fillKey), probs = seq(0, 1, by = 0.20), na.rm=TRUE), dig.lab = 5, include.lowest=T, right=F)
table(fillKey)

# Tasa de secuestros
fillKey  <- subset(tasas_ave$SEC2013, !is.na(tasas_ave$SEC2013))
fillKey  <- cut(unique(fillKey),quantile(unique(fillKey), probs = seq(0, 1, by = 0.20), na.rm=TRUE), dig.lab = 5, include.lowest=T, right=F)
table(fillKey)

# Tasa de robos
fillKey  <- subset(tasas_ave$ROB2013, !is.na(tasas_ave$ROB2013))
fillKey  <- cut(unique(fillKey),quantile(unique(fillKey), probs = seq(0, 1, by = 0.20), na.rm=TRUE), dig.lab = 5, include.lowest=T, right=F)
table(fillKey)

# Tasa de otros delitos
fillKey  <- subset(tasas_ave$OTR2013, !is.na(tasas_ave$OTR2013))
fillKey  <- cut(unique(fillKey),quantile(unique(fillKey), probs = seq(0, 1, by = 0.20), na.rm=TRUE), dig.lab = 5, include.lowest=T, right=F)
table(fillKey)

### Al final sólo se expuso el mapa con la tasa de homicidios por cada 100 mil habitantesç
# para cada municipio en 2013
