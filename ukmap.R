# Dibujando conexiones geolocalizadas con R 
# www.datosimagensonido.com

# Paquetes requeridos
install.packages("data.table")
install.packages("png")
library(data.table)
library(png)

# Leemos dataset
DT=fread("wu03ew_v1.csv", header=T, sep=",")
colnames(DT)[colnames(DT)=="Area of usual residence"] = "residence"
colnames(DT)[colnames(DT)=="Area of workplace"] = "workplace"
DT=DT[, c("residence", "workplace"), with=FALSE]

# Leemos coordenadas
CO=fread("msoa_popweightedcentroids.csv", header=T, sep=",")
CO=CO[, c("Code", "East", "North"), with=FALSE]

# Cruzamos tablas
setkey(CO, Code)

setkey(DT, residence)
DT=DT[CO, nomatch=0]
colnames(DT)[colnames(DT)=="East"] = "x0"
colnames(DT)[colnames(DT)=="North"] = "y0"

setkey(DT, workplace)
DT=DT[CO, nomatch=0]
colnames(DT)[colnames(DT)=="East"] = "x1"
colnames(DT)[colnames(DT)=="North"] = "y1"

# Subseteamos eliminando desplazamientos largos y agrupamos
RANGO=(max(max(DT$x0),max(DT$x1)) - min(min(DT$x0),min(DT$x1)))/10
DT=DT[((x1-x0)^2+(y1-y0)^2)^0.5 <= RANGO, list( trips = .N ), by=.(x0,y0,x1,y1)]

XMIN=min(min(DT$x0),min(DT$x1))
XMAX=max(max(DT$x0),max(DT$x1))

YMIN=min(min(DT$y0),min(DT$y1))
YMAX=max(max(DT$y0),max(DT$y1))

# Resolución imagen de salida
WIDTH = 2000L
HEIGHT = round(WIDTH/(XMAX-XMIN)*(YMAX-YMIN))

# Normalizamos a la resolución de salida
DT$x0=round(1 + (WIDTH-1)  / (XMAX-XMIN) * (DT$x0-XMIN))
DT$x1=round(1 + (WIDTH-1)  / (XMAX-XMIN) * (DT$x1-XMIN))
DT$y0=round(1 + (HEIGHT-1) / (YMAX-YMIN) * (DT$y0-YMIN))
DT$y1=round(1 + (HEIGHT-1) / (YMAX-YMIN) * (DT$y1-YMIN))

# Cargamos en matriz agrupando de nuevo
MAagrup=as.matrix(DT[, list( trips = sum(trips) ), by=.(x0,y0,x1,y1)])

# Matriz acumulador vacía
MAacum=array(0, c(WIDTH, HEIGHT))

# Recorremos matriz agrupada para cargar matriz acumulador
for (k in 1:nrow(MAagrup)) {
    fila = as.numeric(MAagrup[k,])
    x0   = fila[1]
    y0   = fila[2]
    x1   = fila[3]
    y1   = fila[4]
    trips= fila[5]
    
    if(x0==x1 & y0==y1) { # Un punto
      
        MAacum[x0, y0] <- MAacum[x0, y0] + trips
      
    } else if(abs(x1 - x0) >= abs(y1 - y0)) { # Recta de 0 <= |m| <= 1
      
        m = (y1 - y0) / (x1 - x0)
        i <- x0:x1
        j <- round(y0 + m * (i - x0))
        indices <- cbind(i, j)
        MAacum[indices] <- MAacum[indices] + trips # Acumulación vectorizada
  
    } else { # Recta de |m| > 1
    
        # Intercambiamos roles x <-> y
        tmp = x0
        x0 = y0
        y0 = tmp
        tmp = x1
        x1 = y1
        y1 = tmp
        
        m = (y1 - y0) / (x1 - x0)
        i <- x0:x1
        j <- round(y0 + m * (i - x0))
        indices <- cbind(j, i)
        MAacum[indices] <- MAacum[indices] + trips # Acumulación vectorizada
    
    }
}

# Guardamos matriz acumulador como gráfico PNG monocromo
writePNG((MAacum/max(MAacum))^0.8, target="ukmap.png")


# Versión con acumulador encapsulado en una función por Carlos Gil Bellosta
indices.acumular <- function(x0, y0, x1, y1) {
  if (y0 == y1) return(cbind(x0:x1, y0)) # Recta de m=0 o un punto
  if (abs(x1 - x0) >= abs(y1 - y0)) { # Recta de 0 < |m| <= 1
    m = (y1 - y0) / (x1 - x0)
    cbind(x0:x1, round(y0 + m * ((x0:x1) - x0)))
  } else indices.acumular(y0, x0, y1, x1)[, 2:1]  # Recta de |m| > 1
  # Llamada traspuesta recursiva y traspuesta
}

# Recorremos matriz agrupada para cargar matriz acumulador
for (k in 1:nrow(MAagrup)) {
  indices <- indices.acumular(MAagrup[k,1], MAagrup[k,2], MAagrup[k,3], MAagrup[k,4])
  MAacum[indices] <- MAacum[indices] + MAagrup[k,5]
}
