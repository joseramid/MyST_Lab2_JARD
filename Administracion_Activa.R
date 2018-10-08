# Remover todos los objetos del "Environment"
rm(list = ls())

# los 0s aceptados antes de expresas una cifra en notaci?n cient?fica
options("scipen"=100, "digits"=4)

### Cargas librer?as a utilizar
suppressMessages(library(plotly)) # Graficas interactivas
suppressMessages(library(Quandl)) # Descargar Precios
suppressMessages(library(PortfolioAnalytics)) # Teoría Moderna de Portafolios y Sharpe
suppressMessages(library(ROI)) # Optimizacion para portafolio
suppressMessages(library(xlsx))
suppressMessages(library(knitr))  # Opciones de documentaci?n + c?digo
suppressMessages(library(kableExtra)) # Tablas en HTML
options(knitr.table.format = "html") 
suppressMessages(library(openxlsx))

# Cargar el token de QUANDL
Quandl.api_key("Akx24vw1x3ziugMZeuY3")

# Funcion para descagar precios
Bajar_Precios <- function(Columns, Tickers, Fecha_In, Fecha_Fn) {
  Datos <- Quandl.datatable("WIKI/PRICES", qopts.columns=Columns, ticker=Tickers,
                            date.gte=Fecha_In, date.lte=Fecha_Fn)
  return(Datos)
}


DatosETF <- read.xlsx("IHI.xlsx")
#cuando no lo lea, abrir menu Session, luego 'Set Working Directory', luego 
#'to source file location'

tk <- as.character(na.omit(DatosETF[which(DatosETF[,1]=="Ticker")+1:length(DatosETF[,1]),1]))
cs <- c("date", "adj_close")

# Fecha inicial y fecha final
fs <- c("2016-08-01", "2018-08-01")
#ano, mes, dia.

### Descargar Precios y Calcular rendimientos
Datos <- list()

for(i in 1:length(tk))
  Datos[[i]] <- Bajar_Precios(Columns=cs, Ticker=tk[i], Fecha_In=fs[1], Fecha_Fn=fs[2])

names(Datos) <- tk

#for para cambiar el orden de los datos que previamente descargamos para las fechas
for(i in 1:length(tk)){
  Datos[[i]]<-Datos[[i]][order(Datos[[i]][,1]), ]
}


### Hacemos que todas nuestras variales tengan la misma longitud.

longitudes <- c()

for (i in 1:length(Datos)){
  longitudes[i] <- length(Datos [[i]]$date)
}
#En longitudes, guardara la longitud de cada una de las variables.

maximo <- max(longitudes)
completos <- which(longitudes == maximo)
#completos guarda los indices que tambien tienen la maxima longitud.

DatosN <- Datos[completos]
#aqui ya tenemos nuestros datos, con las variables del mismo tamano.


#vector para almacenar columnas de interes
columnas <- c()
nuevos <- c()

#Funcion para repetir una funcion por cada columna del data.frame
Precios <- do.call(cbind, DatosN)

#Crear vector con nombre de columnas de interes = "nombredeactivo.adj_close_r"
for (i in 1:length(tk)) {
  nuevos[i] <- paste(tk[i], ".adj_close", sep = "")
}

#Extraer reglon para obtener los nombres de las columnas 
nombres <- colnames(Precios[1,(names(Precios)%in% nuevos)])

#Elegir una columna Date  y las demas columnas de rendimientos
Precios <- Precios[,(names(Precios) %in% nuevos)]
row.names(Precios) <- DatosN[[1]]$date

# Reasignar nombres al data.frame
tk_completos <- as.character(tk[completos])
colnames(Precios) <- tk_completos

Historico <- data.frame("Date" = row.names(Precios),
                        "Precio" = Precios[,1],
                        "R_Precio" = 0,
                        "R_Activo" = 0,
                        "R_Cuenta" = 0,
                        "Capital" = 0,
                        "Flotante" = 0,
                        "Balance" = 0,
                        "Titulos" = 0,
                        "Titulos_a" = 0,
                        "Operacion" = NA,
                        "Comisiones" = 0,
                        "Comisiones_a" = 0,
                        "Mensaje" = NA)

# Date       : Fecha (Proviene desde los precios que bajaron).
# Precio     : Precio individual del activo.
# R_Precio   : Rendimiento diario del precio (dia a dia).
# R_Activo   : Rendimiento acumulado del precio (Cada dia respecto al precio inicial).
# R_Cuenta   : Balance + Capital (Cada dia respecto al capital inicial).
# Capital    : El dinero no invertido, suelto (Equivalente a Efectivo).
# Flotante   : El valor de la posicion (Precio diario por titulos).
# Balance    : El valor del portafolio (capital + flotante)
# Titulos    : Acciones que se tienen.
# Titulos_a  : Titulos acumulados.
# Operacion  : Indicativo de Compra (1), Mantener (0), Venta (-1).
# Comisiones : 0.0025 o 0.25% por el valor de la transaccion.
#Comisiones_a: Comisiones Acumuladas.
# Mensaje    : Un texto que indique alguna decision o indicativo de que ocurrio algo.


Regla0_R <- -0.01  # Considerar una oportunidad de compra en un rendimiento de -3% o menor.
Regla1_I <- 0.20   # Porcentaje de capital para comprar titulos para posicion Inicial.
Regla2_P <- 0.25   # Se utiliza el P% del L capital restante en cada compra.
Regla3_W <- tk_completos # Se realiza la misma estrategia para todos los activos en el portafolio.
Regla4_C <- 0.0025 # Comisiones pagadas por compra.
Regla5_K <- 1000000 # Capital Inicial.


# -- ----------------------------------------------------------------------------------------- -- #
# -- ----------------------------------------------------------------------------------------- -- #
# -- ----------------------------------------------------------------------------------------- -- #

# -- Calcular los Titulos de posicion inicial
Historico$Titulos[1] <- (Regla5_K*Regla1_I)%/%Historico$Precio[1]

# -- Calcular Titulos acumulados
Historico$Titulos_a[1] <- Historico$Titulos[1]

# -- Se calculan comisiones iniciales
Historico$Comisiones[1] <- Historico$Titulos[1]*Historico$Precio[1]*Regla4_C

# -- Se calcula las comisiones acumuladas 
Historico$Comisiones_a[1] <- Historico$Comisiones[1]

# -- Calcular el valor Flotante
Historico$Flotante[1] <- Historico$Titulos_a[1]*Historico$Precio[1]

# -- Remanente se deja registrado en el efectivo
Historico$Capital[1] <- Regla5_K-Historico$Flotante[1]-Historico$Comisiones[1]

# -- Calcular el Balance
Historico$Balance[1] <- Historico$Flotante[1] + Historico$Capital[1]

# -- Iniciamos con una postura de mantener 
Historico$Operacion[1] <- 1

# -- Todo remanente se dejar? registrado en la cuenta de efectivo.
#Historico$Capital[1] <- Regla5_K-Historico$FLotante[1]-Historico$Comisiones[1]

# -- Iniciamos con una postura de mantener.
#Historico$Operacion[1] <- "Posicion Inicial"

# -- El rendimiento de capital en el tiempo 1 es 0
Historico$R_Cuenta[1] <- 0

# -- Mensaje inicial
Historico$Mensaje[1] <- "Inicializacion de cartera"

# -- Calcular R_Precio
Historico$R_Precio <- round(c(0, diff(log(Historico$Precio))),4)

# -- Calcular R_Activo
PosturaInicial <- Regla5_K%%Historico$Precio[1]

for(i in 1:length(Historico$Date)){
  Historico$R_Activo[i] <- (PosturaInicial*Historico$Precio[i])/(PosturaInicial*Historico$Precio[1])-1
}

# -- ------------------------------------ -- #
# -- ------------------------------------ -- #
# -- ------------------------------------ -- #

for(i in 2:length(Historico$Date)){
  
  if(Historico$R_Precio[i] <= Regla0_R){ # Generar Señal
    
    # Establecer capital actual, inicialmente, igual al capital anterior
    #Historico$Capital[i] <- Historico$Capital[i-1]
    
    if(Historico$Capital[i-1] > 0){ # Si hay capital
      
      Historico$Capital[i]<- Historico$Capital[i-1]
      #print(paste0("iteracion ",i))
      
      if(Historico$Capital[i-1]*Regla2_P > Historico$Precio[i]){ # Si Capital minimo
        
        
        #print(paste0("iteracion ",i))
        
        Historico$Mensaje[i] <- "Señal de Compra Ejecutada"
        
        Historico$Operacion[i] <- 1
        
        Historico$Titulos[i]   <- (Historico$Capital[i-1]*Regla2_P)%/%Historico$Precio[i]
        
        Historico$Titulos_a[i] <- Historico$Titulos_a[i-1] + Historico$Titulos[i]
        
        Historico$Comisiones[i] <- Historico$Precio[i]*Historico$Titulos[i]*Regla4_C
        
        Historico$Comisiones_a[i] <- Historico$Comisiones_a[i-1] +Historico$Comisiones[i]
        
        Historico$Flotante[i] <- Historico$Precio[i]*Historico$Titulos_a[i]
        
        Historico$Capital[i] <- Historico$Capital[i-1]
        
        Historico$Balance[i] <- Historico$Capital[i] + Historico$Flotante[i]
        
        Historico$R_Cuenta[i] <- Historico$Balance[i]/Regla5_K - 1
        
        #compra <- Historico$Precio[i]*Historico$Titulos[i]  
        #Historico$Comisiones[i] <- compra*Regla4_C
        
        #Historico$Titulos_a[i] <- Historico$Titulos[i-1]+Historico$Titulos[i]
        #Historico$Mensaje[i] <- "Compra exitosa"
        
      }
      
    }
    else { # No hubo capital capital minimo para 1 operacion
      
      Historico$Mensaje[i] <- "Hubo señal pero no hubo capital minimo"
      
      Historico$Operacion[i]<- 0
      
      Historico$Titulos[i] <- 0
      
      Historico$Comisiones[i] <- 0
      
      Historico$Comisiones_a[i] <- Historico$Comisiones_a[i-1]
      
      Historico$Titulos_a[i] <- Historico$Titulos_a[i-1]
      
      Historico$Flotante[i] <- Historico$Titulos_a[i]*Historico$Precio[i]
      
      Historico$Capital[i] <- Historico$Capital[i-1]
      
      Historico$Balance[i] <- Historico$Flotante[i] + Historico$Capital[i]
      
      Historico$R_Cuenta[i] <- Historico$Balance[i]/Regla5_K -1
      
      
      
    }
    
    
  }
  else { # No hubo capital
    
    
    
    # Señales de posiciones dentro portafolio
    Historico$Mensaje[i] <- "No hubo Capital "
    
    Historico$Operacion[i] <- 0
    
    Historico$Titulos[i] <- 0
    
    Historico$Titulos_a [i] <- Historico$Titulos_a[i-1]
    
    Historico$Flotante[i] <- Historico$Titulos_a[i]*Historico$Precios[i]
    
    Historico$Comisiones[i]<- 0
    
    Historico$Comisiones_a[i] <-  Historico$Comisiones_a[i-1]
    
    Historico$Flotante[i] <- Historico$Titulos_a[i]*Historico$Precios[i]
    
    Historico$Capital[i]<- Historico$Capital[i-1]- Historico$Titulos[i]*Historico$Precios[i]-Historico$Comisiones[i] 
    
    Historico$Balance[i] <- Historico$Capital[i]+ Historico$Flotante[i]
    
    Historico$R_Cuenta[i]<- Historico$Balance[i]/Regla5_K -1
    
  }
  
}


plot_ly(Historico) %>%
  add_trace(x = ~Date, y = ~round(R_Activo,4), type = 'scatter', mode = 'lines', name = 'Activo',
            line = list(color = 'red')) %>%
  add_trace(x = ~Date, y = ~round(R_Cuenta,4), type = 'scatter', mode = 'lines', name = 'Cuenta',
            line = list(color = 'blue')) %>% 
  layout(title = "Rend del activo VS Rend de la cuenta",
         xaxis = list(title = "Fechas", showgrid = T),
         yaxis = list(title = "Rendimiento"), 
         legend = list(orientation = 'h', y = -0.25, x = 0.5))

##Calculo de Sharpe Ratio y Sortino Ratio
SharpeRatio(R=xts(x = Historico$R_Activo,order.by = as.Date(Historico$Date)),Rf =0.0225, FUN ="StdDev" )
SharpeRatio(R=xts(x = Historico$R_Cuenta,order.by = as.Date(Historico$Date)),Rf =0.0225, FUN ="StdDev" )

SortinoRatio(R =xts(x = Historico$R_Activo,order.by = as.Date(Historico$Date)),MAR =0.0225)
SortinoRatio(R =xts(x = Historico$R_Cuenta,order.by = as.Date(Historico$Date)),MAR =0.0225)

