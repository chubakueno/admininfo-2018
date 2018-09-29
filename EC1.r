library(stringi)
library(lubridate)

nombres_h <- c( "Luis", "Jean Pierre", "Rances" )
nombres_m <- c( "Natalia", "Rubi", "Sofia" )
apellidos <- c( "Rivera", "Nunez", "Alfaro" )
prefijos_calle <- c( "Calle", "Av.", "Jr." )
direcciones <- c( "Republica de Panama", "Sergio Bernales", "Angamos" )
lenguaje_corto <- c("ES", "EN", "AR")
lenguaje_largo <- c("Español", "Portuges", "Ingles")
paises <- list(
  c("Peru", "Chile"),
  c("Rusia", "Turquia"),
  c("Australia", "Nueva Zelanda")
)
continentes <- c( "America", "Europa", "Oceania" )
names(paises) <- continentes

generar_nombres <- function(hombre = TRUE){
  return(ifelse(hombre, sample(nombres_h, 1), sample(nombres_m, 1)))
}
generar_lenguaje <- function(largo = TRUE) {
  return(ifelse(largo, sample(lenguaje_largo, 1), sample(lenguaje_corto, 1)))
}

generar_ip <- function(version = 4) {
  return(ifelse(version==4,
	paste(sample(0:255, 4, replace = T), collapse = "."),
	paste(stri_rand_strings(8, 4, pattern = "[A-F0-9]"), collapse = ":")))
}

generar_apellidos <- function(cantidad = 1){
  return( paste(sample(apellidos,cantidad,replace = T), collapse = ' '))
}

generar_direccion <- function() {
  return (paste(c(sample(prefijos_calle, 1), sample(direcciones, 1), sample(101:9999, 1)), collapse = " "))
}

generar_telefono <- function() {
  return (sample(0:9,9,replace=T))
}

generar_email <- function(dominio = "") {
  if(dominio == "") dominio <- stri_rand_strings(1, 8, pattern = "[a-zA-Z0-9]")
  sdominio <- stri_rand_strings(1, 3, pattern = "[a-zA-Z0-9]")
  usuario <- stri_rand_strings(1, 8, pattern = "[a-zA-Z0-9]")
  tmp <- paste(c(usuario, dominio), collapse = "@")
  return(paste(c(tmp, sdominio), collapse = "."))
}

#Obtenido de: https://stackoverflow.com/questions/14720983/efficiently-generate-a-random-sample-of-times-and-dates-between-two-dates
latemail <- function(st="1990/01/01", et="2017/12/31") {
  st <- as.POSIXct(as.Date(st))
  et <- as.POSIXct(as.Date(et))
  dt <- as.numeric(difftime(et,st,unit="sec"))
  ev <- runif(1, 0, dt)
  return(st + ev)
}
generar_fecha <- function(format = "d-m-y H:M:S") {
  date <- latemail()
  format <- gsub("d", wday(date), format)
  format <- gsub("m", month(date), format)
  format <- gsub("y", year(date), format)
  format <- gsub("H", hour(date), format)
  format <- gsub("M", minute(date), format)
  format <- gsub("S", second(date), format)
  return(format)
}

generar_continente <- function() {
  return(sample(continentes, 1))
}

generar_pais <- function(continente = "") {
  if(continente == "")
    continente <- generar_continente()
  return(sample(paises[[continente]],1))
}

generar_numero <- function(minimo = 0, maximo = 1, decimales = 0) {
  return(round(runif(1, minimo, maximo), decimales))
}

generar_nombres(TRUE)
generar_nombres(FALSE)
generar_apellidos()
generar_apellidos(2)
generar_direccion()
generar_telefono()
generar_email()
generar_fecha("d-m-y H:S")
generar_fecha("d-m-y")
generar_ip(4)
generar_ip(6)
generar_continente()
generar_pais()
generar_pais("America")
generar_lenguaje(TRUE)
generar_lenguaje(FALSE)
generar_numero(minimo = -100, maximo = 100, decimales = 0)
generar_numero(decimales = 5)
