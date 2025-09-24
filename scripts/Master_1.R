##########################################################
# Taller 1 - Económia Urbana
# ejercicio 1
# author: 
##########################################################

# Clean the workspace -----------------------------------------------------

rm(list=ls())

#Definir directorios -----------------------------------------------------

usuario <- tolower(Sys.info()[["user"]])  # o: tolower(Sys.getenv("USERNAME", Sys.getenv("USER")))

# Mapeo de rutas
rutas <- list(
  sebas = "C:/Users/sebas/OneDrive - RADDAR/Documentos/Documents/Sebastian Tellez/MAESTRIA/ECONOMIA URBANA/TALLER/TALLER 1/Taller_1_EU/",
  mora  = "C:/Users/mora/Path/To/TALLER/TALLER 1/"
)

# Validación y asignación
if (!usuario %in% names(rutas)) {
  stop(sprintf("Usuario no reconocido (%s). Usuarios permitidos: %s",
               usuario, paste(names(rutas), collapse = ", ")))
}

directorio <- rutas[[usuario]]






