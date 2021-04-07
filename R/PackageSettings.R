#//////////////////////////////////////////////////////////////////////////////////////
#PSM - psmodul Settings
#//////////////////////////////////////////////////////////////////////////////////////

# Activace vsech prirazenych knihoven
#' @author Konecny Jiri (kon0327)
PSM.activateAllLibs <-function(){
  #API
  library(readxl)
  library(rstudioapi)
  #Èíselné charakteristiky
  library(moments)
  library(dplyr)
  #Grafika
  library(ggplot2)

  library(psmodul)
}
