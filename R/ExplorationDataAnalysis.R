#//////////////////////////////////////////////////////////////////////////////////////
#EDA - Exploration Data Analysis
#//////////////////////////////////////////////////////////////////////////////////////

#Prace se soubory

# Nastaveni pracovniho adresare na slozku source file, pripadne jeji podskozku subdir
SetWorkingDirectoryToSource <-function(subdir=""){
  # Nastaveni pracovniho adresare na složku source file
  WD <- dirname(rstudioapi::getSourceEditorContext()$path);
  # Podslozka
  if( subdir != "" )WD <- paste(WD,subdir,sep = "/");
  if (!is.null(WD)) setwd(WD);

  return(WD);
}

#Vraci nacteny Excel sheet jako DataFrame
ReadExcel <-function(file="", sheet="", colNames=NULL){
  #Nacteni dat ve standartnim predpripravenem formatu, col_names defaultne nacteny z excelu
  data = read_excel(file, sheet = sheet)
  #Prejmenuji sloupce na pouzitelne nazvy
  if( !is.null(colNames) )colnames(data) = colNames;
  #Konvertuji datovy typ na dataframe
  data = as.data.frame(data);

  return(data)
}

#//////////////////////////////////////////////////////////////////////////////////////

#EDA funkce

#Overeni normalniho rozdeleni datasetu
#Vrací TRUE - je N rozdeli, FALSE = není N
IsNorm <- function(skewness, kurtosis){
  result = (skewness > -2.0 && skewness < 2.0);
  result = result & (kurtosis > -2.0 && kurtosis < 2.0);

  return(result);
}

#Vsechny statisticke charkteristiky datasetu - data, pro vybraný sloupec - colName (pouziti dplyr)
GetStats <- function(data, colName){
  stats <-  summarise(.data = data,
                      dataLenght = length(.data[[colName]]),
                      minimum = min(.data[[colName]], na.rm=T),     # preventivni na.rm=T
                      Q1 = quantile(.data[[colName]], 0.25, na.rm=T),
                      mean = mean(.data[[colName]], na.rm=T),
                      median = median(.data[[colName]], na.rm=T),
                      Q3 = quantile(.data[[colName]], 0.75, na.rm=T),
                      maximum = max(.data[[colName]], na.rm=T),
                      D = var(.data[[colName]], na.rm=T),
                      sigma = sd(.data[[colName]],na.rm=T),
                      var_coef_perct = (100*(sigma/mean)),  # variacni koeficient v procentech
                      skewness = (moments::skewness(.data[[colName]], na.rm=T)),       # preventivni specifikace balícku moments
                      kurtosis = (moments::kurtosis(.data[[colName]], na.rm=T)-3),
                      norm = ifelse(IsNorm(skewness, kurtosis), "N", "not-N") #urci zda ma normalni rozdeleni
  )

  return(stats);
}

#Vsechny statisticke charkteristiky datasetu - data, pro vybraný sloupec - colName, zhlukovaná pomocí groupBy - groupColName (pouziti dplyr)
GetStatsWithGroupBy <- function(data, colName, groupColName){
  stats <-
    group_by(.data = data, .data[[groupColName]]) %>%
    summarise(        dataLenght = length(.data[[colName]]),
                      minimum = min(.data[[colName]], na.rm=T),     # preventivní na.rm=T
                      Q1 = quantile(.data[[colName]], 0.25, na.rm=T),
                      mean = mean(.data[[colName]], na.rm=T),
                      median = median(.data[[colName]], na.rm=T),
                      Q3 = quantile(.data[[colName]], 0.75, na.rm=T),
                      maximum = max(.data[[colName]], na.rm=T),
                      D = var(.data[[colName]], na.rm=T),
                      sigma = sd(.data[[colName]],na.rm=T),
                      var_coef_perct = (100*(sigma/mean)),  # variacni koeficient v procentech
                      skewness = (moments::skewness(.data[[colName]], na.rm=T)),       # preventivni specifikace balicku moments
                      kurtosis = (moments::kurtosis(.data[[colName]], na.rm=T)-3),
                      norm = ifelse(IsNorm(skewness, kurtosis), "N", "not-N") #urci zda ma normalni rozdeleni
              )

  return(stats);
}


#Vraci vnitrni hradby DM, HM, a Odlehla pozorovani + BoxPlot
#yýsledek vraci jako DataFrame, kde Outer jsou Odlehla pozorovani, IQR je IQR, LB je dolni mez, HB je horni mez
GetBordersAndOutValues_AsBoxPlot <- function(data, title="No title", y_name="Y", x_name="", color="lightblue", size=3.0){
  #Omezeni
  sigma = sd(data,na.rm=T);
  limMin = min(data) - size*sigma; #min
  limMax = max(data) + size*sigma; #max

  #Odlehle hodnoty a boxplot
  boxplt = boxplot(data,
                   main = title,
                   ylab = y_name,
                   xlab = x_name,
                   col = color,
                   ylim= c(limMin,limMax))
  #Odlehla pozorovani
  outer = boxplt$out

  # Pouziti vnitrnich hradeb - obecnejsí postup
  IQR = IQR(data)

  LB = quantile(data, 0.25, na.rm=T) - 1.5*IQR  # vypocet dolni mezi vnitrnich hradeb
  HB = quantile(data, 0.75, na.rm=T) + 1.5*IQR  # vypocet horni mezi vnitrnich hradeb

  result <- matrix(c(outer,IQR,LB,HB),ncol=4,byrow=TRUE);
  colnames(result) <- c("Outer","IQR","LB","HB");
  result <- as.data.frame(result);

  return(result);
}

#//////////////////////////////////////////////////////////////////////////////////////
