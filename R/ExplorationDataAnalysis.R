#//////////////////////////////////////////////////////////////////////////////////////
#EDA - Exploration Data Analysis
#//////////////////////////////////////////////////////////////////////////////////////

#Práce se soubory

# Nastavení pracovního adresáøe na složku source file, pøípadne jeji podskožku subfir
SetWorkingDirectoryToSource <-function(subdir=""){
  # Nastavení pracovního adresáøe na složku source file
  WD <- dirname(rstudioapi::getSourceEditorContext()$path);
  # Podsložka
  if( subdir != "" )WD <- paste(WD,subdir,sep = "/");
  if (!is.null(WD)) setwd(WD);
  return(WD);
}

#Vrací naètený Excel sheet jako DataFrame
ReadExcel <-function(file="", sheet="", colNames=NULL){
  #Naètení dat ve standartním pøedpøipraveném formatu, col_names defaultne naèteny z excelu
  data = read_excel("data_std_raw.xlsx", sheet = "S1")
  #Pøejmenuji sloupce na použitelné názvy
  if( !is.null(colNames) )colnames(data) = colNames;
  #Convertuji datový typ na dataframe
  data = as.data.frame(data);

  return(data)
}

#//////////////////////////////////////////////////////////////////////////////////////

#Overeni normalniho rozdeleni datasetu
#Vrací TRUE - je normalniho rozdeli, FALSE = není N
IsNorm <- function(skewness, kurtosis){
  result = (skewness > -2.0 && skewness < 2.0);
  result = result & (kurtosis > -2.0 && kurtosis < 2.0);
  return(result);
}

#Všechny statistické charkteristiky pro vybraný sloupec -> dplyr
GetStats <- function(data, colName){
  stats <-  summarise(.data = data,
                      dataLenght = length(.data[[colName]]),
                      minimum = min(.data[[colName]], na.rm=T),     # preventivní na.rm=T
                      Q1 = quantile(.data[[colName]], 0.25, na.rm=T),
                      mean = mean(.data[[colName]], na.rm=T),
                      median = median(.data[[colName]], na.rm=T),
                      Q3 = quantile(.data[[colName]], 0.75, na.rm=T),
                      maximum = max(.data[[colName]], na.rm=T),
                      D = var(.data[[colName]], na.rm=T),
                      sigma = sd(.data[[colName]],na.rm=T),
                      var_coef_perct = (100*(sigma/mean)),  # variaèní koeficient v procentech
                      skewness = (moments::skewness(.data[[colName]], na.rm=T)),       # preventivní specifikace balíèku moments
                      kurtosis = (moments::kurtosis(.data[[colName]], na.rm=T)-3),
                      norm = ifelse(IsNorm(skewness, kurtosis), "N", "not-N") #urci zda ma normalni rozdeleni
  )
  return(stats);
}

#Všechny statistické charkteristiky pro vybraný sloupec -> dplyr zhlukovaná pomocí groupBy - groupColName
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
                      var_coef_perct = (100*(sigma/mean)),  # variaèní koeficient v procentech
                      skewness = (moments::skewness(.data[[colName]], na.rm=T)),       # preventivní specifikace balíèku moments
                      kurtosis = (moments::kurtosis(.data[[colName]], na.rm=T)-3),
                      norm = ifelse(IsNorm(skewness, kurtosis), "N", "not-N") #urci zda ma normalni rozdeleni
    )
  return(stats);
}


#Vrací vnitøní hradby DM, HM, a Odlehlá pozorování + BoxPlot
#Výsledek vrací jako DataFrame, kde Outer jsou Odlehlá pozorování, IQR je IQR, LB je dolní mez, HB je horní mez
GetBordersAndOutValues_AsBoxPlot <- function(data, title="No title", y_name="Y", x_name="", color="lightblue", size=3.0){
  #Omezeni
  sigma = sd(data,na.rm=T);
  limMin = min(data) - size*sigma; #min
  limMax = max(data) + size*sigma; #max

  #Odlehlé hodnoty a boxplot
  boxplt = boxplot(data,
                   main = title,
                   ylab = y_name,
                   xlab = x_name,
                   col = color,
                   ylim= c(limMin,limMax))
  #Odlehlá pozorování
  outer = boxplt$out

  # Použití vnitøních hradeb - obecnìjší postup - uvedeno bez ohledu na výrobce!!!
  IQR = IQR(data)

  DM = quantile(data, 0.25, na.rm=T) - 1.5*IQR  # výpoèet dolní mezi vnitøních hradeb
  HM = quantile(data, 0.75, na.rm=T) + 1.5*IQR  # výpoèet horní mezi vnitøních hradeb

  result <- matrix(c(outer,IQR,DM,HM),ncol=4,byrow=TRUE);
  colnames(result) <- c("Outer","IQR","LB","HB");
  result <- as.data.frame(result);

  return(result);
}

#//////////////////////////////////////////////////////////////////////////////////////
