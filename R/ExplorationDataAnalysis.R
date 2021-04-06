#//////////////////////////////////////////////////////////////////////////////////////
#EDA - Exploration Data Analysis
#//////////////////////////////////////////////////////////////////////////////////////

#Prace se soubory

# Nastaveni pracovniho adresare na slozku source file, pripadne jeji podskozku subdir
#' @author Konecny Jiri (kon0327)
EDA.setWorkingDirectoryToSource <-function(subdir=""){
  # Nastaveni pracovniho adresare na složku source file
  WD <- dirname(rstudioapi::getSourceEditorContext()$path);
  # Podslozka
  if( subdir != "" )WD <- paste(WD,subdir,sep = "/");
  if (!is.null(WD)) setwd(WD);

  return(WD);
}

#Vraci nacteny Excel sheet jako DataFrame
#' @author Konecny Jiri (kon0327)
EDA.readExcel <-function(file="", sheet="", colNames=NULL){
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
#' @author Konecny Jiri (kon0327)
EDA.isNorm <- function(skewness, kurtosis){
  result = (skewness > -2.0 && skewness < 2.0);
  result = result & (kurtosis > -2.0 && kurtosis < 2.0);

  return(result);
}

#Vsechny statisticke charkteristiky datasetu - data, pro vybraný sloupec - colName (pouziti dplyr)
#' @author Konecny Jiri (kon0327)
EDA.getStats <- function(data, colName){
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
#' @author Konecny Jiri (kon0327)
EDA.getStatsWithGroupBy <- function(data, colName, groupColName){
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
#Výsledek vraci jako DataFrame, kde Outer jsou Odlehla pozorovani, IQR je IQR, LB je dolni mez, HB je horni mez
#' @author Konecny Jiri (kon0327)
EDA.getBordersAndOutValues_AsBoxPlot <- function(data, title="No title", y_name="Y", x_name="", color="lightblue", size=1.0){
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

  result <- matrix(c(IQR,LB,HB),ncol=3,byrow=TRUE);
  colnames(result) <- c("IQR","LB","HB");
  result <- as.data.frame(result);

  cat("Outers values: ", outer,"\n")
  print(result)
  return(result);
}

#Vraci vnitrni hradby DM, HM, a Odlehla pozorovani + BoxPlot pro multibox plot
#Vysledek vraci jako DataFrame, kde Outer jsou Odlehla pozorovani, LB je dolni mez, HB je horni mez, LQ - dolni Q, HQ- horni Q, MED = median
#Pro kazdy boxplot zvlast dle parameru group!
#' @author Konecny Jiri (kon0327)
EDA.getBordersAndOutValues_AsBoxPlot_ByGroup <- function(data, group, title="No title", y_name="Y", x_name="", color="lightblue"){
  #Odlehle hodnoty a boxplot
  boxplt = boxplot(data ~ group,
                   main = title,
                   ylab = y_name,
                   xlab = x_name,
                   col = color
  );
  #Odlehla pozorovani
  for (i in 1:length(boxplt$group)) {
    cat(boxplt$names[i],"\n\tOuter value: ", boxplt$out[i], "\tGroup: ", boxplt$group[i], "\n");
  }

  #Hradby
  stats = boxplt$stats;
  index = 1;

  result <- matrix(ncol=6, byrow=TRUE);
  colnames(result) = c("Group","LB","LQ","MED","HQ","HB");
  for (name in boxplt$names) {
    data <- c(name, stats[,index]);
    result <- rbind(result, data)
    index = index + 1;
  }
  result <- as.data.frame(result);
  result = na.omit(result) # vynecháme øádky s NA hodnotami

  print(result);
  return(result);
}

#//////////////////////////////////////////////////////////////////////////////////////
