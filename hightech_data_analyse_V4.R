require(ggpubr)
library(tidyverse)
library(interactions)
library(ggstance)

dataPad = "C:/Users/maxim/OneDrive/MANAMA/Masterproef/Thesis share/src"
#dataPad = "C:/Users/Evert Cuylen/OneDrive - KU Leuven/Thesis share/src"
hightech_dataset <- read.delim(paste(dataPad, "/htec_emp_reg2_tabular.tsv", sep=""))
hightech_dataset <- hightech_dataset %>%
  separate("freq.nace_r2.unit.sex.geo.TIME_PERIOD", c("freq","nace_r2","unit","sex","geo"),sep=",") %>%
  pivot_longer(starts_with("X20"), names_to="year") %>%
  filter(unit != "THS") %>%
  filter(nace_r2 == "HTC") %>%
  filter(str_length(geo) == 4 & year == "X2018") %>%
  filter(!geo %in% c("EA19","EU15", "EU28")) %>%
  dplyr::select(-c(freq,unit)) %>%
  mutate(hightech_employment = as.numeric(str_extract(value, "\\d+\\.\\d+"))) %>%
  mutate(letters = str_extract(value, "[a-z]+"))

processEurostatNUTS2Dataset <- function(locationString = paste(dataPad,"/education_level.tsv", sep="")) {
  baseDataset <- read.delim(locationString, na.strings=":")
  firstColumn <- dimnames(baseDataset)[[2]][1]
  splitFirstColumns <- head(strsplit(firstColumn, "\\.")[[1]], -1)
  baseDataset <- baseDataset %>%
    separate(firstColumn, splitFirstColumns, sep=",") %>%
    pivot_longer(starts_with("X20"), names_to="year") %>%
    filter(str_length(geo) == 4)%>%
    mutate(nbValue = as.numeric(str_extract(value, "\\d+\\.?\\d*"))) %>%
    mutate(letters = str_extract(value, "[a-z]+"))
    return(baseDataset)
  }

age <- processEurostatNUTS2Dataset(paste(dataPad, "/Opgevulde datasets/Age_of_population_percentage_24-64_vs_oldORyoung.tsv",sep=""))

deaths <- processEurostatNUTS2Dataset(paste(dataPad,"/Opgevulde datasets/number_deaths_unscaled.tsv",sep=""))
education <- processEurostatNUTS2Dataset(paste(dataPad,"/Opgevulde datasets/educatie 25-64jaar.tsv",sep="")) %>%
  filter(year == "X2018")
employee_compensation <- processEurostatNUTS2Dataset(paste(dataPad,"/Opgevulde datasets/Compensation of employees.tsv",sep=""))
gerd <- processEurostatNUTS2Dataset(paste(dataPad,"/Opgevulde datasets/gerd_per_capita.tsv",sep=""))
household_income <- processEurostatNUTS2Dataset(paste(dataPad,"/Opgevulde datasets/income_per_household_(per person in Euro).tsv",sep=""))
median_age <- processEurostatNUTS2Dataset(paste(dataPad,"/Opgevulde datasets/median_age.tsv",sep=""))
motorways <- processEurostatNUTS2Dataset(paste(dataPad,"/Opgevulde datasets/motorways km per thousand square kilometers.tsv",sep=""))
population <- processEurostatNUTS2Dataset(paste(dataPad,"/Opgevulde datasets/population.tsv",sep=""))
workhours_long <- processEurostatNUTS2Dataset(paste(dataPad,"/Opgevulde datasets/hours_in_workweek.tsv",sep=""))
worked_hours <- processEurostatNUTS2Dataset(paste(dataPad,"/Opgevulde datasets/thousand_hours_worked.tsv",sep=""))
unemployment <- processEurostatNUTS2Dataset(paste(dataPad,"/Opgevulde datasets/unemployment.tsv",sep=""))

workhours <- workhours_long %>%
  dplyr::select(-c(freq, unit, sex, year, value, letters)) %>%
  pivot_wider(
    names_from = age,
    values_from = nbValue,
    names_prefix = "var_workhours_"
  )



###Joins###
hightech_joined_dataset <- dplyr::select(filter(hightech_dataset, sex == "T"), c(geo, hightech_employment)) %>%
  left_join(rename(dplyr::select(age, c(geo, nbValue)), Leeftijd=nbValue), by = c("geo")) %>%
  left_join(rename(dplyr::select(deaths, c(geo, nbValue)), Overlijdens=nbValue), by = "geo") %>%
  left_join(rename(dplyr::select(education, c(geo, nbValue)), Opleidingsgraad=nbValue), by = "geo") %>%
  left_join(rename(dplyr::select(employee_compensation, c(geo, nbValue)), Werknemer_Compensatie=nbValue), by = "geo") %>%
  left_join(rename(dplyr::select(gerd, c(geo, nbValue)), gerd=nbValue), by = "geo") %>%
  left_join(rename(dplyr::select(household_income, c(geo, nbValue)), Inkomen_Huishouden=nbValue), by = "geo") %>%
  left_join(rename(dplyr::select(median_age, c(geo, nbValue)), Mediane_Leeftijd=nbValue), by = "geo") %>%
  left_join(rename(dplyr::select(motorways, c(geo, nbValue)), Motorwegen=nbValue), by = "geo") %>%
  left_join(rename(dplyr::select(population, c(geo, nbValue)), Populatie=nbValue), by = "geo") %>%
  left_join(workhours, by = "geo") %>%
  left_join(rename(dplyr::select(worked_hours, c(geo, nbValue)), GewerkteUren=nbValue), by = "geo") %>%
  left_join(rename(dplyr::select(unemployment, c(geo, nbValue)), Werkloosheid=nbValue), by = "geo") %>%
  filter(geo != "FFFFJFQDSJKDLMJDSKFLDMSJF")
  
highset <- hightech_joined_dataset %>%
  mutate(Overlijdens = Overlijdens / Populatie) %>%
  mutate(Werknemer_Compensatie = Werknemer_Compensatie / GewerkteUren * 1000000) %>%
  na.exclude()

highset$geo <- NULL



# #Exact Lamba###BOXCOX voor hightech_employment###########NIET GEBRUIKT VOOR EIND MODEL#####################
# 
# library(MASS)
# b <- boxcox(lm(hightech_joined_dataset$hightech_employment ~ 1))
# head(b)
# lambda <- b$x[which.max(b$y)]
# new_exact_employment <- (hightech_joined_dataset$hightech_employment ^ lambda - 1) / lambda
# shapiro.test(new_exact_employment)
# shapiro.test(hightech_joined_dataset$hightech_employment)
# 
# hightech_joined_dataset$hightech_employment <- new_exact_employment

#######################################################################################################
#Exact Lamba###BOXCOX voor hightech_employment####################################################

library(MASS)
b <- boxcox(lm(highset$hightech_employment ~ 1))
head(b)
lambda <- b$x[which.max(b$y)]
#new_exact_employment <- (highset$hightech_employment ^ lambda - 1) / lambda
new_exact_employment <- log(highset$hightech_employment)
shapiro.test(new_exact_employment)
shapiro.test(highset$hightech_employment)
hightech_employment <- highset$hightech_employment
highset$hightech_employment <- new_exact_employment
hightech_employment_boxcox <- new_exact_employment

hist(hightech_employment)
hist(hightech_employment_boxcox, breaks=8)
#######################################################################################################


estartmodel = lm(hightech_employment ~ . , data=highset) 


ggplot(highset) +
  geom_histogram(aes(x=hightech_employment_boxcox), binwidth = 0.3)

# # voorbeeld om data in een excel file met 20 kolommen in te lezen in R,
# # de Y-variable staat in het voorbeeld in kolom 2 
# 
# dataset = as.matrix(read_excel("mijndata.xlsx"),ncol=20)
# 
# # indien de Y variabele in de 2e kolom staat in de excel file, anders cijfer 2 aanpassen.
# # indien de eerste kolom bijvoorbeeld een naam is die niet als variabele gefit moet worden,
# # laten we ook die eerste kolom weg (dit dient enkel als voorbeeld)
# 
# y=as.numeric(dataset[,2])    
# x = matrix(as.numeric(dataset[,-c(1,2)]),ncol=18)
# 
# head(x)
# Dataset = as.data.frame(y=y,x=x)
# 
# # Box-Cox transformatie om de Y variabele meer normaal te maken.
# 
# outbc = boxcox(y~x, data = Dataset)
# outbc$x[which.max(outbc$y)] # dit geeft de geschatte waarde van lambda.
# 
# # je kan iets fijner zoeken rond de geschatte waarde van lambda. 
# # Stel dat de vorige lijn 0.15 als antwoord geeft, dan kan je opnieuw zoeken rond de waarde 0.15
# 
# outbc2 = boxcox(y~x, data= Dataset, lambda = seq(from=0.12,to=0.20,length=10))
# outbc2$x[which.max(outbc2$y)] 
# lambda = outbc2$x[which.max(outbc2$y)]
# 
# ynieuw = (y^lambda-1)/lambda
# 
# par(mfrow=c(1,2))
# hist(y)
# hist(ynieuw) 

# Het histogram van ynieuw zou meer moeten lijken op een normale dichtheidsfunctie 
# dan het oorspronkelijke.

#######################################################

# AIC om variabelen te selecteren. Ieder model krijgt een AIC waarde. 
# Het model met de laagste AIC is het beste. 
# Rapporteer welke modellen in de zoektocht gebruik zijn en de top 5 van beste modellen.

# doorzoek alle deelmodellen van een groot model:
# https://cran.r-project.org/web/packages/MuMIn/MuMIn.pdf

library(MuMIn)
options(na.action = "na.fail")

# deze optie zorgt ervoor dat in geval van ontbrekende gegevens toch telkens dezelfde dataset gebruikt wordt. Anders kan het gebeuren dat bijvoorbeeld voor datasets waar er enkel in kolom 5 ontbrekende waarden zijn, slechts een deel van de totale gegevens gebruikt wordt waar alles geobserveerd is, maar voor modellen waar de variabele in kolom 5 niet aanwezig is, alle datalijnen gebruikt worden. 

startmodel = lm(hightech_employment ~ ., data=highset) 
#  dredge(startmodel,rank=AIC)

# # voorbeeld van een dataset die al in R zit:
# startmodel =  lm(y ~ ., data = Cement)
# selectie = dredge(startmodel, rank=AIC)
# selectie

# stapsgewijze zoektocht door telkens 1 variabele weg te laten of toe te voegen 
# (dit probeert niet alle deelmodellen)

library(MASS)
zoektocht1 = stepAIC(startmodel,k=qchisq(0.01, 1, lower.tail = F),direction="both", scope=list(upper=~.,lower=~1))
summary(zoektocht1)
zoektocht11 = stepAIC(startmodel,k=qchisq(0.05, 1, lower.tail = F),direction="both", scope=list(upper=~.,lower=~1))
summary(zoektocht11)
zoektocht12 = stepAIC(startmodel,k=1,direction="both", scope=list(upper=~.,lower=~1))
summary(zoektocht12)

# Dit doorzoekt alle hoofdeffecten in het model, telkens 1 erbij of 1 weg. 
# Het startmodel wordt niet verder uitgebreid.

zoektocht2 = stepAIC(startmodel,k=qchisq(0.01, 2, lower.tail = F),direction="both", scope=list(upper=~.^2,lower=~1))
summary(zoektocht2)

# Dit doorzoekt alle hoofdeffecten en alle paarsgewijze interacties in het model, 
# telkens 1 erbij of 1 weg. 
# Het startmodel wordt wel verder uitgebreid.

ggplot(highset) +
  geom_point(aes(y=exp(hightech_employment), x=Leeftijd)) +
  labs(x="Leeftijdsverhouding", y="High-techtewerkstelling")
# + geom_smooth(aes(y=hightech_employment, x=population))

ggplot(highset) +
  geom_point(aes(y=exp(hightech_employment), x=Werknemer_Compensatie)) +
  labs(x="Werknemer Compensatie", y="High-techtewerkstelling")

ggplot(highset) +
  geom_point(aes(y=exp(hightech_employment), x=Overlijdens)) +
  labs(x="Overlijdens", y="High-techtewerkstelling")

ggplot(highset) +
  geom_point(aes(y=exp(hightech_employment), x=Mediane_Leeftijd)) +
  labs(x="Overlijdens", y="High-techtewerkstelling")

ggplot(highset) +
  geom_point(aes(y=exp(hightech_employment), x=Opleidingsgraad)) +
  labs(x="Mediane leeftijd", y="High-techtewerkstelling")

ggplot(highset) +
  geom_point(aes(y=exp(hightech_employment), x=Inkomen_Huishouden)) +
  labs(x="Inkomen huishouden", y="High-techtewerkstelling")

ggplot(highset) +
  geom_point(aes(y=exp(hightech_employment), x=`var_workhours_Y15-74`)) +
  labs(x="Werkuren 15-74 jaar", y="High-techtewerkstelling")

ggplot(highset) +
  geom_point(aes(y=exp(hightech_employment), x=`var_workhours_Y25-64`)) +
  labs(x="Werkuren 25-64 jaar", y="High-techtewerkstelling")

ggplot(highset) +
  geom_point(aes(y=exp(hightech_employment), x=GewerkteUren)) +
  labs(x="Gewerkte uren", y="High-techtewerkstelling")


workhours_plot_dataset <- 
  highset[ , grepl( "var_workhours_" , names( highset ) ) ] %>%
  pivot_longer(
    cols = everything(),
    names_to = "Agegroup",
    values_to = "Hours"
  ) %>%
  mutate(Agegroup= str_remove(string = Agegroup, pattern = "var_workhours_"))


ggplot(workhours_plot_dataset, aes(x=Agegroup, y=Hours)) + 
  geom_violin(trim=FALSE) + geom_boxplot(width=0.1, outlier.shape = NA)

cor.test(highset$Overlijdens, highset$hightech_employment)

cor.test(highset$Overlijdens, highset$Mediane_Leeftijd)

cor.test(highset$Inkomen_Huishouden, highset$Werknemer_Compensatie)


#Plot om de impact te zien van de interactie tussen employee_compensation & household_income
#Die het model toont.
johnson_neyman(lm(zoektocht2), pred=employee_compensation, modx = household_income, alpha = 0.05, control.fdr = 1)

