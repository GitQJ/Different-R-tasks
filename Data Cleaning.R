#Part1 - Data Cleaning
install.packages("readxl")  # Install readxl package
library(readxl) # load readxl package

 # Set the working directory to where file is contained

Spain <- read_excel("STAT8010_CA2_2024.xlsx", sheet = "Spain") # Assign each sheet to a Data frame
Info <- read_excel("STAT8010_CA2_2024.xlsx", sheet = "Info") # Assign each sheet to a Data frame

# 1 - Removing variables

# Creating vector with new variable names
vars = c("City",
         "Town",
         "Post Code",
         "Address",
         "Longitude",
         "Latitude",
         "Petrol 95 E10",
         "Petrol 95 E5 Premium",
         "Petrol 98 E10",
         "Diesel B",
         "Diesel C",
         "Bioethanol",
         "% bioalcohol",
         "Biodiesel",
         "% Ether Metil",
         "LPG",
         "Gas",
         "LPG 3",
         "Hidrogen",
         "Opening time")

# Creating empty vector for indexes
indexes = c()

# For each variable in vars vector
for (var in vars) {
  # Store the column index corresponding to column name
  index <- grep(var, colnames(Spain))[1]
  print(index)
  # Append it to list
  indexes <-append(indexes, index)
  print(indexes)
}

# Remove all the columns corresponding to indexes
Spain.small.df <- Spain[,-indexes]

# 2 - Rename variables

# Launch dplyr library
library(dplyr)

# Rename all the necessary columns using rename function
Spain.renamed <- Spain.small.df %>% rename(Side = `Road Side`, 
                                            Date = `Record date`,
                                            Petrol = `Petrol 95 E5`,
                                            P_98 = `Petrol 98 E5`,
                                            Diesel = `Diesel A`,
                                            PremiumD = `Diesel Premium`,
                                            Sell = `Sell Type`)

# 3 - Create Service Variable

# Per Clean data frame, Service is replaced by Service type, therefore renaming column
Spain.renamed$Service <- Spain.renamed$`Service Type`

# For each entry in service type column
for (i in seq(1:length(Spain.renamed$`Service Type`))) {
  
  # If Service contains A or D
  if (grepl("A)", Spain.renamed$`Service Type`[i]) == TRUE |
      grepl("D)", Spain.renamed$`Service Type`[i]) == TRUE) {
    # Update to A
    Spain.renamed$Service[i] <- gsub(Spain.renamed$Service[i], Spain.renamed$Service[1], "A")
  }
  
  # If Service contains P
  if (grepl("P)", Spain.renamed$`Service Type`[i]) == TRUE) {
    # Update to A
    Spain.renamed$Service[i] <- gsub(Spain.renamed$Service[i], Spain.renamed$Service[1], "P")
  }
  # If Service contains A and P
  if (grepl("A", Spain.renamed$Service[i]) == TRUE &
      grepl("P)", Spain.renamed$`Service Type`[i]) == TRUE) {
    # Update to AP
    Spain.renamed$Service[i] <- gsub(Spain.renamed$Service[i], Spain.renamed$Service[1], "AP")
  }
}

# Remove rows of columns containing NAs (Labelled as Sin Datos in data)
Spain.nona = subset(Spain.renamed, Spain.renamed$Service != "Sin datos")

# 4 - Group Brands

# Create a vector for Brands
brands = c("ALCAMPO", "BP", "CAMPSA", "CARREFOUR", "CEPSA", "GALP", "PETRONOR", "PETROPRIX", "PLENOIL", "REPSOL", "SHELL",
           "TAMOIL")

# For each entry in Brand Column
for (i in seq(1:length(Spain.nona$Brand))) {
  # For each brand in vector
  for (brand in brands) {
    # If the entry contains the brand
    if (grepl(brand, Spain.nona$Brand[i])) {
      # Update to the matching brand
      Spain.nona$Brand[i] <- brand
    }
  }
  # If the entry does not contain any brand from vector
  if (!Spain.nona$Brand[i] %in% brands) {
    # Update to other
    Spain.nona$Brand[i] = "Other"
  }
}

# 5 - Substitute

# For each entry in the the updated dataset
for (i in seq(1:length(Spain.nona$Petrol))) {
  # Replace coma with dote in Petrol column
  Spain.nona$Petrol[i] = gsub(",", ".", Spain.nona$Petrol[i])
  # Replace coma with dote in P_98 column
  Spain.nona$P_98[i] = gsub(",", ".", Spain.nona$P_98[i])
  # Replace coma with dote in Diesel column
  Spain.nona$Diesel[i] = gsub(",", ".", Spain.nona$Diesel[i])
  # Replace coma with dote in PremiumD column
  Spain.nona$PremiumD[i] = gsub(",", ".", Spain.nona$PremiumD[i])
}

# Make amended variable as numeric
Spain.nona$Petrol <- as.numeric(Spain.nona$Petrol)
Spain.nona$P_98 <- as.numeric(Spain.nona$P_98)
Spain.nona$Diesel <- as.numeric(Spain.nona$Diesel)
Spain.nona$PremiumD <- as.numeric(Spain.nona$PremiumD)

# 6 - Reduce

# Create a vector of the countries we want
counties = c("MADRID",
             "VALENCIA / VALÈNCIA",
             "BARCELONA",
             "ALICANTE",
             "MURCIA",
             "SEVILLA",
             "ASTURIAS",
             "SANTA CRUZ DE TENERIFE")

# Filter counties based on the vector
Spain.reduced <- filter(Spain.nona, County %in% counties)

# 7 - Create Excel File

# install.packages("xlsx")  if needed
library(xlsx) # Launch xlsx library
# Create excel file with the clean data set
write.xlsx(Spain.reduced, '/Users/Quentin/Desktop/Hdip Data Science and Analytics/Year 1/Semester 1/STAT8010 - Introduction to R for Data Science/Assignment 2/STAT80010_CA2_2024-cleaned_by_Quentin.xlsx')
