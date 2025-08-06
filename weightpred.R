library(readr)  
library(dplyr)
library(psych)  
library(tidyr)  


data <- readxl::read_excel("data.xlsx")


cat("Data Structure:\n")
str(data)

data <- data %>% select(-Contact, -Name)
data$Gender <- ifelse(data$Gender == "Male", 1, 0)


na_counts <- sapply(data, function(x) sum(is.na(x)))
cat("\nNA Counts:\n")
print(na_counts)

# --------------------------------------
# Finding Descriptive Features of Data
# --------------------------------------


describe_stats <- summary(data)
cat("\nDescriptive Statistics:\n")
print(describe_stats)


# -------------------------------
# Creating a Box and Whisker Plot
# -------------------------------

boxplot(data$`Calories Intake`,horizontal=TRUE,col="lightpink",main = "Whisker For Calories Tntake")

boxplot(data$`Gender`,horizontal=TRUE,col="lightblue",main = "Whisker For Gender")

boxplot(data$`Water Intake(mL)`,horizontal=TRUE,col="#fdf342",main = "Whisker For Water Intake")

boxplot(data$`Weight(Kg)`,horizontal=TRUE,col="lightgreen",main = "Whisker For Weight")

#remove_outliers <- function(df) {
  #numeric_columns <- df %>% select(where(is.numeric)) %>% colnames()
  #numeric_columns <- numeric_columns[numeric_columns != "Gender"]
  
  #for (col in numeric_columns)  {
    #Q1 <- quantile(df[[col]], 0.25, na.rm = TRUE)
    #Q3 <- quantile(df[[col]], 0.75, na.rm = TRUE)
    #IQR_val <- IQR(df[[col]], na.rm = TRUE)
    
    
    #lower_bound <- Q1 - 1.5 * IQR_val
   # upper_bound <- Q3 + 1.5 * IQR_val
    
    
    #df <- df %>% filter(df[[col]] >= lower_bound & df[[col]] <= upper_bound)
  #}
  
  #return(df)
#}

# -------------------------------
# Creating a Scatter Plot
# -------------------------------

scatter_data <- data.frame(
  CaloriesIntake = data$`Calories Intake`,
  Weight = data$`Weight(Kg)`,
  WaterIntake = data$`Water Intake(mL)`,
  Gender = as.numeric(data$`Gender`) 
)


pairs(
  scatter_data,
  main = "Scatter Plot Matrix",
  labels = c("Calories Intake", "Weight (Kg)", "Water Intake (mL)", "Gender"),
  pch = 21, 
  bg = "lightblue",
  col = "darkblue" 
)
# -------------------------------
# Making the L.model of Data
# -------------------------------

model<- lm(`Weight(Kg)` ~ `Calories Intake` + `Water Intake(mL)` + `Gender`, data = data)
summary(model)





#Interpretation
#76.2 percent of the data is varied by Calories Intake, Water Intake,Gender and  10 percent by other factors.
#If the outcome is weight in kilograms, the RSE of 3.536 means that the model’s predictions for weight deviate by about 3.536 kg on average.

