#######################
#1 Remove all objects
######################
rm(list=ls())

########################
#2 Installation packages
########################
#install.packages('tsibble')
#install.packages('tidyverse', dependencies = TRUE)
#install.packages('haven')
#install.packages('pillar')
#install.packages('fpp3')
#update.packages(ask = FALSE)
#install.packages("pak")
#pak::pkg_system_requirements("pkgdown", "ubuntu", "20.04")
#install.packages("testthat") 
#install.packages("pkgload")
#install.packages("sjlabelled")
#install.packages('devtools')
#devtools::install_github("martinctc/surveytoolbox")
#install.packages('reduce')
#install.packages("GGally")
#install.packages("corrplot")
#install.packages("raster", dependencies = TRUE) # has to install r-cran-terra using the command: sudo apt -y install r-cran-terra
#install.packages("caret")
#install.packages('doParallel')
###############################

##########################
#Load the required Library
##########################
library(haven)
library(tidyverse)
library(stringr)
library(GGally)
library(corrplot)
library(labelled)
library(raster)
library(caret)
library("parallel")
library("doParallel")


#Get the current working directory
getwd()

# Load the aggregated spss file (16_povgh_2017.sav) into R dataframe.
defaultpath = "/home/charles/Documents/study/MscDataScience/Dissertation/Rstudio/Data"
filename = "16_povgh_2017.sav"
df <- read_sav(paste0(defaultpath,'/',filename))

# Cholograph map of Ghana. To be used to show the distribution of very poor over the country
gh_df1 <- getData("GADM", country = "Ghana", level = 1)
gh_df1



# Set dataframe row display to about 2000
options(max.print=2000)

# Extract the data frame column definition. This data is also contained within the output of str.
datadictionary <- look_for(df)
datadictionary


#### 1. Exploratory Data Analysis

#Check the dataframe dimension
dim(df)

#Review final dataframe
view(df)
head(df,10)
tail(df,10)
df %>% dplyr::select(REGION) %>% unique()
glimpse(df)

#Change all dataframe column name to small letters
names(df) <- tolower(names(df))


# Check the structure of the dataframe
str(df, list.len = ncol(df))


# Check the summary of the dataframe
summary(df)

# Physical inspection to identify columns that can be dropped.
head(df$s1q2)
head(df$pid)
head(df$welfare)
head(df$emp_status)
head(df$country)
head(df$survemo)
head(df$surveyr)
head(df$povqual)

# Check for unique values in the country column. This only holds the value Ghana and hence it will be dropped.
df %>% distinct(country) # all enries in the column is Ghana
df %>% distinct(survemo) # survey date - month
df %>% distinct(surveyr) # survey date - year



# Drop obvious columns: country, survemo, surveyr
df.droppedcolumns <- df %>% dplyr::select(-country, -survemo, -surveyr, -ghana, hhstatus)

# Review the dataframe
head(df.droppedcolumns,10) 

#Check the dataframe dimension
dim(df.droppedcolumns)

# Convert clust column which represents the Enumeration Areas to character. The same applies to nh and pid
df.droppedcolumns <- df.droppedcolumns %>% mutate(clust = as.character(clust), nh = as.character(nh), pid = as.character(pid))
df.droppedcolumns <- df.droppedcolumns %>% mutate_if(is.labelled, as.factor)
df.droppedcolumns <- df.droppedcolumns %>% mutate(sex = as.factor(sex))


# Review dataframe to confirm column datatype change is done
glimpse(df.droppedcolumns)


# Check the summary of the dataframe
summary(df.droppedcolumns)

# Check whether the dataframe contains null values. There are no null values in the dataframe
colSums(is.na(df.droppedcolumns))

# Further investigate on the columns containing null (labforce and disab)
df.droppedcolumns %>% dplyr::select(labforce) %>% unique
df.droppedcolumns %>% dplyr::select(disab) %>% unique

df.droppedcolumns %>% filter(is.na(labforce)) %>% dplyr::select(pstatus) %>% group_by(pstatus) %>% summarise(Count = n())
df.droppedcolumns %>% dplyr::select(labforce) %>% group_by(labforce) %>% summarise(Count = n())

df.droppedcolumns %>% filter(is.na(disab)) %>% dplyr::select(pstatus) %>% group_by(pstatus) %>% summarise(Count = n())
df.droppedcolumns %>% dplyr::select(disab) %>% group_by(disab) %>% summarise(Count = n())

# Check the number of unique values in a dataframe column classified as factor. 
df.only.fct.columns <- df.droppedcolumns %>% select_if(., is.factor)
vec.unique.fct.value <- sapply(lapply(df.only.fct.columns, unique), length)
is.vector(vec.unique.fct.value)
vec.unique.fct.value
vec.col.todrop <- names(vec.unique.fct.value[vec.unique.fct.value <= 1])


# Further drop columns that are factors and have one variables i.e only one factor variable
df.droppedcolumns <- df.droppedcolumns %>% dplyr::select(-all_of(vec.col.todrop))


# Check the number of unique values in a dataframe column classified as character
df.only.chr.columns <- df.droppedcolumns %>% select_if(., is.character)
vec.unique.chr.value <- sapply(lapply(df.only.chr.columns, unique), length)
is.vector(vec.unique.chr.value)
vec.unique.chr.value
vec.col.todrop.chr <- names(vec.unique.chr.value[vec.unique.chr.value <= 1])

# Further drop columns that are factors and have no variables i.e only one factor variable
df.droppedcolumns <- df.droppedcolumns %>% dplyr::select(-all_of(vec.col.todrop.chr))


# Two modules will be built one for data without na and one with the na's replaced by the appropriate method
# save dropped na to a separate dataframe called df.dropna
df.dropna <- df.droppedcolumns %>% drop_na()
colSums(is.na(df.dropna))

#Obtain the total number of observations per each category of the target variable - pstatus
df.pstatus.distribution <- df.droppedcolumns %>% group_by(pstatus) %>% summarize(Count = n())
df.pstatus.distribution


#draw the bar chat of how the target variable is distributed between non poor, poor and very poor.
ggplot(df.pstatus.distribution, aes(x=pstatus, y=Count, label=Count, fill = as.factor(pstatus))) +
  geom_col(width=0.5) +
  geom_text(color="black", position = position_stack(vjust = 0.5))+
  #geom_text(color="white" ,position = position_stack(vjust = 0.5))+
  scale_fill_hue() +
  scale_x_discrete(labels=c('Very Poor', 'Poor', 'Non Poor')) +
  labs(subtitle = "Very Poor categorization has the lowest count",
       caption = "Source: Ghana Living Standard Survey - G7 - 2016/2017",
       x = "Multidimension Poverty Status",
       y = "HouseHold Count")


# Obtain the distribution of the target variable over the 10 regions
df.droppedcolumns %>% group_by(region,pstatus) %>% summarize(TotalCount=n()) %>%
  ggplot(data=., aes(x=reorder(region, region), y=TotalCount, fill=as.factor(pstatus))) +
  geom_col(width = 0.9, position= position_dodge2(padding = .0009)) +
  scale_fill_hue() +
  labs(title = "A Bar chat showing a count of the 3 different Poverty categories across all 10 Regions", x = "Region", y = "Count") +
  theme(
    plot.title = element_text(hjust = 0.5)
  )


# What is the distribution of the target variable between men and women
df.droppedcolumns %>% group_by(sex,pstatus) %>% summarize(Count = n())

# A bar chat showing the distribution of poverty between men (1) and women (2)
df.droppedcolumns %>% group_by(sex,pstatus) %>% summarize(Count = n()) %>%
  ggplot(data=., aes(x = sex, y = Count, fill = as.factor(pstatus), label = Count )) +
  geom_col(width = 0.8, position= position_dodge2(padding = .009)) +
  geom_text(color="black", width = 0.8, position= position_dodge2(padding = .009)) +
  #geom_text(color="black" , position = position_stack(vjust = 0.5)) +
  scale_fill_hue() +
  scale_x_discrete(labels=c('Male', 'Female')) +
  labs(title = "A Bar chat showing poverty distribution across the two genders", x = "Sex", y = "Count") +
  theme(
    plot.title = element_text(hjust = 0.5)
  )


# What is the distribution of the target variable over the highest educational qualification attained
df.droppedcolumns %>% group_by(povqual,pstatus) %>% summarize(Count = n())


# What is the distribution of the target variable over employment status
df.droppedcolumns %>% group_by(emp_status,pstatus) %>% summarize(Count = n())


# What is the distribution of the target variable over housing Ownership type
df.droppedcolumns %>% group_by(owntype,pstatus) %>% summarize(Count = n())


# What is the distribution of the target variable over urban and rural communities
df.droppedcolumns %>% group_by(rururb,pstatus) %>% summarize(Count = n())


# What is the distribution of the target variable over forms of work
df.droppedcolumns %>% group_by(fwork,pstatus) %>% summarize(Count = n())

# What is the distribution of the target variable over disability
df.droppedcolumns %>% group_by(disab,pstatus) %>% summarize(Count = n())

# What is the distribution of the target variable over marital status
#df.droppedcolumns %>% group_by(s1q3,pstatus) %>% summarize(Count = n())


# Outliers
boxplot.stats(df.droppedcolumns$wta_s_hhsize)$out
boxplot.stats(df.droppedcolumns$eqsc)$out

# Correclation - corrplot
df.numeric <- df.droppedcolumns %>% select_if(.,is.numeric) %>% drop_na() # This has to be updated once null values are replaced
corrplot(corr = cor(df.numeric),
         addCoef.col = "white",
         number.cex= 0.4,
         number.digits = 1,
         outline = "black",
         addgrid.col = "white",
         method = "number",
         type = "lower",
         tl.pos = "ld",
         tl.cex = 0.4,
         order = "original")

# Correlation - GGally
df.numeric.col <- df.droppedcolumns %>% select_if(.,is.numeric) %>% colnames()
df.numeric.col
df.GGally <- df.droppedcolumns %>% dplyr::select(df.numeric.col, pstatus) %>% drop_na()

GGally::ggpairs(df.GGally, columns = 1:10,
                ggplot2::aes(color=pstatus))

colnames(df.droppedcolumns)

#Missing value for disab column
# Use K nearest neighbour to identify missing value in disab parameter

# Extract column index of disab set to NA.
index.disab <- df.droppedcolumns %>% dplyr::select(disab) %>% with(which(is.na(disab))) 


# Split data into 2; Non NA and NA.
df.droppedcolumns.non.na.disab.training <- df.droppedcolumns %>% slice(-index.disab) %>% dplyr::select(-c(labforce,phid,hid,nh, fdnonalc_p, fdrecall, hsdiesel, misprost, fdhhds, ppp2005, ppp2011, pl_abs, pl_ext, mpl_abs))
df.droppedcolumns.na.disab.predict <- df.droppedcolumns %>% slice(index.disab) %>% dplyr::select(-c(labforce,phid,hid,nh, fdnonalc_p, fdrecall, hsdiesel, misprost, fdhhds, ppp2005, ppp2011, pl_abs, pl_ext, mpl_abs))

dim(df.droppedcolumns.non.na.disab.training)
view(df.droppedcolumns.non.na.disab.training)
dim(df.droppedcolumns.na.disab.predict)

#Setting up Parallel run
Timedf = data.frame(time="")
Mycluster = makeCluster(detectCores() -1)
registerDoParallel(Mycluster)

# build Knn model
model.Ctrl <- trainControl(method = "cv",
                           number = 3,
                           allowParallel = T
)
set.seed(500)
stime = system.time({
  model.fit <- train(disab ~ .,
                     data = df.droppedcolumns.non.na.disab.training,
                     method = 'knn',
                     tuneLength = 20,
                     trControl = model.Ctrl,
                     preProc = c("center", "scale"),
                     tuneGrid = expand.grid(k = 1:3)
  )
  
})[3]

Timedf$KNN_Para = stime[1]

# Display the model data
model.fit

#Plot the accuracy against the k 
plot(model.fit)

# variable importance
varImp(model.fit)

# predict the disab variable
pred <- predict(model.fit, newdata = df.droppedcolumns.na.disab.predict)
pred


# Replace the null values with the predict value in disab column
df.droppedcolumns[index.disab, "disab"] <- pred

df.droppedcolumns %>% dplyr::select(disab) %>% unique
df.droppedcolumns %>% dplyr::select(disab)


################################
