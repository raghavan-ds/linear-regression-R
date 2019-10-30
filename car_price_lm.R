#upload the data file into R
car <- read.csv("CarPrice_Assignment.csv", stringsAsFactors = F, header = T)
library(dplyr)
library(stringr)
library(MASS)
library(car)
#check the na values in the file
na_values <- car %>% summarise_all(funs(sum(is.na(.)/n())))
View(na_values)
#No na values
#To add the Car company name as a independent vector, need to split the company name from the car name
name <- car$CarName
lname <- str_split_fixed(name, " ", 2)
colnames(lname) <- c("First_Name", "Last_Name")
#since lname is an atomic vector, we cannot pass the column from lname to car dataset. have to convert the
#atomic vector into dataframe
lname <- data.frame(lname)
car$Company <- lname$First_Name

#summary of car$company, if you see that there will lot of spelling mistakes which makes more variables
#In order to clean the data to make sure in correct spelling to eradicate the redundant 
car$Company <- gsub("vw","volkswagen", car$Company)
car$Company <- gsub("vokswagen","volkswagen", car$Company)
car$Company <- gsub("toyouta","toyota", car$Company)
car$Company <- gsub("porcshce","porsche", car$Company)
car$Company <- gsub("Nissan","nissan", car$Company)
car$Company <- gsub("maxda","mazda", car$Company)
summary(car$Company)
#convert company into factors
car$Company <- as.factor(car$Company)


#dummy variable creation
#convert the gas type into numeric variable by assigning 1 for gas and diesel for 0
car$fueltype <- ifelse(car$fueltype == "gas",1,0)
car$fueltype <- as.numeric(car$fueltype)

#convert the aspiration into numeric variable 
car$aspiration <- ifelse(car$aspiration == "std",1,0)
car$aspiration <- as.numeric(car$aspiration)

#convert the door into numeric variable 
car$doornumber <- ifelse(car$doornumber == "two",1,0)
car$doornumber <- as.numeric(car$doornumber)

#convert the engine location into numeric
car$enginelocation <- ifelse(car$enginelocation == "front",1,0)
car$enginelocation <- as.numeric(car$enginelocation)

#convert the cylindernumber into numeric
car$cylindernumber <- ifelse(car$cylindernumber == "two",2,ifelse(car$cylindernumber == "three",3,ifelse(car$cylindernumber == "four",4,ifelse(car$cylindernumber == "five",5,ifelse(car$cylindernumber == "six",6,ifelse(car$cylindernumber == "eight",8,12))))))
car$cylindernumber <- as.numeric(car$cylindernumber)


# Create the dummy variable for carbody variable
dummy_1 <- data.frame(model.matrix( ~carbody, data = car))
View(dummy_1)
dummy_1 <- dummy_1[,-1]

# Combine the dummy variables and the numeric columns of car dataset, in a new dataset called car_1
car_1 <- cbind(car[,-7], dummy_1)


# Create the dummy variable for drivewheel variable
dummy_2 <- data.frame(model.matrix(~drivewheel, data = car_1))
View(dummy_2)
dummy_2 <- dummy_2[,-1]
# Combine the dummy variables and the numeric columns of car dataset, in a new dataset called car_2
car_2 <- cbind(car_1[,-7], dummy_2)

# Create the dummy variable for enginetype variable
dummy_3 <- data.frame(model.matrix(~enginetype, data = car_2))
View(dummy_3)
dummy_3 <- dummy_3[,-1]
# Combine the dummy variables and the numeric columns of car dataset, in a new dataset called car_3
car_3 <- cbind(car_2[,-13], dummy_3)


# Create the dummy variable for enginetype variable
dummy_4 <- data.frame(model.matrix(~fuelsystem, data = car_3))
View(dummy_4)
dummy_4 <- dummy_4[,-1]
# Combine the dummy variables and the numeric columns of car dataset, in a new dataset called car_3
car_4 <- cbind(car_3[,-15], dummy_4)


# Create the dummy variable for company variable
dummy_5 <- data.frame(model.matrix(~Company, data = car_4))
View(dummy_5)
dummy_5 <- dummy_5[,-1]
# Combine the dummy variables and the numeric columns of car dataset, in a new dataset called car_3
car_5 <- cbind(car_4[,-23], dummy_5)

#drop the irrelevant variables(car_ID and carName) from the dataset
car_5 <- drop(car_5[,-1])
car_5 <- drop(car_5[,-2])
View(car_5)

#derived metrics
#compare the enginesize with citympg
car_5$engine_city <- car_5$enginesize/car_5$citympg

#compare the cylindernumber with citympg
car_5$city_cyl <- car_5$citympg/car_5$cylindernumber

#conveninence store the car_5 in other variable name
price_car <- car_5

#seperate training and testing dataset
set.seed(100)
trainindices= sample(1:nrow(price_car), 0.7*nrow(price_car))
train = price_car[trainindices,]
test = price_car[-trainindices,]

#build the model with all variables
model1 <- lm(price~., data = train)
summary(model1)


#execute the stepAIC to obtain the most relevant variables
step <- stepAIC(model1, direction="both")

#based on the stepAIC, Create the model2
model2 <- lm(price ~ aspiration + enginelocation + carwidth + curbweight + 
               cylindernumber + boreratio + compressionratio + horsepower + 
               peakrpm + citympg + carbodyhardtop + carbodyhatchback + carbodysedan + 
               carbodywagon + drivewheelfwd + enginetypedohcv + enginetypel + 
               enginetypeohc + enginetypeohcf + enginetyperotor + fuelsystem2bbl + 
               fuelsystemmpfi + fuelsystemspdi + Companybmw + Companybuick + 
               Companychevrolet + Companydodge + Companyisuzu + Companymazda + 
               Companymitsubishi + Companynissan + Companyplymouth + Companyrenault + 
               Companytoyota + Companyvolkswagen + engine_city, data = price_car)

summary(model2)
#To find the multicollinearity, using VIF
vif(model2)
#Horsepower has the largest vif of 25.55 with max p value of 0.619
#lets build the model by removing the horsepower from the model2
model3 <- lm(price ~ aspiration + enginelocation + carwidth + curbweight + 
              cylindernumber + boreratio + compressionratio + peakrpm + citympg + carbodyhardtop + carbodyhatchback + carbodysedan + 
              carbodywagon + drivewheelfwd + enginetypedohcv + enginetypel + 
              enginetypeohc + enginetypeohcf + enginetyperotor + fuelsystem2bbl + 
              fuelsystemmpfi + fuelsystemspdi + Companybmw + Companybuick + 
              Companychevrolet + Companydodge + Companyisuzu + Companymazda + 
              Companymitsubishi + Companynissan + Companyplymouth + Companyrenault + 
              Companytoyota + Companyvolkswagen + engine_city, data = price_car)
vif(model3)

#Curbweight has the largest VIF of 23.77 and 0.002 p value
#lets build the model by removing the Curbweight from the model3
model4 <- lm(price ~ aspiration + enginelocation + carwidth + cylindernumber + boreratio + compressionratio + peakrpm + citympg + carbodyhardtop + carbodyhatchback + carbodysedan + 
              carbodywagon + drivewheelfwd + enginetypedohcv + enginetypel + 
              enginetypeohc + enginetypeohcf + enginetyperotor + fuelsystem2bbl + 
              fuelsystemmpfi + fuelsystemspdi + Companybmw + Companybuick + 
              Companychevrolet + Companydodge + Companyisuzu + Companymazda + 
              Companymitsubishi + Companynissan + Companyplymouth + Companyrenault + 
              Companytoyota + Companyvolkswagen + engine_city, data = price_car)
summary(model4)
vif(model4)
#Engine_City has the largest VIF of 17.192 
#lets build the model by removing the Engine_city from the model4
model5 <- lm(price ~ aspiration + enginelocation + carwidth + cylindernumber + boreratio + compressionratio + peakrpm + citympg + carbodyhardtop + carbodyhatchback + carbodysedan + 
               carbodywagon + drivewheelfwd + enginetypedohcv + enginetypel + 
               enginetypeohc + enginetypeohcf + enginetyperotor + fuelsystem2bbl + 
               fuelsystemmpfi + fuelsystemspdi + Companybmw + Companybuick + 
               Companychevrolet + Companydodge + Companyisuzu + Companymazda + 
               Companymitsubishi + Companynissan + Companyplymouth + Companyrenault + 
               Companytoyota + Companyvolkswagen, data = price_car)
summary(model5)
vif(model5)

#CarbodySedan has the largest VIF of 12.166 
#lets build the model by removing the carbodysedan from the model5
model6 <- lm(price ~ aspiration + enginelocation + carwidth + cylindernumber + boreratio + compressionratio + peakrpm + citympg + carbodyhardtop + carbodyhatchback + carbodywagon + drivewheelfwd + enginetypedohcv + enginetypel + 
               enginetypeohc + enginetypeohcf + enginetyperotor + fuelsystem2bbl + 
               fuelsystemmpfi + fuelsystemspdi + Companybmw + Companybuick + 
               Companychevrolet + Companydodge + Companyisuzu + Companymazda + 
               Companymitsubishi + Companynissan + Companyplymouth + Companyrenault + 
               Companytoyota + Companyvolkswagen, data = price_car)

summary(model6)
vif(model6)

#fuelsystemmpfi has the largest VIF of 7.07 
#lets build the model by removing the fuelsystemmpfi from the previous model
model7 <- lm(price ~ aspiration + enginelocation + carwidth + cylindernumber + boreratio + compressionratio + peakrpm + citympg + carbodyhardtop + carbodyhatchback + carbodywagon + drivewheelfwd + enginetypedohcv + enginetypel + 
               enginetypeohc + enginetypeohcf + enginetyperotor + fuelsystem2bbl + fuelsystemspdi + Companybmw + Companybuick + 
               Companychevrolet + Companydodge + Companyisuzu + Companymazda + 
               Companymitsubishi + Companynissan + Companyplymouth + Companyrenault + 
               Companytoyota + Companyvolkswagen, data = price_car)

summary(model7)
vif(model7)

#citympg has the largest VIF of 6.227 
#lets build the model by removing the citympg from the previous model
model8 <- lm(price ~ aspiration + enginelocation + carwidth + cylindernumber + boreratio + compressionratio + peakrpm + carbodyhardtop + carbodyhatchback + carbodywagon + drivewheelfwd + enginetypedohcv + enginetypel + 
               enginetypeohc + enginetypeohcf + enginetyperotor + fuelsystem2bbl + fuelsystemspdi + Companybmw + Companybuick + 
               Companychevrolet + Companydodge + Companyisuzu + Companymazda + 
               Companymitsubishi + Companynissan + Companyplymouth + Companyrenault + 
               Companytoyota + Companyvolkswagen, data = price_car)

summary(model8)
vif(model8)


#enginetypeohc has the largest VIF of 4.9889 
#lets build the model by removing the enginetypeohc from the previous model
model9 <- lm(price ~ aspiration + enginelocation + carwidth + cylindernumber + boreratio + compressionratio + peakrpm + carbodyhardtop + carbodyhatchback + carbodywagon + drivewheelfwd + enginetypedohcv + enginetypel + 
               enginetypeohcf + enginetyperotor + fuelsystem2bbl + fuelsystemspdi + Companybmw + Companybuick + 
               Companychevrolet + Companydodge + Companyisuzu + Companymazda + 
               Companymitsubishi + Companynissan + Companyplymouth + Companyrenault + 
               Companytoyota + Companyvolkswagen, data = price_car)

summary(model9)
vif(model9)


#enginetypeohc has the largest VIF of 4.9889 
#lets build the model by removing the enginetypeohc from the previous model
model_10 <- lm(price ~ aspiration + enginelocation + cylindernumber + boreratio + compressionratio + peakrpm + carbodyhardtop + carbodyhatchback + carbodywagon + drivewheelfwd + enginetypedohcv + enginetypel + 
               enginetypeohcf + enginetyperotor + fuelsystem2bbl + fuelsystemspdi + Companybmw + Companybuick + 
               Companychevrolet + Companydodge + Companyisuzu + Companymazda + 
               Companymitsubishi + Companynissan + Companyplymouth + Companyrenault + 
               Companytoyota + Companyvolkswagen, data = price_car)

summary(model_10)
vif(model_10)


#fuelsystem2bbl has the largest VIF of 2.9141 with high pvalue 
#lets build the model by removing the fuelsystem2bbl from the previous model
model_11 <- lm(price ~ aspiration + enginelocation + cylindernumber + boreratio + compressionratio + peakrpm + carbodyhardtop + carbodyhatchback + carbodywagon + drivewheelfwd + enginetypedohcv + enginetypel + 
                 enginetypeohcf + enginetyperotor + fuelsystemspdi + Companybmw + Companybuick + 
                 Companychevrolet + Companydodge + Companyisuzu + Companymazda + 
                 Companymitsubishi + Companynissan + Companyplymouth + Companyrenault + 
                 Companytoyota + Companyvolkswagen, data = price_car)

summary(model_11)
vif(model_11)

#drivewheelfwd has the largest VIF of 2.5889 with high pvalue 
#lets build the model by removing the fuelsystem2bbl from the previous model
model_12 <- lm(price ~ aspiration + enginelocation + cylindernumber + boreratio + compressionratio + peakrpm + carbodyhardtop + carbodyhatchback + carbodywagon + enginetypedohcv + enginetypel + 
                 enginetypeohcf + enginetyperotor + fuelsystemspdi + Companybmw + Companybuick + 
                 Companychevrolet + Companydodge + Companyisuzu + Companymazda + 
                 Companymitsubishi + Companynissan + Companyplymouth + Companyrenault + 
                 Companytoyota + Companyvolkswagen, data = price_car)

summary(model_12)
vif(model_12)

#peakrpm has the largest VIF of 2.404 with high pvalue 
#lets build the model by removing the peakrpm from the previous model
model_13 <- lm(price ~ aspiration + enginelocation + cylindernumber + boreratio + compressionratio + carbodyhardtop + carbodyhatchback + carbodywagon + enginetypedohcv + enginetypel + 
                 enginetypeohcf + enginetyperotor + fuelsystemspdi + Companybmw + Companybuick + 
                 Companychevrolet + Companydodge + Companyisuzu + Companymazda + 
                 Companymitsubishi + Companynissan + Companyplymouth + Companyrenault + 
                 Companytoyota + Companyvolkswagen, data = price_car)

summary(model_13)
vif(model_13)


#fuelsystemspdi has the largest VIF of 2.302 with high pvalue 
#lets build the model by removing the fuelsystemspdi from the previous model
model_14 <- lm(price ~ aspiration + enginelocation + cylindernumber + boreratio + compressionratio + carbodyhardtop + carbodyhatchback + carbodywagon + enginetypedohcv + enginetypel + 
                 enginetypeohcf + enginetyperotor + Companybmw + Companybuick + 
                 Companychevrolet + Companydodge + Companyisuzu + Companymazda + 
                 Companymitsubishi + Companynissan + Companyplymouth + Companyrenault + 
                 Companytoyota + Companyvolkswagen, data = price_car)

summary(model_14)
vif(model_14)
#from model_14, there will be no high high vif value and p value variables -  This is one of the final model
#all multicollinearity was deducted and removed with the help of vif function.
#Till now we considered the variable which has high vif  and p values are removed, 
#now consider only p value to remove the insignificant variables.
#from the model_14, enginetypedohcv has high p value of 0.688541,
#lets remove the enginetypedohcv from the previous model
model_15 <- lm(price ~ aspiration + enginelocation + cylindernumber + boreratio + compressionratio + carbodyhardtop + carbodyhatchback + carbodywagon + enginetypel + 
                 enginetypeohcf + enginetyperotor + Companybmw + Companybuick + 
                 Companychevrolet + Companydodge + Companyisuzu + Companymazda + 
                 Companymitsubishi + Companynissan + Companyplymouth + Companyrenault + 
                 Companytoyota + Companyvolkswagen, data = price_car)

summary(model_15)
vif(model_15)
#after removing the enginetypedohcv from the model_14, adjusted rsquare increases from 0.8793 to 0.8798



#from the model_15, carbodywagon has high p value of 0.618468,
#lets remove the carbodywagon from the previous model
model_16 <- lm(price ~ aspiration + enginelocation + cylindernumber + boreratio + compressionratio + carbodyhardtop + carbodyhatchback + enginetypel + 
                 enginetypeohcf + enginetyperotor + Companybmw + Companybuick + 
                 Companychevrolet + Companydodge + Companyisuzu + Companymazda + 
                 Companymitsubishi + Companynissan + Companyplymouth + Companyrenault + 
                 Companytoyota + Companyvolkswagen, data = price_car)

summary(model_16)
vif(model_16)
#after removing the carbodywagon from the model_15, adjusted rsquare increases from 0.8798 to 0.8803



#from the model_16, carbodyhardtop has high p value of 0.544754,
#lets remove the carbodyhardtop from the previous model
model_17 <- lm(price ~ aspiration + enginelocation + cylindernumber + boreratio + compressionratio + carbodyhatchback + enginetypel + 
                 enginetypeohcf + enginetyperotor + Companybmw + Companybuick + 
                 Companychevrolet + Companydodge + Companyisuzu + Companymazda + 
                 Companymitsubishi + Companynissan + Companyplymouth + Companyrenault + 
                 Companytoyota + Companyvolkswagen, data = price_car)

summary(model_17)
vif(model_17)
#after removing the carbodyhardtop from the model_16, adjusted rsquare increases from 0.8803 to 0.8807

#from the model_17, enginetypel has high p value of 0.356290,
#lets remove the enginetypel from the previous model
model_18 <- lm(price ~ aspiration + enginelocation + cylindernumber + boreratio + compressionratio + carbodyhatchback + 
                 enginetypeohcf + enginetyperotor + Companybmw + Companybuick + 
                 Companychevrolet + Companydodge + Companyisuzu + Companymazda + 
                 Companymitsubishi + Companynissan + Companyplymouth + Companyrenault + 
                 Companytoyota + Companyvolkswagen, data = price_car)

summary(model_18)
vif(model_18)
#after removing the enginetypel from the model_17, adjusted rsquare increases from 0.8807 to 0.8808


#from the model_18, compressionratio has high p value of 0.200736,
#lets remove the compressionratio from the previous model
model_19 <- lm(price ~ aspiration + enginelocation + cylindernumber + boreratio + carbodyhatchback + 
                 enginetypeohcf + enginetyperotor + Companybmw + Companybuick + 
                 Companychevrolet + Companydodge + Companyisuzu + Companymazda + 
                 Companymitsubishi + Companynissan + Companyplymouth + Companyrenault + 
                 Companytoyota + Companyvolkswagen, data = price_car)

summary(model_19)
vif(model_19)
#after removing the compressionratio from the model_18, adjusted rsquare little decreases from 0.8808 to 0.8804 

#from the model_19, carbodyhatchback has high p value of 0.217082,
#lets remove the carbodyhatchback from the previous model
model_20 <- lm(price ~ aspiration + enginelocation + cylindernumber + boreratio +
                 enginetypeohcf + enginetyperotor + Companybmw + Companybuick + 
                 Companychevrolet + Companydodge + Companyisuzu + Companymazda + 
                 Companymitsubishi + Companynissan + Companyplymouth + Companyrenault + 
                 Companytoyota + Companyvolkswagen, data = price_car)

summary(model_20)
vif(model_20)
#after removing the carbodyhatchback from the model_19, adjusted rsquare little decreases from 0.8804 to 0.8801

#from the model_20, Companychevrolet has high p value of 0.106923,
#lets remove the Companychevrolet from the previous model
model_21 <- lm(price ~ aspiration + enginelocation + cylindernumber + boreratio +
                 enginetypeohcf + enginetyperotor + Companybmw + Companybuick + 
                 Companydodge + Companyisuzu + Companymazda + 
                 Companymitsubishi + Companynissan + Companyplymouth + Companyrenault + 
                 Companytoyota + Companyvolkswagen, data = price_car)

summary(model_21)
vif(model_21)
#after removing the Companychevrolet from the model_20, adjusted rsquare little decreases from 0.8801 to 0.879


#from the model_21, Companyvolkswagen has high p value of 0.078943,
#lets remove the Companyvolkswagen from the previous model
model_22 <- lm(price ~ aspiration + enginelocation + cylindernumber + boreratio +
                 enginetypeohcf + enginetyperotor + Companybmw + Companybuick + 
                 Companydodge + Companyisuzu + Companymazda + 
                 Companymitsubishi + Companynissan + Companyplymouth + Companyrenault + 
                 Companytoyota , data = price_car)

summary(model_22)
vif(model_22)
#after removing the Companyvolkswagen from the model_21, adjusted rsquare little decreases from 0.879 to 


#from the model_22, Companyisuzu has high p value of 0.10312,
#lets remove the Companyisuzu from the previous model
model_23 <- lm(price ~ aspiration + enginelocation + cylindernumber + boreratio +
                 enginetypeohcf + enginetyperotor + Companybmw + Companybuick + 
                 Companydodge + Companymazda + 
                 Companymitsubishi + Companynissan + Companyplymouth + Companyrenault + 
                 Companytoyota , data = price_car)

summary(model_23)
vif(model_23)
#after removing the Companyisuzu from the model_22, adjusted rsquare little decreases from 0.8777 to 0.8766

#Now, we get the model with only significant variables. In this significant variables some of the variables
#are less significant. In order to make the model to strong, lets remove the less significant variables

#from the model_23, Companyrenault has high p value of 0.049975,
#lets remove the Companyrenault from the previous model
model_24 <- lm(price ~ aspiration + enginelocation + cylindernumber + boreratio +
                 enginetypeohcf + enginetyperotor + Companybmw + Companybuick + 
                 Companydodge + Companymazda + 
                 Companymitsubishi + Companynissan + Companyplymouth +
                 Companytoyota , data = price_car)

summary(model_24)
vif(model_24)
#after removing the Companyrenault from the model_23, adjusted rsquare little decreases from 0.8766 to  0.8747


#from the model_24, Companymazda has high p value of 0.029303,
#lets remove the Companymazda from the previous model
model_25 <- lm(price ~ aspiration + enginelocation + cylindernumber + boreratio +
                 enginetypeohcf + enginetyperotor + Companybmw + Companybuick + 
                 Companydodge + Companymitsubishi + Companynissan + Companyplymouth +
                 Companytoyota , data = price_car)

summary(model_25)
vif(model_25)
#after removing the Companymazda from the model_24, adjusted rsquare little decreases from 0.8747 to  0.8722


#from the model_25, Companyplymouth has high p value of 0.015678,
#lets remove the Companyplymouth from the previous model
model_26 <- lm(price ~ aspiration + enginelocation + cylindernumber + boreratio +
                 enginetypeohcf + enginetyperotor + Companybmw + Companybuick + 
                 Companydodge + Companymitsubishi + Companynissan + Companytoyota , data = price_car)

summary(model_26)
vif(model_26)
#after removing the Companyplymouth from the model_25, adjusted rsquare little decreases from 0.8722 to 0.8689 


#from the model_26, Companydodge has high p value of 0.018463,
#lets remove the Companydodge from the previous model
model_27 <- lm(price ~ aspiration + enginelocation + cylindernumber + boreratio +
                 enginetypeohcf + enginetyperotor + Companybmw + Companybuick + 
                 Companymitsubishi + Companynissan + Companytoyota , data = price_car)

summary(model_27)
vif(model_27)
#after removing the Companydodge from the model_26, adjusted rsquare little decreases from 0.8689 to 0.8657 

#from the model_27, Companymitsubishi has high p value of 0.002619,
#lets remove the Companymitsubishi from the previous model
model_28 <- lm(price ~ aspiration + enginelocation + cylindernumber + boreratio +
                 enginetypeohcf + enginetyperotor + Companybmw + Companybuick + 
                 Companynissan + Companytoyota , data = price_car)

summary(model_28)
vif(model_28)
#after removing the Companymitsubishi from the model_27, adjusted rsquare little decreases from 0.8657 to 0.86 

#from the model_28, Companytoyota has high p value of 0.001206,
#lets remove the Companytoyota from the previous model
model_29 <- lm(price ~ aspiration + enginelocation + cylindernumber + boreratio +
                 enginetypeohcf + enginetyperotor + Companybmw + Companybuick + 
                 Companynissan, data = price_car)

summary(model_29)
vif(model_29)
#after removing the Companytoyota from the model_28, adjusted rsquare little decreases from 0.86 to 0.853 


#from the model_29, Companynissan has high p value of 0.001206,
#lets remove the Companynissan from the previous model
model_30 <- lm(price ~ aspiration + enginelocation + cylindernumber + boreratio +
                 enginetypeohcf + enginetyperotor + Companybmw + Companybuick, data = price_car)

summary(model_30)
vif(model_30)
#after removing the Companynissan from the model_29, adjusted rsquare little decreases from 0.853 to 0.8463

#Model 30 has low vif and p value and which shows strong model compare to all other models.

#predicting the results in test dataset
Predict_1 <- predict(model_30,test[,-20])
test$test_price <- Predict_1


# Now, we need to test the r square between actual and predicted sales. 
r <- cor(test$price,test$test_price)
rsquared <- cor(test$price,test$test_price)^2
rsquared



#variables are removed based on the high VIF and p value
#In the training dataset, Adjusted r square value of final model 30 is 0.8463
#In the Test dataset, Adjusted r square value is 0.80626
#By comparing actual vs predicted, model predicted 80% authentication of actual price.
#Variables which  contributed to the model are
#aspiration
#enginelocation
#cylindernumber
#boreratio
#enginetypeohcf
#enginetyperotor
#companybmw
#companybuick



