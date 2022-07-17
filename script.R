library(ggplot2)

dtypes <- c('integer', 'integer', 'factor', 'integer', 'numeric', 'integer', 'integer', rep('factor', 6))


setwd
df <- read.csv('cardiovascular_data.csv', sep=";", colClasses = dtypes)
df$id <- NULL
df$gender <- as.factor(ifelse(df$gender == 1, 'Female', 'Male'))
df$disease <- as.factor(ifelse(df$cardio == 0, 'No', 'Yes'))
df$gluc <-factor(df$gluc, labels = c('Normal', 'Above normal', 'Well above normal'))
df$cholesterol <-factor(df$cholesterol, labels = c('Normal', 'Above normal', 'Well above normal'))
df$smoke<-factor(df$smoke, labels = c('No', 'Yes'))
df$alco<-factor(df$alco, labels = c('No', 'Yes'))
df$active<-factor(df$active, labels = c('No', 'Yes'))
df$cardio <- NULL

df$age <- round(df$age/365)

#remove height outliars
# df <- df[!df$height %in% boxplot.stats(df$height)$out, ]
# df <- df[!df$weight %in% boxplot.stats(df$weight)$out, ]

#remove rows with age less than 40
df <- df[df$age > 39, ]

# remove rows with height larger then 240 cm and weight larger than 180 kg (outliars)
df <- df[df$height < 240 & df$height > 130, ]
df <- df[df$weight > 30 & df$weight < 160, ]

# create bmi variable
df$BMI <- (df$weight/(df$height * df$height)) * 10000

# remove rows with BMI less than 5 and larger than 60
df <- df[df$BMI > 5 & df$BMI <= 50, ]

# divide BMI into categories
df$BMI_categorical <- ifelse(df$BMI < 18.5, 'Underweight', 
                             ifelse(df$BMI >= 18.5 & df$BMI <25, 'Normal', 
                                    ifelse(df$BMI >= 25 & df$BMI <30, 'Overweight', 
                                           ifelse(df$BMI >= 30 & df$BMI <35, 'Obese', 'Extremly obese'))))

df$BMI_categorical <- factor(df$BMI_categorical, levels= 
                                  c('Underweight', 'Normal', 'Overweight', 'Obese', 'Extremly obese'),
                                ordered =TRUE)

# divide age into categories
df$age_categorical <- ifelse(df$age >= 40 & df$age < 45, '40-44',
                             ifelse(df$age >= 45 & df$age < 50, '45-49',
                                    ifelse(df$age>= 50 & df$age < 54, '50-54',
                                           ifelse(df$age>=55 & df$age < 59, '55-59', '60-65'))))

df$age_categorical <- as.factor(df$age_categorical)

#remove rows with abnormal blood presure 
df <- df[df$ap_hi >= 80 & df$ap_hi <= 220,]
df <- df[df$ap_lo >= 40 & df$ap_lo <= 160,]

str(df)

summary(df)

head(df)
tail(df)

hist(df$age)

summary(df$age)

# histogram of age
ggplot(df, aes(x=age, fill=gender)) + 
  geom_histogram(breaks=seq(40, 65, 2.5))

# gender proportion over ages
ggplot(data = df, aes(x = age_categorical)) +
  geom_bar(aes(fill = gender), position = "fill") +
  labs(title = "Gender proportions over ages", x = "Age", y = "Proportion of genders") +
  scale_y_continuous(breaks = seq(0,1,0.1))+
  geom_abline(intercept = 0.3496, slope=0)+
  theme(plot.title = element_text(hjust = 0.5),
      text = element_text(size = 20))

# disease proportion over ages
ggplot(data = df, aes(x = age_categorical)) +
  geom_bar(aes(fill = disease), position = "fill") +
  labs(title = "Disease proportion over ages", x = "Age", y = "Proportion of genders") +
  scale_y_continuous(breaks = seq(0,1,0.1))+
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size = 20))


# boxplot of height
ggplot(df, aes(x=height, y= gender, color=gender)) + 
  geom_boxplot()

# boxplot of weight
ggplot(df, aes(x=weight,  color=disease)) + 
  geom_boxplot()

# weight vs height
ggplot(df, aes(x=weight, y= height, color=disease)) + 
  geom_point(size = 2)

# BMI histogram
summary(df$BMI)

ggplot(df, aes(x=BMI, fill="red") ) + 
  geom_density()


# diseased patient over BMI categories
ggplot(data = df, aes(x = BMI_categorical)) +
  geom_bar(aes(fill = disease), position = "fill") +
  labs(title = "Gender proportions over ages", x = "Age", y = "Proportion of genders") +
  scale_y_continuous(breaks = seq(0,1,0.1))+
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size = 20))


# Systolic vs Diastolic blood pressure, shows correlation
ggplot(df, aes(x=ap_hi, y= ap_lo)) + 
  geom_point(size = 3)

#
ggplot(df, aes(x=BMI, y= age)) + 
  geom_point(size = 3)
ggplot(df, aes(x=ap_hi, y= ap_lo, color=disease)) + 
  geom_point(size = 2)

ggplot(df, aes(x=height, y= weight, color=BMI_categorical)) + 
  geom_point(size = 2)




# diseased patient - gluc
ggplot(data = df, aes(x = gluc)) +
  geom_bar(aes(fill = disease), position = "fill") +
  labs(title = "Gender proportions over ages", x = "Age", y = "Proportion of genders") +
  scale_y_continuous(breaks = seq(0,1,0.1))+
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size = 20))

# diseased patient - cholesterol
ggplot(data = df, aes(x = cholesterol)) +
  geom_bar(aes(fill = disease), position = "fill") +
  labs(title = "Disease/cholesterol Proportions", x = "Cholesterol", y = "Proportion of diseased") +
  scale_y_continuous(breaks = seq(0,1,0.1))+
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size = 20))
# diseased patients - cholesterol #2 
ggplot(data = df, aes(x = cholesterol)) +
  geom_bar(aes(fill = disease)) +
  labs(title = "Disease/cholesterol", x = "Cholesterol", y = "Count") +
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size = 20))

# diseased patient - smoke
ggplot(data = df, aes(x = smoke)) +
  geom_bar(aes(fill = disease), position = "fill") +
  labs(title = "Gender proportions over ages", x = "Smoke", y = "Proportion of genders") +
  scale_y_continuous(breaks = seq(0,1,0.1))+
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size = 20))

# diseased patient - alco
ggplot(data = df, aes(x = alco)) +
  geom_bar(aes(fill = disease), position = "fill") +
  labs(title = " ", x = "Alcohol", y = "Proportion of diseased patients ") +
  scale_y_continuous(breaks = seq(0,1,0.1))+
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size = 15))

# diseased patient - active
ggplot(data = df, aes(x = active)) +
  geom_bar(aes(fill = disease), position = "fill") +
  labs(title = "Physical activity/diseased Proportions", x = "Physical activity", y = "Proportion of diseased patients ") +
  scale_y_continuous(breaks = seq(0,1,0.1))+
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size = 15))





