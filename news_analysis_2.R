library(tidyverse)
news_data <- read_csv("~/Downloads/news-consumption-survey-2019-respondent-level-csv-data.csv", locale = locale(encoding = "Latin1"))

#Demographics

colnames(news_data) <- gsub("<[^>]+>", "",colnames(news_data))
colnames(news_data) <- gsub("\xfc\xbe\x8d\x96\x90\xbc","", colnames(news_data))
news_data[news_data=="#NULL!"] <- NA

names <- as.data.frame(colnames(news_data)) %>%
  rename(Variable = `colnames(news_data)`) %>%
  mutate(Type = as.character(NA))

names[1,2] <- "ID"
names[2:11,2] <- "Demographic"
names[12:23,2] <- "Interests"
names[24:28,2] <- "Internet use"
names[25:42,2] <- "Television reception"
names[44:54,2] <- "Devices"
names[55,2] <- "Smartphone"
names[56:66,2] <- "News platforms"
names[67:76, 2] <- "Internet news sources"
names[78:80, 2] <- "Social media news sources"
names[81:91, 2] <- "News sources"
names[92:100,2] <- "First news source"
names[101:127,2] <- "Television"
names[129:153,2] <- "Television news"
names[154:180,2] <- "Newspapers news"
names[207:227,2] <- "Wewspapers"
names[182:206,2] <- "Daily newspapers news"
names[230:248,2] <- "Weekly newspapers news"
names[249:264,2] <- "Magazines"
names[265:273,2] <- "Magazines news"
names[274:314,2] <- "Radio"
names[315:348,2] <- "Radio news"
names[349:362,2] <- "Social media"
names[349:362,2] <- "Social media news sources"
names[363:373,2] <- "Social media news"
names[374:428,2] <- "Other internet sources"
names[429:436,2] <- "Other"
names[437:489,2] <- "Website news"
names[490:498,2] <- "Facebook behaviour"
names[500:502,2] <- "Facebook contact news source"
names[503:530,2] <- "Facebook news online behaviour"
names[531:575,2] <- "News sources followed on Facebook"
names[592:593,2] <- "Know Facebook news source"
names[594:602,2] <- "Instagram behaviour"
names[604:606,2] <- "Instgram contact news source"
names[607:634,2] <- "Instgram news online behaviour"
names[635:679,2] <- "News sources followed on Instagram"
names[906:928, 2] <- "Interacting with news online"
names[930:943, 2] <- "News you use - most important"
names[944:1081,2] <- "Importance as a source of news personally"
names[1082:1095,2] <- "View on different media sources"
names[1122: 1439,2] <- "View on different media sources"
names[1466:1877,2] <- "View on different media sources"
names[1878:1892,2] <- "Sources of international news"
names[1894:1907,2] <- "Satisfaction with international news"
names[1925:1938,2] <- "Satisfaction with national news"
names[1939:2117,2] <- "Source of regional news"
names[2119:2295,2] <- "Typical frequency of use"
names[2296:2314,2] <- "Sources of local news"
names[2316:2332,2] <- "Satisfaction with local news"
names[2333:2349,2] <- "Reasons for following the news"
names[2447:2450,2] <- "Demographics"
names[2456,2] <- "Demographic"
names[2458,2] <- "Demographic"
names[2459:2469,2] <- "Health"
names[2471:2472,2] <- "Demographic"
names[2474:2477,2] <- "Views on life"

demographics <- names %>% filter(Type=="Demographic")

##Okay, now separate the data

##1 DEMOGRAPHICS

thenames <- c("Gender","Age","Refused_Age","Age_Bands","Social Grade","TV Region","Urbanity","Ethnicity","Religion","Religion_Extra","Educational_level","Sex_change","Annual_household_income","Weekly_household_income")
demographic <- as.data.frame(news_data[as.character(demographics$Variable)])
colnames(demographic) <- thenames

##2 HEALTH
health <- names %>% filter(Type=="Health")
health <- as.data.frame(news_data[as.character(health$Variable)])
colnames(health) <- gsub("Which of these, if any, limit your daily activities or the work you can do?","",colnames(health))

health[health==0] <- NA
health[!is.na(health)] <- 1

#LIFE SATISFACTION
life_satisfaction <- names %>% filter(Type=="Views on life")
satisfaction <- as.data.frame(news_data[as.character(life_satisfaction$Variable)])
colnames(satisfaction) <- gsub(" Please use the scale below to indicate how much you agree or disagree with the following statements...................","", colnames(satisfaction))

#Rename the variables

satisfaction[satisfaction=="Strongly agree"]  <- 7
satisfaction[satisfaction=="Agree"]  <- 6
satisfaction[satisfaction=="Slightly agree"] <- 5
satisfaction[satisfaction=="Neither agree nor disagree"]  <- 4
satisfaction[satisfaction=="Slightly disagree"]  <- 3
satisfaction[satisfaction=="Disagree"]  <- 2
satisfaction[satisfaction=="Strongly disagree"]  <- 1
satisfaction[satisfaction=="Refused"] <- NA

satisfaction <- satisfaction %>%
  mutate_all(as.numeric)

##Finally, interests 

##Let's just put this into one variable

interests <- names %>% filter(Type=="Interests")
interests <- as.data.frame(news_data[as.character(interests$Variable)])
interests$ID <- news_data$`Serial Number`

interests <- interests %>%
  reshape2::melt(id.vars = "ID") %>%
  filter(value!="0") %>%
  select(-variable) %>%
  mutate(Total = 1) %>%
  spread(value, Total)

colnames(interests) <- paste0("Interest_",colnames(interests))

interests <- interests %>%
  rename(ID = Interest_ID)

#Great, now to merge together

##DATASET1: FULLDATA

fulldata <- cbind(demographic, health, satisfaction)

fulldata$ID <- news_data$`Serial Number`

fulldata <- fulldata %>%
  left_join(interests, by ="ID")

#Just some more data cleaning :)

fulldata <- fulldata %>%
  select(-Refused_Age, -Age_Bands) %>%
  select(ID, Gender:Interest_Sport)

fulldata <- fulldata %>%
  mutate(`TV Region` = gsub("BBC ", "", `TV Region`))

remove(interests, deographic, health, life_satisfaction, satisfaction, demographics, demographic)

##Now write a function to quickly melt and get all the data that we need

extract_data <- function(x) {
  data <- names %>% filter(Type=={{x}})
  data <- as.data.frame(news_data[as.character(data$Variable)])
  data <- data %>% mutate(ID = news_data$`Serial Number`) %>%
    reshape2::melt(id.vars="ID") %>%
    select(-variable) %>%
    filter(value!=0) %>%
    rename(data = value)
  return(data)
}

##A second function for the frequency variables

extract_data_2 <- function(x, y) {
  data <- names %>% filter(Type=={{x}})
  data <- as.data.frame(news_data[as.character(data$Variable)])
  data <- data %>% mutate(ID = news_data$`Serial Number`) %>%
    reshape2::melt(id.vars="ID") %>%
    mutate(variable = gsub(y,"", variable)) %>%
    rename(Source = variable,
           Frequency = value) %>%
    mutate(Type := !!x) %>%
    filter(!is.na(Frequency))
  return(data) }


##Extract the long

devices <- extract_data("Devices")
news_platforms <- extract_data("News platforms")
internet_news_sources <- extract_data("Internet news sources")
news_sources <- extract_data("News sources")

##NEWS FREQUENCY

#Extract frequency data and merge into one dataset
television_news_frequency <- extract_data_2("Television news","And typically how often do you watch the news on............")
newspaper_news_frequency <- extract_data_2("Daily newspapers news","And typically how often do you read the news in.......")
weekly_newspaper_news_frequency <- extract_data_2("Weekly newspapers news","And typically how often do you read the news in.......")
mazgainze_news_frequency <- extract_data_2("Magazines news","And typically how often do you read the news in............")
radio_news_frequency <- extract_data_2("Radio news","And typically how often do you listen to the news on...........")
socmedia_news_frequency <- extract_data_2("Social media news","And typically how often do you read/watch/listen to the news on .............")
news_websites_frequency <- extract_data_2("Website news","And typically how often do you use.............")
freq_data <- mget(ls(pattern = "frequency"), .GlobalEnv)
rm(list=ls(pattern = "frequency"))
freq_data <- bind_rows(freq_data)

##SATISFACTION
satisfaction_int <- extract_data_2("Satisfaction with international news","How satisfied are you with the quality of international news available from.........")
satisfaction_nat <- extract_data_2("Satisfaction with national news","How satisfied are you with the quality of national news available from.........")
satisfaction_local <- extract_data_2("Satisfaction with local news","How satisfied are you with the quality of the local news available from ..................")
sat_data <- mget(ls(pattern = "satisfaction"), .GlobalEnv)
rm(list=ls(pattern = "satisfaction"))
sat_data <- bind_rows(sat_data)

#View on different media sources
viewpoints <- extract_data_2("View on different media sources","And to what extent do you think the following statements apply to ")
viewpoints$Source <- gsub("as a news source?","",viewpoints$Source)
viewpoints$Source <- gsub("as a news source?","",viewpoints$Source)

viewpoints <- viewpoints %>%
  mutate(Source = gsub("\\.","",Source)) %>%
  separate(Source, into = c("News_source","Metric"), sep="\\?") %>%
  mutate(Frequency = case_when(
    Frequency=="10 = Completely" ~ "10",
    Frequency=="10=Completely" ~ "10",
    Frequency=="10= Completely" ~ "10",
    Frequency=="1 = Not at all" ~ "1",
    TRUE ~ Frequency
  ))

viewpoints <- viewpoints %>%
    mutate(Metric =  case_when(str_detect(Metric,'accurate') ~ "Accurate",
                               str_detect(Metric, 'trust') ~ "Trustworthy",
                               str_detect(Metric, 'quality') ~ "High quality",
                               str_detect(Metric, 'depth of an') ~ "Depth of news",
                               str_detect(Metric, 'journali') ~ "High calibre news contributors",
                               str_detect(Metric, 'imparti') ~ "Impartial",
           TRUE ~ Metric))


##WHERE DO YOU GO FIRST FOR DIFFERENT KINDS OF NEWS

first_news <- extract_data_2("First news source","Where do you tend to go first for")

##Typical frequency of use

typical_frequency <- extract_data_2("Typical frequency of use","And typically how often do you use.............")

