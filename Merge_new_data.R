COVID_2<-read.csv("COVID19_14-Apr.csv")

COVID_2<-COVID_2 %>% select(-c("X"))

New_COVID<-read.csv("04-15-2020.csv")

New_COVID <- New_COVID %>% select(-c("FIPS","Admin2","Last_Update","Active","Combined_Key"))

New_COVID <- New_COVID %>% rename("Province.State"="Province_State",
                                  "Country.Region"="Country_Region",
                                  "Long"="Long_")

New_COVID$Date<-"4/15/20"

Update_COVID<-rbind(COVID_2,New_COVID)

write.csv(Update_COVID,"COVID19_15-Apr.csv")