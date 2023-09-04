#cutting the data by weeks e.g. week2 for the data for Monday 25/4/2022 for signal detection

subdata1 <- flu_data[grepl("2022-04-18", flu_data$SURVEY_RESPONSE_TIMESTAMP),]
subdata2 <- flu_data[grepl("2022-04-19", flu_data$SURVEY_RESPONSE_TIMESTAMP),]
subdata3 <- flu_data[grepl("2022-04-20", flu_data$SURVEY_RESPONSE_TIMESTAMP),]
subdata4 <- flu_data[grepl("2022-04-21", flu_data$SURVEY_RESPONSE_TIMESTAMP),]
subdata5 <- flu_data[grepl("2022-04-22", flu_data$SURVEY_RESPONSE_TIMESTAMP),]
subdata6 <- flu_data[grepl("2022-04-23", flu_data$SURVEY_RESPONSE_TIMESTAMP),]
subdata7 <- flu_data[grepl("2022-04-24", flu_data$SURVEY_RESPONSE_TIMESTAMP),]

week2 <- rbind(subdata1, subdata2, subdata3, subdata4, subdata5, subdata6, subdata7)

#filtering by PREGNANT==1 and MA==1
week2_pregnant <- subset(week2, week2$PREGNANT==1)
#to obtain the numbers of the flu vax's for RECORD
table(week2_pregnant$VAX_LIST)
#to obtain the number of MA for that week
week2_pregnant_MA <- subset(week2_pregnant, week2_pregnant$MA==1)

