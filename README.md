---
title: "recoding_marketing"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:

1, White | 2, Black or African American | 3, American Indian or Alaska Native | 4, Asian | 5, Native Hawaiian or Other Pacific Islander | 6, Multiracial | 7, Another racial identity | 8, Prefer not to respond



```{r}
library(lubridate)
library(dplyr)
library(prettyR)
setwd("T:/CRI_Research/telehealth_evaluation/data_codebooks/satisfaction")
marking_download_5_30_20 = read.csv("TelehealthZoomclient_DATA_LABELS_2020-06-05_1436.csv", header = TRUE)
marking_download_5_30_20

Site_id = rep(NA, dim(marking_download_5_30_20)[1])
Entity_id =  rep(NA, dim(marking_download_5_30_20)[1])
Survey_ID = rep(NA, dim(marking_download_5_30_20)[1])
mention_time = marking_download_5_30_20$Survey.Timestamp


Patient_Email = marking_download_5_30_20$Email
Patient_First_Name = marking_download_5_30_20$First.name
Patient_Last_Name = marking_download_5_30_20$Last.name
Patient_Phone = marking_download_5_30_20$Phone.number
Please_specify_which_state = marking_download_5_30_20$Please.list.the.state.
What_is_your_age = marking_download_5_30_20$What.is.your.age.
What_is_your_gender_identity = marking_download_5_30_20$What.is.your.gender.identity.
What_is_your_racial_identity = marking_download_5_30_20$What.is.your.racial.identity..
Which_state_do_you_currently_live_in = marking_download_5_30_20$Which.state.do.you.currently.live.in.

Are_you_a_new_or_returning_client = marking_download_5_30_20$Are.you.a.new.or.returning.client.
How_would_you_rate_your_overall_experience_with_telehealth_at_Centerstone = marking_download_5_30_20$How.would.you.rate.your.overall.experience.with.telehealth.at.Centerstone.

May_we_follow_up_with_you_about_your_feedback_for_additional_detail = marking_download_5_30_20$May.we.follow.up.with.you.about.your.feedback.for.additional.detail.....
Were_the_instructions_for_how_to_access_your_appointment_online_clear = marking_download_5_30_20$Were.the.instructions.for.how.to.access.your.appointment.online.clear.
Were_you_satisfied_with_how_quickly_you_got_an_appointment = marking_download_5_30_20$Were.you.satisfied.with.how.quickly.you.got.an.appointment.
Would_you_recommend_Centerstone_to_your_family_and_friends = marking_download_5_30_20$Would.you.recommend.Centerstone.s.telehealth.services.to.your.Family.and.Friends.
  

marking_download_5_30_20 = data.frame(Site_id,Entity_id,Survey_ID, mention_time,  Patient_Email, Patient_First_Name, Patient_Last_Name, Patient_Phone, Please_specify_which_state, What_is_your_age, What_is_your_gender_identity, What_is_your_racial_identity, Which_state_do_you_currently_live_in, barriers = marking_download_5_30_20$For.you.personally..what.are.the.barriers.of.using.telehealth.relative.to.in.person.services., benefits = marking_download_5_30_20$For.you.personally..what.are.the.benefits.of.using.telehealth.services.relative.to.in.person.services., Are_you_a_new_or_returning_client, How_would_you_rate_your_overall_experience_with_telehealth_at_Centerstone, May_we_follow_up_with_you_about_your_feedback_for_additional_detail, substance = marking_download_5_30_20$The.use.of.technology.accessed.through.Centerstone.has.helped.me.reduce.my.substance.use.,communicate =  marking_download_5_30_20$The.use.of.technology.accessed.through.Centerstone.has.helped.me.communicate.with.my.provider., manage = marking_download_5_30_20$The.use.of.technology.accessed.through.Centerstone.has.helped.me.manage.my.mental.health.symptoms., recover = marking_download_5_30_20$The.use.of.technology.accessed.through.Centerstone.has.helped.me.support.my.mental.health.recovery., Were_the_instructions_for_how_to_access_your_appointment_online_clear, Were_you_satisfied_with_how_quickly_you_got_an_appointment, Would_you_recommend_Centerstone_to_your_family_and_friends)
marking_download_5_30_20


marking_download_5_30_20 = subset(marking_download_5_30_20, mention_time != "[not completed]")
marking_download_5_30_20

mention_time_fill = list()

for(i in 1:dim(marking_download_5_30_20)[1]){
 mention_time_fill[[i]] =  strsplit(marking_download_5_30_20$mention_time[[i]]," ")[[1]][1]
}
mention_time_fill = unlist(mention_time_fill)
mention_time_fill = ymd(mention_time_fill)
marking_download_5_30_20$mention_time = mention_time_fill
marking_download_5_30_20 = subset(marking_download_5_30_20, mention_time <= "2020-05-31")

typeof(marking_download_5_30_20$mention_time)
typeof(df)

library(stringr)
mention_time_dat_frame = str_split_fixed(marking_download_5_30_20$mention_time, "-", 3)
colnames(mention_time_dat_frame) = c("Year", "Month", "Day") 
mention_time_dat_frame
mention_time_dat_frame = data.frame(mention_time_dat_frame)
marking_download_5_30_20$mention_time = paste0(mention_time_dat_frame$Day, "-", mention_time_dat_frame$Month, "-", mention_time_dat_frame$Year)
marking_download_5_30_20$mention_time = as.factor(marking_download_5_30_20$mention_time)
marking_download_5_30_20$How_would_you_rate_your_overall_experience_with_telehealth_at_Centerstone

marking_download_5_30_20$How_would_you_rate_your_overall_experience_with_telehealth_at_Centerstone = gsub("\\D", "", marking_download_5_30_20$How_would_you_rate_your_overall_experience_with_telehealth_at_Centerstone)
marking_download_5_30_20$How_would_you_rate_your_overall_experience_with_telehealth_at_Centerstone  = as.numeric(marking_download_5_30_20$How_would_you_rate_your_overall_experience_with_telehealth_at_Centerstone)


colnames(marking_download_5_30_20) = c("Site_id", "Entity_id", "Survey_ID", "mention time", "Patient Email", "Patient First_Name", "Patient Last Name", "Patient Phone", "Please specify which state:", "What is your age?", "What is your gender identity?", "What is your racial identity?", "Which state do you currently live in?", "For you personally what are the barriers of using telehealth relative to in-person services?", "For you personally what are the benefits of using telehealth services relative to in-person services?", "Are you a new or returning client?", "How would you rate your overall experience with telehealth at Centerstone?", "May we follow up with you about your feedback for additional detail?", "The use of technology accessed through CenterstoneÂ has helped me reduce my substance use.", "The use of technology accessed throughÂ CenterstoneÂ has helped me communicate with my provider.", "The use of technology accessed throughÂ CenterstoneÂ has helped me manage my mental health symptoms.", "The use of technology accessed throughÂ CenterstoneÂ has helped me support my mental health recovery.", "Were the instructions for how to access your appointment online clear?", "Were you satisfied with how quickly you got an appointment?", "Would you recommend Centerstone to your family and friends?")
marking_download_5_30_20


write.csv(marking_download_5_30_20, "marking_download_5_30_20.csv", row.names = FALSE)
```

