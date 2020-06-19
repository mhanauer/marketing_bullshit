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

1, Telehealth video | 2, Telephone | 3, In-person

```{r}
library(lubridate)
library(dplyr)
library(prettyR)
setwd("T:/CRI_Research/telehealth_evaluation/data_codebooks/satisfaction")
zoom_download = read.csv("TelehealthZoomclient_DATA_LABELS_2020-06-19_0613.csv", header = TRUE)
zoom_download$platform = rep("zoom", dim(zoom_download)[1])

snap_download = read.csv("TelehealthSnapMDclie_DATA_LABELS_2020-06-19_0614.csv", header = TRUE)
snap_download$platform = rep("snap", dim(snap_download)[1])

colnames(snap_download)[4] = "In.the.Zoom.session.you.just.completed.with.your.Centerstone.provider..did.you.use.audio.only.or.audio.and.video."
colnames(snap_download)[26] = "May.we.follow.up.with.you.about.your.feedback.for.additional.detail....."
### Need to put SnapMD dates into the correct order
snap_download$Survey.Timestamp


snap_mention_time_fill = list()

for(i in 1:dim(snap_download)[1]){
 snap_mention_time_fill[[i]] =  strsplit(snap_download$Survey.Timestamp[[i]]," ")[[1]][1]
}
snap_mention_time_fill = unlist(snap_mention_time_fill)
snap_mention_time_fill = ymd(snap_mention_time_fill)
snap_download$Survey.Timestamp = snap_mention_time_fill
### Now fix for Zoom


zoom_mention_time_fill = list()

for(i in 1:dim(zoom_download)[1]){
 zoom_mention_time_fill[[i]] =  strsplit(zoom_download$Survey.Timestamp[[i]]," ")[[1]][1]
}
zoom_mention_time_fill = unlist(zoom_mention_time_fill)
zoom_mention_time_fill = ymd(zoom_mention_time_fill)
zoom_download$Survey.Timestamp = zoom_mention_time_fill


### Now combine
marking_download = rbind(zoom_download, snap_download)
marking_download$Survey.Timestamp
describe.factor(marking_download$platform)


Site_id = rep(NA, dim(marking_download)[1])
Entity_id =  rep(NA, dim(marking_download)[1])
Survey_ID = rep(NA, dim(marking_download)[1])
mention_time = marking_download$Survey.Timestamp


Patient_Email = marking_download$Email
Patient_First_Name = marking_download$First.name
Patient_Last_Name = marking_download$Last.name
Patient_Phone = marking_download$Phone.number
Please_specify_which_state = marking_download$Please.list.the.state.
What_is_your_age = marking_download$What.is.your.age.
What_is_your_gender_identity = marking_download$What.is.your.gender.identity.
What_is_your_racial_identity = marking_download$What.is.your.racial.identity..
Which_state_do_you_currently_live_in = marking_download$Which.state.do.you.currently.live.in.

Are_you_a_new_or_returning_client = marking_download$Are.you.a.new.or.returning.client.
How_would_you_rate_your_overall_experience_with_telehealth_at_Centerstone = marking_download$How.would.you.rate.your.overall.experience.with.telehealth.at.Centerstone.

May_we_follow_up_with_you_about_your_feedback_for_additional_detail = marking_download$May.we.follow.up.with.you.about.your.feedback.for.additional.detail.....
Were_the_instructions_for_how_to_access_your_appointment_online_clear = marking_download$Were.the.instructions.for.how.to.access.your.appointment.online.clear.
Were_you_satisfied_with_how_quickly_you_got_an_appointment = marking_download$Were.you.satisfied.with.how.quickly.you.got.an.appointment.
Would_you_recommend_Centerstone_to_your_family_and_friends = marking_download$Would.you.recommend.Centerstone.s.telehealth.services.to.your.Family.and.Friends.

#In the future how would you prefer to receive services from Centerstone? (Select all those that apply)
#1, Telehealth video | 2, Telephone | 3, In-person
televideo = marking_download$In.the.future..how.would.you.prefer.to.receive.services.from.Centerstone...select.all.that.apply...choice.Telehealth.video.
televideo = dplyr::recode(televideo, "Checked" = "Telehealth video", "Unchecked" = "0")

telephone = marking_download$In.the.future..how.would.you.prefer.to.receive.services.from.Centerstone...select.all.that.apply...choice.Telephone.
telephone = dplyr::recode(telephone, "Checked"= "Telephone", "Unchecked" = "0")

in_person = marking_download$In.the.future..how.would.you.prefer.to.receive.services.from.Centerstone...select.all.that.apply...choice.In.person.
in_person = dplyr::recode(in_person, "Checked"= "In-person", "Unchecked" = "0")

prefer = paste0(televideo, ",", telephone, ",", in_person)
prefer = gsub("0", "", prefer)
prefer = gsub(",,", "",prefer)


marking_download = data.frame(Site_id,Entity_id,Survey_ID, mention_time,  Patient_Email, Patient_First_Name, Patient_Last_Name, Patient_Phone, Please_specify_which_state, What_is_your_age, What_is_your_gender_identity, What_is_your_racial_identity, Which_state_do_you_currently_live_in, barriers = marking_download$For.you.personally..what.are.the.barriers.of.using.telehealth.relative.to.in.person.services., benefits = marking_download$For.you.personally..what.are.the.benefits.of.using.telehealth.services.relative.to.in.person.services., Are_you_a_new_or_returning_client, How_would_you_rate_your_overall_experience_with_telehealth_at_Centerstone, prefer, May_we_follow_up_with_you_about_your_feedback_for_additional_detail, substance = marking_download$The.use.of.technology.accessed.through.Centerstone.has.helped.me.reduce.my.substance.use.,communicate =  marking_download$The.use.of.technology.accessed.through.Centerstone.has.helped.me.communicate.with.my.provider., manage = marking_download$The.use.of.technology.accessed.through.Centerstone.has.helped.me.manage.my.mental.health.symptoms., recover = marking_download$The.use.of.technology.accessed.through.Centerstone.has.helped.me.support.my.mental.health.recovery., Were_the_instructions_for_how_to_access_your_appointment_online_clear, Were_you_satisfied_with_how_quickly_you_got_an_appointment, Would_you_recommend_Centerstone_to_your_family_and_friends, platform = marking_download$platform)
marking_download


describe.factor(marking_download$platform)


marking_download = subset(marking_download, mention_time <= "2020-06-18")


library(stringr)
mention_time_dat_frame = str_split_fixed(marking_download$mention_time, "-", 3)
tail(mention_time_dat_frame, 30)
colnames(mention_time_dat_frame) = c("Year", "Month", "Day") 
tail(mention_time_dat_frame,20)
mention_time_dat_frame = data.frame(mention_time_dat_frame)
marking_download$mention_time = paste0(mention_time_dat_frame$Day, "-", mention_time_dat_frame$Month, "-", mention_time_dat_frame$Year)
#marking_download$mention_time = as.factor(marking_download$mention_time)


marking_download$How_would_you_rate_your_overall_experience_with_telehealth_at_Centerstone = gsub("\\D", "", marking_download$How_would_you_rate_your_overall_experience_with_telehealth_at_Centerstone)
marking_download$How_would_you_rate_your_overall_experience_with_telehealth_at_Centerstone  = as.numeric(marking_download$How_would_you_rate_your_overall_experience_with_telehealth_at_Centerstone)



colnames(marking_download) = c("Site_id", "Entity_id", "Survey_ID", "mention time", "Patient Email", "Patient First_Name", "Patient Last Name", "Patient Phone", "Please specify which state:", "What is your age?", "What is your gender identity?", "What is your racial identity?", "Which state do you currently live in?", "For you personally what are the barriers of using telehealth relative to in-person services?", "For you personally what are the benefits of using telehealth services relative to in-person services?", "Are you a new or returning client?", "How would you rate your overall experience with telehealth at Centerstone?", "In the future how would you prefer to receive services from Centerstone? (Select all those that apply)", "May we follow up with you about your feedback for additional detail?", "The use of technology accessed through CenterstoneÂ has helped me reduce my substance use.", "The use of technology accessed throughÂ CenterstoneÂ has helped me communicate with my provider.", "The use of technology accessed throughÂ CenterstoneÂ has helped me manage my mental health symptoms.", "The use of technology accessed throughÂ CenterstoneÂ has helped me support my mental health recovery.", "Were the instructions for how to access your appointment online clear?", "Were you satisfied with how quickly you got an appointment?", "Would you recommend Centerstone to your family and friends?", "platform")
describe.factor(marking_download$platform)

### Add quates back fro mention time so excel reads it differently
marking_download$`mention time` = paste0("'",marking_download$`mention time`,"'")


write.csv(marking_download, "marking_download_6_18_20.csv", row.names = FALSE)
```

