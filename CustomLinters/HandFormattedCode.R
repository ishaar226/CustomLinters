#  EducationBackground_Congress.R
#  Created on 2020-12-2
#  Created by Pia Deshpande

# This file will use the Vote Smart API to pull in the educational background 
# of members of Congress. In particular, we'll look at House members in the  
# years 2017-2020, and Senate members in the years 2015-2020.  
# After using the API, I will create two data frames 
# (one for senators and one for House members).

# All information except for IPEDs numbers I can get from the VoteSmart API.

#  Vote Smart API documentation is at http://api.votesmart.org/docs/.
library(pvsR)
library(dplyr)
library(tidyr)
library(tidyverse)  # Should not load tidyverse library

# API Key: needs to be an object
pvs.key = "815797a5eea3e9fa08919a3f1cafdd98"

# Notes to John:

# The API has missing information, both in biographical information, 
# and education information. 

# There were some education background errors. I corrected the ones I found, 
# but they were found because I thought it odd that people had two BAs. 
# I have no way to check through all the background.

# I do not have a way to get the IPEDs codes. There's no key or data table 
# on the NCES website with the ids. It may be worth mailing them to ask for 
# one. 

# I have made more specific notes throughout the file. 
# Feel free to read through them.



# **************************************************************************
# IDENTIFY CANDIDATES WE NEED ####
# **************************************************************************

# TO:DO use here:here notation
# Something is up with this API. 
# I wonder if this captures people who resigned midterm 
# and then were replaced. I worry about that.

# House:
House_2017_18 <- Candidates.getByOfficeState(
  stateId      = state.abb, 
  officeId     = 5, 
  electionYear = 2016) %>%
  filter(
    electionStatus == "Won", 
    electionStage %in% c("General", "General Runoff"))

House_2019_20 <- Candidates.getByOfficeState(
  stateId      = state.abb, 
  officeId     = 5, 
  electionYear = 2018) %>%
  filter(
    electionStatus == "Won", 
    electionStage %in% c("General", "General Runoff"))

Senate_2015_16 <- Candidates.getByOfficeState(
  stateId      = state.abb, 
  officeId     = 6, 
  electionYear = 2014) %>%
  filter(
    electionStatus == "Won", 
    electionStage %in% c("General", "General Runoff"))

# Senate
Senate_2017_18 <- Candidates.getByOfficeState(
  stateId      = state.abb, 
  officeId     = 6, 
  electionYear = 2016) %>%
  filter(
    electionStatus == "Won", 
    electionStage %in% c("General", "General Runoff"))
# Warning message:No data available for:  stateId=WY officeId=6 electionYear=2016. 
# The corresponding rows in the data frame are filled with NAs. 

Senate_2019_20 <- Candidates.getByOfficeState(
  stateId      = state.abb, 
  officeId     = 6, 
  electionYear = 2018) %>%
  filter(
    electionStatus == "Won", 
    electionStage %in% c("General", "General Runoff"))



# **************************************************************************
# API CALLS ####
# **************************************************************************

edu_House_2017_18 <- pvsR::CandidateBio.getDetailedBio(House_2017_18$candidateId)

edu_House_2019_20 <- pvsR::CandidateBio.getDetailedBio(House_2019_20$candidateId)

edu_Senate_2015_16 <- pvsR::CandidateBio.getDetailedBio(Senate_2015_16$candidateId)

edu_Senate_2017_18 <- pvsR::CandidateBio.getDetailedBio(Senate_2017_18$candidateId)

edu_Senate_2019_20 <- pvsR::CandidateBio.getDetailedBio(Senate_2019_20$candidateId)

# There are a good number of candidate Ids that return no information (just rows of NAs)



# **************************************************************************
# DATA TRANSFORMATIONS ####
# **************************************************************************

# I want to start by making four dataframes. Two are a list of all the  
# candidates and bio information for them (one for the Senate, one for 
# the House). Two are corresponding educational backgrounds. 

# House:
houseEdu115_raw <- edu_House_2017_18[["education"]]
houseCandidate115_raw <- edu_House_2017_18[["candidate"]]

houseCandidate115 < -select(
  houseCandidate115_raw, -crpId, -photo, -nickName, 
  -middleName, -preferredName, -suffix, -birthDate, 
  -pronunciation, -gender, -family, -homeCity, -homeState,
  -religion, -specialMsg) %>%
  mutate(
      stateOfBirth = str_sub(houseCandidate115_raw$birthPlace,-2,-1),
          congress = "115",
  yearSessionStart = "2017") %>%
  rename(
    nameFirst = firstName, 
     nameLast = lastName)
# works nearly all of the time, except when rep is born outside USA

houseEdu116_raw <- edu_House_2019_20[["education"]]
houseCandidate116_raw <- edu_House_2019_20[["candidate"]]

houseCandidate116 <- select(
  houseCandidate116_raw, -crpId, -photo, -nickName, 
  -middleName, -preferredName, -suffix, -birthDate, 
  -pronunciation, -gender, -family, -homeCity, -homeState,
  -religion, -specialMsg) %>%
  mutate(
      stateOfBirth = str_sub(houseCandidate116_raw$birthPlace,-2,-1),
          congress = "116",
  yearSessionStart = "2019") %>%
  rename(
    nameFirst = firstName, 
     nameLast = lastName)

houseCandidate <- rbind(houseCandidate115, houseCandidate116)
houseEducation <- rbind(houseEdu115_raw, houseEdu116_raw)

# Senate:
senateEdu114_raw <- edu_Senate_2015_16[["education"]]
senateCandidate114_raw <- edu_Senate_2015_16[["candidate"]]

senateCandidate114 <- select(
  senateCandidate114_raw, -crpId, -photo, -nickName, 
  -middleName, -preferredName, -suffix, -birthDate, 
  -pronunciation, -gender, -family, -homeCity, -homeState,
  -religion, -specialMsg) %>%
  mutate(
      stateOfBirth = str_sub(senateCandidate114_raw$birthPlace,-2,-1),
          congress = "114",
  yearSessionStart = "2015") %>%
  rename(
    nameFirst = firstName, 
     nameLast = lastName)

senateEdu115_raw <- edu_Senate_2017_18[["education"]]
senateCandidate115_raw <- edu_Senate_2015_16[["candidate"]]

senateCandidate115 <- select(
  senateCandidate115_raw, -crpId, -photo, -nickName, 
  -middleName, -preferredName, -suffix, -birthDate, 
  -pronunciation, -gender, -family, -homeCity, -homeState,
  -religion, -specialMsg) %>%
  mutate(
      stateOfBirth = str_sub(senateCandidate115_raw$birthPlace,-2,-1),
          congress = "115",
  yearSessionStart = "2017") %>%
  rename(
    nameFirst = firstName, 
     nameLast = lastName)

senateEdu116_raw <- edu_Senate_2019_20[["education"]]
senateCandidate116_raw <- edu_Senate_2019_20[["candidate"]]

senateCandidate116 <- select(
  senateCandidate116_raw, -crpId, -photo, -nickName, 
  -middleName, -preferredName, -suffix, -birthDate, 
  -pronunciation, -gender, -family, -homeCity, -homeState,
  -religion, -specialMsg) %>%
  mutate(
      stateOfBirth = str_sub(senateCandidate116_raw$birthPlace,-2,-1),
          congress = "116",
  yearSessionStart = "2019") %>%
  rename(
    nameFirst = firstName, 
     nameLast = lastName)

senateCandidate <- rbind(senateCandidate114, senateCandidate115, senateCandidate116)
senateEducation <- rbind(senateEdu114_raw, senateEdu115_raw, senateEdu116_raw)

senateEducation_raw <- select(senateEducation, -span, -gpa, -fullText, -field)
houseEducation_raw <- select(houseEducation, -span, -gpa, -fullText, -field)

senateEducation <- unique(senateEducation_raw)
houseEducation <- unique(houseEducation_raw)



# pull in Bachelor's institutions (includes individuals who received an 
# undergrad degree in law from the UK as their first degree)

# some fixes: I started looking around once I realized that there
# were a non-negligible number of candidates listed as having two or more BAs.

senateEducation_undergradCheck <- senateEducation %>%group_by(candidateId) %>%
  filter(educCategory == "instBachelors") %>%
  count(candidateId)

# Booker, Cory (76151) is listed as having two BAs. One from Alma College, and one from
# Oxford. This is incorrect. Booker an MA from Oxford on a Rhodes scholarship: 
# https://ballotpedia.org/Cory_Booker.
# TO DO: this coding.
senateEducation$degree[44] <- "MA"

# Udall, Thomas (22658) is listed as having two bachelors. The second is an LLB from
# Cambridge. Though the LLB is an undergraduate law degree in the UK, I've 
# coded it here as a grad degree. (need to do this after next code block, see below).
houseEducation_undergradCheck <- houseEducation %>%group_by(candidateId) %>%
  filter(educCategory == "instBachelors") %>%
  count(candidateId)

# 116935 has NO candidate information 

# Cooper, Jim (48891) got a BA/MA from Oxford in addition to a BA from Univ. of
# North Carolina. Here it says he just got an MA from Oxford: https://en.wikipedia.org/wiki/Jim_Cooper
houseEducation$degree[623] <- "MA"

# Omar, Ilhan (171628) is listed as having a BA and a BS from North Dakota State
# Univ. She just has a BA: https://en.wikipedia.org/wiki/Ilhan_Omar
houseEducation$degree[852] <- "remove"

# Wenstrup, Brad (135326) is listed as having a BA and a BS. His Wikipedia page 
# says this is TRUE: https://en.wikipedia.org/wiki/Brad_Wenstrup, 
# another source:https://media.cq.com/members/43608?rel=memberLink
# TO DO: Ask John about including a new Bachelors column for second degrees.
# It would be mostly empty.(I'd also have to rework some of my code)

# Schrader, Walter(10813) is listed as having a BA and a BS as well as a DVM. 
# Oregon Sec of State states that he only has a BA and and a DVM.
# https://secure.sos.state.or.us/orestar/cfDetail.do;JSESSIONID_ORESTAR=21ssJksTLyhvTlq3nQpJZpTyN09HyLqvZXsl2cd1Sp7s121TcfKm!261229845?page=search&cfRsn=13071
houseEducation$degree[567] <- "remove"

# Crawford, Eric(119208) is listed as having a BA and a BS from Arkansas State Univ. 
# He only has a BS: https://en.wikipedia.org/wiki/Rick_Crawford_(politician)
houseEducation$degree[32] <- "remove"

# Lewis, John(26820) is listed as having two BAs. One from Fisk University and one from
# American Baptist Theological Seminary.
# This is TRUE: https://en.wikipedia.org/wiki/John_Lewis.
# TO DO: Ask John about including a new Bachelors column for second degrees.
# It would be mostly empty.(I'd also have to rework some of my code)

houseEducation <- houseEducation %>%filter(degree != "remove")

# TO DO: need to pull in education information as requested
senateEducation <- senateEducation %>%
  filter(degree != "Attended", 
         degree != "MBA Candidate",
         degree != "Attended, PhD Program",
         degree != "AAS",
         degree != "Graduated") %>%  # not sure if removing this is the move
  mutate(educCategory = case_when(
    degree == "BA"   |degree == "Bachelor's"| 
    degree == "BS"   |degree == "B.A."| 
    degree == "AB"   |degree == "BBA"|
    degree == "LLB" & school == "Cambridge University" 
    |degree== "BLL"~ 'instBachelors'))

houseEducation <- houseEducation %>%
  filter(
    degree != "Attended", 
    degree != "MBA Candidate",
    degree != "Attended, PhD Program",
    degree != "AAS",
    degree != "Graduated",
    degree != "Certified", 
    degree != "AA",
    degree != "Gradated",
    degree != "Fellow",
    degree != "Diploma",
    degree != "Certificate",
    degree != "Attending",
    degree != "Associates",
    degree != "Associate's",
    degree != "AS") %>%
  mutate(educCategory = case_when(
    degree == "BA"   |degree == "Bachelor's"| 
    degree == "BS"   |degree == "B.A."| 
    degree == "AB"   |degree == "BBA"|
    degree == "LLB" & school == "Cambridge University"|
    degree == "BLL"|
    degree == "BSIE"     |degree == "BSFS"|
    degree == "BSBAGE"   |degree == "BSBA"|
    degree == "BSA"      |degree == "BS/BA"|
    degree == "Bachelors"|degree == "BAchelor's"|
    degree == "BA"       |degree =="ALB"|
    degree == "BA/MA"~ 'instBachelors'))
# There are some instances of Oxford being listed as giving a BA and MA at  
# once. For the purposes of data pull, I'll label them instBachelors to pull  
# in the school name. Then, I'll also need to pull in the grad name from it 
# as well. 


# FINAL FIX: Udall, Thomas (22658) is listed as having two bachelors. 
# The second is an LLB from Cambridge. Though the LLB is an undergraduate  
# law degree in the UK, I've coded it here as a grad degree. 
senateEducation$educCategory[48] <- "NA"



# **************************************************************************
# PULL IN GRADUATE DEGREES ####
# **************************************************************************

# John, there are some differences in the way the VoteSmart API documents
# institutions compared to the way we've done it. JDs are sometimes
# from Harvard Law School
# as opposed to just Harvard University. (This is different from your 
# instructions on the manual data I collected).

# if a candidate received an additional "BA" from Oxford or Cambridge, they 
# were likely an Oxbridge scholar. Remove.

senateEducation <- senateEducation %>% 
  map_df(rev) %>%
  group_by(candidateId) %>% 
  mutate(gradNo = row_number()) %>%
  mutate(educCategory= case_when(
    gradNo == 1 ~ "instBachelors",
    gradNo == 2 ~ "instGrad1",
    gradNo == 3 ~ "instGrad2",
    gradNo == 4 ~ "instGrad3",
  ))

houseEducation <- houseEducation %>% 
  map_df(rev) %>%
  group_by(candidateId) %>% 
  mutate(gradNo = row_number()) %>%
  mutate(educCategory= case_when(
    gradNo == 1 ~ "instBachelors",
    gradNo == 2 ~ "instGrad1",
    gradNo == 3 ~ "instGrad2",
    gradNo == 4 ~ "instGrad3",
  ))

senateCandidate <- 
  senateEducation %>%
  select(school, educCategory, candidateId, degree) %>%
  right_join(senateCandidate, by = "candidateId") %>%
  mutate(degreeType = case_when(
    educCategory == "instBachelors" ~ "degreeBach",
    educCategory == "instGrad1" ~"gradDegree1",
    educCategory == "instGrad2"~ "gradDegree2",
    educCategory == "instGrad3"~ "gradDegree3")) %>%
  ungroup() %>%
  mutate(row = row_number())

houseCandidate <- 
  houseEducation %>%
  select(school, educCategory,candidateId, degree) %>%
  right_join(houseCandidate, by = "candidateId") %>%
  mutate(degreeType = case_when(
    educCategory == "instBachelors" ~ "degreeBach",
    educCategory == "instGrad1" ~"gradDegree1",
    educCategory == "instGrad2"~ "gradDegree2",
    educCategory == "instGrad3"~ "gradDegree3")) %>%
  ungroup() %>%
  mutate(row = row_number()) %>%
  select(-birthPlace)
# # I need to verify the below works the way I intend it to, but spotchecks 
# # make me hopeful. 

senateCandidate_wide <- senateCandidate %>%
  tidyr::pivot_wider(names_from   = educCategory, 
                     values_from  = school,
                     names_repair = "minimal") %>%
  tidyr::pivot_wider(names_from   = degreeType, 
                     values_from  = degree,
                     names_repair = "minimal") %>%
  select(-row, -birthPlace)

# I'm trying to consolidate rows here:
senateCandidate_wide[is.na(senateCandidate_wide)] <- "" # removes the NAs repeating below

senateCheck <- senateCandidate_wide %>%
  group_by(candidateId, congress) %>%
  summarise_all(funs(trimws(paste(., collapse = '')))) %>%
  select(-nameFirst, -nameLast, -stateOfBirth, -yearSessionStart)

# I could just re-pull the columns that have been jumbled...
# nameFirst through yearSessionStart
senateEduFinal <- 
  senateCandidate_wide %>%
  select(nameFirst, nameLast, stateOfBirth, yearSessionStart, candidateId) %>%
  right_join (senateCheck, by = "candidateId") # aha!

senateEduFinal$stateOfBirth <- toupper(senateEduFinal$stateOfBirth)

houseCandidate_wide <- houseCandidate %>%
  tidyr::pivot_wider(names_from  = educCategory, 
                     values_from = school,
                     names_repair= "minimal") %>%
  tidyr::pivot_wider(names_from  = degreeType, 
                     values_from = degree,
                     names_repair= "minimal") %>%
  select(-"NA", -row)

houseCandidate_wide[is.na(houseCandidate_wide)] <- ""

houseCheck <- houseCandidate_wide %>%
  group_by(candidateId, congress) %>%
  summarise_all(funs(trimws(paste(., collapse = '')))) %>%
  select(-nameFirst, -nameLast, -stateOfBirth, -yearSessionStart)

houseEduFinal <- 
  houseCandidate_wide %>%
  select(nameFirst, nameLast, stateOfBirth, yearSessionStart, candidateId) %>%
  right_join(houseCheck, by = "candidateId")  # aha!(the blanks here are in the orig dataset)

houseEduFinal$stateOfBirth <- toupper(houseEduFinal$stateOfBirth)


# need to pull in party and state information
# if they caucus with a major party, put down that party.
houseOffice115_raw <- edu_House_2017_18[["office"]] %>%
  select(candidateId, parties, stateId)
houseOffice116_raw <- edu_House_2019_20[["office"]] %>%
  select(candidateId, parties, stateId)

senateOffice114_raw <- edu_Senate_2015_16[["office"]] %>%
  select(candidateId, parties, stateId)
senateOffice115_raw <- edu_Senate_2017_18[["office"]] %>%
  select(candidateId, parties, stateId)
senateOffice116_raw <- edu_Senate_2019_20[["office"]] %>%
  select(candidateId, parties, stateId)

senateOffice <- rbind(senateOffice114_raw, senateOffice115_raw, senateOffice116_raw)
houseOffice <- rbind(houseOffice115_raw, houseOffice116_raw)

# the following two pulls still produce missing entries. it seems like
#  these are present in the original API pull as well. (the candidate Ids 
# exist in them, but there's no information)
senateCandidate <- senateOffice %>%
  right_join(senateCandidate, by = "candidateId")

houseCandidate <- houseOffice %>%
  right_join(houseCandidate, by = "candidateId")

# TO DO: need to pull in IPEDs codes