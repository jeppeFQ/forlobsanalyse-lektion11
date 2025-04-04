
# Indlæsning af biblioteker

library(survival)
library(tidyverse)

## Dan et datasæt, hvor alle forløb inden for de enkelte personer indgår
### Husk at tiden skal være 0 ved forløbets start
### Der skal ikke tages højde for tidsvarierende variable

##Dannelse af et datasæt

#Data reduceres til kun at indeholde observationer af personer fra deres 18. leveår
#køn, alder og antal børn i familien rekodes. Køn og alder ændres fra character til numeric 

load("Forlob_14032023.rda")

df1 <- Forlob_18032022 %>% 
  mutate(age = as.numeric(ALDER)) %>%
  filter(age >= 18) %>%                                               
  mutate(person_start = case_when(lag(pnr) == pnr ~ 0, TRUE ~ 1)) %>% 
  mutate(born = as.numeric(C_ANTBOERNF)) %>% 
  mutate(kvinde = if_else(KOEN == 2, 1, 0)) %>%
  select(- C_ANTBOERNF, - KOEN, -ALDER)  


#Event-variablen og tidsopgørelsen (start, slut) dannes


df2 <- df1 %>% 
  mutate(event = if_else(lag(born) < born  
          & person_start == 0 | person_start == 1 & born >= 1, 1, 0))        
   
  

df3 <- df2 %>%                                                        
  mutate(start = age - 18) %>%                                                
  mutate(slut = age - 17)


#Variablen period_slut dannes skal senere bruges til at fastholde observationen af afslutningen
#inden for den enkelte enhed (personen) til analyse af multiple hændelsesforløb

df4 <- df3 %>%
  mutate(new = case_when(person_start == 1 ~ 0, 
                          person_start != 1 ~ lag(event))) %>% 
  arrange(pnr, aar) %>%
  group_by(pnr) %>%
  mutate(period = cumsum(new)+1) %>%  
  select(-new)%>% 
  mutate(person_slut = case_when(lead(pnr) == pnr ~ 0, TRUE ~ 1)) %>%
  mutate(period_slut = case_when(event == 1 | person_slut == 1 ~ 1, TRUE ~ 0)) %>%
  mutate(igud = as.factor(case_when(i_udd == 1 | i_udd == 2 |
                                    i_udd == 3 | i_udd == 4 ~ "under_udd" ,
                                    TRUE ~ "ikke_igang")))  

df5 <- df4 %>% 
  filter(person_start == 1) %>% 
  select(pnr, region) %>% 
  mutate(region18 = as.factor(case_when(region == 1 ~ "Hovedstaden", 
                                        region == 2 ~ "Sjælland",
                                        region == 3 ~ "Syddanmark",
                                        region == 4 ~ "Midtjylland",
                                        region == 5 ~ "Nordjylland"))) %>% 
  select(-region)

df6 <- left_join(df4, df5, by = "pnr")







df7 <- df6 %>% 
  mutate(periode_start = case_when(lag(event == 1) & person_start == 0 |  person_start == 1 ~ 1, TRUE ~ 0)) %>% 
  filter(periode_start == 1) %>% 
  mutate(periode_startaar = aar, periode_age = age) %>% 
  select(pnr, period, periode_startaar, periode_age)



#Her kombineres datasættene, så oplysningen omkring startåret for det enkelte forløb inden for personen

#I princippet kan denne fremgangsmåde bruges til at omdanne tidsvarierende variable til tidskonstante inden for det enkelte forløb

df8 <- left_join(df6, df7, by = c("pnr","period"))



## Dannelse af tidsvariable inden for det enkelte forløb

multi_all <- mutate(df8, pt_start = aar - periode_startaar,
                   pt_slut = aar - periode_startaar + 1) 

multi_event <- filter(multi_all, period_slut == 1)

multi_all2 <- filter(multi_all, period == 2)

multi_event2 <- filter(multi_all2, period_slut == 1)



