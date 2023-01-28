library("tidyverse")

kn_raw <- read.csv("data/data_kommuneniveau.csv", sep = ";", dec = ",") %>% tibble()
vn_raw <- read.csv("data/data_valgstedsniveau.csv", sep = ";", dec = ",") %>% tibble()

kn <- kn_raw %>% 
  mutate(skoleluk_aar_i_valgperiode = ifelse(is.na(skoleluk_aar_i_valgperiode), 0, skoleluk_aar_i_valgperiode),
         koalition_kat = case_when(
           koalition > .5 & koalition < .625 ~ 1,
           koalition >= .625 & koalition < .75 ~ 2,
           koalition >= .75 & koalition < .875 ~ 3,
           koalition >= .875 & koalition <= 1 ~ 4,
           TRUE ~ 0
         ))

vn <- vn_raw %>% 
  group_by(knr, aar) %>% 
  summarize(antal_skole_lukning = sum(skolelukning)) %>% 
  right_join(vn_raw, by = c("knr", "aar")) %>% 
  filter(antal_skole_lukning > 0) %>% 
  mutate(koalition_kat = case_when(
    koalition > .5 & koalition < .625 ~ 1,
    koalition >= .625 & koalition < .75 ~ 2,
    koalition >= .75 & koalition < .875 ~ 3,
    koalition >= .875 & koalition <= 1 ~ 4,
    TRUE ~ 0
  ),
  friskole_kat = case_when(
    friskole == 0 ~ 1,
    friskole == 1 ~ 2,
    TRUE ~ 0
  ),
  kl_sidste_skole_kat = case_when(
    kl_sidste_skole == 0 ~ 1,
    kl_sidste_skole == 1 ~ 2,
    TRUE ~ 0
  ))

kn |> write_csv("data_kn.csv")
vn |> write_csv("data_vn.csv")
