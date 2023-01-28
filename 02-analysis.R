library("tidyverse")
library("stargazer")
library("patchwork")

theme_set(theme_minimal() %+replace%
            theme(panel.grid.major.x = element_blank(),
                  panel.grid.minor.x = element_blank(),
                  panel.grid.major.y = element_blank(),
                  panel.grid.minor.y = element_blank(),
                  legend.position = "bottom",
                  legend.title = element_blank(),
                  axis.line = element_line(size = 0.2)))

kn <- read_csv("data_kn.csv")
vn <- read_csv("data_vn.csv")

mean(kn$d_borgmesterparti[kn$skolelukning == 1], na.rm=TRUE)
mean(kn$d_borgmesterparti[kn$skolelukning == 0], na.rm=TRUE)
summary(kn$d_borgmesterparti)


fig1a <- kn %>% 
  filter(aar == 2013) %>% 
  mutate(skolelukning = ifelse(skolelukning == 0, "Ingen skolelukning", "Skolelukning")) %>% 
  ggplot(aes(x = bm_andel_forrige_valg, y = bm_andel_dette_valg, shape = skolelukning)) +
  geom_abline(slope =  1,intercept = 0, size = 0.2) +
  geom_point() +
  scale_shape_manual(values = c(1, 16)) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                     name = "Borgmesterpartiets tilslutning, KV09",
                     limits = c(0, 0.7)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     name = "Borgmesterpartiets tilslutning, KV13",
                     limits = c(0, 0.7)) +
  theme(text=element_text(family="Times New Roman"))


fig1b <- kn %>% 
  filter(aar == 2017) %>% 
  mutate(skolelukning = ifelse(skolelukning == 0, "Ingen skolelukning", "Skolelukning")) %>% 
  ggplot(aes(x = bm_andel_forrige_valg, y = bm_andel_dette_valg, shape = skolelukning)) +
  geom_abline(slope = 1, intercept = 0, size = 0.2) +
  geom_point() +
  scale_shape_manual(values = c(1, 16)) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                     name = "Borgmesterpartiets tilslutning, KV13",
                     limits = c(0, 0.7)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     name = "Borgmesterpartiets tilslutning, KV17",
                     limits = c(0, 0.7)) +
  theme(text=element_text(family="Times New Roman"))


figur_kommuneniveau <- fig1a + fig1b + plot_layout(guides = 'collect') & theme(legend.position = "bottom")

ggsave("figur_kommuneniveau.png", 
       plot = figur_kommuneniveau,
       width = 10,
       height = 5,
       dpi = 300)

reg_1 <- lm(d_borgmesterparti ~ skolelukning, data = kn)
reg_2 <- lm(d_borgmesterparti ~ skolelukning + ny_borgmester + valgvind_bm + I(d_nettodriftsudgifter/10000) + aar, data = kn)

kn %>% 
  filter(is.na(d_borgmesterparti) | is.na(skolelukning)) %>% 
  select(aar, kommunenavn, d_borgmesterparti, skolelukning)

stargazer(lm(d_borgmesterparti ~ skolelukning, data = filter(kn, !(aar == 2013 & kommunenavn == "Slagelse"), !(aar == 2017 & kommunenavn == "Assens"))),
          lm(d_borgmesterparti ~ skolelukning + ny_borgmester + valgvind_bm + I(d_nettodriftsudgifter/10000) + aar, data = filter(kn, !(aar == 2013 & kommunenavn == "Slagelse"), !(aar == 2017 & kommunenavn == "Assens"))),
          type = "text")


stargazer(lm(d_borgmesterparti ~ skolelukning, data = filter(kn, !kommunenavn %in% c("Odense", "Aarhus", "København", "Aalborg"))),
          lm(d_borgmesterparti ~ skolelukning + ny_borgmester + valgvind_bm + I(d_nettodriftsudgifter/10000) + aar, data = filter(kn, !kommunenavn %in% c("Odense", "Aarhus", "København", "Aalborg"))),
          type = "text")

stargazer(lm(d_borgmesterparti ~ skolelukning, data = filter(kn, borgmesterparti == "A")),
          lm(d_borgmesterparti ~ skolelukning, data = filter(kn, borgmesterparti == "V")),
          lm(d_borgmesterparti ~ skolelukning*borgmesterparti, data = filter(kn, borgmesterparti %in% c("A", "V"))),
          type = "text")

stargazer(lm(d_borgmesterparti ~ skolelukning, data = filter(kn, sammenlagt_kommune == 0)),
          lm(d_borgmesterparti ~ skolelukning, data = filter(kn, sammenlagt_kommune == 1)),
          lm(d_borgmesterparti ~ skolelukning*sammenlagt_kommune, data = kn),
          type = "text")



kn %>% 
  filter(!is.na(d_borgmesterparti)) %>% 
  filter(is.na(valgvind_bm) | is.na(d_nettodriftsudgifter))

stargazer(reg_1, reg_2, 
          covariate.labels = c("Skolelukning", "Første-periode-borgmester", "Ændring i landspolitisk opbakning",  "Ændring i nettodriftsudg./indb.", "Periode (2017)"),
          decimal.mark = ",",
          type = "text",
          column.labels = c("Uden kontrolvariable", "Med kontrolvariable"),
          keep.stat = c("rsq", "n"),
          digits = 3,
          single.row = TRUE,
          out = "table1.htm")



reg_3 <- lm(d_borgmesterparti ~ factor(skoleluk_aar_i_valgperiode), data = kn)
reg_4 <- lm(d_borgmesterparti ~ factor(koalition_kat), data = kn)
reg_5 <- lm(d_borgmesterparti ~ factor(skoleluk_aar_i_valgperiode) + ny_borgmester + valgvind_bm + I(d_nettodriftsudgifter/10000) + aar, data = kn)
reg_6 <- lm(d_borgmesterparti ~ factor(koalition_kat) + ny_borgmester + valgvind_bm + I(d_nettodriftsudgifter/10000) + aar, data = kn)

stargazer(reg_3, reg_4, reg_5, reg_6,# reg_7, 
          keep.stat = c("rsq", "n"),
          decimal.mark = ",",
          covariate.labels = c("... i år 1", "... i år 2", "... i år 3", "... i år 4", 
                               "... med 50-62,4% flertal", "... med 62,5-74,9% flertal", "... med 75-87,4% flertal", "... med 87,5-100% flertal",
                               "Første-periode-borgmester", "Ændring i landspolitisk opbakning",  "Ændring i nettodriftsudg./indb.", "Periode (2017)"),
          column.labels = c("Timing", "Koalition", "Timing, med kontrolvariable", "Koalition, med kontrolvariable"),
          digits = 3,
          single.row = TRUE,
          type = "text",
          out = "table2.htm")


udvalg_kn_1 <- lm(d_udvalgsformandsparti ~ ny_borgmester + valgvind_uf + I(d_nettodriftsudgifter/10000) + aar + skolelukning, data = kn)
udvalg_kn_2 <- lm(d_udvalgsformandsparti ~ ny_borgmester + valgvind_uf + I(d_nettodriftsudgifter/10000) + aar + factor(koalition_kat), data = kn)
udvalg_kn_3 <- lm(d_udvalgsformandsparti ~ ny_borgmester + valgvind_uf + I(d_nettodriftsudgifter/10000) + aar + factor(skoleluk_aar_i_valgperiode), data = kn)

stargazer(udvalg_kn_1, udvalg_kn_2, udvalg_kn_3, type = "text")


fig2a <- vn %>% 
  filter(aar == 2013) %>% 
  mutate(skolelukning = ifelse(skolelukning == 0, "Ingen skolelukning", "Skolelukning")) %>% 
  ggplot(aes(x = bm_andel_forrige_valg, y = bm_andel_dette_valg, shape = skolelukning)) +
  geom_abline(slope =  1,intercept = 0, size = 0.2) +
  geom_point(alpha = 0.6) +
  scale_shape_manual(values = c(1, 16)) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                     name = "Borgmesterpartiets tilslutning, KV09",
                     limits = c(0, 0.7)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     name = "Borgmesterpartiets tilslutning, KV13",
                     limits = c(0, 0.7)) +
  theme(text=element_text(family="Times New Roman"))


fig2b <- vn %>% 
  filter(aar == 2017) %>% 
  mutate(skolelukning = ifelse(skolelukning == 0, "Ingen skolelukning", "Skolelukning")) %>% 
  ggplot(aes(x = bm_andel_forrige_valg, y = bm_andel_dette_valg, shape = skolelukning)) +
  geom_abline(slope =  1,intercept = 0, size = 0.2) +
  geom_point(alpha = 0.6) +
  scale_shape_manual(values = c(1, 16)) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                     name = "Borgmesterpartiets tilslutning, KV13",
                     limits = c(0, 0.7)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     name = "Borgmesterpartiets tilslutning, KV17",
                     limits = c(0, 0.7)) +
  theme(text=element_text(family="Times New Roman"))

figur_valgstedsniveau <- fig2a + fig2b + plot_layout(guides = 'collect') & theme(legend.position = "bottom")

ggsave("figur_valgstedsniveau.png", 
       plot = figur_valgstedsniveau,
       width = 10,
       height = 5,
       dpi = 300)


reg_8_pre <- lm(d_borgmesterparti ~ factor(kommunenavn) + skolelukning + aar, data = vn)
reg_8 <- lmtest::coeftest(reg_8_pre, vcov = sandwich::vcovCL(reg_8_pre, cluster = ~ kommunenavn))

reg_9_pre <- lm(d_borgmesterparti ~ factor(kommunenavn) + factor(koalition_kat) + aar, data = vn)
reg_9 <- lmtest::coeftest(reg_9_pre, vcov = sandwich::vcovCL(reg_9_pre, cluster = ~ kommunenavn))

reg_10_pre <- lm(d_borgmesterparti ~ factor(kommunenavn) + factor(friskole_kat) + aar, data = vn)
reg_10 <- lmtest::coeftest(reg_10_pre, vcov = sandwich::vcovCL(reg_10_pre, cluster = ~ kommunenavn))

reg_11_pre <- lm(d_borgmesterparti ~ factor(kommunenavn) + factor(kl_sidste_skole_kat) + aar, data = vn)
reg_11 <- lmtest::coeftest(reg_11_pre, vcov = sandwich::vcovCL(reg_11_pre, cluster = ~ kommunenavn))

stargazer(reg_8, reg_9, reg_10, reg_11,
          keep = c("skolelukning", "koalition", "friskole", "kl_", "aar"),
          decimal.mark = ",",
          digits = 3,
          covariate.labels = c("Skolelukning", "... med 50-62,4% flertal", "... med 62,5-74,9% flertal", "... med 75-87,4% flertal", "... med 87,5-100% flertal",
                               "Skolelukning uden etabl. friskole", "Skolelukning med etabl. friskole", "Lukning men ikke sidste skole", "Lukning af sidste skole", "Periode (2017)"),
          add.lines = list(c("N", nobs(reg_8_pre), nobs(reg_9_pre), nobs(reg_10_pre), nobs(reg_11_pre)),
                           c("R2", round(summary(reg_8_pre)$r.squared, 3), round(summary(reg_9_pre)$r.squared, 3), round(summary(reg_10_pre)$r.squared, 3), round(summary(reg_11_pre)$r.squared, 3))),
          type = "text",
          out = "table3.htm")

summary(lm(d_borgmesterparti ~ factor(kommunenavn) + factor(koalition_kat) + aar, data = filter(vn, skolelukning == 1)))

# Deskriptiv statistik
kn %>% 
  select(skolelukning, d_borgmesterparti, skoleluk_aar_i_valgperiode, koalition_kat, ny_borgmester, valgvind_bm, d_nettodriftsudgifter) %>% 
  as.data.frame() %>% 
  stargazer(type = "text",
            out = "table-deskriptiv-kn.htm",
            covariate.labels = c(
              "Skolelukning",
              "Ændring i opbakning til borgmesterparti",
              "År for skolelukning",
              "Koalitionsstørrelse",
              "Første-periode-borgmester",
              "Ændring i landspolitisk opbakn.",
              "Ændring i nettodriftsudg./indb."
            ),
            decimal.mark = ",")

vn %>% 
  as.data.frame() %>% 
  mutate(friskole_1 = ifelse(friskole_kat == 1, 1, 0),
         friskole_2 = ifelse(friskole_kat == 2, 1, 0),
         sidsteskole_1 = ifelse(kl_sidste_skole_kat == 1, 1, 0),
         sidsteskole_2 = ifelse(kl_sidste_skole_kat == 2, 1, 0)
         ) %>% 
  select(skolelukning, d_borgmesterparti, koalition_kat, friskole_1, friskole_2, sidsteskole_1, sidsteskole_2) %>% 
  stargazer(type = "text",
            covariate.labels = c(
              "Skolelukning",
              "Ændring i opbakning til borgmesterparti",
              "Koalitionsstørrelse",
              "Skolelukning uden etabl. friskole",
              "Skolelukning med etabl. friskole",
              "Lukning men ikke sidste skole", 
              "Lukning af sidste skole"
            ),
            out = "table-deskriptiv-vn.htm",
            decimal.mark = ",")
         