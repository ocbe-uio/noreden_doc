# initial exploration of norkost data
library(data.table)

norkost1 <- read_excel("data_raw/norkost/norkost1.xlsx")
norkost2 <- read_excel("data_raw/norkost/norkost2.xlsx")

norkost1 <- data.table(norkost1)
norkost2 <- data.table(norkost2)


norkost1
colnames(norkost1)
norkost1$Løpedag |> table()

norkost1$Chi_ID |> unique() |> length() # 1964
norkost1$Kjønn |> table() # 2 should be women

# overall mean ----
# overall average for certain nutrients 

tag_outcome <- c(
  "Energi", 
  "Prot", 
  "Fett", 
  "Mettet", 
  "Trans_u", 
  "C_enum", 
  "C_flerum", 
  "Kolest", 
  "Omega_3", 
  "Omega_6", 
  "Karboh", 
  "Stiv", 
  "Fiber", 
  "MonoDi", 
  "Sukker", 
  "Suktils", 
  "Sukfritt", 
  "Alko", 
  "Fullk"
)


norkost1[, ..tag_outcome] |> 
  apply(MARGIN = 2, mean) |> 
  round(2)




# weekday ----
# check weekday. need to split round 1 and round 2

norkost1[Løpedag == 1]$Ukedag_numerisk |> hist()
norkost1[Løpedag == 2]$Ukedag_numerisk |> hist()

norkost1$Ukedag_numerisk |> table() # overall (twice)
norkost1[Løpedag == 1]$Ukedag_numerisk |> table() # first 
norkost1[Løpedag == 2]$Ukedag_numerisk |> table() # second

# overall freq used for weighting

# check within-subject reporting days
norkost1[Chi_ID == 10000]
norkost1[Chi_ID == 10001]
norkost1[Chi_ID == 10002]
# can be different, can be same (maybe different weeks)

# compute weighting 
freq_day <- table(norkost1$Ukedag_numerisk)
percent_day <- freq_day / nrow(norkost1)

# monday 19.7%
# tuesday 16.2% ...
round(percent_day, 3)

w_day <- (1/7)/percent_day

# try to rescale
freq_day * w_day


# education ----
# national level: 1-5
# 1-3: low; 4-5: high
table(norkost1$Utdanningsnivå_nasjonal_stat, 
      norkost1$Utdanning_nas_stat_høy_lav)

table(norkost1$Utdanningsnivå_nasjonal_stat, 
      norkost1$Aldersgruppe_utd_SSB)

norkost1$Vekting_utdanning |> table() # 15 values
# hist(norkost1$Aldersgruppe_utd_SSB)
# hist(norkost1$Alder_v_første_kont)


# percent: low/high
freq_edu <- table(norkost1$Utdanning_nas_stat_høy_lav)
percent_edu <- freq_edu / nrow(norkost1)
percent_edu # 40% low, 60% high

w_edu <- (1/2)/percent_edu
w_edu

freq_edu * w_edu

1556 * 0.3/(1556/3920)
2360 * 0.7/(2360/3920)
1556*0.75 + 2360*(0.7/0.6)
(0.3*0.143) / (297/3920) * 297
(0.7*0.143) / (470/3920) * 470
# education * weekday ----

freq_edu_weekday <- table(norkost1$Utdanning_nas_stat_høy_lav, 
                          norkost1$Ukedag_numerisk)

freq_edu_weekday/nrow(norkost1)
