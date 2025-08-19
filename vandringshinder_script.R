




df <- read.csv("C:/Users/caio0001/Documents/gis/rawdata/Vandringshinder_csv.csv", sep=";", dec=",", fileEncoding = "Latin1")

df <- df[df$Northing > 4000000,] # wrong location!
df <- df[df$VandringshinderID != 85406,] # wrong location!

df <- df %>% filter(Relevant == "TRUE")

table(as.data.frame(table(df$VandringshinderID))$Freq)

df2 <- df %>% arrange(VandringshinderID, desc(Karteringsdatum)) %>% group_by(VandringshinderID) %>% slice(1) %>% ungroup()

table(as.data.frame(table(df2$VandringshinderID))$Freq)

write.table(df2, file = "C:/Users/caio0001/Documents/gis/rawdata/Vandringshinder_endast_Relevant.csv", sep=";", dec=",", row.names = F)

###
###
###





table(smhi_dams$STATUS, useNA = "always")






with(df, table(Vandringshindertyp, Naturligt.hinder))

with(df, table(Naturligt.hinder, För.Öring))


which(df$Vandringshindertyp == "damm")

with(df, table(Fullständigt.åtgärdat.eller.inte.längre.aktuellt, Relevant))

with(df, table(Fiskväg, FiskvagTyp))
with(df, table(Fiskväg, FiskvägID))
