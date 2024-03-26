




df <- read.csv(file="C:/jobb/rawdata/Vandringshinder.csv", sep=";")

with(df, table(Vandringshindertyp, Naturligt.hinder))

with(df, table(Naturligt.hinder, För.Öring))


which(df$Vandringshindertyp == "damm")

with(df, table(Fullständigt.åtgärdat.eller.inte.längre.aktuellt, Relevant))

with(df, table(Fiskväg, FiskvagTyp))
with(df, table(Fiskväg, FiskvägID))
