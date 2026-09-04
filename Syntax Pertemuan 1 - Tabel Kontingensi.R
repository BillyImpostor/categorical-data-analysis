library(readxl)
library(DescTools)
library(sjPlot)

### Input data dari file
df = read_excel("D:/OneDrive - UGM 365/Perkuliahan/Semester 7/asprak ANDAKAT/Pertemuan 1/Data Pertemuan 1 - Tabel Kontingensi.xlsx")
df$`Makan Sambal`<- factor(df$`Makan Sambal`, levels = c(1, 2), labels = c("Ya", "Tidak"))
df$`Sakit Perut`<- factor(df$`Sakit Perut`, levels = c(1, 2), labels = c("Ya", "Tidak"))

tab_xtab(
  var.row = df$`Makan Sambal`,
  var.col = df$`Sakit Perut`,
  show.summary = FALSE,
  show.exp = TRUE,
  var.labels = c("Makan Sambal", "Sakit Perut")
)

cross_table <- table("Makan Sambal" = df$`Makan Sambal`, "Sakit Perut"=df$`Sakit Perut`)
Xsq = chisq.test(cross_table,correct=F)
Xsq
Xsq$expected

OddsRatio(
  cross_table,
  method = "wald",
  conf.level = 0.95
)


### Input data manual
data=matrix(c(109,10,116,84),nrow=2)
colnames(data)=c("Sakit Perut","Tidak Sakit Perut")
rownames(data)=c("Makan Sambal","Tidak Makan Sambal")
data

Xsq = chisq.test(data,correct = F)
Xsq
Xsq$expected

OddsRatio(
  data,
  method = "wald",
  conf.level = 0.95
)




