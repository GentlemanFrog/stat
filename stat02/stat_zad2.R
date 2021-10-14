# konwersja do txt
 txt_tab = read.table("rzepak_ozimy.txt", header = TRUE)
 txt_tab
mean(txt_tab$glukozynolany)

#konwersja do xlsx
library(openxlsx)
xlsx_tab = openxlsx::read.xlsx("konopie.xlsx")
xlsx_tab$ciezwlokna
sd(xlsx_tab$ciezwlokna)

 