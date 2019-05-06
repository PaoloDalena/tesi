#test 07.03.2019

library(corecage)
library(here)

# Estrazione testo --------------------------------------------------------

# metto i pdf nella cartella relazioni_pdf
pdf_file_names(here("relazioni_pdf"))
# creo cartella vuota relazioni_txt
# inserisco pdfact.jar nella wd
create_txt(here("relazioni_pdf"),here("relazioni_txt"))
create_txt


# 12.03.2019 --------------------------------------------------------------

#creo funzione create_txt_tesi
# --role headings
create_txt(here("relazioni_pdf"),here("relazioni_txt","create_txt_default"))
create_txt_1(here("relazioni_pdf"),here("relazioni_txt","definitivo"))


# 21.03.2019 --------------------------------------------------------------

#carico i pdf con le considerazioni finali e utilizzo role tutti e unit words
source("create_txt_tesi.R")
here()
create_txt_tesi(here("cf_pdf"),here("cf_txt"))
