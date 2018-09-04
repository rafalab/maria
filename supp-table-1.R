#####
#####
#####     Using old code to parse Registro Demografico and create Table 1
#####
#####
library(htmlTable)
library(tidyverse)
library(pdftools)
library(stringr)

# Reading the pdf files
filename <- "data/puerto-rico/Peticion\ de\ datos\ de\ defuncio\ nes\ años\ 2013-2018\ (12\ 18).pdf"
txt      <- pdf_text(filename)

# Getting rid of all unnecessary characters
s <- str_replace_all(txt[1], "\\*.*", "") %>%
  str_replace_all("Defunciones por mes de ocurrencia, Puerto Rico: a??os 2013-2018", "") %>%
  str_replace_all("Y(201\\d)\\*?", "\\1") %>%
  str_replace("Enero",        "1") %>%
  str_replace("Febrero",      "2") %>%
  str_replace("Marzo",        "3") %>%
  str_replace("Abril",        "4") %>%
  str_replace("Mayo",         "5") %>%
  str_replace("Junio",        "6") %>%
  str_replace("Julio",        "7") %>%
  str_replace("Agosto",       "8") %>%
  str_replace("Semptiembre",  "9") %>%
  str_replace("Octubre",     "10") %>%
  str_replace("Noviembre",   "11") %>%
  str_replace("Diciembre",   "12") %>%
  str_replace("Total", "@") 

# More parsing
tmp <- str_split(s, "\n") %>% .[[1]] %>% 
  str_trim %>% str_split_fixed("\\s+", 50) %>% 
  .[,1:7] %>% as_tibble()
colnames(tmp) <- tmp[3,]
tmp <- tmp[-(1:3),]
j <- which(tmp[,1]=="@")

# More parsing
if(colnames(tmp)[1]=="2") 
{ 
  k <- which(tmp==29)
  the_number <- unlist(tmp[k,-1])
  the_number <- the_number[the_number!=""]
  tmp[k, colnames(tmp)!="2016" & colnames(tmp)!="2"] <- 0
  tmp[k, "2016"] <- the_number
}

tmp <- tmp %>% slice(1:(j-1)) %>% 
  mutate_all(funs(as.numeric(gsub(",", "",.))))
tmp[9,1] <- 9

tmp <- tmp %>% 
        htmlTable(rnames = FALSE,
            align = c("l", "l", "r", "r", "r", "r", "r", "r"))

## supp table
sink("figs/supp-table-1.html")
print(tmp ,type="html",useViewer=FALSE)
sink()

