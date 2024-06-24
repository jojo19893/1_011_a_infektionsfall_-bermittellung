##### Ocr-Laborberichte ###

## Objective: Parse the relevant information from a pdf form scan of filled out lab reports

## Date 13.04.2020

## Author Marius Sältzer


# Definitions and packages

positions<-function(td){
  return(data.frame(
    x1=as.numeric(sapply(strsplit(td$bbox,","),"[[",1)),
    y1=as.numeric(sapply(strsplit(td$bbox,","),"[[",2)),
    x2=as.numeric(sapply(strsplit(td$bbox,","),"[[",3)),
    y2=as.numeric(sapply(strsplit(td$bbox,","),"[[",4))))
}

span_box<-function(b11,b12,b21,b22,position){
  box<-position[position$x1>=b11,]
  box<-box[box$y1>=b12,]
  box<-box[box$x2<=b21,]
  box<-box[box$y2<=b22,]
  return(box)
  
}

## Define a function to extract crossed dots
### ((Wie werden diese Punkte ausgefüllt?)) 
    ## Alternativen: evasys macht ähnliche sachen?

check_dot<-function(x,check){
  return(ifelse(x$word[grep(check,x$word)-1]=="O|o","Negative","Positive"))
} #!! anfällig


## Define a function to extract the terms AFTER the searchterms

get_entry<-function(x,check){
  return(x$word[grep(check,x$word)+1])
} ## plausicheck plus error handler!

if(!require("tesseract")){install.packages("tesseract")}
if(!require("pdftools")){install.packages("pdftools")}
if(!require("magick")){install.packages("magick")}
if(!require("jsonlite")){install.packages("jsonlite")}

library(tidyverse)
library(tesseract)
library(pdftools)
library(magick)
library(jsonlite)

if(is.na(match("deu", tesseract_info()$available)))
  tesseract_download("deu")

# Import

##!! muss an code angepasst werden
setwd("./ocr_labor/input")

## find a pdf to parse 
testfile=list.files()[grepl("pdf",list.files())]

png<-pdftools::pdf_convert(testfile,dpi=500,format="tiff")
input <- image_read(png[1])

# Preprocessing 

text <- input %>%
  image_resize("2000x") %>%
  image_convert(type = 'Grayscale') %>%
  image_trim(fuzz = 40) %>%
  image_write(format = 'png', density = '400x400') %>%
  tesseract::ocr_data() 

td <- tesseract::ocr_data(png,engine=tesseract('deu'))

# remove very wierd stuff 
td<-td[td$confidence>5,]

position<-positions(td)

position$word<-iconv(td$word,"UTF-8","latin1")





### Empirical positions of main document:

# Main document is splitted in boxes based on x and y variables

## center
mpos<-1/2*max(position$x2)

## splits on y
boxsplits<-c(561,1646,2222,3023,4908,5636)

## create list of data frames: words in each box

split<-list(
  private=span_box(0,boxsplits[1],mpos,boxsplits[2],position),
  amt=span_box(mpos,boxsplits[1],2*mpos,boxsplits[2],position),
  patient=span_box(0,boxsplits[2],2*mpos,boxsplits[3],position),
  befund=span_box(0,boxsplits[3],2*mpos,boxsplits[4],position),
  sereol=span_box(0,boxsplits[4],mpos,boxsplits[5],position),
  direkt=span_box(mpos,boxsplits[4],2*mpos,boxsplits[5],position),
  sender=span_box(0,boxsplits[5],mpos,boxsplits[6],position),
  interp=span_box(mpos,boxsplits[5],2*mpos,boxsplits[6],position)
)


## create a copy
parsed<-split

## Define each element in the list with either check dot for a term or get_entry (see above)

parsed$direkt<-list()
  parsed$direkt$antigen<-check_dot(split$direkt,"Antigennachweis")
  parsed$direkt$erreger<-check_dot(split$direkt,"Erregerisolierung")
  parsed$direkt$mikro<-check_dot(split$direkt,"Mikroskopischer")
  parsed$direkt$elektronen<-check_dot(split$direkt,"Elektronenmikroskopie")

  
split$sereol  
  
parsed$sereol<-list()
  parsed$sereol$antigen<-check_dot(split$sereol,"Antigennachweis")
  
  
  parsed$sereol$erreger<-check_dot(split$sereol,"Erregerisolierung")
  
  
  parsed$sereol$mikro<-check_dot(split$sereol,"Mikroskopischer")
  
  
  parsed$sereol$elektronen<-check_dot(split$sereol,"Elektronenmikroskopie")
  
  
  
parsed$private<-list()
  parsed$private$Amt<-get_entry(split$private,"Gesundheitsamt")
  parsed$private$PLZ<-get_entry(split$private,"PLZ")
  parsed$private$Ort<-get_entry(split$private,"Ort")
  parsed$private$Fax<-get_entry(split$private,"Fax")
  parsed$private$Telefon<-get_entry(split$private,"Tel.")
  parsed$private$Street<-get_entry(split$private,"Straße")

parsed$amt<-list()
  parsed$private$amt<-get_entry(split$amt,"Untersuchungsstelle")
  parsed$private$amt<-get_entry(split$amt,"Email")
  
  # ...
  
parsed$patient<-list()
  parsed$patient$Name<-get_entry(split$patient,"Vorname")
  parsed$patient$female<-check_dot(split$patient,"Weiblich")
  parsed$patient$male<-check_dot(split$patient,"Männlich")
  parsed$patient$date<-get_entry(split$patient,"Geburtsdatum")
  # ...  

parsed$sender<-list()  
  parsed$sender$sender_name<-get_entry(split$sender,"Person")
  parsed$sender$number<-get_entry(split$sender,"Telefon")
  parsed$sender$hospital<-get_entry(split$sender,"Krankenhaus")
  
# paste interpretation
parsed$interp<-paste(split$word[grep("Informationen",split$interp$word):nrow(split$interp),],collapse=" ")  

# Parse Output  

output<-toJSON(parsed)
write_json(output,"result.json")  
  