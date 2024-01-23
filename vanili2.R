setwd("D:/METOPEL UAS/FIZRI METOPEL")
library(readxl)
library(tidyverse)
library(kableExtra)
read_excel("vanili2.xlsx")
dat <- read_excel("vanili2.xlsx")
kbl(dat) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

# regresi
reg1<-lm(ekspor~tot+prodt+xqty+lahan+kurs,data=dat)
summary(reg1)

# Plot 
plot(dat$tahun,dat$ekspor,xlab="Tahun",ylab="Nilai FOB Ekspor Vanila ")
plot(dat$tahun,dat$tot,xlab="Tahun",ylab="Nilai FOB Total Ekspor")
plot(dat$tahun,dat$prodt,xlab="Tahun",ylab="Total Produksi Vanila (ton)")
plot(dat$tahun,dat$xqty,xlab="Tahun",ylab="Jumlah Ekspor Vanila (ton)")
plot(dat$tahun,dat$lahan,xlab="Tahun",ylab="Luas Lahan Tanaman Vanila")
plot(dat$tahun,dat$kurs,xlab="Tahun",ylab="Nilai Tukar USD/RP")

# Plot Error

dat$m<-resid(reg1)
plot(dat$ekspor,dat$m,xlab="Nilai Ekspor Vanila Indonesia",ylab="error")
abline(h=0) # membuat garis horizontal di y=0

dat$m<-resid(reg1)
plot(dat$tot,dat$m,xlab="Total Nilai Ekspor Indonesia",ylab="error")
abline(h=0) # membuat garis horizontal di y=0

dat$m<-resid(reg1)
plot(dat$prodt,dat$m,xlab="Total Produksi Vanila (ton)",ylab="error")
abline(h=0) # membuat garis horizontal di y=0

dat$m<-resid(reg1)
plot(dat$xqty,dat$m,xlab="Total Ekspor Vanila (ton)",ylab="error")
abline(h=0) # membuat garis horizontal di y=0

dat$m<-resid(reg1)
plot(dat$lahan,dat$m,xlab="Luas Lahan Tanaman Vanila (ha)",ylab="error")
abline(h=0) # membuat garis horizontal di y=0

dat$m<-resid(reg1)
plot(dat$kurs,dat$m,xlab="Nilai Tukar USD/RP",ylab="error")
abline(h=0) # membuat garis horizontal di y=0