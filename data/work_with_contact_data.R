
library(socialmixr)
library(here)
data(polymod)

ages =  c(4,9,14,19,24,29,34,39,44,49,54,59,64,69,74,100) # Upper end of age bands
age.categories <- as.factor(ages)

survey_countries(polymod)
contact = contact_matrix(
  polymod, countries = "United Kingdom", 
  age.limits = c(0,as.numeric(as.character(age.categories[1:(length(ages)-1)]))), 
  symmetric = TRUE)


matrix_plot(contact$matrix)

ls<-list_surveys()
uk_vh <- get_survey("https://doi.org/10.5281/zenodo.1409506")
saveRDS(uk_vh, "uk_vanHoek.rds")

contact_vh = contact_matrix(
  uk_vh, countries = "United Kingdom", 
  symmetric = TRUE)
contact_vh$matrix


load(here("data","contact_synth2021_all.rdata"))

matrix_plot(contact_all$BRA)
