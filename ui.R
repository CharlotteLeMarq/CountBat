#load packages - not sure if necessary
library(rsconnect)
library(shiny)
library(rmarkdown) 
library(ggplot2)
library(knitr)
library(pander)
library(dplyr)
library(tidyr)
library(rcompanion)
library(rdrop2)
library(shinyjs)

drop_auth(rdstoken = "droptoken.rds")

####

#which fields get saved
fieldsAll <- c("Source", "SurvetDate", "Latitude", "Longitude", "Species", "SpeciesConfirmationMethod", 
               "TotalCount", "StructureType", "primarycat", "sensitivity", "assignment", "county", "minorRegion",
               "majorRegion", "save", "Email", "Author", "SiteName", "geographicRange", "timeRange", "No.of.exits", 
               "Cloud.cover", "Wind.strength", "Rainfall", "Temp", "EmergenceTime", "Number.of.observers", 
               "Duration", "notes" )

#should be able to delete as now in server but wait til everything fixed to delete
#which are mandatory to fill in
#fieldsMandatory <- c("Source", "SurveyDate", "Latitude", "Longitude", "Species", "SpeciesConfirmationMethod",
 #                    "TotalCount", "StructureType", "primarycat", "sensitivity", "assignment", "county", 
  #                   "minorRegion", "majorRegion", "save", "Email")

# add an asterisk to an input label
labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

#get current Epoch time
epochTime <- function() {
  as.integer(Sys.time())
}

#get a formatted string of the timestamp (exclude colons as they are invalid characters in windows file names)
humanTime <- function() {
  format(Sys.time(), "%Y%m%d-%H%M%OS")
}

#save the results to a file
saveData <- function(data) {
  fileName <- sprintf("%s_%s.csv",
                      humanTime(),
                      digest::digest(data))
  
  write.csv(x = data, file = file.path(responseDir, fileName),
            row.names = FALSE, quote = TRUE)
}

# load all responses into a data.frame
loadData <- function() {
  files <- list.files(file.path(responseDir), full.names = TRUE)
  data <- lapply(files, read.csv, stringsAsFactors = FALSE)
  #data <- dplyr::rbind_all(data)
  data <- do.call(rbind, data)
  data
}

#connect to the dropbox account
drop_auth(rdstoken = "droptoken.rds")

# directory where responses get stored
responsesDir <- file.path("tempprof")

appCSS <-
  ".mandatory_star { color: red; }
   .shiny-input-container { margin-top: 25px; }
   #submit_msg { margin-left: 15px; }
   #error { color: red; }
   body { background: #fcfcfc; }
   #header { background: #fff; border-bottom: 1px solid #ddd; margin: -20px -15px 0; padding: 15px 15px 10px; }
  "

###


ui = fluidPage(
  shinyjs:: useShinyjs(),
  shinyjs::inlineCSS(appCSS),
  titlePanel("Count Bat"),
  #h5(img(src= "ecobat-logo.png", heigth=100, width=100, style="display: block; margin-left: auto; margin-right: auto;")), original line
  h5(img(src= "ecobat-logo.png", heigth=100, width=100, style="display: block")),
  div(
    id = "form",
      
    textInput(inputId = "Source", label = labelMandatory("Insert Data Source")),
    textInput(inputId = "SurveyDate", label = labelMandatory("Survey Date as DD/MM/YYYY")),
    textInput(inputId = "Latitude", label = labelMandatory("Roost Latitude")),
    textInput(inputId = "Longitude", label = labelMandatory("Roost Longitude")),
    selectInput("Species", labelMandatory("Species"), c("Please Select", "Barbastella barbastellus", "Eptesicus serotinus", 
                                                        "Eptesicus","Myotis alcathoe", "Myotis bechsteinii", "Myotis brandtii",
                                                        "Myotis daubentonii", "Myotis mystacinus", "Myotis nattereri",
                                                        "Myotis", "Nyctalus leisleri", "Nyctalus noctula", "Nyctalus",
                                                        "Nyctaloid", "Pipistrellus nathusii", "Pipistrellus pipistrellus",
                                                        "Pipistrellus pygmaeus", "Pipistrellus", "Plecotus auritus", 
                                                        "Plecotus austriacus", "Plecotus", "Rhinolophus ferrumequinum",
                                                        "Rhinolophus hipposideros", "Rhinolophus")),
    selectInput("SpeciesConfirmationMethod", labelMandatory("Species Confirmation Method"), c(
      "Please Select", "Visual: Count of every bat in roost", "Visual: Emergency survey", "DNA", "Acoustic")),
    textInput(inputId = "TotalCount", label = labelMandatory("Total Count of Bats")),
    selectInput("StructureType", labelMandatory("Structure Type"), c("Please Select", "Dwelling house", "Building",
                                                                     "Large Building", "Church", "Fortifications",
                                                                     "Barn or outbuilding", "Bridge", "Bat Box", "Tree",
                                                                     "Other", "Unknown")),
    selectInput("primarycat", labelMandatory("Primary Category"), c("Please Select", "Maternity", "Formation", "Hibernation",
                                                                    "Night/feeding", "Mating", "Other")),
    selectInput("sensitivity", labelMandatory("Sensitivity"), c("Please Select", "Public", "Blur to 10km", "Do not publish")),
    selectInput("assignment", labelMandatory("Assignment"), c("Prebreeding", "Postbreeding")),
    textInput(inputId = "county", label = labelMandatory("County")),
    selectInput("minorRegion", labelMandatory("Minor Region"), c("Scotland North", "Scotland East", "Scotland West",
                                                                 "Northern Ireland", "England East & North East", 
                                                                 "England North West & North Wales", "Midlands", "East Anglia",
                                                                 "South Wales & England South West", 
                                                                 "England South East & Central South")),
    selectInput("majorRegion", labelMandatory("Country"), c("Scotland", "Northern Ireland", "England", "Wales")),
    selectInput("save", labelMandatory("Save Options"), c("Please Select",
                                                          "I have not uploaded this data before, please save to database",
                                                          "I have uploaded before, do NOT save to database")),
    textInput(inputId = "Email", label = labelMandatory("Insert Email Address")),
    "The following fields are all optional, if author and site name are filled in, these will appear at the top of your report",
    tags$br(), #hopefully adds a gap
    textInput(inputId = "Author", label = "Insert Author Name"), 
    textInput(inputId = "SiteName", label = "Insert Site Name"), 
    selectInput("geographicRange", "Geographic Range", c("No Limit", "County", "Country")),
    selectInput("timeRange", "Time Range", c("No Limit", "+/- 1 month from survey date")),
    textInput(inputId = "no.of.exits", label = "Number of Exits - optional"),
    textInput(inputId = "Cloud.cover", label = "Cloud Cover - optional"),
    textInput(inputId = "Wind.strength", label = "Wind Strength - optional"),
    textInput(inputId = "Rainfall", label = "Rainfall - optional"),
    textInput(inputId = "Temp", label = "Temperature - optional"),
    textInput(inputId = "EmergenceTime", label = "Emergence Time - optional"),
    textInput(inputId = "Number.of.observers", label = "Number of Observers - optional"),
    textInput(inputId = "Duration", label = "Duration - optional"),
    textInput(inputId = "notes", label = "Notes"),
    strong("*By submitting this form you agree to the saving of your email address in the event the Ecobat team need to contact you"),
    
    tags$hr(), #adds a horizontal line
    
    actionButton("submit", "Submit and Generate Report", class = "btn-primary"),
      
  
    shinyjs::hidden(
      span(id = "submit_msg", "Submitting..."),
      div(id = "error",
          div(br(), tags$b("Error:"), span(id = "error_msg"))
      )
    )
  ),
  
  shinyjs::hidden(
    div(
      id = "thankyou_msg",
      h3("Thanks, your response was submitted successully"),
      actionLink("submit_another", "Submit another response")
    )
  ),
              
    
    tags$hr(), #adds a horizontal line
    
    
    h5(img(src= "TMS_logo.png", heigth=100, width=100, style="display: block") #gets it to display Ecobat logo
    )
)