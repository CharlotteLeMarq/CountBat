# ACurrent error when pressing submit button: "Couldn not find function Save Data"

#for refreence this is what I have been using to try and make this form: https://deanattali.com/2015/06/14/mimicking-google-form-shiny/

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
library(suncalc)
library(ggforce)
library(janitor)
library(rdrop2)
library(devtools)
library(tinytex) #need for creating a pdf
library(shinyjs)

fieldsMandatory <- c("Source", "SurveyDate", "Latitude", "Longitude", "Species", "SpeciesConfirmationMethod",
                     "TotalCount", "StructureType", "primarycat", "sensitivity", "assignment", "county", 
                     "minorRegion", "majorRegion", "save", "Email")


server = function(input, output, session) {
  
  #Enable the submit button when all mandatory fields are filled out
  observe({
    mandatoryFilled <- 
      vapply(fieldsMandatory, 
             function(x) {
               !is.null(input[[x]]) && input[[x]] !=""
             },
             logical(1))
               
          mandatoryFilled <- all(mandatoryFilled)
          
          shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
          
  })
  
  #gather all the form inputs (and add timestamp)
  formData <- reactive({
    data <- sapply(fieldsAll, function(x) input[[x]])
    data <- c(data, timestamp = epochTime())
    data <- t(data)
    data
  })
  
  #when the submit button is clicked, submit the response
  observeEvent(input$submit, {
  
    #User-experience stuff
    shinyjs::disable("submit")
    shinyjs::show("submit_msg")
    shinyjs::hide("error")
    
    #save the data (show an error message in case of error)
    tryCatch({
      saveData(formData())
      shinyjs::reset("form")
      shinyjs::hide("form")
      shinyjs::show("thankyou_msg")
    },
    
    error = function(err) {
      shinyjs::html("error_msg", err$message)
      shinyjs::show(id = "error", anim = TRUE, animType = "fade")
    },
    
    finally = {
      shinyjs::enable("submit")
      shinyjs::hide("submit_msg")
    })
    
  })

  #submit another response
  observeEvent(input$submit_another, {
    shinyjs::show("form")
    shinyjs::hide("thankyou_msg")
  })
  
}



  
  
