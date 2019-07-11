library(shiny)

ui <- fluidPage(
  titlePanel("Bodyfat Calculator"),
  helpText("We can only estimate male's bodyfat right now."),
  helpText("if you are a female, you can try the formula below given by Wikipedia:"),
  helpText("1.2 × BMI + 0.23 × age - 5.4"),
  numericInput("Weight", "Weight (lbs):",value = NA),
  helpText("1 kg equals to 2.2 lbs."),
  numericInput("Abdomen", "Abdomen (cm):",value = NA),
  helpText("1 inch equals to 2.54cm"),
  numericInput("Wrist", "Wrist (cm):",value = NA),
  helpText("1 inch equals to 2.54cm"),
  helpText("Following is the reference of the bodyfat of a male."),
  helpText("Athletes: 2-13%"),
  helpText("Fitness: 14-17%"),
  helpText("Average: 18-25%"),
  helpText("Obese: >25%"),
  h3(textOutput("Output"),style = "width: 80% ; height: 100px"),
  helpText("If you have further question about this result, please contact our develop team through 'jzhou345@wisc.edu'."),
  helpText("Any suggestion or advice is warmly welcomed.")
)
server <- function(input, output) {
  BODYFAT = function(wei,abd,wri){
    if(wei <= 0|is.na(wei)){
      return("a")
    }else if(wei >= 400){
      return("d")
    }else if(abd <= 0|is.na(abd)){
      return("b")
    }else if(abd >= 200){
      return("e")
    }else if(wri <= 0|is.na(wri)){
      return("c")
    }else if(wri > 40){
      return("f")
    }else{
      w = ceiling(wei)-wei
      if (w<0.125){
        wei = ceiling(wei)
      }else if(w<0.375){
        wei = ceiling(wei) - 0.25
      }else if(w<0.625){
        wei = ceiling(wei) - 0.5
      }else if(w<0.875){
        wei = ceiling(wei) - 0.75
      }else{
        wei = ceiling(wei) - 1
      }
      bodyfat = -23.02 - 0.09*wei + 0.89*abd - 1.33*wri
      return(bodyfat)
    }
  }
  OUTPUT <- function(bodyfat){
    if(bodyfat == "a"){
      return("There is something wrong with your first input. Check whether it is negative or include any letter.")
    }else if(bodyfat == "b"){
      return("There is something wrong with your second input. Check whether it is negative or include any letter.")
    }else if(bodyfat == "c"){
      return("There is something wrong with your third input. Check whether it is negative or include any letter.")
    }else if(bodyfat == "d"){
      return("Please double check your first input. We are unable to calculate for you if there is no mistake. Sorry about that!")
    }else if(bodyfat == "e"){
      return("Please double check your second input. We are unable to calculate for you if there is no mistake. Sorry about that!")
    }else if(bodyfat == "f"){
      return("Please double check your third input. We are unable to calculate for you if there is no mistake. Sorry about that!")
    }else if(bodyfat <= 2){
      return("Please double check your inputs, a healthy male's bodyfat percentage should not be less than 2!!!")
    }else{
      return(bodyfat)
    }
  }
  output$Output <- renderText({paste(OUTPUT(BODYFAT(input$Weight,input$Abdomen,input$Wrist)))})
}
shinyApp(ui = ui, server = server)






