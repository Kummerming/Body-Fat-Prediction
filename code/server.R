library(shiny)
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