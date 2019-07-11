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