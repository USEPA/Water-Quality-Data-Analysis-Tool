

# Module UI function
menuitemUI <- function(id, label, shape) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  menuItem(ns("sidemenu"),
           text = label,
           icon = icon(shape))
  
}





# 
# 
# # Module UI function
# menuitemUI <- function(id, label, icons = "table") {
#   # Create a namespace function using the provided id
#   ns <- NS(id)
#   menuItem(ns("sidemenu"), text = label, icon = icon(icons))
#   
# }































