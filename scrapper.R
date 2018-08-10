get_data <- function() {
  # Load Packages
  library(rvest)
  library(dplyr)
  library(stringr)
  
  # Specify URL
  url <-
    "http://www.bcra.gov.ar/PublicacionesEstadisticas/Principales_variables_datos.asp?descri=1&fecha=Fecha_Serie&campo=Res_Int_BCRA"
  cocorahs <- html_session(url)
  
  # Grab Initial Form
  #  Form is filled in stages
  form.unfilled <- cocorahs %>%
    html_node(".form-horizontal") %>%
    html_form()
  
  form.filled <- form.unfilled %>%
    set_values(desde = "02/01/2003", hasta = "07/08/2018")
  
  # submit the form and save as a new session
  session <- submit_form(cocorahs, form.filled)
  
  # look for a table in the nodes
  table <- session %>% html_nodes("table")
  
  # The table you want
  reservas <- table[[1]] %>%
    html_table(dec = ",") %>%
    rename(fecha = FECHA, reservas = VALOR) %>%
    mutate(
      reservas = as.integer(str_replace(reservas, "\\.", "")),
      fecha = as.Date(fecha, format="%d/%m/%Y")
    )
  
  return(reservas)
  
}
