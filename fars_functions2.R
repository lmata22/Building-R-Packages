fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}


#'La funcion crea el nombre del archivo, cambia el año por un entero y se agrega a una cadena
#' Name file
#'
#' @param year 
#'
#' @return
#' devuelve un vector de caracteres
#'
#' @examples
#' \dontrun{
#' make_filename(2013)
#' make_filename(2014)
#' make_filename(2015)
#'}
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}



#'la funcion lee los datos, mes y año del archivo de entrada y despues almacena los datos en una lista
#' \code{make_filename} from within
#'
#' @param years 
#' vector numerico de los años analizados
#' 
#'
#' @return 
#' devuelve una lista de los años seleccinados que contiene el mes y el año

#' @examples
#' \dontrun{
#' fars_read_years(2013)
#' fars_read_years(c(2013,2014))
#' fars_read_years(2013:2015)
#'}
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr select
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(.data$MONTH, .data$year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}




#' 
#' la funcion guarda los accidentes de cada año y cada mes
#'
#' @param years
#'
#' @return 
#'devuelve los meses como filas y los años como columnas y cada celda es el numero de accidentes en el mes y el año que corresponda
#'
#'
#' @examples
#' \dontrun{
#' fars_summarize_years(2013)
#' fars_summarize_years(c(2013,2014))
#' fars_summarize_years(2013:2015)
#'}
#' @importFrom dplyr bind_rows
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom tidyr spread
#'
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(.data$year, .data$MONTH) %>%
    dplyr::summarize(n = dplyr::n()) %>%
    tidyr::spread(.data$year, .data$n)
}



#' la funcion traza un mapa con la ubicadion de cada accidente
#' 
#' \code{fars_read} from within. T
#'
#' @param state.num 
#' da el codigo FARS
#' 
#' @param year 
#'
#' @return 
#' devuelve un mapa con las ubicaciones de los accidentes
#'
#' @examples
#' \dontrun{
#' fars_summarize_years(2013)
#' fars_summarize_years(c(2013,2014))
#' fars_summarize_years(2013:2015)
#'}
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#' @importFrom rlang .data
#'
#' @export
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)
  
  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, .data$STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}
