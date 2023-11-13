box::use(
  htmlwidgets[JS]
)

#' @export
left_label_formatter <- JS(
  "function(params){
      return(
        'Power: ' + params.value[1] +
        '<br/>Effect size: ' + params.value[0]
      )
    }"
)

#' @export
right_label_formatter <- JS(
  "function(params){
      return(
        'Power: ' + params.value[1] +
        '<br/>Effect size: ' + params.value[0]
      )
    }"
)

