expandingContent <- function(id, text){

  div(id=id, class="expanding-content",
    div(class="content-block collapse", id=paste0(id, "-content"), "aria-expanded"="false",
      HTML(text)
      ),
    a(role="button", class="collapsed", "data-toggle"="collapse", href=paste0("#",id,"-content"),
      "aria-expanded"="false", "aria-controls"=paste0(id,"-content"))
  )
}