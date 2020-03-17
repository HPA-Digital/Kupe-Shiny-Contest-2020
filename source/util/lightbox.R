infoLightboxBtn <- function(id, width=NULL){
  btn <- img(src="img/util/Info_btn.svg", class="info-btn", style=
             if(!is.null(width)){paste0("width: ", width, "px;", " height: ", width, "px;")})
  lightboxBtn(id, btn)
}

lightboxBtn <- function(id, item){
  htmltools::tagAppendAttributes(item,
                                 'data-toggle'="modal",
                                 'data-target'=paste0("#",id))
}

lightboxBody <- function(id, content, title=""){
    div(class="modal fade", id=id, tabindex="-1", role="dialog",
        div(class="modal-dialog modal-lg", role="document",
            div(class="modal-content",
                div(class="modal-header container-fluid", style="width: 90%; border-bottom: none",
                    div(class="col-sm-10 pull-left",
                        HTML(paste0('<h3 class="large-title">', title,'</h3>'))
                        ),
                    div(class="col-sm-2 pull-right",
                        HTML('<button type="button" class="close" data-dismiss="modal" aria-label="Close"><span aria-hidden="true">&times;</span></button>')
                        )
                ),
                div(class="topic-overview-binding modal-body",
                    content
                )
            )
        )
    )
}