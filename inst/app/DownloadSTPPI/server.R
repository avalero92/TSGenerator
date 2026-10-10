function(input, output, session) {
  result <- reactiveVal(NULL); status <- reactiveVal("Ready.")
  make_client <- function() {
    u <- if (nzchar(input$user)) input$user else NULL
    p <- if (nzchar(input$password)) input$password else NULL
    TSGenerator::hda_client(username=u, password=p)
  }
  observeEvent(input$check, {
    output$diag <- renderPrint(TSGenerator::check_wekeo(online=TRUE, client=make_client()))
  })
  run_request <- function(do_download) {
    req(length(input$product)>0, nzchar(input$tile))
    if (do_download && !nzchar(trimws(input$outdir))) {
      status("Choose an output directory before downloading."); return(invisible(NULL))
    }
    status(if (do_download) "Downloading..." else "Searching...")
    x <- TSGenerator::download_st(start=input$start, end=input$end, output_dir=input$outdir,
      product=input$product, tile_id=input$tile, client=make_client(), download=do_download,
      overwrite=isTRUE(input$overwrite), prompt=FALSE)
    result(x); status(if (do_download) "Download completed." else "Preview completed.")
  }
  observeEvent(input$preview, { tryCatch(run_request(FALSE), error=function(e) status(conditionMessage(e))) })
  observeEvent(input$download, { tryCatch(run_request(TRUE), error=function(e) status(conditionMessage(e))) })
  output$summary <- renderTable({ req(result()); result()$summary }, striped=TRUE, bordered=TRUE)
  output$status <- renderText(status())
}
