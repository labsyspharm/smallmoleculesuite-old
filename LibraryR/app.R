library(shiny)
library(dplyr)
library(readr)
library(DT)
library(shiny.semantic)
library(shinyjs)
library(markdown)
library(clipr)
library(rclipboard)

# load tables
selection_table_selectivity = read_csv("input/selection_table_selectivity_edited_121017.csv") %>%
  mutate_at(vars(mean_Kd),
            function(x) signif(x, 2))
selection_table_clindev = read_csv("input/selection_table_clinical_development.csv") %>%
  mutate_at(vars(c(`mean_Kd`, `SD_aff`)),
            function(x) signif(x, 2))
merge_cmpd_info = read_csv("input/cmpd_info_library_designer.csv")
merge_table_geneinfo = read_csv("input/gene_info_library_designer.csv")

# add SD_aff column
selection_table_selectivity$SD_aff = NA

# table for kinase example
ex1 = read_tsv("gene_lists/Dark GPCRs.txt", col_names = FALSE)
ex2 = read_tsv("gene_lists/Dark Ion Channels.txt", col_names = FALSE)
ex3 = read_tsv("gene_lists/Dark_Kinome_20180320.txt", col_names = FALSE)
ex4 = read_tsv("gene_lists/Full_LigandedGenome_20171217.txt", col_names = FALSE)
ex5 = read_tsv("gene_lists/GPCRs.txt", col_names = FALSE)
ex6 = read_tsv("gene_lists/Ion Channels.txt", col_names = FALSE)
ex7 = read_tsv("gene_lists/Kinome.txt", col_names = FALSE)
ex8 = read_tsv("gene_lists/Nuclear Hormone Receptors.txt", col_names = FALSE)
ex9 = read_tsv("gene_lists/Transporters.txt", col_names = FALSE)

gene_lists = mget(paste0("ex", 1:9))
names(gene_lists) = c("Dark GPCRs", "Dark Ion Channels", "Dark_Kinome", "Full Liganded Genome", "GPCRs", "Ion Channels", "Kinome", "Nuclear Hormone Receptors", "Transporters")

# Define genes found in our data
all_genes = union(unique(selection_table_clindev$symbol), unique(selection_table_selectivity$symbol))

# Names of classes in the selectivity table
best = c("Most selective") # Most selective
second = c("Semi-selective") # Semi selective
non = c("Poly-selective") # Poly-selective
un = c("Unknown") # Unknown
none = NULL
# Names of phases in the clinical development table
approved = "approved"
three = c("max_phase_3")
two = c("max_phase_2")
one = c("max_phase_1")

tab.js = "$('.menu .item')
.tab()
;"

about.modal.js = "$('.ui.mini.modal')
.modal({
blurring: true
})
$('#about_modal').modal('show')
;"
genelist.modal.js = "$('.ui.mini.modal')
.modal({
blurring: false
})
$('#genelist_modal').modal('show')
;"
bookmark.modal.js = "$('.ui.mini.modal')
.modal({
blurring: true
})
$('#bookmark_modal').modal('show')
;"

submit.js = '
$("#submitButton").click()
'

server = function(input, output, session) {
  runjs(tab.js)
  
  # Set locale so that sorting works correctly
  Sys.setlocale("LC_COLLATE","en_US.UTF-8")
  
  onRestore(function(state) {
    print("restore")
    for(i in names(state$values)) {
      values[[i]] = state$values[[i]]
    }
  })
  
  onRestored(function(state) {
    print("restored")
    updateQueryString("?")
    runjs(submit.js)
  })
  
  onBookmark(function(state) {
    print("bookmark")
  })
  
  onBookmarked(function(url) {
    print("bookmarked")
    session$sendCustomMessage("bookmark_url", message = url)
    values$url = url
  })
  
  observeEvent(input$bookmark1, {
    session$doBookmark()
  })
  
  # Load "bookmark" modal
  observeEvent(input$bookmark1, {
    runjs(bookmark.modal.js)
  })
  # Load "about" modal
  observeEvent(input$about, {
    runjs(about.modal.js)
  })
  
  # Show gene lists in modal
  observeEvent(input$choose_gene_list, {
    temp_table = gene_lists[[input$choose_gene_list]]
    updateTextAreaInput(session, inputId = "gene_list", value = paste0(temp_table$X1, collapse = "\n"))
    runjs("$('#genelist_modal').modal('hide')")
  }, ignoreInit = TRUE)
  
  # Add clipboard buttons
  output$clip <- renderUI({
    rclipButton("clipbtn", "Copy", values$url, icon("clipboard"))
  })
  
  # Workaround for execution within RStudio
  observeEvent(input$clipbtn, clipr::write_clip(values$url))
  
  ##### For updating URL query string
  # observe({
  #   # Needed to call input to trigger bookmark
  #   all_vars = reactiveValuesToList(input, all.names = T)
  #   # Don't delete above line -- needed for point selection bookmarking
  #   session$doBookmark()
  # })
  
  # Define reactive values
  values = reactiveValues(gene_list = NULL, genes_not_found = NULL, gene_list_found = 0,
                          genes_not_found_paste = NULL, submitted = F,
                          sources = NULL, probes = NULL, clinical = NULL,
                          display_per_cmpd = NULL, display_per_entry = NULL)
  # Make app stop when you close the webpage
  #session$onSessionEnded(stopApp)
  
  # Load "about" modal
  observeEvent(input$about, {
    runjs(about.modal.js)
  })
  
  # Load example gene set of kinases
  observeEvent(eventExpr = c(input$load_example_kinases, input$load_example_kinases2), handlerExpr = {
    runjs(genelist.modal.js)
    }, ignoreInit = T, ignoreNULL = T)
  
  # Jump to results tab when "Submit" is clicked
  observeEvent(input$submitButton, {
    showElement("tab2_top")
    removeClass(id = "tab1_top", class = "active")
    removeClass(id = "tab1_bottom", class = "active")
    addClass(id = "tab2_top", class = "active")
    addClass(id = "tab2_bottom", class = "active")
  })
  
  # Disable suspend for outputs to fix issue with certain outputs not updating 
  # after "submit" button is pushed... probably an issue with "shiny.semantic"
  output$search_genes = renderText("")
  output$gene_total = renderText("")
  outputOptions(output, "search_genes", suspendWhenHidden = FALSE)
  outputOptions(output, "gene_total", suspendWhenHidden = FALSE)
  
  
  # Make sure genes given are in the genes that we have info for
  observeEvent(eventExpr = input$gene_list, handlerExpr = {
    values$gene_list = unlist(strsplit(input$gene_list, "\n"))
    values$genes_not_found = values$gene_list[!values$gene_list %in% all_genes]
    values$genes_not_found_paste = paste(values$genes_not_found, collapse = ", ")
    output$search_genes = renderText({ paste("The following targets do not have a known qualifying ligand, please check HUGO name:", values$genes_not_found_paste) })
    values$gene_list_found = values$gene_list[values$gene_list %in% all_genes]
    output$gene_total = renderText({ paste(length(values$gene_list_found), "target(s) with at least one ligand") })
  })
  
  observeEvent(input$submitButton, {
    values$submitted = T
    # Get probe class selections
    observeEvent(input$probes, {
      print(input$probes)
      values$probes = NULL
      if(!is.null(input$probes)) {
        for(i in 1:length(input$probes)) {
          values$probes = c(values$probes, get(input$probes[i]))
        }
      } else {
        values$probes = NULL
      }
      print(values$probes)
    }, ignoreNULL = F)
    
    # Get clinical phase selections
    observeEvent(input$clinical, {
      print(input$clinical)
      if(!is.null(input$clinical)) {
        values$clinical = NULL
        for(i in 1:length(input$clinical)) {
          values$clinical = c(values$clinical, get(input$clinical[i]))
        }
      } else {
        values$clinical = NULL
      }
      print(values$clinical)
    }, ignoreNULL = F)
    observeEvent(c(input$clinical, input$probes, input$meas, 
                   input$sd, input$affinity, input$legacy), {
                     output_selectivity = selection_table_selectivity %>%
                       filter(symbol %in% values$gene_list_found) %>%
                       filter(source %in% values$probes)
                     output_clindev = selection_table_clindev %>%
                       filter_(~symbol %in% values$gene_list_found) %>%
                       filter_(~source %in% values$clinical) %>%
                       # sliders "sd" and "affinity" are in log10 scale
                       filter_(~mean_Kd <= 10^input$affinity) %>%
                       filter_(~SD_aff <= 10^input$sd | is.na(SD_aff)) %>%
                       filter_(~n_measurement >= input$meas)
                     output_table = rbind(output_selectivity[c("gene_id","molregno","mean_Kd", "SD_aff",
                                                               "n_measurement","source")],
                                          output_clindev[c("gene_id","molregno","mean_Kd", "SD_aff",
                                                           "n_measurement","source")])
                     print(output_table)
                     values$display_per_entry = unique(output_table %>%
                                                         merge(merge_cmpd_info[c("molregno","chembl_id","pref_name","max_phase")],
                                                               by="molregno") %>%
                                                         merge(merge_table_geneinfo,by="gene_id"))
                     
                     values$display_per_entry = values$display_per_entry[c("symbol","chembl_id",
                                                                           "pref_name","source","max_phase","mean_Kd", "SD_aff","n_measurement",
                                                                           "gene_id","tax_id")]
                     values$display_per_entry = values$display_per_entry %>% mutate(
                       symbol = factor(symbol), chembl_id = factor(chembl_id), pref_name = factor(pref_name),
                       source = factor(source), gene_id = factor(gene_id), tax_id = factor(tax_id),
                       max_phase = as.integer(max_phase)
                     ) %>% mutate_at(c("mean_Kd", "SD_aff"), funs(if_else(. > 1, round(.,1), signif(.,2)))) %>%
                       rename(`mean_Kd_(nM)` = mean_Kd, `SD_Kd_(nM)` = SD_aff, reason_included = source)
                     # rounds mean and SD to closest 0.1 if greater than 1.
                     # if less than one, rounds to two significant digits.
                     
                     values$display_per_cmpd = unique(output_table %>%
                                                        merge(merge_cmpd_info[c("molregno","chembl_id","pref_name",
                                                                                "max_phase","alt_names","inchi")], by="molregno") %>%
                                                        merge(merge_table_geneinfo,by="gene_id")) %>%
                       group_by(molregno,chembl_id,pref_name,alt_names,inchi,max_phase) %>%
                       summarise(sources=toString(paste0(symbol,";",source))) %>% as.data.frame %>% mutate(
                         molregno = factor(molregno), chembl_id = factor(chembl_id), pref_name = factor(pref_name),
                         max_phase = as.integer(max_phase)
                       ) %>% rename(reason_included = sources)
                   }, ignoreNULL = F)
  })
  
  # Toggle filters when button is clicked
  observeEvent(input$filter_button, {
    toggleElement(id = "filters", anim = T, animType = "fade")
    toggleElement(id = "filter_down")
    toggleElement(id = "filter_right")
  })
  
  # display correct table
  observeEvent(input$table, {
    if(input$table == "entry") {
      output$output_table = DT::renderDataTable({
        datatable(values$display_per_entry, extensions = c('Buttons', 'FixedHeader'),
                  filter = 'top',
                  rownames = F, options = list(
                    dom = 'lBfrtip',
                    buttons = c('copy', 'csv', 'excel', 'colvis'),
                    initComplete = JS(
                      "function(settings, json) {",
                      "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff', 'width': '100px'});",
                      "}"),
                    searchHighlight = TRUE,
                    fixedHeader = TRUE,
                    autoWidth = TRUE
                  ))
      }, server = FALSE)
    } else {
      output$output_table = DT::renderDataTable({
        datatable(values$display_per_cmpd, extensions = c('Buttons','FixedHeader'),
                  filter = 'top',
                  rownames = F, options = list(
                    dom = 'lBfrtip',
                    buttons = c('copy', 'csv', 'excel', 'colvis'),
                    initComplete = JS(
                      "function(settings, json) {",
                      "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                      "}"),
                    # hide molregno column
                    columnDefs = list(list(visible=FALSE, targets=0)),
                    searchHighlight = TRUE,
                    fixedHeader = TRUE,
                    autoWidth = TRUE
                  ))
      })
    }
  })
  session$allowReconnect("force")
}


#### UI

# logifySlider javascript function
JS.logify <-
  "
// function to logify a sliderInput
function logifySlider (sliderId) {
// regular number style
$('#'+sliderId).data('ionRangeSlider').update({
'prettify': function (num) {
return (Math.pow(10, num).toLocaleString());
}
})
}"

# call logifySlider for each relevant sliderInput
JS.onload <-
  "
// execute upon document loading
$(document).ready(function() {
// wait a few ms to allow other scripts to execute
setTimeout(function() {
// include call for each slider
logifySlider('sd')
logifySlider('affinity')
}, 5)})
"

ui = function(request) {
  semanticPage(
    title = "HMS-LINCS Small Molecule Suite - LibraryR",
    shinyjs::useShinyjs(),
    suppressDependencies("bootstrap"),
    tags$head(tags$script(HTML(JS.logify))),
    tags$head(tags$script(HTML(JS.onload))),
    singleton(
      tags$head(tags$script('Shiny.addCustomMessageHandler("bookmark_url",
                            function(message) {
                            document.getElementById("bookmark_text").value = message;
                            }
      );'))
      ),
    tags$head(rclipboardSetup()),
    # Fix for mobile viewing
    tags$meta(name="viewport", content="width=device-width, initial-scale=1.0"),
    # CSS for sizing of data table search boxes
    inlineCSS(".form-control {
              box-sizing: border-box;
              }"),
    # CSS for slider styling
    tags$style(type = "text/css", "
               .irs-bar {width: 100%; height: 5px; background: black; border-top: 0px solid black; border-bottom: 0px solid black;}
               .irs-bar-edge {background: black; border: 0px solid black; height: 5px; width: 10px; border-radius: 0px;}
               .irs-line {border: 0px solid black; height: 5px; border-radius: 0px;}
               .irs-grid-text {font-family: 'arial'; font-size: 10px;}
               .irs-max {font-family: 'arial'; color: black;}
               .irs-min {font-family: 'arial'; color: black;}
               .irs-single {font-family: 'arial'; color:black; background:white;}
               .irs-slider {width: 20px; height: 20px; top: 17px;}
               "),
    # CSS for hiding border on horizontal segments
    tags$style(type = "text/css", "
               .ui.noshadow.segments {
               box-shadow: none;
               border: none;
               margin-top: 0px;
               margin-bottom: 0px;
               }"
    ),
    div(class = "ui mini modal", id = "about_modal",
        div(class = "actions",
            div(class = "ui red basic circular cancel icon button", uiicon(type = "window close"))
        ),
        div(class = "ui center aligned basic segment",
            includeMarkdown("www/about.md")
        )
    ),
    div(class = "ui mini modal", id = "genelist_modal", style = "width: 450px; hposition: absolute; left: 50%; margin-left: -225px;",
        div(class = "actions",
            div(class = "ui red basic circular cancel icon button", uiicon(type = "window close"))
        ),
        div(class = "ui center aligned basic segment",
            h3("Choose a gene list from the drop-down menu:"),
            dropdown(name = "choose_gene_list", choices = names(gene_lists), default_text = "Choose gene list")
        )
    ),
    div(class = "ui mini modal", id = "bookmark_modal", style = "width: 450px; hposition: absolute; left: 50%; margin-left: -225px;",
        # div(class = "actions",
        #     div(class = "ui red basic circular cancel icon button", uiicon(type = "window close"))
        # ),
        div(class = "ui center aligned basic segment",
            div(class = "ui form",
                div(class = "field",
                    tags$label("Sharing URL:"),
                    tags$input(type = "text", id = "bookmark_text")
                ),
                # UI ouputs for the copy-to-clipboard buttons
                uiOutput("clip", inline = T)
            )
        )
    ),
    div(class = "ui container",
        div(class = "ui top attached inverted five item stackable menu",
            div(class = "ui center aligned container",
                a(class = "item", img(class = "logo", src = "dcic.png"),
                  href = "https://shiny.ilincs.org/apps/hms_small_mol/"),
                a(class = "item", "SelectivitySelectR", href = "http://shiny.ilincs.org/apps/SensitivitySelectR/", style = "font-size: 16px; padding: 5px; margin: 0px;"),
                a(class = "item", "SimilaritySelectR", href = "http://shiny.ilincs.org/apps/SimilaritySelectR/", style = "font-size: 16px; padding: 5px; margin: 0px;"),
                a(class = "item", "LibraryR", href = "http://shiny.ilincs.org/apps/LibraryR/", style = "font-size: 16px; padding: 5px; margin: 0px;"),
                a(class = "item", img(class = "logo", src = "logo_harvard_150.png"),
                  href = "http://sorger.med.harvard.edu" )
            )
        ),
        div(class = "ui main attached segment", style = "margin: 0px;",
            div(class="ui top secondary pointing menu", id = "tabs",
                a(class="item active", `data-tab`="tab1", "Gene input", id = "tab1_top"),
                hidden( a(class="item", `data-tab`="tab2", "Results", id = "tab2_top") )
            ),
            div(class="ui bottom active tab basic segment", `data-tab`="tab1", id = "tab1_bottom",
                div(class = "ui stackable two column centered grid", 
                    div(class = "column", id = "col_input",
                        h4(class="ui header", "Type/paste gene symbols below", uiicon("paste")),
                        a(class = "ui red ribbon label", "Genes (drug targets)"),
                        div(class = "ui form",
                            textAreaInput(inputId = "gene_list", label = "", value = "", placeholder = "MTOR\r\nRPS6KB1\r\nAKT1\r\n...\r\netc.")
                        ),
                        br(),
                        div(class = "ui button red action-button", "Submit", id = "submitButton"),
                        br(), br(),
                        textOutput("gene_total"),
                        br(),
                        textOutput("search_genes"),
                        h4(class="ui horizontal divider header",
                           div(class = "item action-button shiny-bound-input", id = "load_example_kinases",
                               a(class = "action-button", "Or load example", uiicon("lightning"), href = "#")))
                    ),
                    tags$style(type='text/css', "#col_input { min-width: 300px; width: 300px;}"),
                    tags$style(type='text/css', "#instructions { font-size: medium; padding: 0px; margin: 0px;}"),
                    div(class = "column", style = "width: calc(100% - 300px);",
                        div(class = "ui form",
                            h3(class="ui horizontal divider header", uiicon("info circle"), "Instructions"),
                            div(class = "ui container segment basic", id = "instructions",
                                p("Type/paste a list of gene symbols into the text box (or load ",
                                  a(class = "action-button", "one of the example gene-lists", href = "#", id = "load_example_kinases2"), ") and click 'Submit'."),
                                p("Only gene symbols from ", a("HUGO Gene Nomenclature Committee (HGNC)", href = "http://www.genenames.org/"),
                                  " are accepted. Non-HGNC gene symbols and genes for which we lack drug information will be ignored."),
                                p("After submitting your gene list, a downloadable table of drugs targeting those genes will be generated. You may further filter these drugs by selectivity level, FDA approval/clinical phase, and other parameters.")
                            )
                        )
                    )
                )
            ),
            div(class="ui bottom tab basic segment", `data-tab`="tab2", id = "tab2_bottom",
                style = "padding: 0px;",
                div(class = "ui basic segment", style = "padding: 0px;",
                    h4(class="ui horizontal divider header",
                       div(class = "item action-button shiny-bound-input", id = "filter_button",
                           a(class = "action-button", p(uiicon("caret down", id = "filter_right"),
                                                        hidden(uiicon(type = "caret right", id = "filter_down")),
                                                        "Show/hide filters", uiicon(type = "filter")), href = "#")
                       )
                    ),
                    div(class = "ui form", id = "filters",
                        div(class = "ui stackable two column grid container",
                            div(class = "column", id = "col01and2",
                                div(class = "row",
                                    div(class = "ui noshadow horizontal segments",
                                        tags$style(type='text/css', "#col01and2 { min-width: 550px; width: 550px; border-left: 0px;
                                                   border-right: 0px;}"),
                                        tags$style(type='text/css', "#col0 { min-width: 100px; width: 100px; border-left: 0px;
                                                   border-right: 0px;}"),
                                        tags$style(type='text/css', "#col1 { min-width: 250px; width: 250px; border-left: 0px;
                                                   border-right: 0px;}"),
                                        tags$style(type='text/css', "#col2 { min-width: 200px; width: 200px; border-left: 0px;
                                                   border-right: 0px;}"),
                                        tags$style(type='text/css', "#col3 { min-width: 300px; width: 300px;}"),
                                        tags$style(type='text/css', "#col4 { min-width: 100px; width: 100px; border-left: 0px;
                                                   border-right: 0px;}"),
                                        tags$style(type='text/css', "#col5 { min-width: 200px; width: 200px; border-left: 0px;
                                                   border-right: 0px;}"),
                                        tags$style(type='text/css', "#steps { font-size: medium; }"),
                                        div(class = "ui basic compact segment", id = "col0",
                                            h2(class = "ui header",
                                               uiicon("bullseye"),
                                               div(class = "content", 1)
                                            )
                                        ),
                                        div(class = "ui basic segment", id = "col1",
                                            p("Choose the selectivity levels for which you want chemical probes to be included in the library.", id = "steps")
                                        ),
                                        div(class = "ui basic segment", id = "col2",
                                            a(class = "ui red label", "Probes"),
                                            selectizeInput("probes", "", choices = list(`Most selective` = "best", 
                                                                                        `Semi-selective` = "second", `Poly-selective` = "non", 
                                                                                        `Unknown` = "un"), selected = "best", multiple = T, options = list(
                                                                                          'plugins' = list('remove_button')))
                                        )
                                        )
                                        ),
                                div(class = "row",
                                    div(class = "ui noshadow horizontal segments",
                                        div(class = "ui basic compact segment", id = "col0",
                                            h2(class = "ui header",
                                               uiicon("pie chart"),
                                               div(class = "content", 2)
                                            )
                                        ),
                                        div(class = "ui basic segment", id = "col1",
                                            p("Select compounds in clinical development to be added to the library.", id = "steps")
                                        ),
                                        div(class = "ui basic segment", id = "col2",
                                            a(class = "ui red label", "Maximum clinical phase"),
                                            selectizeInput("clinical", "", choices = list(Approved = "approved", 
                                                                                          `Phase III` = "three", `Phase II` = "two", `Phase I` = "one"), 
                                                           selected = "approved", multiple = T, options = list(
                                                             'plugins' = list('remove_button')))
                                        )
                                    )
                                ),
                                div(class = "row",
                                    div(class = "ui noshadow horizontal segments",
                                        div(class = "ui basic compact segment", id = "col0",
                                            h2(class = "ui header",
                                               uiicon("add square"),
                                               div(class = "content", 3)
                                            )
                                        ),
                                        div(class = "ui basic segment", id = "col1",
                                            p("Select compounds that are endorsed by other users to be added to the library", id = "steps")
                                        ),
                                        div(class = "ui basic segment", id = "col2",
                                            a(class = "ui red label", "Expert opinion compounds"),
                                            selectizeInput("legacy", "", choices = c(`Gray best inhibitor list` = "gray",
                                                                                     `chemicalprobes.org 4.0 star rating` = "chem_probe"), multiple = T, options = list(
                                                                                       'plugins' = list('remove_button')))
                                        )
                                    )
                                )
                                    ),
                            div(class = "centered column", id = "col3",
                                div(class = "row",
                                    div(class = "ui noshadow horizontal segments",
                                        div(class = "ui basic segment", id = "col4",
                                            h2(class = "ui header",
                                               uiicon("options"),
                                               div(class = "content", 4)
                                            )
                                        ),
                                        div(class = "ui basic segment", id = "col5",
                                            p("Refine binding filters for compound in clinical development.", id = "steps")
                                        )
                                    )
                                ),
                                div(class = "ui center aligned basic compact segment", style="
                                    padding-top: 0px; margin: 0px;",
                                    div(class = "row",
                                        sliderInput(inputId = "affinity", label = "Maximum Kd for query target (nM)",
                                                    min = log10(10), max = log10(10000), value = log10(1000))
                                    ),
                                    div(class = "row",
                                        sliderInput(inputId = "meas", label = "Minimum number of measurements",
                                                    min = 1, max = 40, value = 2)
                                    ),
                                    div(class = "row",
                                        sliderInput(inputId = "sd", label = "Maximum std. dev. of Kd (nM)",
                                                    min = log10(10), max = log10(100000), value = log10(100))
                                    )
                                )
                )
                                )
                ),
                h4(class="ui horizontal divider header", "Output table", uiicon("table"), style = "margin: 0px;"),
                div(class = "ui one column centered grid",
                    div(class = "column",
                        radioButtons(inputId = "table", "", choiceNames = c("Display per entry", "Display per compound"),
                                     choiceValues = c("entry", "cmpd"), inline = T),
                        br(),
                        DT::dataTableOutput("output_table"),
                        div(class = "ui grey button action-button shiny-bound-input", id = "bookmark1", "Bookmark...", uiicon("linkify")
                        )
                    )
                )
                            )
    )
        ),
    div(class = "ui bottom attached inverted footer segment", style = "margin: 0px;",
        div(class = "ui center aligned container",
            div(class = "ui horizontal inverted large divided link list",
                a(class = "item", div(class = "action-button", "About", id = "about") ),
                a(class = "item", "Contact Us"),
                a(class = "item", "Github", uiicon("github"), href = "https://github.com/sorgerlab/drug_browser")
            )
        )
    )
        )
    )
}

shinyApp(ui, server, enableBookmarking = "server")