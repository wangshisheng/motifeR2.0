library(shiny)
library(shinyjs)
library(shinyBS)
library(shinyWidgets)
library(openxlsx)
library(gdata)
library(ggsci)
library(DT)
library(data.table)
library(Biostrings)
library(stringi)
library(stringr)
library(msa)
library(rmotifx)
library(ggseqlogo)
library(igraph)
library(ggraph)
library(ggplot2)
library(ggrepel)
library(tidyr)
library(graphlayouts)
library(scales)
library(impute)
library(igraph)
library(ggraph)
library(scatterpie)
library(plotfunctions)
library(mapplots)
colpalettes<-unique(c(pal_npg("nrc")(10),pal_aaas("default")(10),pal_nejm("default")(8),pal_lancet("lanonc")(9),
                      pal_jama("default")(7),pal_jco("default")(10),pal_ucscgb("default")(26),pal_d3("category10")(10),
                      pal_locuszoom("default")(7),pal_igv("default")(51),
                      pal_uchicago("default")(9),pal_startrek("uniform")(7),
                      pal_tron("legacy")(7),pal_futurama("planetexpress")(12),pal_rickandmorty("schwifty")(12),
                      pal_simpsons("springfield")(16),pal_gsea("default")(12)))
#
ui<-renderUI(
  fluidPage(
    title="motifeR2.0",
    shinyjs::useShinyjs(),
    fluidRow(div(
      HTML(
        "<div style='text-align:center;margin-top:20px;margin-right:0px'>
          <a href='#' target=''><img src='motifeR2ti.png' width='200px'>
          </a>
          </div>"
      )
    )),
    tagList(
      tags$head(
        tags$link(rel="stylesheet", type="text/css",href="busystyle.css"),
        tags$script(type="text/javascript", src = "busy.js"),
        tags$style(type="text/css", "
                           #loadmessage {
                     position: fixed;
                     top: 0px;
                     left: 0px;
                     width: 100%;
                     height:100%;
                     padding: 250px 0px 5px 0px;
                     text-align: center;
                     font-weight: bold;
                     font-size: 100px;
                     color: #000000;
                     background-color: #D6D9E4;
                     opacity:0.6;
                     z-index: 105;
                     }
                     "),
        tags$script('
                            var dimension = [0, 0];
                    $(document).on("shiny:connected", function(e) {
                    dimension[0] = window.innerWidth;
                    dimension[1] = window.innerHeight;
                    Shiny.onInputChange("dimension", dimension);
                    });
                    $(window).resize(function(e) {
                    dimension[0] = window.innerWidth;
                    dimension[1] = window.innerHeight;
                    Shiny.onInputChange("dimension", dimension);
                    });
                    '),
        tags$style(type="text/css", "
                   #tooltip {
			position: absolute;
			border: 1px solid #333;
			background: #fff;
			padding: 1px;
			color: #333;
      display: block;
      width:300px;
      z-index:5;
		}
                   "),#F5F5DC
        tags$style(type="text/css", "
                   #tooltip2 {
			position: absolute;
			border: 1px solid #333;
			background: #fff;
			padding: 1px;
			color: #333;
      display: block;
      width:300px;
      z-index:5;
		}
                   "),
        tags$style(type="text/css", "
                   #tooltip3 {
			position: absolute;
			border: 1px solid #333;
			background: #fff;
			padding: 1px;
			color: #333;
      display: block;
      width:300px;
      z-index:5;
		}
                   "),
        tags$style(type="text/css", "
                   #tooltip4 {
			position: absolute;
			border: 1px solid #333;
			background: #fff;
			padding: 1px;
			color: #333;
      display: block;
      width:300px;
      z-index:5;
		}
                   "),
        tags$style(type="text/css", "
                   #tooltip5 {
			position: absolute;
			border: 1px solid #333;
			background: #fff;
			padding: 1px;
			color: #333;
      display: block;
      width:300px;
      z-index:5;
		}
                   "),
        tags$style(type="text/css", "
                   #tooltip6 {
			position: absolute;
			border: 1px solid #333;
			background: #fff;
			padding: 1px;
			color: #333;
      display: block;
      width:300px;
      z-index:5;
		}
                   ")
      )
    ),
    
    conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                     tags$div(h2(strong("Calculating......")),img(src="rmd_loader.gif"),id="loadmessage")),
    tabsetPanel(
      tabPanel(
        "Welcome",
        uiOutput("welcomeui")
      ),
      tabPanel(
        "Import Data",
        sidebarLayout(
          sidebarPanel(
            width=3,
            h3(
              "Step 1: Import Sequence Data",
              tags$span(
                id = 'span1',
                `data-toggle` = "tooltip",
                title = '
                In this part, users can upload their own peptide sequences with modification (e.g. phosphorylation). The example data can be found when users click "Load example data" below.
                ',
                tags$span(class = "glyphicon glyphicon-question-sign")
              )
            ),
            prettyRadioButtons(
              inputId = "loadseqdatatype",
              label = "",
              thick = TRUE,
              choices = list("Import modified sequences" = 1,"Load example data"=2),
              animation = "pulse",
              status = "info",
              selected = 1,
              inline = TRUE
            ),
            hr(),
            #radioButtons(
            #  "loadseqdatatype",
            #  label = "",
            #  choices = list("Import modified sequences" = 1,"Load example data"=2),
            #  selected = 1,
            #  inline = TRUE
            #),
            #tags$hr(style="border-color: grey;"),
            conditionalPanel(
              condition = "input.loadseqdatatype==1",
              radioButtons(
                "metabopathfileType_Input",
                label = h5("1. File format:"),
                choices = list(".xlsx" = 1,".xls"=2, ".csv/txt" = 3),
                selected = 1,
                inline = TRUE
              ),
              fileInput('metabopathfile1', h5('1.1. Import your data:'),
                        accept=c('text/csv','text/plain','.xlsx','.xls')),
              checkboxInput('metabopathheader', '1.2. Header ?', TRUE),
              checkboxInput('metabopathfirstcol', '1.3. First column ?', FALSE),
              conditionalPanel(condition = "input.metabopathfileType_Input==1",
                               numericInput("metabopathxlsxindex",h5("1.4. Sheet index:"),value = 1)),
              conditionalPanel(condition = "input.metabopathfileType_Input==2",
                               numericInput("metabopathxlsxindex",h5("1.4. Sheet index:"),value = 1)),
              conditionalPanel(condition = "input.metabopathfileType_Input==3",
                               radioButtons('metabopathsep', h5('1.4. Separator:'),
                                            c(Comma=',',
                                              Semicolon=';',
                                              Tab='\t',
                                              BlankSpace=' '),
                                            ','))
            ),
            conditionalPanel(
              condition = "input.loadseqdatatype==2",
              downloadButton("loadseqdatadownload1","1. Download example modified peptide sequence data",style="color: #fff; background-color: #6495ED; border-color: #6495ED")
            ),
            tags$hr(style="border-color: grey;"),
            selectInput("origidatatype",h5("2. Data type:"),choices = c("Normal","MaxQuant","Spectronaut")),
            bsTooltip("origidatatype",'The original post-translational modification (PTM) data obtained from which kind of search software. If you have processed the PTM data with standard format (e.g. NPT#Y#GSWFTEK), you should choose the "Normal", otherwise, if your PTM data are obtained from MaxQuant or Spectronaut, you should choose the relative type.',
                      placement = "right",options = list(container = "body")),
            textInput("centralres",h5("3. Central amino acid:"),value = "STY"),
            bsTooltip("centralres",'The central residue that users want to analyze, for example, phosphorylation motif analysis, can center on phosphorylated S, T or Y residues. If they want to analyze multi motif sites, here should be "STY".',
                      placement = "right",options = list(container = "body")),
            div(id="centralresfuhao_div",textInput("centralresfuhao",h5("4. Label of modification:"),value = "#")),
            bsTooltip("centralresfuhao_div",'The label represents modification, users can use some label they like, such as "#", "@", where "#" is recommended.',
                      placement = "right",options = list(container = "body")),
            div(id="minseqs_div",numericInput("minseqs",h5("5. Width:"),value = 7)),
            bsTooltip("minseqs_div",'It is the number of left/right side characters of the central residue. The default is "7" but can be changed by the user.',
                      placement = "right",options = list(container = "body")),
            tags$hr(style="border-color: grey;"),
            div(id="xuanzebgdatabase_div",radioButtons("xuanzebgdatabase",label=h5("6. Select or upload the background data:"),choices = list("6.1. Select" = 1,"6.2. Upload"=2),
                                                       selected = 1,inline = TRUE)),
            bsTooltip("xuanzebgdatabase_div","Background data here means the whole protein sequences of one species in the fasta format.'Select' means users can directly select one species whose protein sequences have been built in motifeR 2.0; 'Upload' means users can upload their own protein sequences, which can be downloaded from some public database, such as UniProt (http://www.uniprot.org).",
                      placement = "right",options = list(container = "body")),
            conditionalPanel(
              condition = "input.xuanzebgdatabase==1",
              uiOutput("metabopathspecies")
            ),
            conditionalPanel(
              condition = "input.xuanzebgdatabase==2",
              fileInput('fastafileown', h5('Please upload your fasta file:'),accept=c('.fasta')),
              textInput("wuzhongid",h5("Taxonomic identifier of uploaded species:"),value = "",placeholder="10116")
            )
          ),
          mainPanel(
            width = 9,
            hr(),
            h4("Sequence data:"),
            dataTableOutput("seqrawdata")
            #actionButton("mcsbtn_seqrawdata","Calculate",icon("paper-plane"),
            #             style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
            #radioButtons(
            #  "seqrawdataxuanze",
            #  label = h4(""),
            #  choices = list("Original data" = 1,"Annotated data"=2),
            #  selected = 1,
            #  inline = TRUE
            #),
            #tags$hr(style="border-color: grey;"),
            #conditionalPanel(
            #  condition = 'input.seqrawdataxuanze==1',
            #  dataTableOutput("seqrawdata")
            #),
            #conditionalPanel(
            #  condition = 'input.seqrawdataxuanze==2',
            #  downloadButton("seqannotatedatadl","Download"),
            #  dataTableOutput("seqannotatedata")
            #)
          )
        )
      ),
      tabPanel(
        "Pre-alignment",
        sidebarLayout(
          sidebarPanel(
            width = 3,
            h3(
              "Step 2: Pre-alignment",
              tags$span(
                id = 'span2',
                `data-toggle` = "tooltip2",
                title = '
                This step aligns those peptide sequences with the background database (protein sequences) and force the modified sites/residues to be central sites, then users can get the standard peptide window sequences.
                ',
                tags$span(class = "glyphicon glyphicon-question-sign")
              )
            ),
            div(id="seqalignif_div",checkboxInput('seqalignif', '1. Pre-aligned or not ?', TRUE)),
            bsTooltip("seqalignif_div","Whether to pre-align your sequences. If your sequences are standard (e.g. 15 length amino acids), you can unselect this parameter. Default is true.",
                      placement = "right",options = list(container = "body")),
            #div(id="classicmultisiteif_div",checkboxInput('classicmultisiteif', '2. Classical multiple sites analysis or not ?', TRUE)),
            #bsTooltip("classicmultisiteif_div",'Whether to process classical analysis. Classical analysis means not replacing the other modified sites with letter "Z" after pre-alignment, for example "TSLWNPT#Y#GSWFTEK" to "TSLWNPTYGSWFTEK", not to "TSLWNPZYGSWFTEK". If true, do not process transformation, otherwise, transformation.',
            #          placement = "right",options = list(container = "body")),
            #div(id="seqalignhanif_div",checkboxInput('seqalignhanif', '3. Check if containing some regular sequence ?', FALSE)),
            #bsTooltip("seqalignhanif_div",'If users want to check whether the aligned peptides contain some specific sequences, for example, you want to find those peptides whose 3th and 5th position are R (arginine), then you can select this parameter and type in a simple regular expression, like "^\\\\w{2}R\\\\w{1}R". Otherwise, you just unselect it.',
            #          placement = "right",options = list(container = "body")),
            #conditionalPanel(
            #  condition = "input.seqalignhanif==true",
            #  textInput("seqalignhan","Regular expression:",value = "^\\w{2}R\\w{1}R")
            #),
            tags$hr(style="border-color: grey;"),
            actionButton("mcsbtn_seqalign","Calculate",icon("paper-plane"),
                         style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
          ),
          mainPanel(
            width = 9,
            radioButtons(
              "prealignxuanze",
              label = h4(""),
              choices = list("Alignment results" = 1,"Sites number distribution plot"=2),
              selected = 1,
              inline = TRUE
            ),
            tags$hr(style="border-color: grey;"),
            conditionalPanel(
              condition = "input.prealignxuanze==1",
              fluidRow(
                column(
                  2,
                  downloadButton("seqduiqidl","Download")
                ),
                column(
                  2,
                  actionButton("mcsbtn_resjieshi","Result description",icon("file-alt"),
                               style="color: black; background-color: #E6E6FA; border-color: #E6E6FA")
                )
              ),
              dataTableOutput("seqduiqi")
            ),
            conditionalPanel(
              condition = "input.prealignxuanze==2",
              h4("Plot:"),
              downloadButton("seqduiqiplotdl","Download"),
              plotOutput("seqduiqiplot",height = "800px")#,
              #tags$hr(style="border-color: grey;"),
              #h4("Multi-Sites Data:"),
              #fluidRow(
              #  column(
              #    2,
              #    downloadButton("seqduiqiduositedl","Download")
              #  ),
              #  column(
              #    2,
              #    actionButton("mcsbtn_resjieshi2","Result description",icon("file-alt"),
              #                 style="color: black; background-color: #E6E6FA; border-color: #E6E6FA")
              #  )
              #),
              #dataTableOutput("seqduiqiduosite")
            )
          )
        )
      ),
      tabPanel(
        "Blast",
        sidebarLayout(
          sidebarPanel(
            width = 3,
            h3(
              "Step3: Blast to Human proteins",
              tags$span(
                id = 'span3',
                `data-toggle` = "tooltip3",
                title = '
                This step will map the PTM site and protein sequences and identifiers between non-human species and Human.
                ',
                tags$span(class = "glyphicon glyphicon-question-sign")
              )
            ),
            div(id="evalueyuzhi_div",numericInput("evalueyuzhi",h5("1. Expectation value (E) threshold:"),value = 0.00001)),
            bsTooltip("evalueyuzhi_div",'Expectation value (E) threshold for saving hits.',
                      placement = "bottom",options = list(container = "body")),
            #div(id="similaryuzhi_div",numericInput("similaryuzhi",h5("2. Expectation value (E) threshold:"),value = 70)),
            #bsTooltip("similaryuzhi_div",'Expectation value (E) threshold for saving hits.',
            #          placement = "bottom",options = list(container = "body"))
            div(id="preblastif_div",checkboxInput("preblastif",h5("2. Using pre-blast results or not?"),TRUE)),
            bsTooltip("preblastif_div",'Please Note: This step is a little time-consuming! If true, this tool will retrieve the pre-blast results and it will be faster to finish this step. Otherwise, this tool will blast the uploaded sequences against human protein sequences, which will take more time.',
                      placement = "bottom",options = list(container = "body")),
            div(id="centeraamatach_div",selectInput("centeraamatach",h5("3. Central amino acid matching degree:"),choices = c("Exact matching"=1,"Fuzzy matching"=2,"All"=3))),
            bsTooltip("centeraamatach_div",'The matching degree of central amino acids (CAAs) when the uploaded peptides are blasted to Human prtein sequences. 1. Exact matching: The CAAs are same, for example, the CAA is "S" in the uploaded peptides and the CAA is also "S" in the blasted sequence. 2. Fuzzy matching: For example, the CAA is "S" in the uploaded peptides and the CAA could be "S", "T", or "Y" in the blasted sequence. All: Reporting all results.',
                      placement = "right",options = list(container = "body")),
            div(id="seqmatachsimilar_div",numericInput("seqmatachsimilar",h5("4. Sequence windows similarity:"),value = 7)),
            bsTooltip("seqmatachsimilar_div",'The similarity of sequence windows between the uploaded peptides and the blasted peptides. For example, there are 15 amino acids in one sequence window, 7 here means there are 7 amino acids are exactly same (amino acids names and positions in both windows are all same).',
                      placement = "right",options = list(container = "body")),
            tags$hr(style="border-color: grey;"),
            actionButton("blastbtn_seqalign","Calculate",icon("paper-plane"),
                         style="color: #fff; background-color: #337ab7; border-color: #2e6da4")#,
            #h5("Please Note: This step is a little time-consuming!")
          ),
          mainPanel(
            width = 9,
            h4("The blast results:"),
            tags$hr(style="border-color: grey;"),
            fluidRow(
              column(
                2,
                downloadButton("blastresdl","Download")
              ),
              column(
                2,
                actionButton("mcsbtn_resjieshi2_1","Result description",icon("file-alt"),
                             style="color: black; background-color: #E6E6FA; border-color: #E6E6FA")
              )
            ),
            dataTableOutput("blastres")
          )
        )
      ),
      tabPanel(
        "Motif Enrichment",
        sidebarLayout(
          sidebarPanel(
            width = 3,
            h3(
              "Step4: Motif Enrichment and Plot",
              tags$span(
                id = 'span4',
                `data-toggle` = "tooltip4",
                title = '
                This step will find overrepresented sequence motifs and visualize them.
                ',
                tags$span(class = "glyphicon glyphicon-question-sign")
              )
            ),
            #div(id="motifquanbuif_div",checkboxInput("motifquanbuif","Species data as background ?",TRUE)),
            #bsTooltip("motifquanbuif_div",'If you upload your own fasta file as background database in the "Import Data" step, you can ignore this parameter (select or unselect is same). Otherwise, if you choose the database in our system (i.e., human) in the 'Import Data' step, selecting this parameter means this software will take the database in our system as background database. If you don't choose, the software will take the foreground data as background database.',
            #          placement = "bottom",options = list(container = "body")),
            #div(id="onlymultisiteif_div",checkboxInput("onlymultisiteif","Only use multi-site data ?",FALSE)),
            #bsTooltip("onlymultisiteif_div",'If selected, this tool will only take the peptides with multi modification sites as foreground data, that is, it will use the sequences in the Seqwindows_MultiSites column obtain from "Pre-alignment" step as foreground data.',
            #          placement = "bottom",options = list(container = "body")),
            div(id="minseqsnum_div",numericInput("minseqsnum",h5("1. Minimum number:"),value = 20)),
            bsTooltip("minseqsnum_div","This threshold refers to the minimum number of times you wish each of your extracted motifs to occur in the data set.",
                      placement = "right",options = list(container = "body")),
            div(id="pvalcutoff_div",numericInput("pvalcutoff",h5("2. P-value threshold:"),value = 0.000001,min = 0)),
            bsTooltip("pvalcutoff_div","The p-value threshold for the binomial probability. This is used for the selection of significant residue/position in the motif.",
                      placement = "right",options = list(container = "body")),
            div(id='enrichseqnum_div',textInput("enrichseqnum",h5("3. Motif index for plot:"),value = "1")),
            bsTooltip("enrichseqnum_div",'Which motif would be plotted. If users only type in one number, it will plot the relative motif. If users type in "1-10", it will plot the 1th to 10th motifs.',
                      placement = "bottom",options = list(container = "body")),
            div(id='equalheightif_div',checkboxInput("equalheightif",h5("4. Equal height or not?"),FALSE)),
            bsTooltip("equalheightif_div",'Whether all residues in the figure have equal height. Default is false.',
                      placement = "bottom",options = list(container = "body")),
            numericInput("motifplot_height",h5("5. Figure height:"),value = 800),
            tags$hr(style="border-color: grey;"),
            actionButton("mcsbtn_motifquanbu","Calculate",icon("paper-plane"),
                         style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
          ),
          mainPanel(
            width = 9,
            radioButtons(
              "motiffujidfxuanze",
              label = h4(""),
              choices = list("1. Uploaded peptide motifs" = 1,"2. Blasted peptide motifs"=2),
              selected = 1,
              inline = TRUE
            ),
            tags$hr(style="border-color: grey;"),
            conditionalPanel(
              condition = 'input.motiffujidfxuanze==1',
              radioButtons(
                "motifplotxuanze1",
                label = h4(""),
                choices = list("1.1 Uploaded peptide motif table" = 1,"1.2 Uploaded peptide motif plot"=2),
                selected = 1,
                inline = TRUE
              ),
              hr(),
              conditionalPanel(
                condition = "input.motifplotxuanze1==1",
                hidden(
                  div(
                    id="motiffujidfxuanze_btn",
                    #h4("1. Uploaded peptide motif enrichment results:"),
                    fluidRow(
                      column(
                        2,
                        downloadButton("motiffujidl","Download")
                      ),
                      column(
                        2,
                        actionButton("mcsbtn_resjieshi3","Result description",icon("file-alt"),
                                     style="color: black; background-color: #E6E6FA; border-color: #E6E6FA")
                      )
                    ),
                    dataTableOutput("motiffuji")#,
                    #h4("2. Enrichment results mapped to alignment results:"),
                    #downloadButton("motiffujidl2","Download"),
                    #dataTableOutput("motiffuji2")
                  )
                )
              ),
              conditionalPanel(
                condition = "input.motifplotxuanze1==2",
                downloadButton("motifplotdownload","Download"),
                plotOutput("motifplot")
              )
            ),
            conditionalPanel(
              condition = 'input.motiffujidfxuanze==2',
              radioButtons(
                "motifplotxuanze2",
                label = h4(""),
                choices = list("2.1 Blasted peptide motif table" = 1,"2.2 Blasted peptide motif plot"=2),
                selected = 1,
                inline = TRUE
              ),
              hr(),
              conditionalPanel(
                condition = "input.motifplotxuanze2==1",
                #h4("1. Blasted peptide motif enrichment results:"),
                fluidRow(
                  column(
                    2,
                    downloadButton("motiffujiblastdl","Download")
                  ),
                  column(
                    2,
                    actionButton("mcsbtn_resjieshi3x","Result description",icon("file-alt"),
                                 style="color: black; background-color: #E6E6FA; border-color: #E6E6FA")
                  )
                ),
                dataTableOutput("motiffujiblast")
              ),
              conditionalPanel(
                condition = "input.motifplotxuanze2==2",
                downloadButton("motifblastplotdl","Download"),
                plotOutput("motifblastplot")
              )
            )
          )
        )
      ),
      tabPanel(
        "Annotation",
        sidebarLayout(
          sidebarPanel(
            width = 3,
            h3(
              "Step5: Annotation based on Kinase-Substrate database",
              tags$span(
                id = 'span5',
                `data-toggle` = "tooltip5",
                title = '
                This step will Offer more flexible annotation based on kinase-substrate databases (e.g. PhosphoSitePlus) and network plots.
                ',
                tags$span(class = "glyphicon glyphicon-question-sign")
              )
            ),
            #div(id='NetworKINcutoff_div',numericInput("NetworKINcutoff","minimum NetworKIN score:",value = 3)),
            #bsTooltip("NetworKINcutoff_div",'A numeric value between 1 and infinity setting the minimum NetworKIN score.',
            #          placement = "bottom",options = list(container = "body")),
            #div(id='matchtypex_div',selectInput("matchtypex","Match type:",choices = c("Kinase"=1,"Substrate"=2))),
            #bsTooltip("matchtypex_div",'If you choose "Kinase", it will match the protein accession id to the "KIN_ACC_ID" column in the Kinase-Substrate database, otherwise, it matches the "SUB_ACC_ID" column.',
            #          placement = "right",options = list(container = "body")),
            uiOutput("kinasemotifui"),
            div(id='genenamesif_div',checkboxInput("genenamesif","Show gene names or not?",TRUE)),
            bsTooltip("genenamesif_div",'If true, the gene names will be appeared in the network plot, otherwise, the uniprot ids will be shown.',
                      placement = "bottom",options = list(container = "body")),
            #uiOutput("kinasemotifui"),
            tags$hr(style="border-color: grey;"),
            actionButton("mcsbtn_kniase","Calculate",icon("paper-plane"),
                         style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
          ),
          mainPanel(
            width = 9,
            tabsetPanel(
              id="subnav",
              tabPanel(
                "Results",
                radioButtons(
                  "annotationxuanze",
                  label = h4(""),
                  choices = list("1. Uploaded peptide annotation" = 1,"2. Blasted peptide annotation"=2),
                  selected = 1,
                  inline = TRUE
                ),
                tags$hr(style="border-color: grey;"),
                fluidRow(
                  column(
                    2,
                    downloadButton("kinasedatadl","Download")
                  ),
                  column(
                    2,
                    actionButton("mcsbtn_resjieshi4","Result description",icon("file-alt"),
                                 style="color: black; background-color: #E6E6FA; border-color: #E6E6FA")
                  )
                ),
                dataTableOutput("kinasedata")
              ),
              tabPanel(
                "Node and edge table",
                h4("Please note, the node table and the edge table are used for network plot next. When the network is large, actually the network plot can not be shown immediately and may be corrupted in the next panel (i.e. Network Plot), thus users can download the two tables and input them into other tools (e.g. Cytoscape)."),
                tags$hr(style="border-color: grey;"),
                h4("1. Node table:"),
                downloadButton("nodetableresdl","Download"),
                dataTableOutput("nodetableres"),
                tags$hr(style="border-color: grey;"),
                h4("2. Edge table:"),
                downloadButton("edgetableresdl","Download"),
                dataTableOutput("edgetableres")
              ),
              tabPanel(
                "Network Plot",
                div(id="cmheatmap_div",checkboxInput("cmheatmap","Change figure size？",FALSE)),
                conditionalPanel(
                  condition = "input.cmheatmap==true",
                  sliderInput("cmheatmap_height","figure height:",min = 500,max = 5000,step = 100,value = 800)
                ),
                downloadButton("cmheatmappicdl","Download"),
                plotOutput("cmheatmappic")
              )
            )
          )
        )
      ),
      tabPanel(
        "Interaction",
        sidebarLayout(
          sidebarPanel(
            width = 3,
            h3(
              "Step6: Interaction between non-human proteins and human proteins",
              tags$span(
                id = 'span6',
                `data-toggle` = "tooltip6",
                title = '
                This step will visualize the expression of modification sites on interacting proteins on the basis of protein-protein interaction data.
                ',
                tags$span(class = "glyphicon glyphicon-question-sign")
              )
            ),
            #checkboxInput('datainputfileif', '1. Input your own expression data?', FALSE),
            #conditionalPanel(
            #  condition = "input.datainputfileif==true",
            #  fileInput('datainputfile', h5('1.1 Please select your data file:'),
            #            accept=c('text/csv'))
            #),
            #hr(),
            #h5("2. Samples information:"),
            #textInput("grnums",h5("2.1 Group and replicate number:"),value = ""),
            #bsTooltip("grnums",'Type in the group number and replicate number here. Please note, the group number and replicate number are linked with ";", and the replicate number of each group is linked with "-". For example, if you have two groups, each group has three replicates, then you should type in "2;3-3" here. Similarly, if you have 3 groups with 5 replicates in every groups, you should type in "3;5-5-5".',
            #          placement = "right",options = list(container = "body")),
            #textInput("grnames",h5("2.2 Group names:"),value = ""),
            #bsTooltip("grnames",'Type in the group names of your samples. Please note, the group names are linked with ";". For example, there are two groups, you can type in "Control;Experiment".',
            #          placement = "right",options = list(container = "body")),
            radioButtons(
              "loaddatatype",
              label = "",
              choices = list("Upload experimental data" = 1,"Load example data"=2),
              selected = 1,
              inline = TRUE
            ),
            tags$hr(style="border-color: grey;"),
            conditionalPanel(
              condition = "input.loaddatatype==1",
              h4("1. Expression data:"),
              fileInput('file1', h5('1.1 Import your expression data:'),
                        accept=c('.csv')),
              checkboxInput('header', '1.1.1 First row as column names ?', TRUE),
              checkboxInput('firstcol', '1.1.2 First column as row names ?', FALSE),
              tags$hr(style="border-color: #B2B2B2;"),
              h4("2. Samples information:"),
              textInput("grnums",h5("2.1 Group and replicate number:"),value = ""),
              bsTooltip("grnums",'Type in the group number and replicate number here. Please note, the group number and replicate number are linked with ";", and the replicate number of each group is linked with "-". For example, if you have two groups, each group has three replicates, then you should type in "2;3-3" here. Similarly, if you have 3 groups with 5 replicates in every groups, you should type in "3;5-5-5".',
                        placement = "right",options = list(container = "body")),
              textInput("grnames",h5("2.2 Group names:"),value = ""),
              bsTooltip("grnames",'Type in the group names of your samples. Please note, the group names are linked with ";". For example, there are two groups, you can type in "Control;Experiment".',
                        placement = "right",options = list(container = "body")),
              tags$hr(style="border-color: #B2B2B2;"),
              h4("3. Interaction data:"),
              fileInput('Interfile1', h5('3.1 Import interaction data:'),
                        accept=c('.csv')),
              checkboxInput('Interheader', '3.1.1 First row as column names ?', TRUE),
              checkboxInput('Interfirstcol', '3.1.2 First column as row names ?', FALSE)
            ),
            conditionalPanel(
              condition = "input.loaddatatype==2",
              downloadButton("loaddatadownload1","1. Download example expression data",style="color: #fff; background-color: #6495ED; border-color: #6495ED"),
              tags$hr(style="border-color: grey;"),
              #downloadButton("loaddatadownload2","Download example sample group data",style="color: #fff; background-color: #6495ED; border-color: #6495ED")
              h4("2. Samples information:"),
              textInput("examgrnums",h5("2.1 Group and replicate number:"),value = "6;2-2-3-3-3-3"),
              textInput("examgrnames",h5("2.2 Group names:"),value = "0h;2h;4h;8h;12h;24h"),
              tags$hr(style="border-color: grey;"),
              downloadButton("interdatadownload1","3. Download example interaction data",style="color: #fff; background-color: #6495ED; border-color: #6495ED")
            )#,
            #tags$hr(style="border-color: grey;"),
            #uiOutput("sarsproteinsui")
            #actionButton("mcsbtn_motifplot","Calculate",icon("paper-plane"),
            #             style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
          ),
          mainPanel(
            width = 9,
            radioButtons(
              "interactionxuanze",
              label = h4(""),
              choices = list("1. Original Expression data" = 1,"2. Processed Expression data"=2,"3. Interaction plot"=3),
              selected = 1,
              inline = TRUE
            ),
            hr(),
            conditionalPanel(
              condition = "input.interactionxuanze==1",
              h4("1.a. Expression Data:"),
              dataTableOutput("peaksdata"),
              hr(),
              h4("1.b. Interaction Data:"),
              dataTableOutput("interactiondata")
            ),
            conditionalPanel(
              condition = "input.interactionxuanze==2",
              fluidRow(
                column(
                  3,
                  checkboxInput('mediannormif', '2.1 Median normalization or not?', TRUE),
                  bsTooltip("mediannormif",'If true, the values in expression matrix will be devided by its column median value to make the samples to have the same median. (Please note, StatsPro was not designed to perform sophisticated normalization analysis. Any normalized datasets with NA can be accepted for analysis).',
                            placement = "right",options = list(container = "body"))
                ),
                column(
                  3,
                  checkboxInput('logif', '2.2 Log or not?', TRUE),
                  bsTooltip("logif",'If true, the values in expression matrix will be log-transformed with base 2.',
                            placement = "right",options = list(container = "body"))
                )
              ),
              hr(),
              downloadButton("processedEdatadl","Download"),
              dataTableOutput("processedEdata")
            ),
            conditionalPanel(
              condition = "input.interactionxuanze==3",
              fluidRow(
                column(
                  3,
                  textInput("sarscol",h5("3.1 Node color for SARS-CoV-2 virus prtein:"),value = "red")
                ),
                column(
                  3,
                  textInput("humanprocol",h5("3.2 Node color for Human prtein:"),value = "grey")
                ),
                column(
                  3,
                  textInput("interactvaluecol",h5("3.3 Node color for expression data:"),value = "blue;white;darkred"),
                  bsTooltip("interactvaluecol",'To change the pie node color, which corresponds to the intensity value in the expression data. Users should input three colour names linked with semicolons. The first one is for the lowest intensity value, the second one is for the middle intensity value, and the third one is for the highest intensity value.',
                            placement = "right",options = list(container = "body"))
                ),
                column(
                  3,
                  checkboxInput('zscoreif', '3.4 Scaled expression data (Z-score) or not?', TRUE)
                )
              ),
              hr(),
              fluidRow(
                column(
                  3,
                  uiOutput("sarsproteinsui")
                ),
                column(
                  3,
                  numericInput("interactfigheight",h5("Figure Height:"),800)
                )
              ),
              downloadButton("interactplotdl","Download"),
              plotOutput("interactplot")
            )
          )
        )
      )#,
      #tabPanel(
      #  "Building Species Database",
      #  sidebarLayout(
      #    sidebarPanel(
      #      width = 3,
      #      div(id='refastafileif_div',checkboxInput("refastafileif","Re-upload?",FALSE)),
      #      bsTooltip("refastafileif_div",'This step can build the standard database based on the fata file that users upload, herein there is no species limit. And this results can also be used in "Own background" step. If users want to build their own database, they can select this parameter and then upload a fasta file.',
      #                placement = "bottom",options = list(container = "body")),
      #      conditionalPanel(
      #        condition = "input.refastafileif==true",
      #        fileInput('fastafile', 'Please upload your fasta file:',accept=c('.fasta'))
      #      ),
      #      tags$hr(style="border-color: grey;"),
      #      actionButton("mcsbtn_fastaalign","Calculate",icon("paper-plane"),
      #                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
      #    ),
      #    mainPanel(
      #      width = 9,
      #      fluidRow(
      #        column(
      #          2,
      #          downloadButton("allfastadl","Download")
      #        ),
      #        column(
      #          2,
      #          actionButton("mcsbtn_resjieshi5","Result description",icon("file-alt"),
      #                       style="color: black; background-color: #E6E6FA; border-color: #E6E6FA")
      #        )
      #      ),
      #      dataTableOutput("allfasta")
      #    )
      #  )
      #)
    )
  )
)
#
server<-shinyServer(function(input, output, session){
  options(shiny.maxRequestSize=30*1024^2)
  usertimenum<-as.numeric(Sys.time())
  #ui
  output$welcomeui<-renderUI({
    screenwidth<-input$dimension[1]
    #screenheight<-input$dimension[2]
    #tryCatch({},error=function(e) NULL)
    if(is.null(screenwidth)){
      return(NULL)
    }else{
      if(screenwidth<=1024){
        imgwidth<-150
      }
      else if(screenwidth>1024 & screenwidth<=1440){
        imgwidth<-250
      }
      else{
        imgwidth<-350
      }
    }
    
    fluidRow(
      #div(style="text-align:center",h1("~~Welcome~~")),
      div(
        id="mainbody",
        column(3),
        column(
          6,
          div(style="text-align:left;margin-top:20px;font-size:140%;color:darkred",
              HTML("~~ <em>Dear Users, Welcome to motifeR 2.0</em> ~~")),
          div(style="text-align:center;margin-top: 20px",
              a(href='#',
                img(src='Figure1app.png',height=imgwidth))),
          div(style="width:fit-content;width:-webkit-fit-content;width:-moz-fit-content;font-size:120%;margin-top:15px",
              HTML("<b>motifeR 2.0</b> is a web-based tool, which possesses four core functions as previously implemented in the first version (<a href='https://doi.org/10.1002/pmic.201900245' target='_blank'>motifeR published in 2019</a>) with an addition of three new functions. First, input peptide sequence data. This tool accepts multiple types of input data, such as the peptide sequences with a standard form, which means every peptide with a user-defined PTM label next to the modification amino acid (e.g. GIGT#PPNTTPIK), or the PTM results directly from some common software (e.g. MaxQuant and Spectronaut). Second, irregular peptide sequences with modification sites are pre-aligned with the whole protein sequences, which can be downloaded from some well-known database (e.g. UniProt). Third, motif enrichment analysis is fulfilled based on an iterative statistical approach implemented in rmotifx package and motif plots are visualized using the ggseqlogo package. Fourth, this tool can retrieve the kinase-substrate annotation information derived from the PhosphoSitePlus for the pre-aligned results and depict the networks of kinase-substrate relationship. In addition, we added three new functions to accomplish kinase-substrate annotation for modification peptides/proteins across different species, as well as relative results visualization for diverse analysis requirements from users: 1. Mapping protein sequences and identifiers between non-human species and H.sapiens, 2. Calculating sequence window similarity, 3. Visualizing the expression of modification sites on interacting proteins on the basis of protein-protein interaction data. In addition, this tool supports both online access and local installation. The source codes and installation instructions can be available in the GitHub repository: <a href='https://github.com/wangshisheng/motifeR2.0' target='_blank'>https://github.com/wangshisheng/motifeR2.0</a> under an MIT license.")),
          div(style="width:fit-content;width:-webkit-fit-content;width:-moz-fit-content;margin-top:15px;font-size:120%",
              #HTML("<br />"),
              HTML("Finally, motifeR 2.0 is developed by <a href='https://shiny.rstudio.com/' target='_blank'>R shiny (Version 1.6.0)</a>, and is free and open to all users with no login requirement. It can be readily accessed by all popular web browsers including Google Chrome, Mozilla Firefox, Safari and Internet Explorer 10 (or later), and so on. We would highly appreciate that if you could send your feedback about any bug or feature request to Shisheng Wang at <u>wsslearning@omicsolution.com</u>.")),
          div(style="text-align:center;margin-top:20px;font-size:140%;color:darkgreen",
              HTML("<br />"),
              HTML("^_^ <em>Enjoy yourself in motifeR 2.0</em> ^_^")),
          tags$hr(style="border-color: grey60;"),
          div(style="text-align:center;margin-top: 20px;font-size:100%",
              HTML(" &copy; 2021 <a href='https://www.yslproteomics.org/' target='_blank'>Yansheng Liu's Group</a> and <a href='http://english.cd120.com/' target='_blank'>Hao Yang's Group</a>. All Rights Reserved.")),
          div(style="text-align:center;margin-bottom: 20px;font-size:100%",
              HTML("&nbsp;&nbsp; Created by Shisheng Wang. E-mail: <u>shishengwang@wchscu.cn</u>."))
        ),
        column(3)
      )
    )
  })
  #show data
  output$metabopathspecies<-renderUI({
    metabopath_spedf<-read.csv("metabopath-species.csv",header = T,stringsAsFactors = F)
    metabopath_spedf_paste<-paste(metabopath_spedf$Organism.ID,metabopath_spedf$Organism,sep = "-")
    selectizeInput('metabopathspeciesselect', h5('Species:'), choices =metabopath_spedf_paste,options = list(maxOptions = 6000))
  })
  #######
  exampledataout<-reactive({
    if(input$origidatatype=="MaxQuant"){
      dataread<-read.csv("MaxQuant_Exampledata.csv",stringsAsFactors = F)
    }
    else if(input$origidatatype=="Spectronaut"){
      dataread<-read.csv("Spectronaut_Exampledata.csv",stringsAsFactors = F)
    }
    else{
      dataread<-read.csv("Normal_Exampledata.csv",stringsAsFactors = F)
    }
    dataread
  })
  output$loadseqdatadownload1<-downloadHandler(
    filename = function(){paste("Example_SequenceData_",usertimenum,".csv",sep="")},
    content = function(file){
      write.csv(exampledataout(),file,row.names = FALSE)
    }
  )
  seqrawdataoutxx<-reactive({
    files <- input$metabopathfile1
    if(is.null(files)){
      dataread<-data.frame(Description="motifeR 2.0 detects that you did not upload your data. Please upload the sequence data, or load the example data to check first.")
      dataread
    }else{
      if (input$metabopathfileType_Input == "1"){
        dataread<-read.xlsx(files$datapath,rowNames=input$metabopathfirstcol,
                            colNames = input$metabopathheader,sheet = input$metabopathxlsxindex)
      }
      else if(input$metabopathfileType_Input == "2"){
        if(sum(input$metabopathfirstcol)==1){
          rownametfmetabopath<-1
        }else{
          rownametfmetabopath<-NULL
        }
        dataread<-read.xls(files$datapath,sheet = input$metabopathxlsxindex,header=input$metabopathheader,
                           row.names = rownametfmetabopath, sep=input$metabopathsep,stringsAsFactors = F)
      }
      else{
        if(sum(input$metabopathfirstcol)==1){
          rownametfmetabopath<-1
        }else{
          rownametfmetabopath<-NULL
        }
        dataread<-read.csv(files$datapath,header=input$metabopathheader,
                           row.names = rownametfmetabopath, sep=input$metabopathsep,stringsAsFactors = F)
      }
    }
    dataread
  })
  output$seqrawdata<-renderDataTable({
    if(input$loadseqdatatype==1){
      dataread<-seqrawdataoutxx()
    }else{
      dataread<-exampledataout()
    }
    datatable(dataread, options = list(pageLength = 10))
  })
  
  seqrawdataout<-reactive({
    if(input$loadseqdatatype==1){
      dataread<-seqrawdataoutxx()
    }else{
      dataread<-exampledataout()
    }
    datareadx<-dataread
    origidatatypex<-isolate(input$origidatatype)
    Peptides<-vector()
    if(origidatatypex=="MaxQuant"){
      withProgress(message = 'Generating data', style = "notification", detail = "index 1", value = 0,{
        for(i in 1:nrow(datareadx)){
          pep1<-datareadx[[1]][i]
          Peptidesi1<-strsplit(gsub("[^0-9.]", ";", pep1),";")[[1]]
          Peptidesi2<-Peptidesi1[Peptidesi1!=""]
          for(ii in 1:length(Peptidesi2)){
            if(as.numeric(Peptidesi2[ii])>=0.75){
              pep1<-gsub(paste0("\\(",Peptidesi2[ii],"\\)"),input$centralresfuhao,pep1)#"#"
            }else{
              pep1<-gsub(paste0("\\(",Peptidesi2[ii],"\\)"),"",pep1)
            }
          }
          Peptides[i]<-pep1
          
          incProgress(1/nrow(datareadx), detail = paste("index", i))
        }
      })
      dataconvert<-data.frame(AnnotatedPeps=Peptides,stringsAsFactors = FALSE)
    }
    else if(origidatatypex=="Spectronaut"){
      withProgress(message = 'Annotated data:',min = 0, max = 2, style = "notification", detail = "Generating data", value = 1,{
        phosphoindex<-grep("\\[Phospho \\(STY\\)\\]",datareadx[[1]], perl = TRUE)
        uploaddata1<-datareadx[phosphoindex,]
        Peptidesx<-gsub("_","",uploaddata1, perl = TRUE)
        Peptidesx<-gsub("\\[Phospho \\(STY\\)\\]",input$centralresfuhao,Peptidesx, perl = TRUE)#"#"
        Peptidesx2<-str_replace_all(Peptidesx,"\\[.*?\\]","")
        
        shiny::incProgress(1, detail = "Generating data")
      })
      dataconvert<-data.frame(AnnotatedPeps=Peptidesx2,stringsAsFactors = FALSE)
    }
    else{
      dataconvert<-datareadx
    }
    dataconvertindex<-grep(input$centralresfuhao,dataconvert[[1]])
    dataconvert<-dataconvert[dataconvertindex,,drop=FALSE]
    dataconvert
  })
  #observeEvent(
  #  input$mcsbtn_seqrawdata,{
  #    output$seqannotatedata<-renderDataTable({
  #      dataread<-seqrawdataout()
  #      datatable(dataread, options = list(pageLength = 10))
  #    })
  #    output$seqannotatedatadl<-downloadHandler(
  #      filename = function(){paste("Annotated_data",usertimenum,".csv",sep="")},
  #      content = function(file){
  #        write.csv(seqrawdataout(),file,row.names=FALSE)
  #      }
  #    )
  #  }
  #)
  #
  seqbjdataout<-reactive({
    if(input$beijingif){
      files <- input$goortbeijingkufile1
      if(is.null(files)){
        dataread<-NULL
      }else{
        if (input$goortbeijingkufileType_Input == "1"){
          dataread<-read.xlsx(files$datapath,rowNames=input$goortbeijingkufirstcol,
                              colNames = input$goortbeijingkuheader,sheet = input$goortbeijingkuxlsxindex)
        }
        else if(input$goortbeijingkufileType_Input == "2"){
          if(sum(input$goortbeijingkufirstcol)==1){
            rownametfgoortbeijingku<-1
          }else{
            rownametfgoortbeijingku<-NULL
          }
          dataread<-read.xls(files$datapath,sheet = input$goortbeijingkuxlsxindex,header=input$goortbeijingkuheader,
                             row.names = rownametfgoortbeijingku, sep=input$goortbeijingkusep,stringsAsFactors = F)
        }
        else{
          if(sum(input$goortbeijingkufirstcol)==1){
            rownametfgoortbeijingku<-1
          }else{
            rownametfgoortbeijingku<-NULL
          }
          dataread<-read.csv(files$datapath,header=input$goortbeijingkuheader,
                             row.names = rownametfgoortbeijingku, sep=input$goortbeijingkusep,stringsAsFactors = F)
        }
      }
    }else{
      dataread<-NULL
    }
    dataread
  })
  output$seqbjdata<-renderDataTable({
    datareadbj<-seqbjdataout()
    datatable(datareadbj, options = list(pageLength = 10))
  })
  
  fastaseqownout<-reactive({
    files <- input$fastafileown
    if(is.null(files)){
      datareadfasta<-NULL
    }else{
      datafasta<-readAAStringSet(files$datapath)
      pro_seqdf<-pro_seqdf1<-as.data.frame(datafasta)
      #pro_seqdf_rown<-unlist(lapply(rownames(pro_seqdf1),function(x) strsplit(x,"\\|")[[1]][2]))
      pro_seqdf_rown1<-unlist(lapply(rownames(pro_seqdf1),function(x) strsplit(x,"\\|")[[1]][1]))
      pro_seqdf_rown2<-unlist(lapply(rownames(pro_seqdf1),function(x) strsplit(x,"\\|")[[1]][2]))
      if(sum(duplicated(pro_seqdf_rown1))>=1 & sum(duplicated(pro_seqdf_rown2))>=1){
        pro_seqdf_rown<-rownames(pro_seqdf1)
      }
      else if(sum(duplicated(pro_seqdf_rown1))>=1){
        pro_seqdf_rown<-pro_seqdf_rown2
      }
      else if(sum(duplicated(pro_seqdf_rown2))>=1){
        pro_seqdf_rown<-pro_seqdf_rown1
      }
      else{
        pro_seqdf_rown<-rownames(pro_seqdf1)
      }
      rownames(pro_seqdf1)<-pro_seqdf_rown
      pro_seqdfncar<-unlist(lapply(pro_seqdf1$x,nchar))
      danlength<-input$minseqs
      pro_seqdf<-pro_seqdf1[pro_seqdfncar>(2*danlength+1),,drop=FALSE]
      n_data_fasta<-nrow(pro_seqdf)
      wincenter<-strsplit(input$centralres,"")[[1]]
      seqwindowsall_S<-vector()
      seqnamesall_S<-vector()
      wincenteri<-vector()
      k<-1
      for(ii in wincenter){
        withProgress(message = paste('Generating data',ii), style = "notification", detail = "index 1", value = 0,{
          for(i in 1:n_data_fasta){
            seqindex1<-stri_locate_all(pattern = ii, pro_seqdf$x[i], fixed = TRUE)[[1]][,1]
            if(length(seqindex1)>0){
              seqnchar<-nchar(pro_seqdf$x[i])
              seqseq<-vector()
              for(j in 1:length(seqindex1)){
                indexjian1<-seqindex1[j]-danlength
                indexjian2<-seqindex1[j]+danlength
                if(indexjian1<=0){
                  xhx1<-paste(rep("_",abs(indexjian1)+1),collapse ="")
                  xhx2<-stri_sub(pro_seqdf$x[i],from = 0,to=indexjian2)
                  xhx3<-paste0(xhx1,xhx2)
                }
                else if(indexjian2>seqnchar){
                  xhx1<-paste(rep("_",(indexjian2-seqnchar)),collapse="")
                  xhx2<-stri_sub(pro_seqdf$x[i],from = indexjian1,to=seqnchar)
                  xhx3<-paste0(xhx2,xhx1)
                }
                else{
                  xhx3<-stri_sub(pro_seqdf$x[i],from = indexjian1,to=indexjian2)
                }
                seqwindowsall_S[k]<-xhx3
                seqnamesall_S[k]<-rownames(pro_seqdf)[i]
                wincenteri[k]<-ii
                k<-k+1
              }
            }
            incProgress(1/n_data_fasta, detail = paste("index", i))
          }
        })
      }
      datareadfasta<-data.frame(ID=seqnamesall_S,Windows=seqwindowsall_S,
                                Center=wincenteri,stringsAsFactors = F)
    }
    datareadfasta
  })
  #
  observeEvent(input$mcsbtn_resjieshi, {
    showModal(modalDialog(
      title = "Pre-alignment result description:",
      paste0("1. Pep.upload: this column contains those peptides users upload."),br(),
      paste0("2. Stripped.pep: the peptide skeleton."),br(),
      paste0("3. Pep.main.index: the position of the main modified amino acid in the peptide, for example, if users upload their peptides containing Class I phosphorylation sites with high confidence, such as 'TSLWNPT#Y@GSWFTEK', then this software will recognize '#' as Class I phosphorylation site and '@' as non-Class I phosphorylation site by default, so the Pep.main.index will be 7."),br(),
      paste0("4. Pep.all.index: the position of all modified amino acid in the peptide. As the example in Pep.main.index, the Pep.all.index will be 7;8."),br(),
      paste0("5. Center.amino.acid: the central amino acid in the aligned peptide."),br(),
      paste0("6. Seqwindows: the aligned standard peptides. Note for multiple modification sites or types, the column provides peptides with all the sites respectively centered."),br(),
      paste0("7. PRO.from.Database: provide the protein name containing this peptide from the fasta file the user uploaded."),br(),
      paste0("8. PROindex.from.Database: the position of modified amino acid in the protein sequence."),br(),
      #paste0("9. Contain.if: whether containing the sequences that match the regular expression (see above), if true, marked with 'Yes', otherwise, 'No'. This column only appears when users choose the parameter--- Check if containing some regular sequence."),
      paste0("9. PRO.CombinedID: Combining the protein ID, Center.amino.acid and PROindex.from.Database together with '_'."),
      size ="l",
      easyClose = TRUE,
      footer = modalButton("Cancel")
    ))
  })
  observeEvent(input$mcsbtn_resjieshi2, {
    showModal(modalDialog(
      title = "Pre-alignment result description:",
      paste0("1. Pep.upload: this column contains those peptides users upload."),br(),
      paste0("2. Stripped.pep: the peptide skeleton."),br(),
      paste0("3. Pep.main.index: the position of the main modified amino acid in the peptide, for example, if users upload their peptides containing Class I phosphorylation sites with high confidence, such as 'TSLWNPT#Y@GSWFTEK', then this software will recognize '#' as Class I phosphorylation site and '@' as non-Class I phosphorylation site by default, so the Pep.main.index will be 7."),br(),
      paste0("4. Pep.all.index: the position of all modified amino acid in the peptide. As the example in Pep.main.index, the Pep.all.index will be 7;8."),br(),
      paste0("5. Center.amino.acid: the central amino acid in the aligned peptide."),br(),
      paste0("6. Seqwindows: the aligned standard peptides. Note for multiple modification sites or types, the column provides peptides with all the sites respectively centered."),br(),
      paste0("7. PRO.from.Database: provide the protein name containing this peptide from the fasta file the user uploaded."),br(),
      paste0("8. PROindex.from.Database: the position of modified amino acid in the protein sequence."),br(),
      paste0("9. Contain.if: whether containing the sequences that match the regular expression (see above), if true, marked with 'Yes', otherwise, 'No'. This column only appears when users choose the parameter--- Check if containing some regular sequence."),br(),
      paste0("10. Seqwindows_MultiSites: there are two situations here: First, the modified amino acid will be replaced with 'X' if it is not the central residue, for example, 'NKPTSLWNPT(0.832)Y(0.168)GSWFTEK' has two phosphosites, one is the 10th amino acid with 0.832 location probability, the other is the 11th amino acid with 0.168 location probability, thus if we transform it like 'NKPTSLWNPT#Y@GSWFTEK' (high probability is replaced with '#', while low probability is replaced with '@'). Then in motifeR 2.0, the 10th amino acid will be considered as central residue, the 11th amino acid will be replaced with 'X', thus the standard sequence is 'PTSLWNPTYGSWFTE', correspondingly, the Seqwindows_MultiSites should be 'PTSLWNPTXGSWFTE'. Second, if we transform this peptide like 'NKPTSLWNPT#Y#GSWFTEK', the two amino acids will be both considered as central residue, thus the standard sequence is 'PTSLWNPTYGSWFTE;TSLWNPTYGSWFTEK', correspondingly, the Seqwindows_MultiSites is still 'PTSLWNPTYGSWFTE;TSLWNPTYGSWFTEK'."),
      size ="l",
      easyClose = TRUE,
      footer = modalButton("Cancel")
    ))
  })
  observeEvent(input$mcsbtn_resjieshi2_1, {
    showModal(modalDialog(
      title = "The blast result description:",
      paste0("1. Pep.upload: this column contains those peptides users upload."),br(),
      paste0("2. Stripped.pep: the peptide skeleton."),br(),
      paste0("3. Pep.main.index: the position of the main modified amino acid in the peptide, for example, if users upload their peptides containing Class I phosphorylation sites with high confidence, such as 'TSLWNPT#Y@GSWFTEK', then this software will recognize '#' as Class I phosphorylation site and '@' as non-Class I phosphorylation site by default, so the Pep.main.index will be 7."),br(),
      paste0("4. Pep.all.index: the position of all modified amino acid in the peptide. As the example in Pep.main.index, the Pep.all.index will be 7;8."),br(),
      paste0("5. Center.amino.acid: the central amino acid in the aligned peptide."),br(),
      paste0("6. Seqwindows: the aligned standard peptides."),br(),
      paste0("7. PRO.from.Database: provide the protein ID/name containing this peptide from the fasta file the user uploaded."),br(),
      paste0("8. PROindex.from.Database: the position of modified amino acid in the protein sequence."),br(),
      paste0("9. PRO.CombinedID: Combining the protein ID, Center.amino.acid and PROindex.from.Database together with '_'."),br(),
      paste0("10. Center.amino.acids.Human: the central amino acid mapped from the human peptides."),br(),
      paste0("11. Seqwindows.Human: the standard peptides mapped from the human peptides."),br(),
      paste0("12. PRO.from.Human: the protein ID/name from the mapped human protein."),br(),
      paste0("13. PROindex.from.Human: the position of modified amino acid in the mapped human protein sequence."),br(),
      paste0("14. PRO.CombinedID.Human: Combining the PRO.from.Human, Center.amino.acids.Human and PROindex.from.Human together with '_'."),br(),
      paste0("15. Center.aa.match: The matching degree of central amino acids (CAAs) when the uploaded peptides are blasted to Human prtein sequences. 1. Exact matching: The CAAs are same, for example, the CAA is 'S' in the uploaded peptides and the CAA is also 'S' in the blasted sequence. 2. Fuzzy matching: For example, the CAA is 'S' in the uploaded peptides and the CAA could be 'S', 'T', or 'Y' in the blasted sequence. All: Reporting all results."),br(),
      paste0("16. Window.similarity: The similarity of sequence windows between the uploaded peptides and the blasted peptides. For example, there are 15 amino acids in one sequence window, 7 here means there are 7 amino acids are exactly same (amino acids names and positions in both windows are all same)."),br(),
      size ="l",
      easyClose = TRUE,
      footer = modalButton("Cancel")
    ))
  })
  observeEvent(input$mcsbtn_resjieshi3, {
    showModal(modalDialog(
      title = "Uploaded motif Enrichment result description:",
      paste0("1. motif: the overrepresented motif."),br(),
      paste0("2. score: the motif score, which is calculated by taking the sum of the negative log probabilities used to fix each position of the motif. Higher motif scores typically correspond to motifs that are more statistically significant as well as more specific."),br(),
      paste0("3. fg.matches: frequency of sequences matching this motif in the foreground set."),br(),
      paste0("4. fg.size: total number of foreground sequences."),br(),
      paste0("5. bg.matches: frequency of sequences matching this motif in the background set."),br(),
      paste0("6. bg.size: total number of background sequences."),br(),
      paste0("7. fold.increase: An indicator of the enrichment level of the extracted motifs. Specifically, it is calculated as (foreground matches/foreground size)/(background matches/background size)."),br(),
      paste0("8. Enrich.seq: those peptides are overrepresented in this motif."),br(),
      paste0("9. Enrich.pro: those proteins in which the peptides exist from Enrich.seq."),
      size ="l",
      easyClose = TRUE,
      footer = modalButton("Cancel")
    ))
  })
  observeEvent(input$mcsbtn_resjieshi3x, {
    showModal(modalDialog(
      title = "Blasted motif Enrichment result description:",
      paste0("1. motif: the overrepresented motif."),br(),
      paste0("2. score: the motif score, which is calculated by taking the sum of the negative log probabilities used to fix each position of the motif. Higher motif scores typically correspond to motifs that are more statistically significant as well as more specific."),br(),
      paste0("3. fg.matches: frequency of sequences matching this motif in the foreground set."),br(),
      paste0("4. fg.size: total number of foreground sequences."),br(),
      paste0("5. bg.matches: frequency of sequences matching this motif in the background set."),br(),
      paste0("6. bg.size: total number of background sequences."),br(),
      paste0("7. fold.increase: An indicator of the enrichment level of the extracted motifs. Specifically, it is calculated as (foreground matches/foreground size)/(background matches/background size)."),br(),
      paste0("8. Enrich.seq: those peptides are overrepresented in this motif."),br(),
      paste0("9. Enrich.pro: those proteins in which the peptides exist from Enrich.seq."),
      size ="l",
      easyClose = TRUE,
      footer = modalButton("Cancel")
    ))
  })
  observeEvent(input$mcsbtn_resjieshi4, {
    showModal(modalDialog(
      title = "Kinase-substrate result description:",
      paste0("1. KIN_ACC_ID: kinase uniprot id."),br(),
      paste0("2. SUB_ACC_ID: substrate uniprot id."),br(),
      paste0("3. Pep.upload: the original peptide."),br(),
      paste0("4. Pep.all.index: the position of all modified amino acid in the peptide."),br(),
      paste0("5. Center.amino.acid: the central amino acid in the aligned peptide. Or, Center.amino.acids.Human: the central amino acid mapped from the human peptides."),br(),
      paste0("6. Seqwindows: the aligned standard peptides. Or, Seqwindows.Human: the standard peptides mapped from the human peptides."),br(),
      paste0("7. PROindex.from.Database: the position of modified amino acid in the protein sequence. Or, PROindex.from.Human: the position of modified amino acid in the mapped human protein sequence."),br(),
      #paste0("2. KINASE: kinase id."),br(),
      paste0("8. GENE: kinase gene name."),br(),
      #paste0("4. SUBSTRATE: substrate id."),br(),
      paste0("9. SUB_GENE: substrate gene name."),br(),
      #paste0("7. networkin_score: the prediction score from networKIN database (https://networkin.info/)."),br(),
      #paste0("8. Enrich.seq: the peptide that is overrepresented in the relevant motif."),br(),
      #paste0("9. Motif: the overrepresented motif."),br(),
      #paste0("12. PROindex.from.Database: the position of modified amino acid in the protein sequence."),br(),
      size ="l",
      easyClose = TRUE,
      footer = modalButton("Cancel")
    ))
  })
  observeEvent(input$mcsbtn_resjieshi5, {
    showModal(modalDialog(
      title = "Building species database result description:",
      paste0("1. ID: uniprot ids."),br(),
      paste0("2. Windows: the standard peptides."),br(),
      paste0("3. Center: Central residue."),br(),
      size ="l",
      easyClose = TRUE,
      footer = modalButton("Cancel")
    ))
  })
  seqduiqioutx<-reactive({
    uploaddata1<-datareaddq<-seqrawdataout()
    if(input$seqalignif){
      centralres1<-strsplit(input$centralresfuhao,";|")[[1]]
      centralres<-centralres1[centralres1!=""]
      centralres2<-paste(centralres,collapse = "|")
      uploaddata1$Stripped.pep<-gsub(paste0("_|",centralres2),"",datareaddq[[1]], perl = TRUE)
      EGindex<-lapply(datareaddq[[1]],function(x){
        xx4<-gregexpr(centralres[1],x)[[1]]
        xx3<-gregexpr(centralres2,x)[[1]]
        xx5<-unlist(lapply(xx4,function(x) which(x==xx3)))
        xx1<-1:length(xx3)
        xx6<-as.numeric(xx3)-xx1
        xx2<-paste(xx6[xx5],collapse = ";")
        xx2
      })
      EGindex1<-lapply(datareaddq[[1]],function(x){
        xx3<-gregexpr(centralres2,x)[[1]]
        xx1<-1:length(xx3)
        xx6<-as.numeric(xx3)-xx1
        xx2<-paste(xx6,collapse = ";")
        xx2
      })
      centeranjisuan<-lapply(datareaddq[[1]],function(x){
        pepi<-strsplit(gsub(centralres2,"",x),"")[[1]]
        xx4<-gregexpr(centralres[1],x)[[1]]
        xx3<-gregexpr(centralres2,x)[[1]]
        xx5<-unlist(lapply(xx4,function(x) which(x==xx3)))
        xx1<-1:length(xx3)
        xx6<-as.numeric(xx3)-xx1
        xx2<-paste(pepi[xx6[xx5]],collapse = ";")
        xx2
      })
      uploaddata1$Pep.main.index<-unlist(EGindex)
      uploaddata1$Pep.all.index<-unlist(EGindex1)
      uploaddata1$Center.amino.acid<-unlist(centeranjisuan)
      colnames(uploaddata1)<-c("Pep.upload","Stripped.pep","Pep.main.index","Pep.all.index","Center.amino.acid")
      uploaddata1<<-uploaddata1
      if(input$xuanzebgdatabase==1){
        wuzhong<-strsplit(input$metabopathspeciesselect,"-")[[1]][1]
        if(is.na(wuzhong)){
          stop("Please select one background dataset or upload a fasta file as background dataset in the 'Import Data' step!")
        }else{
          datafasta<-readAAStringSet(paste0("fasta/",wuzhong,".fasta"))
        }
      }else{
        wuzhong<-input$wuzhongid
        files <- isolate(input$fastafileown)
        if(is.null(files)){
          stop("Please select one background dataset or upload a fasta file as background dataset in the 'Import Data' step!")
        }else{
          datafasta<-readAAStringSet(files$datapath)
        }
      }
      n_data_fasta<<-length(datafasta@ranges@NAMES)
      pro_seqdf<<-as.data.frame(datafasta)
      pro_seqdfnames<-unlist(lapply(rownames(pro_seqdf),function(x) strsplit(x,"\\|")[[1]][2]))
      if(sum(is.na(pro_seqdfnames))>n_data_fasta/2){
        pro_seqdfnames<-unlist(lapply(rownames(pro_seqdf),function(x) strsplit(x,"\\ ")[[1]][1]))
      }
      danlength<<-input$minseqs
      seqseqall<-vector()
      proidall<-vector()
      proidindexall<-vector()
      PRO.CombinedID<-vector()
      withProgress(message = 'Generating data', style = "notification", detail = "index 1", value = 0,{
        for(i in 1:nrow(uploaddata1)){
          seqindex1<-grep(uploaddata1$Stripped.pep[i],pro_seqdf$x, perl = TRUE)
          seqindex3<-as.numeric(strsplit(uploaddata1$Pep.main.index[i],";")[[1]])
          seqseqall1<-vector()
          proidindexall1<-vector()
          procomb<-vector()
          if(length(seqindex1)>0 & length(seqindex3)>0){
            for(k in 1:length(seqindex1)){
              #stri_locate_all
              seqindex2<-stri_locate_first(pattern = uploaddata1$Stripped.pep[i], pro_seqdf$x[seqindex1[k]], fixed = TRUE)[[1]][1]#[,1]
              seqnchar<-nchar(pro_seqdf$x[seqindex1[k]])
              indexjian<-unlist(lapply(seqindex2, function(x){x+seqindex3-1}))
              seqseq<-vector()
              for(j in 1:length(indexjian)){
                indexjian1<-indexjian[j]-danlength
                indexjian2<-indexjian[j]+danlength
                if(indexjian1<=0){
                  xhx1<-paste(rep("_",abs(indexjian1)+1),collapse ="")
                  xhx2<-stri_sub(pro_seqdf$x[seqindex1[k]],from = 0,to=indexjian2)
                  xhx3<-paste0(xhx1,xhx2)
                }
                else if(indexjian2>seqnchar){
                  xhx1<-paste(rep("_",(indexjian2-seqnchar)),collapse="")
                  xhx2<-stri_sub(pro_seqdf$x[seqindex1[k]],from = indexjian1,to=seqnchar)
                  xhx3<-paste0(xhx2,xhx1)
                }
                else{
                  xhx3<-stri_sub(pro_seqdf$x[seqindex1[k]],from = indexjian1,to=indexjian2)
                }
                seqseq[j]<-xhx3
              }
              seqseqall1[k]<-paste(seqseq,collapse = ";")
              proidindexall1[k]<-paste(indexjian,collapse = ";")
              procomb[k]<-paste(paste0(pro_seqdfnames[seqindex1[k]],"_",strsplit(uploaddata1$Center.amino.acid[i],";")[[1]],indexjian),collapse = ";")
            }
            seqseqall[i]<-paste(seqseqall1,collapse = "::")#"_",";"
            proidall[i]<-paste(pro_seqdfnames[seqindex1],collapse = "::")
            proidindexall[i]<-paste(proidindexall1,collapse = "::")
            PRO.CombinedID[i]<-paste(procomb,collapse = "::")
            #PRO.CombinedID[i]<-paste(paste0(pro_seqdfnames[seqindex1],"_",
            #                                strsplit(uploaddata1$Center.amino.acid[i],";")[[1]],
            #                                strsplit(proidindexall1,";")[[1]]),
            #                         collapse = "::")
          }else{
            seqseqall[i]<-"No Match"
            proidall[i]<-"No Match"
            proidindexall[i]<-"No Match"
            PRO.CombinedID[i]<-"No Match"
          }
          
          incProgress(1/nrow(uploaddata1), detail = paste("index", i))
        }
      })
      
      uploaddata1$Seqwindows<-seqseqall
      uploaddata1$PRO.from.Database<-proidall
      uploaddata1$PROindex.from.Database<-proidindexall
      uploaddata1$PRO.CombinedID<-PRO.CombinedID
      datareaddq<-uploaddata1
    }
    
    if(FALSE){#input$seqalignhanif
      containif<-rep("No",nrow(datareaddq))
      if(ncol(datareaddq)==1){
        containif[grep(input$seqalignhan,datareaddq[[1]])]<-"Yes"
      }else{
        containif[grep(input$seqalignhan,datareaddq$Seqwindows)]<-"Yes"
      }
      datareaddq$Contain.if<-containif
    }
    if(ncol(datareaddq)>2){
      datareaddqxx<-datareaddq[datareaddq$Seqwindows!="No Match",]
    }else{
      datareaddqxx<-datareaddq
    }
    datareaddqxx
  })
  seqduiqiduositeout<-reactive({
    datareaddq<-datareaddqxx<-seqduiqioutx()#isolate(seqduiqioutx())
    sitesnum<-unlist(lapply(datareaddq$Pep.all.index,function(x){
      length(strsplit(x,";")[[1]])
    }))
    if(is.null(sitesnum)){
      datareaddqx<-matrix(ncol = 1, nrow = 0)
    }else{
      datareaddqx<-datareaddqx1<-datareaddq[sitesnum>1,]
    }
    if(nrow(datareaddqx)>0){
      Seqwindows_MultiSites<-vector()
      withProgress(message = 'Generating data', style = "notification", detail = "index 1", value = 0,{
        for(i in 1:nrow(datareaddqx)){
          pepindexi1<-as.numeric(strsplit(datareaddqx$Pep.all.index[i],";")[[1]])
          pepindexi2<-as.numeric(strsplit(datareaddqx$Pep.main.index[i],";")[[1]])
          pepindexi<-setdiff(pepindexi1,pepindexi2)
          seqwindowi<-strsplit(datareaddqx$Seqwindows[i],";")[[1]]
          
          Seqwindows_multix<-vector()
          for(ii in 1:length(pepindexi2)){
            seqwindowix<-strsplit(seqwindowi[ii],"")[[1]]
            if(length(pepindexi)>0){
              posi<-input$minseqs+1+(pepindexi-pepindexi2[ii])
              posi_low<-which(posi>length(seqwindowix) | posi<1)
              if(length(posi_low)>0){
                seqwindowix[posi[-posi_low]]<-"X"
              }else{
                seqwindowix[posi]<-"X"
              }
              Seqwindows_multix[ii]<-paste(seqwindowix,collapse ="")
            }else{
              Seqwindows_multix[ii]<-seqwindowi[ii]
            }
          }
          Seqwindows_MultiSites[i]<-paste(Seqwindows_multix,collapse =";")
          incProgress(1/nrow(datareaddqx), detail = paste("index", i))
        }
        datareaddqx$Seqwindows_MultiSites<-Seqwindows_MultiSites
      })
      if(FALSE){#!input$classicmultisiteif
        sitesnum_main<-unlist(lapply(datareaddqx$Pep.main.index,function(x){
          length(strsplit(x,";")[[1]])
        }))
        datareaddqx2<-datareaddqx[sitesnum_main>1,]
        Seqwindows_MultiSites_main<-vector()
        withProgress(message = 'Generating data', style = "notification", detail = "index 1", value = 0,{
          for(i in 1:nrow(datareaddqx2)){
            pepindexi2<-as.numeric(strsplit(datareaddqx2$Pep.main.index[i],";")[[1]])
            seqwindowi<-strsplit(datareaddqx2$Seqwindows[i],";")[[1]]
            
            Seqwindows_multix_main<-vector()
            for(ii in 1:length(pepindexi2)){
              seqwindowix<-strsplit(seqwindowi[ii],"")[[1]]
              posi<-input$minseqs+1+(pepindexi2[-ii]-pepindexi2[ii])
              posi_low<-which(posi>length(seqwindowix) | posi<1)
              if(length(posi_low)>0){
                seqwindowix[posi[-posi_low]]<-"Z"
              }else{
                seqwindowix[posi]<-"Z"
              }
              Seqwindows_multix_main[ii]<-paste(seqwindowix,collapse ="")
              
            }
            Seqwindows_MultiSites_main[i]<-paste(Seqwindows_multix_main,collapse =";")
            incProgress(1/nrow(datareaddqx2), detail = paste("index", i))
          }
          datareaddqx$Seqwindows_MultiSites[which(sitesnum_main>1)]<-Seqwindows_MultiSites_main
        })
        #datareaddq_all1<-datareaddq[sitesnum<=1,]
        #datareaddq_all<-rbind(datareaddq_all1,datareaddqx)
      }
    }else{
      datareaddqx<-NULL
    }
    datareaddqx
    #list(datareaddq_all=datareaddq_all,datareaddq_multi=datareaddqx)
  })
  seqduiqiout<-reactive({
    duiqidfall1<-seqduiqioutx()#isolate(seqduiqioutx())
    datareaddq_multi1<-isolate(seqduiqiduositeout())
    if(is.null(datareaddq_multi1)){
      duiqidfall<-duiqidfall1
    }else{
      datareaddq_multi1$Seqwindows<-datareaddq_multi1$Seqwindows_MultiSites
      sitesnum<-unlist(lapply(duiqidfall1$Pep.main.index,function(x){
        length(strsplit(x,";")[[1]])
      }))
      if(FALSE){#!input$classicmultisiteif
        datareaddq_all1<-duiqidfall1[sitesnum<=1,]
        duiqidfall<-rbind(datareaddq_all1,datareaddq_multi1[,-ncol(datareaddq_multi1)])
      }else{
        duiqidfall<-duiqidfall1
      }
    }
    unique(duiqidfall)
  })
  seqduiqievent<-eventReactive(input$mcsbtn_seqalign,{
    datareaddq<-seqduiqiout()
  })
  output$seqduiqi<-renderDataTable({
    datareaddq<-seqduiqievent()#isolate(seqduiqiout())
    datatable(datareaddq, options = list(pageLength = 10))
  })
  output$seqduiqidl<-downloadHandler(
    filename = function(){paste("Prealign_data",usertimenum,".csv",sep="")},
    content = function(file){
      write.csv(seqduiqievent(),file,row.names=FALSE)
    }
  )
  
  output$seqduiqiplot<-renderPlot({
    datareaddq<-seqduiqievent()#isolate(seqduiqiout())
    sitesnum<-unlist(lapply(datareaddq$Pep.all.index,function(x){
      length(strsplit(x,";")[[1]])
    }))
    if(is.null(sitesnum)){
      plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
      text(x = 0.34, y = 0.9, paste("No plot here~~"),
           cex = 1.5, col = "black", family="serif", font=2, adj=0.5)
    }else{
      datareaddq1<-as.data.frame(table(sitesnum))
      ggplot(datareaddq1,aes(x=sitesnum,y=Freq, group=1))+
        geom_bar(stat = "identity",col=colpalettes[1:nrow(datareaddq1)],fill=colpalettes[1:nrow(datareaddq1)],alpha=0.8)+
        geom_line(size=1.5,col=colpalettes[17]) +
        geom_point(size=6, col=colpalettes[16],shape=18)+
        geom_text_repel(aes(label=Freq),size=6)+
        labs(x="Sites Number",y="Counts",title = "Distribution of Modification Sites")+
        theme_bw()
    }
  })
  seqduiqiplotout<-reactive({
    datareaddq<-seqduiqievent()#isolate(seqduiqiout())
    sitesnum<-unlist(lapply(datareaddq$Pep.all.index,function(x){
      length(strsplit(x,";")[[1]])
    }))
    datareaddq1<-as.data.frame(table(sitesnum))
    ggplot(datareaddq1,aes(x=sitesnum,y=Freq, group=1))+
      geom_bar(stat = "identity",col=colpalettes[1:nrow(datareaddq1)],fill=colpalettes[1:nrow(datareaddq1)],alpha=0.8)+
      geom_line(size=1.5,col=colpalettes[17]) +
      geom_point(size=6, col=colpalettes[16],shape=18)+
      geom_text_repel(aes(label=Freq),size=6)+
      labs(x="Sites Number",y="Counts",title = "Distribution of Modification Sites")+
      theme_bw()
  })
  output$seqduiqiplotdl<-downloadHandler(
    filename = function(){paste("SiteNumplot",usertimenum,".pdf",sep="")},
    content = function(file){
      pdf(file, width = 7,height = 7)
      print(seqduiqiplotout())
      dev.off()
    }
  )
  
  output$seqduiqiduosite<-renderDataTable({
    datatable(seqduiqiduositeout())
  })
  output$seqduiqiduositedl<-downloadHandler(
    filename = function(){paste("Prealign_MultiSites_data",usertimenum,".csv",sep="")},
    content = function(file){
      write.csv(seqduiqiduositeout(),file,row.names=FALSE)
    }
  )
  ##blast
  blastresout<-reactive({
    danlength<-input$minseqs
    if(input$xuanzebgdatabase==1){
      wuzhong<-strsplit(input$metabopathspeciesselect,"-")[[1]][1]
      if(is.na(wuzhong)){
        stop("Please select one background dataset or upload a fasta file as background dataset in the 'Import Data' step!")
      }else{
        datafasta<-readAAStringSet(paste0("fasta/",wuzhong,".fasta"))
      }
    }else{
      wuzhong<-input$wuzhongid
      files <- isolate(input$fastafileown)
      if(is.null(files)){
        stop("Please select one background dataset or upload a fasta file as background dataset in the 'Import Data' step!")
      }else{
        datafasta<-readAAStringSet(files$datapath)
      }
    }
    readfastaqnames<-unlist(lapply(names(datafasta),function(x){
      strsplit(strsplit(x," ")[[1]][1],"\\|")[[1]][2]
    }))
    if(sum(is.na(readfastaqnames))>length(readfastaqnames)/2){
      readfastaqnames<-unlist(lapply(names(datafasta),function(x) strsplit(x,"\\ ")[[1]][1]))
    }
    datafastahuman<-readAAStringSet(paste0("fasta/9606.fasta"))
    readfastasubjnames<-unlist(lapply(names(datafastahuman),function(x){
      strsplit(strsplit(x," ")[[1]][1],"\\|")[[1]][2]
    }))
    load(file=paste0(wuzhong,"_blast_best_seqs_9606.RData"))
    blaseresdf<-read.csv(paste0(wuzhong,"_blast_best_9606.csv"),stringsAsFactors = FALSE)
    blaseresdf<-blaseresdf[blaseresdf$evalue<=input$evalueyuzhi,]
    datasuiqiallx2<-datasuiqiallxx<<-seqduiqievent()
    datasuiqiallxx2<-separate_rows(datasuiqiallxx,6:8,sep = "::")
    datasuiqiallx<-datasuiqiall<-unique(separate_rows(datasuiqiallxx2,3:8,sep = ";"))
    datasuiqiallx$PRO.CombinedID<-paste0(datasuiqiall$PRO.from.Database,"_",datasuiqiall$Center.amino.acid,
                                         datasuiqiall$PROindex.from.Database)
    Center.amino.acids.Human<-Seqwindows.Human<-PRO.from.Human<-PROindex.from.Human<-vector()
    #Center.aa.match<-Window.similarity<-vector()
    withProgress(message = 'Generating data', style = "notification", detail = "index 1", value = 0,{
      for(i in 1:nrow(datasuiqiall)){
        centeraas<-strsplit(datasuiqiall$Center.amino.acid[i],";")[[1]]
        Pepmainindex<-as.numeric(strsplit(datasuiqiall$Pep.main.index[i],";")[[1]])
        promainindex<-as.numeric(strsplit(datasuiqiall$PROindex.from.Database[i],";")[[1]])
        grepproindex<-grep(datasuiqiall$PRO.from.Database[i],blaseresdf$query_id)
        if(length(grepproindex)>0){
          queryindex1<-blaseresdf$query_id[grepproindex]
          queryindex2<-blaseresdf$subject_id[grepproindex]
          subjectpro<-as.data.frame(datafastahuman[readfastasubjnames==queryindex2])$x
          seqnchar<-nchar(subjectpro)
          if(input$preblastif){
            alignquerydf<-blastseqlist[[queryindex1]]
          }else{
            alignquery<-msa(c(datafasta[readfastaqnames==queryindex1],datafastahuman[readfastasubjnames==queryindex2]))
            alignquerydf<-as.data.frame(alignquery@unmasked)$x
          }
          alignquerydf1<-strsplit(alignquerydf[1],"")[[1]]
          alignquerydf2<-strsplit(alignquerydf[2],"")[[1]]
          alignquerydfhengxian1<-grep("-",alignquerydf1)
          alignquerydfhengxian2<-grep("-",alignquerydf2)
          if(length(alignquerydfhengxian1)>0){
            alignquerydf3<-alignquerydf2[-alignquerydfhengxian1]
            humanhenggangindex<-which(alignquerydf3[promainindex]=="-")
            if(length(humanhenggangindex)>0){
              promainindex<-promainindex[-humanhenggangindex]
              if(length(promainindex)>0){
                blaseprocenter<-paste(alignquerydf3[promainindex],collapse=";")
              }else{
                blaseprocenter<-NA
              }
            }else{
              blaseprocenter<-paste(alignquerydf3[promainindex],collapse=";")
            }
            
            blaseproindex<-unlist(lapply(promainindex,function(x){
              x1<-sum(alignquerydfhengxian1<x)
              if(length(alignquerydfhengxian2)>0){
                x2<-sum(alignquerydfhengxian2<x1+x)
                x1+x-x2
              }else{
                x1+x
              }
            }))
          }else{
            humanhenggangindex<-which(alignquerydf2[promainindex]=="-")
            if(length(humanhenggangindex)>0){
              promainindex<-promainindex[-humanhenggangindex]
              if(length(promainindex)>0){
                blaseprocenter<-paste(alignquerydf2[promainindex],collapse=";")
              }else{
                blaseprocenter<-NA
              }
            }else{
              blaseprocenter<-paste(alignquerydf2[promainindex],collapse=";")
            }
            blaseproindex<-unlist(lapply(promainindex,function(x){
              if(length(alignquerydfhengxian2)>0){
                x2<-sum(alignquerydfhengxian2<x)
                x-x2
              }else{
                x
              }
            }))
          }
          blastpepwindows<-unlist(lapply(blaseproindex,function(x){
            indexjian1<-x-danlength
            indexjian2<-x+danlength
            if(indexjian1<=0){
              xhx1<-paste(rep("_",abs(indexjian1)+1),collapse ="")
              xhx2<-stri_sub(subjectpro,from = 0,to=indexjian2)
              xhx3<-paste0(xhx1,xhx2)
            }
            else if(indexjian2>seqnchar){
              xhx1<-paste(rep("_",(indexjian2-seqnchar)),collapse="")
              xhx2<-stri_sub(subjectpro,from = indexjian1,to=seqnchar)
              xhx3<-paste0(xhx2,xhx1)
            }
            else{
              xhx3<-stri_sub(subjectpro,from = indexjian1,to=indexjian2)
            }
          }))
          Center.amino.acids.Human[i]<-blaseprocenter#ifelse(length(blaseprocenter)>0,blaseprocenter,NA)
          Seqwindows.Human[i]<-paste(blastpepwindows,collapse = ";")
          PRO.from.Human[i]<-queryindex2#strsplit(queryindex2,"\\|")[[1]][2]
          PROindex.from.Human[i]<-paste(blaseproindex,collapse = ";")
        }else{
          Center.amino.acids.Human[i]<-NA
          Seqwindows.Human[i]<-NA
          PRO.from.Human[i]<-NA
          PROindex.from.Human[i]<-NA
        }
        incProgress(1/nrow(datasuiqiall), detail = paste("index", i))
      }
    })
    datasuiqiallx$Center.amino.acids.Human<-Center.amino.acids.Human
    datasuiqiallx$Seqwindows.Human<-Seqwindows.Human
    datasuiqiallx$PRO.from.Human<-PRO.from.Human
    datasuiqiallx$PROindex.from.Human<-PROindex.from.Human
    datasuiqiallx2<-datasuiqiallx[datasuiqiallx$Center.amino.acids.Human!="NA",]
    datasuiqiallx3<-datasuiqiallx2[!is.na(datasuiqiallx2$Seqwindows.Human),]
    datasuiqiallx3$PRO.CombinedID.Human<-paste0(datasuiqiallx3$PRO.from.Human,"_",datasuiqiallx3$Center.amino.acids.Human,
                                                datasuiqiallx3$PROindex.from.Human)
    unique(datasuiqiallx3)
  })
  blastresout2<-reactive({
    blastresoutx<<-blastresout()
    Center.aa.match<-apply(blastresoutx[,c("Center.amino.acid","Center.amino.acids.Human")],1,function(x){
      if(x[1]==x[2]){
        "Exact"
      }else{
        if(sum(x%in%c("S","T","Y"))==2){
          "Fuzzy"
        }else{
          "Not match"
        }
      }
    })
    Window.similarity<-apply(blastresoutx[,c("Seqwindows","Seqwindows.Human")],1,function(x){
      x1<-strsplit(x[1],"")[[1]]
      x2<-strsplit(x[2],"")[[1]]
      sum(x1==x2)
    })
    blastresoutx$Center.aa.match<-Center.aa.match
    blastresoutx$Window.similarity<-Window.similarity
    if(input$centeraamatach==1){
      blastresoutx1<-blastresoutx[blastresoutx$Center.aa.match=="Exact",]
    }else if(input$centeraamatach==2){
      blastresoutx1<-blastresoutx[grep("Exact|Fuzzy",blastresoutx$Center.aa.match),]
    }else{
      blastresoutx1<-blastresoutx
    }
    blastresoutx1<-blastresoutx1[blastresoutx1$Window.similarity>=input$seqmatachsimilar,]
    blastresoutx1
  })
  observeEvent(
    input$blastbtn_seqalign,{
      output$blastres<-renderDataTable({
        datatable(blastresout2())
      })
      output$blastresdl<-downloadHandler(
        filename = function(){paste("BlastToHuman_",usertimenum,".csv",sep="")},
        content = function(file){
          write.csv(blastresout2(),file,row.names=FALSE)
        }
      )
    }
  )
  #
  motiffujiout<-reactive({
    datareaddq<<-seqduiqiout()
    #if(ncol(datareaddq)==1) colnames(datareaddq)<-"Seqwindows"
    #if(ncol(datareaddq)==2) colnames(datareaddq)<-c("Seqwindows","Contain.if")
    datareadbj<-NULL#seqbjdataout()
    fastaseqownoutdf<<-fastaseqownout()
    if(input$xuanzebgdatabase==1){
      wuzhong<-strsplit(input$metabopathspeciesselect,"-")[[1]][1]
    }else{
      wuzhong<-input$wuzhongid
    }
    if(FALSE){#input$onlymultisiteif
      seqduiqiduositedf<-seqduiqiduositeout()
      fgseqs<-unique(unlist(lapply(seqduiqiduositedf$Seqwindows_MultiSites,function(x) strsplit(x,";|::")[[1]])))
    }else{
      fgseqs<-unique(unlist(lapply(datareaddq$Seqwindows,function(x) strsplit(x,";|::")[[1]])))
    }
    
    withProgress(message = 'Motif Enrichment:',min = 0, max = 2, style = "notification", detail = "Generating data", value = 1,{
      if(is.null(datareadbj)){
        if(input$xuanzebgdatabase==1){
          #if(input$motifquanbuif){
          #  load(file = paste0("winsSTY_",wuzhong,".RData"))
          #  motseq <- motifx(fg.seqs=fgseqs, bg.seqs=unique(seqseqalldf_STY$Windows), central.res = input$centralres,
          #                   min.seqs = input$minseqsnum, pval.cutoff = input$pvalcutoff)
          #}else{
          #  #warning("Please note: No background dataset is chosen or uploaded! The foreground dataset is treated as the background database by default, but this is not recommended!")
          #  motseq <- motifx(fg.seqs=fgseqs, bg.seqs=fgseqs, central.res = input$centralres,
          #                   min.seqs = input$minseqsnum, pval.cutoff = input$pvalcutoff)
          #}
          load(file = paste0("winsSTY_",wuzhong,".RData"))
          motseq <- motifx(fg.seqs=fgseqs, bg.seqs=unique(seqseqalldf_STY$Windows), central.res = input$centralres,
                           min.seqs = input$minseqsnum, pval.cutoff = input$pvalcutoff)
        }else{
          motseq <- motifx(fg.seqs=fgseqs, bg.seqs=unique(fastaseqownoutdf$Windows), central.res = input$centralres,
                           min.seqs = input$minseqsnum, pval.cutoff = input$pvalcutoff)
        }
      }else{
        motseq <- motifx(fg.seqs=fgseqs, bg.seqs=unique(datareadbj[[1]]),central.res = input$centralres,
                         min.seqs = input$minseqsnum, pval.cutoff = input$pvalcutoff)
      }
      
      shiny::incProgress(1, detail = "Generating data")
    })
    
    #motseq<-motseq
    if(is.null(motseq)){
      stop("No enrichment results, maybe you need to adjust the 'Minimum number' and/or 'P-value threshold' parameters~~")
    }
    motseqdf<-motseq$df
    motseqdf$Enrich.seq<-sapply(motseq$motiflist,function(x) paste(x$pos,collapse = ";"))
    matchpro<-sapply(motseq$motiflist,function(x){
      xx<-unlist(lapply(x$pos,function(x) grep(x,datareaddq$Seqwindows,perl=TRUE)))
      paste(unique(datareaddq$PRO.from.Database[xx]),collapse = ";")
    })
    motseqdf$Enrich.pro<-matchpro
    motseqdf
  })
  ##blast motif
  motiffujiblastout<-reactive({
    datareaddq<-seqduiqiout()
    datareaddqblast<<-blastresout()
    datareadbj<-NULL#seqbjdataout()
    fastaseqownoutdf<<-fastaseqownout()
    #wuzhong<<-strsplit(input$metabopathspeciesselect,"-")[[1]][1]
    if(FALSE){#input$onlymultisiteif
      seqduiqiduositedf<-seqduiqiduositeout()
      fgseqs<-unique(unlist(lapply(seqduiqiduositedf$Seqwindows_MultiSites,function(x) strsplit(x,";|::")[[1]])))
    }else{
      fgseqsblast<-unique(unlist(lapply(datareaddqblast$Seqwindows.Human,function(x) strsplit(x,";|::")[[1]])))
      #fgseqsblast<-fgseqsblast[nchar(fgseqsblast)==(2*input$minseqs+1)]
    }
    
    withProgress(message = 'Motif Enrichment:',min = 0, max = 2, style = "notification", detail = "Generating data", value = 1,{
      if(is.null(datareadbj)){
        if(input$xuanzebgdatabase==1){
          load(file = paste0("winsSTY_9606.RData"))
          motseqblast <- motifx(fg.seqs=fgseqsblast, bg.seqs=unique(seqseqalldf_STY$Windows), central.res = input$centralres,
                                min.seqs = input$minseqsnum, pval.cutoff = input$pvalcutoff)
        }else{
          motseqblast <- motifx(fg.seqs=fgseqsblast, bg.seqs=unique(fastaseqownoutdf$Windows), central.res = input$centralres,
                                min.seqs = input$minseqsnum, pval.cutoff = input$pvalcutoff)
        }
      }else{
        motseqblast <- motifx(fg.seqs=fgseqsblast, bg.seqs=unique(datareadbj[[1]]),central.res = input$centralres,
                              min.seqs = input$minseqsnum, pval.cutoff = input$pvalcutoff)
      }
      shiny::incProgress(1, detail = "Generating data")
    })
    
    #motseq<-motseq
    if(is.null(motseqblast)){
      stop("No enrichment results, maybe you need to adjust the 'Minimum number' and/or 'P-value threshold' parameters~~")
    }
    motseqdf<-motseqblast$df
    motseqdf$Enrich.seq<-sapply(motseqblast$motiflist,function(x) paste(x$pos,collapse = ";"))
    matchpro<-sapply(motseqblast$motiflist,function(x){
      xx<-unlist(lapply(x$pos,function(x) grep(x,datareaddq$Seqwindows,perl=TRUE)))
      paste(unique(datareaddq$PRO.from.Database[xx]),collapse = ";")
    })
    motseqdf$Enrich.pro<-matchpro
    motseqdf
  })
  motiffujiout2<-reactive({
    datareaddq<-seqduiqiout()
    if(ncol(datareaddq)<3){
      tabdata4<-NULL
    }else{
      motiffujioutx<-motiffujiout()[,-9]
      tabdata1<-tidyr::separate_rows(motiffujioutx, Enrich.seq, sep =";")
      tabdata1x<-unique(tabdata1)
      tabdata2<-tidyr::separate_rows(datareaddq[,-c(2:4)], Seqwindows,PROindex.from.Database, sep =";")
      tabdata3<-unique(tabdata2)
      tabdata4<-base::merge(tabdata1x,tabdata3,by.x="Enrich.seq",by.y="Seqwindows",sort=FALSE)
    }
    tabdata4
  })
  regularmotiffujiout<-reactive({
    datareaddq<-seqduiqiout()
    if(ncol(datareaddq)==1) colnames(datareaddq)<-"Seqwindows"
    if(ncol(datareaddq)==2) colnames(datareaddq)<-c("Seqwindows","Contain.if")
    datareadbj<-seqbjdataout()
    fastaseqownoutdf<-fastaseqownout()
    if(input$xuanzebgdatabase==1){
      wuzhong<-strsplit(input$metabopathspeciesselect,"-")[[1]][1]
    }else{
      wuzhong<-input$wuzhongid
    }
    #wuzhong<-strsplit(input$metabopathspeciesselect,"-")[[1]][1]
    if(FALSE){#input$onlymultisiteif
      seqduiqiduositedf<-seqduiqiduositeout()
      datareaddq1<-seqduiqiduositedf$Seqwindows_MultiSites[seqduiqiduositedf$Contain.if=="Yes"]
      fgseqs<-unique(unlist(lapply(datareaddq1,function(x) strsplit(x,";|::")[[1]])))
    }else{
      datareaddq1<-datareaddq$Seqwindows[datareaddq$Contain.if=="Yes"]
      fgseqs<-unique(unlist(lapply(datareaddq1,function(x) strsplit(x,";|::")[[1]])))
    }
    withProgress(message = 'Motif Enrichment:',min = 0, max = 2, style = "notification", detail = "Generating data", value = 1,{
      if(is.null(datareadbj)){
        if(input$xuanzebgdatabase==1){
          #if(input$motifquanbuif){
          #  load(file = paste0("winsSTY_",wuzhong,".RData"))
          #  motseq <- motifx(fg.seqs=fgseqs, bg.seqs=unique(seqseqalldf_STY$Windows), central.res = input$centralres,
          #                   min.seqs = input$minseqsnum, pval.cutoff = input$pvalcutoff)
          #}else{
          #  #warning("Please note: No background dataset is chosen or uploaded! The foreground dataset is treated as the background database by default, but this is not recommended!")
          #  motseq <- motifx(fg.seqs=fgseqs, bg.seqs=fgseqs, central.res = input$centralres,
          #                   min.seqs = input$minseqsnum, pval.cutoff = input$pvalcutoff)
          #}
          load(file = paste0("winsSTY_",wuzhong,".RData"))
          motseq <- motifx(fg.seqs=fgseqs, bg.seqs=unique(seqseqalldf_STY$Windows), central.res = input$centralres,
                           min.seqs = input$minseqsnum, pval.cutoff = input$pvalcutoff)
        }else{
          motseq <- motifx(fg.seqs=fgseqs, bg.seqs=unique(fastaseqownoutdf$Windows), central.res = input$centralres,
                           min.seqs = input$minseqsnum, pval.cutoff = input$pvalcutoff)
        }
      }else{
        motseq <- motifx(fg.seqs=fgseqs, bg.seqs=unique(datareadbj[[1]]),central.res = input$centralres,
                         min.seqs = input$minseqsnum, pval.cutoff = input$pvalcutoff)
      }
      
      shiny::incProgress(1, detail = "Generating data")
    })
    
    #motseq<-motseq
    if(is.null(motseq)){
      stop("No enrichment results, maybe you need to adjust the 'Minimum number' and/or 'P-value threshold' parameters~~")
    }
    motseqdf<-motseq$df
    motseqdf$Enrich.seq<-sapply(motseq$motiflist,function(x) paste(x$pos,collapse = ";"))
    matchpro<-sapply(motseq$motiflist,function(x){
      xx<-unlist(lapply(x$pos,function(x) grep(x,datareaddq$Seqwindows,perl=TRUE)))
      paste(unique(datareaddq$PRO.from.Database[xx]),collapse = ";")
    })
    motseqdf$Enrich.pro<-matchpro
    motseqdf
  })
  motifplot_height <- reactive({
    heightx<-input$motifplot_height
    heightx
  })
  observeEvent(
    input$mcsbtn_motifquanbu,{
      shinyjs::show(id = "motiffujidfxuanze_btn", anim = FALSE)
      output$motiffuji<-renderDataTable({
        motiffujidf<-isolate(motiffujiout())
        datatable(motiffujidf, options = list(pageLength = 10))
      })
      output$motiffujidl<-downloadHandler(
        filename = function(){paste("Motif.Enrich_uploaded",usertimenum,".csv",sep="")},
        content = function(file){
          write.csv(motiffujiout(),file,row.names=FALSE)
        }
      )
      #blast motif
      output$motiffujiblast<-renderDataTable({
        motiffujidf<-motiffujiblastout()
        datatable(motiffujidf, options = list(pageLength = 10))
      })
      output$motiffujiblastdl<-downloadHandler(
        filename = function(){paste("Motif.Enrich_blast",usertimenum,".csv",sep="")},
        content = function(file){
          write.csv(motiffujiblastout(),file,row.names=FALSE)
        }
      )
      #
      output$motiffuji2<-renderDataTable({
        motiffujidf<-isolate(motiffujiout2())
        datatable(motiffujidf, options = list(pageLength = 10))
      })
      output$motiffujidl2<-downloadHandler(
        filename = function(){paste("Motif.Enrich.mapped_data",usertimenum,".csv",sep="")},
        content = function(file){
          write.csv(motiffujiout2(),file,row.names=FALSE)
        }
      )
      #
      output$regularmotiffuji<-renderDataTable({
        motiffujidf<-isolate(regularmotiffujiout())
        datatable(motiffujidf, options = list(pageLength = 10))
      })
      output$regularmotiffujidl<-downloadHandler(
        filename = function(){paste("RegularMotif.Enrich_data",usertimenum,".csv",sep="")},
        content = function(file){
          write.csv(regularmotiffujiout(),file,row.names=FALSE)
        }
      )
      ##############
      output$motifplot<-renderPlot({
        motiffujidf<-isolate(motiffujiout())
        enrichseqnumstr<-isolate(as.numeric(strsplit(input$enrichseqnum,"-|;")[[1]]))
        if(input$equalheightif){
          equalh<-"probability"
        }else{
          equalh<-"bits"
        }
        if(length(enrichseqnumstr)==1){
          enrichseq<-strsplit(motiffujidf$Enrich.seq[enrichseqnumstr],";")[[1]]
          ggseqlogo(enrichseq, method=equalh)+
            scale_x_discrete(limits=as.character(-input$minseqs:input$minseqs))
        }else{
          motiffujidf1<-motiffujidf[enrichseqnumstr[1]:enrichseqnumstr[2],]
          enrichseq<-lapply(motiffujidf1$Enrich.seq,function(x){
            xx<-strsplit(x,";")[[1]]
          })
          names(enrichseq)<-motiffujidf1$motif
          ggseqlogo(enrichseq, ncol = 2, method=equalh)+
            scale_x_discrete(limits=as.character(-input$minseqs:input$minseqs))
        }
      },height = motifplot_height)
      motifplotout<-reactive({
        motiffujidf<-isolate(motiffujiout())
        enrichseqnumstr<-isolate(as.numeric(strsplit(input$enrichseqnum,"-|;")[[1]]))
        if(input$equalheightif){
          equalh<-"probability"
        }else{
          equalh<-"bits"
        }
        if(length(enrichseqnumstr)==1){
          enrichseq<-strsplit(motiffujidf$Enrich.seq[enrichseqnumstr],";")[[1]]
          ggseqlogo(enrichseq, method=equalh)+
            scale_x_discrete(limits=as.character(-input$minseqs:input$minseqs))
        }else{
          motiffujidf1<-motiffujidf[enrichseqnumstr[1]:enrichseqnumstr[2],]
          enrichseq<-lapply(motiffujidf1$Enrich.seq,function(x){
            xx<-strsplit(x,";")[[1]]
          })
          names(enrichseq)<-motiffujidf1$motif
          ggseqlogo(enrichseq, ncol = 2, method=equalh)+
            scale_x_discrete(limits=as.character(-input$minseqs:input$minseqs))
        }
      })
      output$motifplotdownload<-downloadHandler(
        filename = function(){paste("Motifplot_uploaded",usertimenum,".pdf",sep="")},
        content = function(file){
          pdf(file, width = motifplot_height()/100,height = motifplot_height()/100+3)
          print(motifplotout())
          dev.off()
        }
      )
      #blast motif plot
      output$motifblastplot<-renderPlot({
        motifblastfujidf<-motiffujiblastout()
        enrichseqnumstr<-isolate(as.numeric(strsplit(input$enrichseqnum,"-|;")[[1]]))
        if(input$equalheightif){
          equalh<-"probability"
        }else{
          equalh<-"bits"
        }
        if(length(enrichseqnumstr)==1){
          enrichseq<-strsplit(motifblastfujidf$Enrich.seq[enrichseqnumstr],";")[[1]]
          ggseqlogo(enrichseq, method=equalh)+
            scale_x_discrete(limits=as.character(-input$minseqs:input$minseqs))
        }else{
          motifblastfujidf1<-motifblastfujidf[enrichseqnumstr[1]:enrichseqnumstr[2],]
          enrichseq<-lapply(motifblastfujidf1$Enrich.seq,function(x){
            xx<-strsplit(x,";")[[1]]
          })
          names(enrichseq)<-motifblastfujidf1$motif
          ggseqlogo(enrichseq, ncol = 2, method=equalh)+
            scale_x_discrete(limits=as.character(-input$minseqs:input$minseqs))
        }
      },height = motifplot_height)
      motifblastplotout<-reactive({
        motifblastfujidf<-motiffujiblastout()
        enrichseqnumstr<-isolate(as.numeric(strsplit(input$enrichseqnum,"-|;")[[1]]))
        if(input$equalheightif){
          equalh<-"probability"
        }else{
          equalh<-"bits"
        }
        if(length(enrichseqnumstr)==1){
          enrichseq<-strsplit(motifblastfujidf$Enrich.seq[enrichseqnumstr],";")[[1]]
          ggseqlogo(enrichseq, method=equalh)+
            scale_x_discrete(limits=as.character(-input$minseqs:input$minseqs))
        }else{
          motifblastfujidf1<-motifblastfujidf[enrichseqnumstr[1]:enrichseqnumstr[2],]
          enrichseq<-lapply(motifblastfujidf1$Enrich.seq,function(x){
            xx<-strsplit(x,";")[[1]]
          })
          names(enrichseq)<-motifblastfujidf1$motif
          ggseqlogo(enrichseq, ncol = 2, method=equalh)+
            scale_x_discrete(limits=as.character(-input$minseqs:input$minseqs))
        }
      })
      output$motifblastplotdl<-downloadHandler(
        filename = function(){paste("Motifplot_blast",usertimenum,".pdf",sep="")},
        content = function(file){
          pdf(file, width = motifplot_height()/100,height = motifplot_height()/100+3)
          print(motifblastplotout())
          dev.off()
        }
      )
    }
  )
  #
  fastaseqout<-reactive({
    files <<- input$fastafile
    if(is.null(files)){
      datareadfasta<-NULL
    }else{
      datafasta<-readAAStringSet(files$datapath)
      pro_seqdf<-pro_seqdf1<-as.data.frame(datafasta)
      pro_seqdf_rown1<-unlist(lapply(rownames(pro_seqdf1),function(x) strsplit(x,"\\|")[[1]][1]))
      pro_seqdf_rown2<-unlist(lapply(rownames(pro_seqdf1),function(x) strsplit(x,"\\|")[[1]][2]))
      if(sum(duplicated(pro_seqdf_rown1))>=1 & sum(duplicated(pro_seqdf_rown2))>=1){
        pro_seqdf_rown<-rownames(pro_seqdf1)
      }
      else if(sum(duplicated(pro_seqdf_rown1))>=1){
        pro_seqdf_rown<-pro_seqdf_rown2
      }
      else if(sum(duplicated(pro_seqdf_rown2))>=1){
        pro_seqdf_rown<-pro_seqdf_rown1
      }
      else{
        pro_seqdf_rown<-rownames(pro_seqdf1)
      }
      rownames(pro_seqdf1)<-pro_seqdf_rown
      pro_seqdfncar<-unlist(lapply(pro_seqdf1$x,nchar))
      pro_seqdf<-pro_seqdf1[pro_seqdfncar>20,,drop=FALSE]
      n_data_fasta<-nrow(pro_seqdf)
      danlength<-input$minseqs
      wincenter<-strsplit(input$centralres,"")[[1]]
      seqwindowsall_S<-vector()
      seqnamesall_S<-vector()
      wincenteri<-vector()
      k<-1
      for(ii in wincenter){
        withProgress(message = paste('Generating data',ii), style = "notification", detail = "index 1", value = 0,{
          for(i in 1:n_data_fasta){
            seqindex1<-stri_locate_all(pattern = ii, pro_seqdf$x[i], fixed = TRUE)[[1]][,1]
            if(length(seqindex1)>0){
              seqnchar<-nchar(pro_seqdf$x[i])
              seqseq<-vector()
              for(j in 1:length(seqindex1)){
                indexjian1<-seqindex1[j]-danlength
                indexjian2<-seqindex1[j]+danlength
                if(indexjian1<=0){
                  xhx1<-paste(rep("_",abs(indexjian1)+1),collapse ="")
                  xhx2<-stri_sub(pro_seqdf$x[i],from = 0,to=indexjian2)
                  xhx3<-paste0(xhx1,xhx2)
                }
                else if(indexjian2>seqnchar){
                  xhx1<-paste(rep("_",(indexjian2-seqnchar)),collapse="")
                  xhx2<-stri_sub(pro_seqdf$x[i],from = indexjian1,to=seqnchar)
                  xhx3<-paste0(xhx2,xhx1)
                }
                else{
                  xhx3<-stri_sub(pro_seqdf$x[i],from = indexjian1,to=indexjian2)
                }
                seqwindowsall_S[k]<-xhx3
                seqnamesall_S[k]<-rownames(pro_seqdf)[i]
                wincenteri[k]<-ii
                k<-k+1
              }
            }
            incProgress(1/n_data_fasta, detail = paste("index", i))
          }
        })
      }
      datareadfasta<-data.frame(ID=seqnamesall_S,Windows=seqwindowsall_S,
                                Center=wincenteri,stringsAsFactors = F)
    }
    datareadfasta
  })
  observeEvent(
    input$mcsbtn_fastaalign,{
      output$allfasta<-renderDataTable({
        if(input$refastafileif){
          datareaddq<-isolate(fastaseqout())
        }else{
          datareaddq<-isolate(fastaseqownout())
        }
        datatable(datareaddq, options = list(pageLength = 10))
      })
      output$allfastadl<-downloadHandler(
        filename = function(){paste("Fasta.align_data",usertimenum,".csv",sep="")},
        content = function(file){
          fwrite(fastaseqout(),file)
        }
      )
    }
  )
  #
  kinasedataout<-reactive({
    #load(file = "PSP_NetworKIN_Kinase_Substrate_Dataset_July2016.rdata")
    KSData<-read.csv("Kinase_Substrate_Dataset.csv",stringsAsFactors = F)
    KSData.filter<-KSData[,c(1,3,7,8)]#,4,9
    datareaddqblast<<-blastresout()
    if(input$annotationxuanze==1){
      datareaddqblast1<-datareaddqblast[,c("Pep.upload","Pep.all.index","Center.amino.acid",
                                           "Seqwindows","PRO.from.Database","PROindex.from.Database")]
      datareaddqblast2<-base::merge(datareaddqblast1,KSData.filter,by.x="PRO.from.Database",
                                    by.y="KIN_ACC_ID",sort=FALSE)
      datareaddqblast2<-datareaddqblast2[,c(1,8,2:7,9)]
      #if(input$matchtypex==1){
      #  datareaddqblast2<-base::merge(datareaddqblast1,KSData.filter,by.x="PRO.from.Database",
      #                                  by.y="KIN_ACC_ID",sort=FALSE)
      #  datareaddqblast2<-datareaddqblast2[,c(1,8,2:7,9)]
      #}else{
      #  datareaddqblast2<-base::merge(datareaddqblast1,KSData.filter,by.x="PRO.from.Database",
      #                                  by.y="SUB_ACC_ID",sort=FALSE)
      #  datareaddqblast2<-datareaddqblast2[,c(8,1:7,9)]
      #}
    }else{
      datareaddqblast1<-datareaddqblast[,c("Pep.upload","Pep.all.index","Center.amino.acids.Human",
                                           "Seqwindows.Human","PRO.from.Human","PROindex.from.Human")]
      datareaddqblast2<-base::merge(datareaddqblast1,KSData.filter,by.x="PRO.from.Human",
                                    by.y="KIN_ACC_ID",sort=FALSE)
      datareaddqblast2<-datareaddqblast2[,c(1,8,2:7,9)]
      #if(input$matchtypex==1){
      #  datareaddqblast2<-base::merge(datareaddqblast1,KSData.filter,by.x="PRO.from.Human",
      #                                by.y="KIN_ACC_ID",sort=FALSE)
      #  datareaddqblast2<-datareaddqblast2[,c(1,8,2:7,9)]
      #}else{
      #  datareaddqblast2<-base::merge(datareaddqblast1,KSData.filter,by.x="PRO.from.Human",
      #                                by.y="SUB_ACC_ID",sort=FALSE)
      #  datareaddqblast2<-datareaddqblast2[,c(8,1:7,9)]
      #}
    }
    datareaddqblast3<-unique(datareaddqblast2)
    colnames(datareaddqblast3)[1:2]<-c("KIN_ACC_ID","SUB_ACC_ID")
    datareaddqblast3
  })
  
  output$kinasemotifui<-renderUI({
    kkdf<-kinasedataout()
    selectInput("kinasemotif","Select one or more kinases for network plot:",choices = c("All",unique(kkdf$KIN_ACC_ID)),selected = "All",multiple = TRUE)
    #bsTooltip("kinasemotif","By default, 'All' is selected and means selecting all kinases to plot network. Otherwise, users can delete 'All' and select one or several kinases to plot network.",
    #          placement = "right",options = list(container = "body"))
  })
  
  observeEvent(
    input$mcsbtn_kniase,{
      output$kinasedata<-renderDataTable({
        kinasedatadf<<-kinasedataout()
        if(nrow(kinasedatadf)==0){
          datareadxx<-data.frame(Description="No annotation data!")
          datatable(datareadxx)
        }else{
          kinasedatadf1<-paste0("https://www.uniprot.org/uniprot/",kinasedatadf[[1]])
          kinasedatadf[[1]]<-paste0("<a href='",kinasedatadf1,"' target='_blank'>",kinasedatadf[[1]],"</a>")
          kinasedatadf2<-paste0("https://www.uniprot.org/uniprot/",kinasedatadf[[2]])
          kinasedatadf[[2]]<-paste0("<a href='",kinasedatadf2,"' target='_blank'>",kinasedatadf[[2]],"</a>")
          datatable(kinasedatadf,escape = FALSE,selection="single",class = "cell-border hover",
                    options = list(pageLength = 10,columnDefs = list(list(className = 'dt-center', targets = 0:2))))
        }
      })
      output$kinasedatadl<-downloadHandler(
        filename = function(){paste("KinaseSubstrate_data",usertimenum,".csv",sep="")},
        content = function(file){
          write.csv(kinasedataout(),file,row.names=FALSE)
        }
      )
      #
      cmheatmap_height<-reactive({
        input$cmheatmap_height
      })
      cmheatmappicdataout<-reactive({
        kinasemotifx<<-input$kinasemotif
        dfmerge1<<-kinasedataout()
        if(kinasemotifx[1]=="All"){
          dfmerge<-dfmerge1
        }else{
          dfmerge<-dfmerge1[dfmerge1$KIN_ACC_ID%in%kinasemotifx,]
        }
        if(input$genenamesif){
          edgesdf<-data.frame(from=dfmerge$GENE,to=dfmerge$SUB_GENE,stringsAsFactors = FALSE)
          edgesdf<-unique(edgesdf)
          nodesdf1<-data.frame(name=c(dfmerge$GENE,dfmerge$SUB_GENE),
                               Groups=c(rep("Kinase",length(dfmerge$GENE)),rep("Substrate",length(dfmerge$SUB_GENE))),
                               stringsAsFactors = FALSE)
          nodesdf3<-nodesdf2<-unique(nodesdf1)
          jiaohudouyou<-intersect(dfmerge$GENE,dfmerge$SUB_GENE)
          if(length(jiaohudouyou)>0) nodesdf3$Groups[nodesdf2$name %in% jiaohudouyou]<-"Combine"
          nodesdf<-unique(nodesdf3)
        }else{
          edgesdf<-data.frame(from=dfmerge$KIN_ACC_ID,to=dfmerge$SUB_ACC_ID,stringsAsFactors = FALSE)
          edgesdf<-unique(edgesdf)
          nodesdf1<-data.frame(name=c(dfmerge$KIN_ACC_ID,dfmerge$SUB_ACC_ID),
                               Groups=c(rep("Kinase",length(dfmerge$KIN_ACC_ID)),rep("Substrate",length(dfmerge$SUB_ACC_ID))),
                               stringsAsFactors = FALSE)
          nodesdf3<-nodesdf2<-unique(nodesdf1)
          jiaohudouyou<-intersect(dfmerge$KIN_ACC_ID,dfmerge$SUB_ACC_ID)
          if(length(jiaohudouyou)>0) nodesdf3$Groups[nodesdf2$name %in% jiaohudouyou]<-"Combine"
          nodesdf<-unique(nodesdf3)
        }
        list(nodesdf=nodesdf,edgesdf=edgesdf)
      })
      output$cmheatmappic<-renderPlot({
        nodesdf<-cmheatmappicdataout()$nodesdf
        edgesdf<-cmheatmappicdataout()$edgesdf
        gp<-graph_from_data_frame(edgesdf, directed=TRUE, vertices=nodesdf)
        V(gp)$Groups <- nodesdf$Groups
        #,layout="stress"
        ggraph(gp, layout = 'kk')+
          geom_edge_link(aes(col=I("grey60")),width=0.6,arrow = arrow(length = unit(4, 'mm')),show.legend=FALSE)+
          geom_node_point(aes(col=Groups),size=5)+geom_node_text(aes(label = name), nudge_x = 0.1, nudge_y = 0.2)+
          scale_color_brewer(palette = "Set1")+
          theme_graph(base_family="sans")
      },height = cmheatmap_height)
      cmheatmappicout<-reactive({
        nodesdf<-cmheatmappicdataout()$nodesdf
        edgesdf<-cmheatmappicdataout()$edgesdf
        gp<-graph_from_data_frame(edgesdf, directed=TRUE, vertices=nodesdf)
        V(gp)$Groups <- nodesdf$Groups
        #,layout="stress"
        ggraph(gp, layout = 'kk')+
          geom_edge_link(aes(col=I("grey60")),width=0.6,arrow = arrow(length = unit(4, 'mm')),show.legend=FALSE)+
          geom_node_point(aes(col=Groups),size=5)+geom_node_text(aes(label = name), nudge_x = 0.1, nudge_y = 0.2)+
          scale_color_brewer(palette = "Set1")+
          theme_graph(base_family="sans")
      })
      output$cmheatmappicdl<-downloadHandler(
        filename = function(){paste("KinaseSubstrate_network",usertimenum,".pdf",sep="")},
        content = function(file){
          pdf(file, width = cmheatmap_height()/80+3,height = cmheatmap_height()/80+2)
          print(cmheatmappicout())
          dev.off()
        }
      )
      output$nodetableres<-renderDataTable({
        datatable(cmheatmappicdataout()$nodesdf)
      })
      output$nodetableresdl<-downloadHandler(
        filename = function(){paste("Nodesdata",usertimenum,".csv",sep="")},
        content = function(file){
          write.csv(cmheatmappicdataout()$nodesdf,file,row.names=FALSE)
        }
      )
      output$edgetableres<-renderDataTable({
        datatable(cmheatmappicdataout()$edgesdf)
      })
      output$edgetableresdl<-downloadHandler(
        filename = function(){paste("Edgesdata",usertimenum,".csv",sep="")},
        content = function(file){
          write.csv(cmheatmappicdataout()$edgesdf,file,row.names=FALSE)
        }
      )
    }
  )
  #####Interaction data
  examplepeakdatas<-reactive({
    dataread<-read.csv("Expression_Exampledata.csv",stringsAsFactors = F,check.names = F)
    dataread
  })
  exampleinterdataout<-reactive({
    dataread2<-read.csv("SARS_HumanProteins.csv",stringsAsFactors = FALSE)
    dataread2
  })
  output$loaddatadownload1<-downloadHandler(
    filename = function(){paste("Example_ExpressionData_",usertimenum,".csv",sep="")},
    content = function(file){
      write.csv(examplepeakdatas(),file,row.names = FALSE)
    }
  )
  output$interdatadownload1<-downloadHandler(
    filename = function(){paste("Example_InteractionData_",usertimenum,".csv",sep="")},
    content = function(file){
      write.csv(exampleinterdataout(),file,row.names = FALSE)
    }
  )
  peaksdataout<-reactive({
    files1 <- input$file1
    if(is.null(files1)){
      dataread<-data.frame(Description="motifeR 2.0 detects that you do not upload your data. Please upload the expression data, or load the example data to check first.")
      list(yuanshidf=dataread)
    }else{
      if(sum(input$firstcol)==1){
        rownametf<-1
      }else{
        rownametf<-NULL
      }
      dataread<-read.csv(files1$datapath,header=input$header,
                         row.names = rownametf,stringsAsFactors = F)
      dataread1<-dataread[,-c(1,2,3)]
      dataread2<-dataread[,c(1,2,3)]
      #rowpaste<-apply(dataread2,1,function(x){
      #  paste0(x,collapse = "_")
      #})
      rowpaste<-paste0(dataread2$PTM.ProteinId,"_",dataread2$PTM.SiteAA,"_",dataread2$PTM.SiteLocation)
      dataread1x<-dataread1[!duplicated(rowpaste),]
      rownames(dataread1x)<-rowpaste[!duplicated(rowpaste)]
      list(yuanshidf=dataread,yuanshidata=dataread1x,objectinfo=dataread2)
    }
  })
  output$peaksdata<-renderDataTable({
    if(input$loaddatatype==1){
      datatable(peaksdataout()$yuanshidf, options = list(pageLength = 10))
    }else{
      datatable(examplepeakdatas(), options = list(pageLength = 10))
    }
  })
  interactiondataout<-reactive({
    files1 <- input$Interfile1
    if(is.null(files1)){
      dataread<-data.frame(Description="motifeR 2.0 detects that you do not upload your data. Please upload the interaction data, or load the example data to check first.")
      dataread
    }else{
      if(sum(input$firstcol)==1){
        rownametf<-1
      }else{
        rownametf<-NULL
      }
      dataread<-read.csv(files1$datapath,header=input$header,
                         row.names = rownametf,stringsAsFactors = F)
      dataread
    }
    dataread
  })
  output$interactiondata<-renderDataTable({
    if(input$loaddatatype==1){
      datatable(interactiondataout(), options = list(pageLength = 10))
    }else{
      datatable(exampleinterdataout(), options = list(pageLength = 10))
    }
  })
  processedEdataout<-reactive({
    if(input$loaddatatype==1){
      nadatax<-peaksdataout()$yuanshidata
    }else{
      dataread<-examplepeakdatas()
      dataread1<-dataread[,-c(1,2,3)]
      dataread2<-dataread[,c(1,2,3)]
      #rowpaste<-apply(dataread2,1,function(x){
      #  paste0(x,collapse = "_")
      #})
      rowpaste<-paste0(dataread2$PTM.ProteinId,"_",dataread2$PTM.SiteAA,"_",dataread2$PTM.SiteLocation)
      dataread1x<-dataread1[!duplicated(rowpaste),]
      rownames(dataread1x)<-rowpaste[!duplicated(rowpaste)]
      nadatax<-dataread1x
    }
    datadfchuli<-nadatax
    if(input$mediannormif){
      medianval<-apply(datadfchuli,2,function(x) {median(x,na.rm = TRUE)})
      datadfchuli<-sweep(datadfchuli,2,medianval,FUN = "/")
    }
    if(input$logif){
      datadfchuli<-log2(datadfchuli)
    }
    narowsum<-apply(datadfchuli,1,function(x){sum(is.na(x))})/ncol(datadfchuli)
    datadfchuli<-datadfchuli[narowsum<=0.5,]
    data_zero1<-impute.knn(as.matrix(datadfchuli),k = 10, rowmax = 1, colmax = 1)
    datadfchuli1<<-data_zero1$data
    datadfchuli1
  })
  output$processedEdata<-renderDataTable({
    datatable(processedEdataout(), options = list(pageLength = 10))
  })
  output$processedEdatadl<-downloadHandler(
    filename = function(){paste("ProcessedExpressionData_",usertimenum,".csv",sep="")},
    content = function(file){
      write.csv(processedEdataout(),file,row.names = FALSE)
    }
  )
  ##
  output$sarsproteinsui<-renderUI({
    if(input$loaddatatype==1){
      sarsdata<-interactiondataout()
    }else{
      sarsdata<-exampleinterdataout()
    }
    #sarsdata<<-read.csv("SARS_HumanProteins.csv",stringsAsFactors = FALSE)
    selectInput("sarsproteins",h5("Select one interacting protein:"),choices = unique(sarsdata[[1]]),selected = 1,multiple = FALSE)
    #bsTooltip("kinasemotif","By default, 'All' is selected and means selecting all kinases to plot network. Otherwise, users can delete 'All' and select one or several kinases to plot network.",
    #          placement = "right",options = list(container = "body"))
  })
  interactfigheightx<-reactive({
    input$interactfigheight
  })
  interactfigwidthx<-reactive({
    input$interactfigheight*0.8
  })
  output$interactplot<-renderPlot({
    if(is.null(input$sarsproteins)){
      sarsproteinsx<<-"nsp1"
    }else{
      sarsproteinsx<<-input$sarsproteins
    }
    
    if(input$loaddatatype==1){
      sarsdata<-interactiondataout()
      blastresoutx<-blastresout()
      classnames<-strsplit(input$grnames,";")[[1]]
      grnum1<-as.numeric(strsplit(input$grnums,";")[[1]][1])
      classnamesnum<-as.numeric(strsplit(strsplit(input$grnums,";")[[1]][2],"-")[[1]])
    }else{
      sarsdata<-exampleinterdataout()
      blastresoutx<-read.csv("BlastToHuman_1628320018.448.csv",stringsAsFactors = FALSE)
      classnames<-strsplit(input$examgrnames,";")[[1]]
      grnum1<-as.numeric(strsplit(input$examgrnums,";")[[1]][1])
      classnamesnum<-as.numeric(strsplit(strsplit(input$examgrnums,";")[[1]][2],"-")[[1]])
    }
    sarsdata<<-sarsdata
    sarsdatanames<-colnames(sarsdata)
    blastresoutx<<-blastresoutx
    #sarsdata<<-read.csv("SARS_HumanProteins.csv",stringsAsFactors = FALSE)
    expressdata<<-processedEdataout()
    classnames<<-classnames
    classnamesnum<<-classnamesnum
    grinfo<<-rep(classnames,times=classnamesnum)
    #expressdata2<-as.data.frame(cbind(grinfo=factor(grinfo,levels=classnames),t(expressdata)))
    #expressdata3<-stats::aggregate(.~grinfo,expressdata2,mean)
    expressdata2<-NULL
    for(i in 1:length(classnames)){
      expressdata2i<-expressdata[,grinfo==classnames[i],drop=FALSE]
      expressdata2<-cbind(expressdata2,rowMeans(expressdata2i,na.rm = TRUE))
    }
    colnames(expressdata2)<-classnames
    ##
    sarsdata1<-sarsdata[sarsdata[[1]]==sarsproteinsx,]#$Bait
    expresspros<-unlist(lapply(rownames(expressdata2),function(x){
      strsplit(x,"_")[[1]][1]
    }))
    expresscaas<-unlist(lapply(rownames(expressdata2),function(x){
      strsplit(x,"_")[[1]][2]
    }))
    expresssites<-unlist(lapply(rownames(expressdata2),function(x){
      strsplit(x,"_")[[1]][3]
    }))
    expressphositesUpload<-paste0(expresspros,"_",paste0(expresscaas,expresssites))
    expressdata3<-as.data.frame(expressdata2)
    expressdata3$uploadpros<-expressphositesUpload
    blastresoutx2<-unique(blastresoutx[blastresoutx$PRO.from.Human%in%sarsdata1[[2]],
                                       c("PRO.CombinedID","PRO.CombinedID.Human")])#$Preys
    if(nrow(blastresoutx2)==0){
      blastresoutx2<-unique(blastresoutx[blastresoutx$PRO.from.Database%in%sarsdata1[[2]],
                                         c("PRO.CombinedID","PRO.CombinedID.Human")])
      expressdata4<-base::merge(blastresoutx2,expressdata3,by.x = "PRO.CombinedID", by.y = "uploadpros",sort=FALSE)
      PROComIDHuman<-expressdata4$PRO.CombinedID
      for(i in 1:length(sarsdata1[[2]])){#$Preys
        PROComIDHumani<-grep(sarsdata1[[2]][i],PROComIDHuman)#$Preys
        if(length(PROComIDHumani)>0){
          PROComIDHuman[PROComIDHumani]<-gsub(sarsdata1[[2]][i],sarsdata1$PreyGene[i],PROComIDHuman[PROComIDHumani])
        }
      }
      PROComIDHuman1<-unlist(lapply(PROComIDHuman,function(x){
        strsplit(x,"_")[[1]][1]
      }))
      PROComIDHuman2<-unlist(lapply(PROComIDHuman,function(x){
        strsplit(x,"_")[[1]][2]
      }))
      nodesdf<-data.frame(name=c(sarsproteinsx,unique(PROComIDHuman1),unique(PROComIDHuman2)),
                          group=c(sarsdatanames[1],rep(sarsdatanames[3],length(unique(PROComIDHuman1))),rep("Pho Site",length(unique(PROComIDHuman2)))),
                          size=c(2,rep(1,length(c(unique(PROComIDHuman1),unique(PROComIDHuman2))))))
      edgesdf<-data.frame(from=c(rep(sarsproteinsx,length(unique(PROComIDHuman1))),PROComIDHuman1),
                          to=c(unique(PROComIDHuman1),PROComIDHuman2))
    }else{
      expressdata4<-base::merge(blastresoutx2,expressdata3,by.x = "PRO.CombinedID", by.y = "uploadpros",sort=FALSE)
      PROComIDHuman<-expressdata4$PRO.CombinedID.Human
      for(i in 1:length(sarsdata1[[2]])){
        PROComIDHumani<-grep(sarsdata1[[2]][i],PROComIDHuman)
        if(length(PROComIDHumani)>0){
          PROComIDHuman[PROComIDHumani]<-gsub(sarsdata1[[2]][i],sarsdata1$PreyGene[i],PROComIDHuman[PROComIDHumani])
        }
      }
      PROComIDHuman1<-unlist(lapply(PROComIDHuman,function(x){
        strsplit(x,"_")[[1]][1]
      }))
      PROComIDHuman2<-unlist(lapply(PROComIDHuman,function(x){
        strsplit(x,"_")[[1]][2]
      }))
      nodesdf<-data.frame(name=c(sarsproteinsx,unique(PROComIDHuman1),unique(PROComIDHuman2)),
                          group=c(sarsdatanames[1],rep(sarsdatanames[3],length(unique(PROComIDHuman1))),rep("Pho Site",length(unique(PROComIDHuman2)))),
                          size=c(2,rep(1,length(c(unique(PROComIDHuman1),unique(PROComIDHuman2))))))
      edgesdf<-data.frame(from=c(rep(sarsproteinsx,length(unique(PROComIDHuman1))),PROComIDHuman1),
                          to=c(unique(PROComIDHuman1),PROComIDHuman2))
    }
    #expressphosites<-paste0(blastresoutx2$Center.amino.acids.Human,blastresoutx2$PROindex.from.Human)
    #expressdata3<-as.data.frame(expressdata2[expresspros%in%unique(blastresoutx2$PRO.from.Database),])
    #expressdata3$uploadpros<-expressphositesUpload[expresspros%in%unique(blastresoutx2$PRO.from.Database)]
    linkpp<-igraph::graph_from_data_frame(edgesdf, directed = FALSE, vertices = nodesdf)
    if(input$zscoreif){
      phositedata<-round(t(scale(t(expressdata4[,-c(1,2)]))),3)
    }else{
      phositedata<-round(expressdata4[,-c(1,2)],3)
    }
    map2color<-function(x,pal,limits=NULL){
      if(is.null(limits)) limits=range(x)
      pal[findInterval(x,seq(limits[1],limits[2],length.out=length(pal)+1), all.inside=TRUE)]
    }
    interactvaluecolx<-strsplit(input$interactvaluecol,";")[[1]]
    mypal<-colorRampPalette(c(interactvaluecolx))(50)
    pievalueslist<-rep(list(rep(1,ncol(phositedata))),nrow(nodesdf))
    pievaluescollist1<-list()
    for(i in 1:nrow(phositedata)){
      pievaluescollist1[[i]]<-map2color(phositedata[i,],mypal,limits = range(phositedata))
    }
    sarscolx<-input$sarscol
    humanprocolx<-input$humanprocol
    pievaluescollist<-c(list(sarscolx),rep(list(humanprocolx),length(unique(PROComIDHuman1))),
                        pievaluescollist1)
    set.seed(123)
    plot(linkpp,vertex.size=c(10,rep(8,length(c(unique(PROComIDHuman1),unique(PROComIDHuman2))))),
         vertex.shape=c("square",rep("circle",length(unique(PROComIDHuman1))),
                        rep("pie",length(unique(PROComIDHuman2)))),
         vertex.color=c(sarscolx,rep(humanprocolx,length(unique(PROComIDHuman1)))),vertex.frame.color="grey60",
         vertex.pie=pievalueslist,
         vertex.pie.color=pievaluescollist,
         vertex.label.dist=1.5,
         edge.width=2)
    legend(x = "bottomleft",legend = c(sarsdatanames[1],sarsdatanames[3]),pch = c(15,16),
           col = c(sarscolx,humanprocolx),bty = "n")#"SARS-CoV-2","Human"
    gradientLegend(valRange=c(min(phositedata),max(phositedata)), 
                   color=mypal,
                   border.col=NA,
                   pos=.5,pos.num=1,depth=0.03,
                   side=1,inside=TRUE)
    legend.pie(-1,1,labels=rev(colnames(phositedata)), radius=0.1, bty="n", cex=0.8, 
               col="white",label.dist=1.3)
  },height=interactfigheightx,width = interactfigwidthx)
  interactplotout<-reactive({
    sarsproteinsx<<-input$sarsproteins
    if(input$loaddatatype==1){
      sarsdata<-interactiondataout()
      blastresoutx<-blastresout()
      classnames<-strsplit(input$grnames,";")[[1]]
      grnum1<-as.numeric(strsplit(input$grnums,";")[[1]][1])
      classnamesnum<-as.numeric(strsplit(strsplit(input$grnums,";")[[1]][2],"-")[[1]])
    }else{
      sarsdata<-exampleinterdataout()
      blastresoutx<-read.csv("BlastToHuman_1628320018.448.csv",stringsAsFactors = FALSE)
      classnames<-strsplit(input$examgrnames,";")[[1]]
      grnum1<-as.numeric(strsplit(input$examgrnums,";")[[1]][1])
      classnamesnum<-as.numeric(strsplit(strsplit(input$examgrnums,";")[[1]][2],"-")[[1]])
    }
    sarsdata<<-sarsdata
    sarsdatanames<-colnames(sarsdata)
    blastresoutx<<-blastresoutx
    #sarsdata<<-read.csv("SARS_HumanProteins.csv",stringsAsFactors = FALSE)
    expressdata<<-processedEdataout()
    classnames<<-classnames
    classnamesnum<<-classnamesnum
    grinfo<<-rep(classnames,times=classnamesnum)
    #expressdata2<-as.data.frame(cbind(grinfo=factor(grinfo,levels=classnames),t(expressdata)))
    #expressdata3<-stats::aggregate(.~grinfo,expressdata2,mean)
    expressdata2<-NULL
    for(i in 1:length(classnames)){
      expressdata2i<-expressdata[,grinfo==classnames[i],drop=FALSE]
      expressdata2<-cbind(expressdata2,rowMeans(expressdata2i,na.rm = TRUE))
    }
    colnames(expressdata2)<-classnames
    ##
    sarsdata1<-sarsdata[sarsdata[[1]]==sarsproteinsx,]#$Bait
    expresspros<-unlist(lapply(rownames(expressdata2),function(x){
      strsplit(x,"_")[[1]][1]
    }))
    expresscaas<-unlist(lapply(rownames(expressdata2),function(x){
      strsplit(x,"_")[[1]][2]
    }))
    expresssites<-unlist(lapply(rownames(expressdata2),function(x){
      strsplit(x,"_")[[1]][3]
    }))
    expressphositesUpload<-paste0(expresspros,"_",paste0(expresscaas,expresssites))
    expressdata3<-as.data.frame(expressdata2)
    expressdata3$uploadpros<-expressphositesUpload
    blastresoutx2<-unique(blastresoutx[blastresoutx$PRO.from.Human%in%sarsdata1[[2]],
                                       c("PRO.CombinedID","PRO.CombinedID.Human")])#$Preys
    if(nrow(blastresoutx2)==0){
      blastresoutx2<-unique(blastresoutx[blastresoutx$PRO.from.Database%in%sarsdata1[[2]],
                                         c("PRO.CombinedID","PRO.CombinedID.Human")])
      expressdata4<-base::merge(blastresoutx2,expressdata3,by.x = "PRO.CombinedID", by.y = "uploadpros",sort=FALSE)
      PROComIDHuman<-expressdata4$PRO.CombinedID
      for(i in 1:length(sarsdata1[[2]])){#$Preys
        PROComIDHumani<-grep(sarsdata1[[2]][i],PROComIDHuman)#$Preys
        if(length(PROComIDHumani)>0){
          PROComIDHuman[PROComIDHumani]<-gsub(sarsdata1[[2]][i],sarsdata1$PreyGene[i],PROComIDHuman[PROComIDHumani])
        }
      }
      PROComIDHuman1<-unlist(lapply(PROComIDHuman,function(x){
        strsplit(x,"_")[[1]][1]
      }))
      PROComIDHuman2<-unlist(lapply(PROComIDHuman,function(x){
        strsplit(x,"_")[[1]][2]
      }))
      nodesdf<-data.frame(name=c(sarsproteinsx,unique(PROComIDHuman1),unique(PROComIDHuman2)),
                          group=c(sarsdatanames[1],rep(sarsdatanames[3],length(unique(PROComIDHuman1))),rep("Pho Site",length(unique(PROComIDHuman2)))),
                          size=c(2,rep(1,length(c(unique(PROComIDHuman1),unique(PROComIDHuman2))))))
      edgesdf<-data.frame(from=c(rep(sarsproteinsx,length(unique(PROComIDHuman1))),PROComIDHuman1),
                          to=c(unique(PROComIDHuman1),PROComIDHuman2))
    }else{
      expressdata4<-base::merge(blastresoutx2,expressdata3,by.x = "PRO.CombinedID", by.y = "uploadpros",sort=FALSE)
      PROComIDHuman<-expressdata4$PRO.CombinedID.Human
      for(i in 1:length(sarsdata1[[2]])){
        PROComIDHumani<-grep(sarsdata1[[2]][i],PROComIDHuman)
        if(length(PROComIDHumani)>0){
          PROComIDHuman[PROComIDHumani]<-gsub(sarsdata1[[2]][i],sarsdata1$PreyGene[i],PROComIDHuman[PROComIDHumani])
        }
      }
      PROComIDHuman1<-unlist(lapply(PROComIDHuman,function(x){
        strsplit(x,"_")[[1]][1]
      }))
      PROComIDHuman2<-unlist(lapply(PROComIDHuman,function(x){
        strsplit(x,"_")[[1]][2]
      }))
      nodesdf<-data.frame(name=c(sarsproteinsx,unique(PROComIDHuman1),unique(PROComIDHuman2)),
                          group=c(sarsdatanames[1],rep(sarsdatanames[3],length(unique(PROComIDHuman1))),rep("Pho Site",length(unique(PROComIDHuman2)))),
                          size=c(2,rep(1,length(c(unique(PROComIDHuman1),unique(PROComIDHuman2))))))
      edgesdf<-data.frame(from=c(rep(sarsproteinsx,length(unique(PROComIDHuman1))),PROComIDHuman1),
                          to=c(unique(PROComIDHuman1),PROComIDHuman2))
    }
    #expressphosites<-paste0(blastresoutx2$Center.amino.acids.Human,blastresoutx2$PROindex.from.Human)
    #expressdata3<-as.data.frame(expressdata2[expresspros%in%unique(blastresoutx2$PRO.from.Database),])
    #expressdata3$uploadpros<-expressphositesUpload[expresspros%in%unique(blastresoutx2$PRO.from.Database)]
    linkpp<-igraph::graph_from_data_frame(edgesdf, directed = FALSE, vertices = nodesdf)
    if(input$zscoreif){
      phositedata<-round(t(scale(t(expressdata4[,-c(1,2)]))),3)
    }else{
      phositedata<-round(expressdata4[,-c(1,2)],3)
    }
    map2color<-function(x,pal,limits=NULL){
      if(is.null(limits)) limits=range(x)
      pal[findInterval(x,seq(limits[1],limits[2],length.out=length(pal)+1), all.inside=TRUE)]
    }
    interactvaluecolx<-strsplit(input$interactvaluecol,";")[[1]]
    mypal<-colorRampPalette(c(interactvaluecolx))(50)
    pievalueslist<-rep(list(rep(1,ncol(phositedata))),nrow(nodesdf))
    pievaluescollist1<-list()
    for(i in 1:nrow(phositedata)){
      pievaluescollist1[[i]]<-map2color(phositedata[i,],mypal,limits = range(phositedata))
    }
    sarscolx<-input$sarscol
    humanprocolx<-input$humanprocol
    pievaluescollist<-c(list(sarscolx),rep(list(humanprocolx),length(unique(PROComIDHuman1))),
                        pievaluescollist1)
    set.seed(123)
    plot(linkpp,vertex.size=c(10,rep(8,length(c(unique(PROComIDHuman1),unique(PROComIDHuman2))))),
         vertex.shape=c("square",rep("circle",length(unique(PROComIDHuman1))),
                        rep("pie",length(unique(PROComIDHuman2)))),
         vertex.color=c(sarscolx,rep(humanprocolx,length(unique(PROComIDHuman1)))),vertex.frame.color="grey60",
         vertex.pie=pievalueslist,
         vertex.pie.color=pievaluescollist,
         vertex.label.dist=1.5,
         edge.width=2)
    legend(x = "bottomleft",legend = c(sarsdatanames[1],sarsdatanames[3]),pch = c(15,16),
           col = c(sarscolx,humanprocolx),bty = "n")#"SARS-CoV-2","Human"
    gradientLegend(valRange=c(min(phositedata),max(phositedata)), 
                   color=mypal,
                   border.col=NA,
                   pos=.5,pos.num=1,depth=0.03,
                   side=1,inside=TRUE)
    legend.pie(-1,1,labels=rev(colnames(phositedata)), radius=0.1, bty="n", cex=0.8, 
               col="white",label.dist=1.3)
  })
  output$interactplotdl<-downloadHandler(
    filename = function(){paste("InteractionPlot",usertimenum,".pdf",sep="")},
    content = function(file){
      pdf(file,width = interactfigwidthx()/100+1,height = interactfigheightx()/100+1)
      print(interactplotout())
      dev.off()
    }
  )
})

shinyApp(ui = ui, server = server)
