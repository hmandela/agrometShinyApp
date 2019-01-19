library(shiny)
library(shinyFiles)
require(rJava)
require(xlsxjars)
require(xlsx)
require(shinythemes)
require(matrixStats)
require(trend)
require(Kendall)
require(ggplot2)
library(rhandsontable)
source('fonction.R')


ui <- fluidPage(title = "PARAMETRES AGROMET",
                theme = shinythemes::shinytheme("united"),
                tabsetPanel(              
                  tabPanel(title = "DATE DE DEBUT (DDebut)",
                           fluidRow(
                             plotOutput("plotbox"),
                             radioButtons("var3", "type de fichier", choices = list("png","pdf")),
                             downloadButton("down","Telecharger"),
                             verbatimTextOutput('msg'),
                             verbatimTextOutput('results')
                             
                           ),
                           fluidRow(
                             column(4,
                                    directoryInput('directory', label = 'Selectionner le dossier des stations', value = '~'),
                                    dateInput("date1", "Choisir la date de demarrage des calculs", value = "2012-02-29", format = "dd/mm/yy"),
                                    numericInput("hauteur", "hauteur seuil en mm", 20, min = 0, max = 50)
                             ),
                             column(4,
                                    sliderInput("nbrjrseq", "Nombre de Jour", min = 1, max = 3, value = c(3), post = "Jours"),
                                    radioButtons("seqq", "Strictement consecutif ou en 1 2 ou 3j consecutifs:",
                                                 c("1,2 ou 3...consecutifs" = "nonconsec",
                                                   "Strictement consecutif" = "consec")),
                                    numericInput("sequencesec", "sequence seche maximale", 10, min = 0, max = 50)
                                    
                             ),
                             
                             column(4,
                                    sliderInput("nbrjr", "Dans les... qui suivent:", min = 1, max = 50, value = c(30), post = "Jours"),
                                    directoryInput('sortie', label = 'Enregistrer les resultats ici', value = '~'),
                                    actionButton("clic", label = "valider")
                             )
                             
                           )
                  ),                  
                  
                  tabPanel(title = "DATE DE FIN (DFin)",
                           fluidRow(
                             plotOutput("plotbox1"),
                             radioButtons("var4", "type de fichier", choices = list("png","pdf")),
                             downloadButton("down1","Telecharger"),
                             verbatimTextOutput('msg1')
                           ),
                           fluidRow(
                             column(4,wellPanel(
                               directoryInput('directory1', label = 'Selectionner le dossier des stations', value = '~'),
                               dateInput("date2", "Choisir la date de demarrage des calculs", value = "2012-02-29", format = "dd/mm/yy"),
                               sliderInput("ETP", "Evapotranspiration potentielle", min = 1, max = 10, value = c(5), post = "mm")
                             )),
                             column(4,wellPanel(
                               numericInput("capretmax", "Capacite de retention du sol", 70, min = 0, max = 100),
                               radioButtons("dist", "Voulez vous une date limite:",
                                            c("Non" = "norm",
                                              "Oui" = "unif")),
                               dateInput("date3", "Si oui, choisir la date limite", value = "2012-08-15", format = "dd/mm/yy")
                             )),
                             
                             column(3,wellPanel(
                               directoryInput('sortie1', label = 'Enregistrer les resultats ici', value = '~'),
                               actionButton("clic1", label = "valider") ))
                             
                           )
                  ),
                  
                  tabPanel(title = "LONGEUR SAISON (LS)",
                           fluidRow(
                             plotOutput("plotbox4"),
                             radioButtons("var5", "type de fichier", choices = list("png","pdf")),
                             downloadButton("down2","Telecharger"),
                             verbatimTextOutput('msg3')
                           ),
                           fluidRow(
                             column(5,wellPanel(
                               fileInput("uploadFile3", "Selectionner le fichier des dates de debut .xlsx"),
                               fileInput("uploadFile4", "Selectionner le fichier des dates de fin .xlsx")
                             )),
                             # 
                             column(6,wellPanel(
                               directoryInput('sortie3', label = 'Enregistrer les resultats ici', value = '~'),
                               actionButton("clic3", label = "valider") ))
                             
                           )
                           
                           
                  ),
                  
                  tabPanel(title = "SEQUENCES SECHES (SS)",
                           fluidRow(
                             plotOutput("plotbox3"),
                             radioButtons("var6", "type de fichier", choices = list("png","pdf")),
                             downloadButton("down3","Telecharger"),
                             verbatimTextOutput('msg2')
                           ),
                           fluidRow(
                             column(4,wellPanel(
                               fileInput("uploadFile1", "Selectionner le fichier des dates de debut .xlsx"),
                               fileInput("uploadFile2", "Selectionner le fichier des dates de fin .xlsx"),
                               directoryInput('directory2',label = 'Selectionner le dossier des stations', value = '~')
                             )),
                             column(4,wellPanel(
                               radioButtons("typeseq", "Selectionner le type de sequences seches:",
                                            c("En debut de saison" = "SeqDD",
                                              "En fin de saison" = "SeqDF",
                                              "Entre debut et fin de saison" = "SeqDDDF"  )),
                               numericInput("longperiod", "Longeur de la periode (X)", 50, min = 0, max = 100),
                               radioButtons("typeseqfin", "Selectionner la variante de sequences seches en fin de saison:",
                                            c("Entre DDebut+X et Dfin " = "Seqfdf",
                                              "Entre DDebut+X et Dfin Moyenne" = "Seqfdfmoy",
                                              "Longeur de la periode:LSmoyenne-X et de la DDebut+X" = "Seqflmoy"))
                             )),
                             # 
                             column(3,wellPanel(
                               directoryInput('sortie2', label = 'Enregistrer les resultats ici', value = '~'),
                               actionButton("clic2", label = "valider") ))
                             
                           )
                           
                           
                  ),
                  
                  
                  
                  tabPanel(title = "CONTROLE PARAMETRES",

                           fluidRow(
                             column(3,wellPanel(
                               fileInput("uploadFile7", "Selectionner le fichier du parametre"),
                               numericInput("valmini", "seuil minimal en rose", 10, min = 0, max = 300),
                               numericInput("valmaxi", "seuil maximal en vert", 20, min = 0, max = 300),
                               actionButton("clic6", label = "valider")
                               
                             )
                             ),
                             
                             column(8,wellPanel(
                               rHandsontableOutput("hotable2")
                             )
                             )
                             
                           )
                           ),
                  
                  
                  
                  
                  tabPanel(title = "EVALUATION PREVISION",
                           fluidRow(
                             helpText("Saisir dans la colonne prevision les previsions pour l'annee selectionne comme suit:
                                      PN: Precoce-normale, NP: Normale-Precoce, TN: Tardif-Normal, NT:Normale_Tardif, 
                                      CN: Courte-Normale, NC:Normale-Courte, LN: Longue-Normale, NL: Normale-Longue,")
                             #tableOutput('mandela')
                             ),
                           
                           fluidRow(
                             column(3,wellPanel(
                               fileInput("uploadFile6", "Selectionner le fichier du parametre"),
                               uiOutput("variable11"),
                               sliderInput("moyennne", "Choisir la periode moyenne:",
                                           min = 1951, max = 2020, value = c(1981,2010)),

                               actionButton("clic5", label = "valider")
                               
                             )
                             ),
                             
                             column(6,wellPanel(
                               rHandsontableOutput("hotable1"),
                               textOutput("pourvrai"),
                               textOutput("pourfaux")
                               
                               
                             )
                             ),
                             column(3,wellPanel(
                               directoryInput('sortie4', label = 'Choisir un dossier', value = '~'),
                               actionButton("save", label = "enregistrer la table")
                               
                               
                               
                             )
                             )
                             
                             
                             
                           )
                           ),
                  
                  
                  
                  
                  
                  tabPanel(title = "TENDANCES",
                           fluidRow(
                             plotOutput("plot1"),
                             radioButtons("var7", "type de fichier", choices = list("png","pdf")),
                             downloadButton("down4","Telecharger"),
                             verbatimTextOutput('results1'),
                             tableOutput('results2'),
                             uiOutput("plot")
                           ),
                           fluidRow(
                             column(4,wellPanel(
                               fileInput("uploadFile5", "Selectionner le fichier du parametre"),
                               selectInput("input_type", "Choisir une Analyse",
                                           c("Variabilite et tendances", "Analyse de risque")),
                               uiOutput("ui"),
                               actionButton("clic4", label = "valider")
                               
                             )
                             ),
                             
                             column(4,wellPanel(
                               selectInput("dataset","Choisir l'analyse Determinee:", 
                                           list(variabilite = "iris", risque = "mtcars")
                               ),
                               uiOutput("variable")
                               
                             )
                             )
                             
                           )
                           
                  ) ,
                  
                  
                  tabPanel(title = "AUTRES CARACTERISTIQUES",

                           fluidRow(
                             column(3,wellPanel(
                               verbatimTextOutput('message10'),
                               tags$head(tags$style("#message10{color: red;
                                 font-size: 18px;
                                                    font-style: bold;
                                                    }")),
                                    directoryInput('directory10', label = 'Selectionner le dossier des stations', value = '~'),
                                    selectInput("input_type10", "Choisir la periode",
                                                c("MAM","AMJ","MJJ","JJA","JAS","ASO","SON")),
                                    directoryInput('sortie11', label = 'Enregistrer les resultats ici', value = '~'),
                                    
                                    actionButton("clic10", label = "valider")

                             )),
                             column(3,wellPanel(
                               verbatimTextOutput('message13'),
                               tags$head(tags$style("#message13{color: red;
                                                    font-size: 18px;
                                                    font-style: bold;
                                                    }")),
                               directoryInput('directory14', label = 'Selectionner le dossier des stations', value = '~'),
                               radioButtons("dist40", "Variante cumul pluviometrique:",
                                            c("Cumul entre Debut et fin" = "un4",
                                              "Cumul annuel" = "nor4"
                                            )),
                               fileInput("uploadFile40", "Selectionner le fichier des dates de debut .xlsx"),
                               fileInput("uploadFile41", "Selectionner le fichier des dates de fin .xlsx"),
                               
                               directoryInput('sortie40', label = 'Enregistrer les resultats ici', value = '~'),
                               
                               actionButton("clic40", label = "valider")
                               
                               )),
                             
                             column(3,wellPanel(
                               verbatimTextOutput('message11'),
                               tags$head(tags$style("#message11{color: red;
                                 font-size: 18px;
                                                    font-style: bold;
                                                    }")),
                               directoryInput('directory12', label = 'Selectionner le dossier des stations', value = '~'),
                               radioButtons("dist20", "Variante Maxi pluviometrique:",
                                            c("Maxi entre Debut et fin" = "un",
                                              "Maxi annuel" = "nor"
                                              )),
                               fileInput("uploadFile20", "Selectionner le fichier des dates de debut .xlsx"),
                               fileInput("uploadFile21", "Selectionner le fichier des dates de fin .xlsx"),
                               
                               directoryInput('sortie20', label = 'Enregistrer les resultats ici', value = '~'),
                               
                               actionButton("clic20", label = "valider")

                             )),
                             column(3,wellPanel(
                               verbatimTextOutput('message12'),
                               tags$head(tags$style("#message12{color: red;
                                                    font-size: 18px;
                                                    font-style: bold;
                                                    }")),
                               directoryInput('directory13', label = 'Selectionner le dossier des stations', value = '~'),
                               radioButtons("dist30", "Variante nombre de jour de pluie:",
                                            c("Nombre de jour entre Debut et fin" = "nbr_D",
                                              "Nombre de jour annuel" = "nbr_an"
                                            )),
                               numericInput("haut", "Hauteur seuil ", 0.85, min = 0, max = 25, step = 0.1),
                               fileInput("uploadFile30", "Selectionner le fichier des dates de debut .xlsx"),
                               fileInput("uploadFile31", "Selectionner le fichier des dates de fin .xlsx"),
                               
                               directoryInput('sortie30', label = 'Enregistrer les resultats ici', value = '~'),
                               
                               actionButton("clic30", label = "valider")
                               
                               ))
                             
                           )
                  )
                  
                  
                  
                  )
                )