
# paquetes
library(shiny)
library(shinythemes)
library(tidyverse)
library(xlsx)
library(writexl)
options(shiny.maxRequestSize=1000*1024^2)

# crear todas secciones ----
archivo <- readxl::read_xlsx("lista_completa.xlsx", skip = 5)
secciones <- unique(archivo$`Número de sección`)
lista_secciones <- lapply(secciones,function(i) archivo %>% 
                              filter(`Número de sección`==i))
secciones_zip <- archivo %>% select(6,9,10) %>% 
    separate(`E-mail`, into = c("user" , "basura"),
             sep = "@") %>% select(1,3,4) %>% 
    mutate(Profesor = iconv(Profesor,
                            from="UTF-8",to="ASCII//TRANSLIT"))

####


# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = shinytheme("darkly"),
    h1("Probabilidad y Estadística"),
    tabsetPanel(
        # Principal crear secciones tab ----
        tabPanel(
            title = "Secciones", fluid = T,
            sidebarLayout(
                sidebarPanel(
                    downloadButton("descargar_secciones", 
                                   "Descargar Secciones")
                ),
                mainPanel(
                    tableOutput("tabla_general")
                )
            )),
        # Crear grupos aleatorios tab ----
        tabPanel(
            title = "Grupos Aleatorios", fluid = T,
            sidebarLayout(
                sidebarPanel(
                    selectInput("sec","Sección", selected = 1,
                                choices = factor(archivo$`Número de sección`)),
                    numericInput("num","Número integrantes", 
                                 4, min=2, max=7),
                    actionButton(inputId = "update", 
                                 label =  "Generar grupos"),
                    downloadButton("download_data", 
                                   "Descargar Grupos")),
                mainPanel(
                    tableOutput("tabla")
                )
            )) ,
        # matriz calificación tab----
        tabPanel(
            title = "Matriz Calificación",
            sidebarLayout(
                sidebarPanel(
                    selectInput("sec1","Sección", selected = 1,
                                choices = factor(archivo$`Número de sección`)),
                    selectInput("npreg", "Número de preguntas", 
                                choices = seq(1,10,1)),
                    uiOutput("input_numerales"),
                    actionButton("activar", "Crear Matriz"),
                    downloadButton("descargar_matriz",
                                   "Descargar Matriz Calificación")
                    
                ),
                mainPanel(
                    tableOutput("tabla_matriz")
                ))
            
        )

    )
)

server <- function(input, output) {
    # Tabla descriptiva, tab crear todas las secciones ----
    output$tabla_general <- renderTable(
        archivo %>% 
            mutate(clase = paste0(Profesor,"_",
                                  `Número de sección`)) %>% 
            group_by(clase) %>% 
            summarise(total = n())
    )
    # boton descargar todos las secciones
    output$descargar_secciones <- downloadHandler(
        filename = function() {
            paste("secciones.zip")
        },
        content = function(file){
            owd <- setwd(tempdir()) # evitar problemas de permisos
            on.exit(setwd(owd))
            files <- NULL;
            for (i in 1:length(secciones)){
                fileName <- paste0("Sección ",
                                   secciones[i], " ",
                                   archivo[archivo[,9]==secciones[i],10][[1]][1]
                                   ,".csv")
                write.table(lista_secciones[[i]], sep = ";", ###@@@
                            fileName, col.names = T, row.names = F)
                files <- c(fileName,files)
            }
            zip(file, files)
        },
        contentType = "application/zip"
    )
    
    # Datos secciones, crear grupos aleatorios ----
    filtered_data <- reactive({
        input$update
        i <- isolate(input$sec)
        j <- isolate(input$num)
        seccion_grupo = i
        integrantes = j
        
        base <- filter(archivo, `Número de sección` == seccion_grupo) 
        base <- base %>% 
            mutate(aleatorio = sample(100, nrow(base), replace = F),
                   rango = rank(aleatorio),
                   grupo = ceiling(rango/integrantes))
        total_grupos = floor(nrow(base)/integrantes)
        base <- base %>% 
            mutate(grupo = apply(base[,13, drop=F], 1,
                                 function(x){ifelse(x<=total_grupos,
                                                    x,
                                                    sample(c(1:total_grupos)))})) %>%
            arrange(grupo) %>% 
            select(-c(1,5,7,8,11,12)) 
    })
    # Preview de datos
    output$tabla <- renderTable(filtered_data(), digits = 0)
    # Descargar datos
    output$download_data <- downloadHandler(
        filename =  function(){
            paste0("grupos_seccion_",input$sec,".csv")
        },
        content = function(file) {
            data <- filtered_data()
            write.csv(data, file, row.names = FALSE)
        }
    )
    
    # matriz calificacion ----
    secciones_filtadas <- reactive({
        base <- filter(archivo, 
                       `Número de sección` == input$sec1) %>% 
            select(6,2,3,4,9)
    })
    
    output$input_numerales <- renderUI({
        numerales <- as.integer(input$npreg)
        lapply(1:numerales, function(i){
            textInput(paste0("valor_numeral", i), 
                      label = paste0("Valor cada numeral punto ", i, 
                                     " (separar por comas)"),
                      value = "1,1")
        })
    })
    
    datos_react <- eventReactive(input$activar, {
        num <- as.integer(input$npreg)
        data.frame(pregunta = seq(1,num,1), t(data.frame(lapply(1:num, function(i) {
            input[[paste0("valor_numeral", i)]]
        }))))
    })
    
    output$tabla_matriz <- renderTable({
        num <- as.integer(input$npreg)
        datos <- datos_react()
        colnames(datos) <- c("@","puntos")
        datos <- datos %>% 
            separate(puntos, letters, ",") 
        datos <- datos[colSums(!is.na(datos)) > 0]
        datos[is.na(datos)] <- "@"
        final <- list()
        for (i in 1:nrow(datos)) {
            for (j in 1:ncol(datos)) {
                final <-append(final, paste0("Puntos ",datos[i,1],
                                             colnames(datos)[j], "(",
                                             datos[i,j],")"))
                final <- append(final, paste0("Comentarios ",datos[i,1],
                                              colnames(datos)[j], "(",
                                              datos[i,j],")"))
            }
        }
        final <- unlist(final)
        final <- final[!str_detect(final,"@")]
        a <- data.frame(matrix(nrow = nrow(secciones_filtadas()),
                               ncol = length(final)))
        colnames(a)<-final
        cbind(secciones_filtadas(), a)
    })
    
    para_descarga <- reactive({
        num <- as.integer(input$npreg)
        datos <- datos_react()
        colnames(datos) <- c("@","puntos")
        datos <- datos %>% 
            separate(puntos, letters, ",") 
        datos <- datos[colSums(!is.na(datos)) > 0]
        datos[is.na(datos)] <- "@"
        final <- list()
        for (i in 1:nrow(datos)) {
            for (j in 1:ncol(datos)) {
                final <-append(final, paste0("Puntos ",datos[i,1],
                                             colnames(datos)[j], "(",
                                             datos[i,j],")"))
                final <- append(final, paste0("Comentarios ",datos[i,1],
                                              colnames(datos)[j], "(",
                                              datos[i,j],")"))
            }
        }
        final <- unlist(final)
        final <- final[!str_detect(final,"@")]
        a <- data.frame(matrix(nrow = nrow(secciones_filtadas()),
                               ncol = length(final)))
        colnames(a)<-final
        cbind(secciones_filtadas(), a)
    })
    
    #descarga
    output$descargar_matriz <- downloadHandler(
        filename =  function(){
            paste0("matriz seccion",input$sec1,".csv")
        },
        content = function(file) {
            data <- para_descarga()
            write.csv(data, file, row.names = FALSE)
        }
    )

}

# Correr
shinyApp(ui = ui, server = server)

