##############################################################################################
# 1. IMPORTAÇÃO DOS PACOTES
##############################################################################################
library(shiny)
library(shinyjs)
library(shinyvalidate)
library(shinycssloaders)
library(tidyverse)
library(broom)
library(bslib)
library(thematic)
library(DT)
library(plotly)
##############################################################################################
# 2. CARREGAMENTO DOS DADOS
##############################################################################################
dados_desc_te <- read.csv(
  'https://raw.githubusercontent.com/ThiagoFerreiraWD/StatisticalTestsDashboard_Shiny/main/descricaoTestes.csv', 
  encoding = 'UTF-8')
##############################################################################################
# 3. UI - INTERFACE GRÁFICA
##############################################################################################
# Página de Navegação
ui <- navbarPage(
  shinyjs::useShinyjs(),
  theme = bs_theme(version = 5,
                   bootswatch = 'yeti',
                   primary = '#343434',
                   secondary = '#343434'),
  # Formatação CSS
  tags$style(HTML( 
  '#nometestedesc {
    white-space: pre-wrap; 
    font-weight: bolder;
    border-color: black;
    box-shadow: 2px 2px grey;
    text-align: justify;}
  
  h4{
    font-weight: bolder;
    font-size: 25px;
  }'
  )),
  
  tags$style(type='text/css',
             '.shiny-output-error { visibility: hidden; }',
             '.shiny-output-error:before { visibility: hidden; }'
  ),
  
  # Título Dashboard
  title = 'Dashboard para Automação de Testes Estatísticos',
  tabPanel(
    title = 'Principal',
    sidebarLayout(
      # Sidebar
      sidebarPanel(
        width = 3,
        # Input
        selectInput(
          inputId = 'nometeste',
          label = 'Selecione o teste estatístico desejado:',
          choices = c('Teste t Para Uma Amostra', 
                      'Teste t Para Duas Amostras', 
                      'Teste de Wilcoxon Signed Rank', 
                      'Teste de Shapiro-Wilk',
                      'Teste Kolmogorov-Smirnov'),
        ),
        textInput(
          '1,2,3,4,5,6,7,8,9,10',
          inputId = 'primeira_amostra',
          label = 'Informe uma lista de valores numéricos (separados por vírgula) ou use o botão para gerar 20 exemplos randômicos:'),
        uiOutput('vector'),
        h5( actionButton(
            inputId = 'randomnum',
            label = 'Gerar Números Aleatórios'),
            align = 'center'),
        uiOutput('samplemean'),
        uiOutput('confidencelevel'),
        h5( 
            actionButton(
            inputId = 'generate',
            label = 'Executar Teste'),
            align = 'center')),
      
      # MainPanel
      mainPanel(
        fluidRow(
          fluidRow(h4(textOutput('descriptiontitle')), align = 'center'),
          fluidRow(br()),
          fluidRow(
            withSpinner(verbatimTextOutput('nometestedesc'), type = 7),
          fluidRow(br())),
          
          column(
            width = 12, 
            h4(textOutput('testresulttitle')), 
            withSpinner(DTOutput('testresult'), type = 7), 
            align = 'center'),
          
          column(
            width = 12, 
            h4(textOutput('histogramtitle')), 
            withSpinner(plotlyOutput('hist', width = '100%'), type = 7),
            align = 'center'),
          
          column(
            width = 12, 
            h4(textOutput('histogramtitle2')), 
            withSpinner(plotlyOutput('hist2', width = '100%'), type = 7),
            align = 'center'),
          
        ),
        
      )
    )
  ),
  nav_item(a(href = 'https://www.thiagoferreirads.com', 'Outros Projetos', target='_blank'))
)

##############################################################################################
# 4. SERVER
##############################################################################################

# CRIAÇÃO DO SERVER
server <- function(input, output, session) {
  # Tema do shiny
  thematic::thematic_shiny()
  
  # Geração dos números randômicos
  randomnumx <- eventReactive(input$randomnum, {randomnum <- rnorm(n = 50)})
  randomnumy <- eventReactive(input$randomnum, {randomnum <- rnorm(n = 50)})
  
  # Vetor dos testes de uma amostra
  output$vector <- renderUI({
    onevector <- c('Teste t Para Uma Amostra', 'Teste de Wilcoxon Signed Rank', 'Teste de Shapiro-Wilk')
    # Se o teste selecionado não estiver na lista anterior, mostra a caixa para a segunda amostra
    if(!input$nometeste %in% onevector){
      textInput(
        '11,12,13,14,15,16,17,18,19,20',
        inputId = 'segunda_amostra',
        label = 'Informe uma lista de valores numéricos (separados por vírgula) ou use o botão para gerar 50 exemplos randômicos:'
      )}})
  
  # Vetor dos testes que requerem a média amostral
  output$samplemean <- renderUI({
    samplemean <- c('Teste t Para Uma Amostra', 'Teste de Wilcoxon Signed Rank', 'Teste t Para Duas Amostras')
    # Se o teste selecionado estiver na lista anterior, mostra a caixa solicitando a média da amostra
    if(input$nometeste %in% samplemean){
      numericInput(
        inputId = 'mu',
        label = 'Média da Amostra',
        value = 0
      )}})
  
  # Vetor dos testes que requerem intervalo de confiança
  output$confidencelevel <- renderUI({
    confidencelevel <- c('Teste t Para Uma Amostra', 'Teste de Wilcoxon Signed Rank', 'Teste t Para Duas Amostras')
    # Se o teste selecionado estiver na lista anterior, mostra a caixa solicitando o intervalo de confiança
    if(input$nometeste %in% confidencelevel){
      selectInput(
        inputId = 'conf.level',
        label = 'Selecione o Intervalo de Confiança:',
        choices = list('90%' = 0.90, '95%' = 0.95, '99%' = 0.99),
        selected = 0.90
      )}})
  
  # Primeira amostra de  dados
  observe({updateTextInput(session, 'primeira_amostra', value = paste(randomnumx(), collapse = ', '))})
  # Segunda amostra de dados
  observe({updateTextInput(session, 'segunda_amostra', value = paste(randomnumy(), collapse = ', '))})
  
  # Validação das amostras
  iv <- InputValidator$new()
  iv$add_rule('primeira_amostra', sv_required())
  iv$add_rule('segunda_amostra', sv_required())
  iv$add_rule('primeira_amostra',function(value) {
    if(is.na(sum(as.numeric(unlist(str_split(value, pattern = ',')))))) {
        'Informar apenas valores numéricos, separados por vírgula'}})
  iv$add_rule('segunda_amostra', function(value) {
    if(is.na(sum(as.numeric(unlist(str_split(value, pattern = ',')))))) {
      'Informar apenas valores numéricos, separados por vírgula'}})
  
  iv$enable()
  
  # Validação das Amostras
  observe({
    onevector <- c('Teste t Para Uma Amostra', 'Teste de Wilcoxon Signed Rank', 'Teste de Shapiro-Wilk')
    if (input$nometeste %in% onevector) {
      shinyjs::toggleState('generate', !is.null(input$primeira_amostra) && input$primeira_amostra != '')} 
    else {
      shinyjs::toggleState(
        'generate', 
        !is.null(input$primeira_amostra) && input$primeira_amostra != ''
        && !is.null(input$segunda_amostra) && input$segunda_amostra != '')}})
  
  # Execução do Teste Estatístico
  stat_test <- eventReactive(input$generate, {
    primeira_amostra <- as.numeric(unlist(str_split(input$primeira_amostra, pattern = ',')))
    segunda_amostra <- as.numeric(unlist(str_split(input$segunda_amostra, pattern = ',')))
    conf.level <- as.numeric(input$conf.level)
    if(input$nometeste == 'Teste t Para Uma Amostra') {
      test_result <- t.test(primeira_amostra, mu = input$mu, conf.level = conf.level) %>% tidy()} 
    else if (input$nometeste == 'Teste t Para Duas Amostras') {
      test_result <- t.test(x = primeira_amostra, y = segunda_amostra, mu = input$mu, conf.level = conf.level) %>% tidy()} 
    else if (input$nometeste == 'Teste de Wilcoxon Signed Rank') {
      test_result <- wilcox.test(primeira_amostra, mu = input$mu, conf.level = conf.level) %>% tidy()} 
    else if (input$nometeste == 'Teste de Shapiro-Wilk') {
      test_result <- shapiro.test(primeira_amostra) %>% tidy()} 
    else if (input$nometeste == 'Teste Kolmogorov-Smirnov') {
      test_result <- ks.test(x = primeira_amostra, y = segunda_amostra) %>% tidy()} 
    
      # Organização do Resultado do Teste
      test_result_tidy <- test_result %>% 
        mutate(result = ifelse(p.value <= 0.05, 'Estatisticamente Significante, Rejeitamos a H0', 
                                                'Estatisticamente Insignificante, Falhamos em Rejeitar a H0')) %>% 
        t() %>% 
        tibble(Parameter = rownames(.), Value = .[,1]) %>% 
        select(-1) %>% 
        mutate(Parameter = str_to_title(Parameter))
    
    return(test_result_tidy)})
  
  # Tabela de Resultado do Teste
  output$testresult <- renderDT({
    datatable(
      stat_test(),
      rownames = FALSE,
      options = list(
        dom = 't',
        columnDefs = list(
          list(
            className = 'dt-center',
            targets = '_all'))))})
  
  # Gráfico 1 - Valores 1 
  hist_vector <- eventReactive(input$generate, {
    primeira_amostra <- density(as.numeric(unlist(str_split(input$primeira_amostra, pattern = ','))))
    return(primeira_amostra)})
  
  output$hist <- renderPlotly({
    hist_vector <- hist_vector()
    plot_ly(x = ~hist_vector$x, 
            y = ~hist_vector$y, 
            type = 'scatter', 
            mode = 'line',
            line = list(color = '#343434', width = 2),
            fill = 'tozeroy',
            fillcolor = 'rgba(52, 52, 52, 0.85)') %>%
      layout(xaxis = list(title = 'Dados'), 
             yaxis = list(title = 'Densidade'))})
  
  # Gráfico 2 - Valores 2 
  hist_vector2 <- eventReactive(input$generate, {
    segunda_amostra <- density(as.numeric(unlist(str_split(input$segunda_amostra, pattern = ','))))
    return(segunda_amostra)})
  
  output$hist2 <- renderPlotly({
    hist_vector2 <- hist_vector2()
    plot_ly(x = ~hist_vector2$x, 
            y = ~hist_vector2$y,
            type = 'scatter', 
            mode = 'line',
            line = list(color = '#343434', width = 2),
            fill = 'tozeroy',
            fillcolor = 'rgba(52, 52, 52, 0.85)') %>% 
      layout(xaxis = list(title = 'Dados'), 
             yaxis = list(title = 'Densidade'))})


  # GERAÇÃOPLOTAGEM DOS RESULTADOS
  # Descrição dos testes
  testdescription <- eventReactive(input$generate, {'Descrição do Teste e Hipótese Nula (H0)'})
  output$descriptiontitle <- renderText({paste(testdescription())})
  nometestedesc <- eventReactive(input$generate, {
    nometestedesc <- dados_desc_te %>% 
      dplyr::filter(nometeste == input$nometeste) %>% 
      dplyr::select(desc)})
  output$nometestedesc <- renderText({paste(nometestedesc())})
  
  # Resultados do Teste - Tabela
  testresulttitle <- eventReactive(input$generate, {'Resultado do Teste'})
  output$testresulttitle <- renderText({paste(testresulttitle())})
  
  # Resultados do Teste - Gráfico 1  
  histogramtitle <- eventReactive(input$generate, {'Histograma - Valores 1'})
  output$histogramtitle <- renderText({paste(histogramtitle())})

  # Resultados do Teste - Gráfico 2
  histogramtitle2 <- eventReactive(input$generate, {'Histograma - Valores 2'})
  output$histogramtitle2 <- renderText({paste(histogramtitle2())})

}

##############################################################################################
# 5. EXECUÇÃO DA APP
##############################################################################################
shinyApp(ui = ui, server = server)
