suppressWarnings(
  {
    suppressMessages(
      {
        library(xts)
        library(DBI)
        library(odbc)
        library(seasonal)
        }
      )
    }
  )

#' Title
#' @description Trata-se de uma função de ajuste sazonal. Ela aceita nome do país e utiliza os feriados deste país, 
#' bem como aceita spec_list do macrobond ou descrição completa conforme sintaxe da X13. A função tenta ajustar por 
#' 3 métodos diferentes: Sendo o primeiro deles um X13 considerando os feriados do país, dias úteis e mudanças de nível. 
#' Se não rodar, ela tenta um X13 convencional sem configurações. Se não rodar, ela tenta ajuste por Decompose.
#' @param dado_original trata-se de uma tabela em format-wide com uma coluna do tipo date e outras colunas do tipo numeric 
#' @param frequencia_data Trata-se da frequencia das datas sendo possivel "month" e "quarter"
#' @param encoding_pc Trata-se do enconding que o PC usa para puxar dados do SQL. Default: "utf-8"
#' @param var_xreg    Trata-se do parâmetro xreg de entrada na própria função X13. Utiliza-se a mesma sintaxe. Default: NULL
#' @param usa_dado_original Caso os ajustes sazonais falhem, retorna dado NSA de entrada. Default: FALSE
#' @param spec_list_path  Caso queira usar um spec_list utilizado pela macrobond, insira o path aqui. Default: NULL
#' @param automethod  Método de ajuste a ser utlilizado. Pode ser "multiplicative" ou "additive". Default: "multiplicative"
#' @param country   Nome do país do dado escrito em inglês.Ex: "United States", "Mexico". Default: "none"
#' @param var_arima.model  Trata-se do parâmetro arima.model de entrada na própria função X13. Utiliza-se a mesma sintaxe. Default: NULL
#'
#' @return Retorna um data frame igual entrada mas com colunas trocadas de posição, e com valores substituidos pelos valores ajustados
#' @export
#'
#' @examples 
#' Entrada NSA
#' > dados_nsa
#'   # A tibble: 287 × 25
#'  data          SERIE_1 SERIE_2 SERIE_3 SERIE_4 SERIE_5 SERIE_6 ...
#'  <date>         <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>  ...
#'  1 2000-02-01   0.266   5660.  17844   12445.  81176   21579.  ...
#'  2 2000-03-01   0.244   6154.  19999.  12440. 104076.  25403.  ...
#'  3 2000-04-01   0.244   5398.  24735.  17457.  97978.  23880.  ...
#'  4 2000-05-01   0.282   4872.  17866.  14538.  89206.  25150   ...
#'  
#' > dados_sa <- kptl_ajuste_sazonal(dado_original = dados_nsa,
#'                                   frequencia_data = "month",
#'                                   country = "Mexico")
#'                                   
#' > dados_sa        
#' # A tibble: 287 × 25
#' data          SERIE_1 SERIE_10 SERIE_11 SERIE_12 SERIE_13 ...
#' <date>         <dbl>    <dbl>    <dbl>    <dbl>    <dbl>  ...
#' 1 2000-02-01   0.291    7069.    1796.    2686.    1676.  ...
#' 2 2000-03-01   0.257    7668.    1815.    2681.    1172.  ...
#' 3 2000-04-01   0.272    7188.    1830.    2712.    1947.  ...
#' 
#' Obs: A série não retorna com as colunas no lugar original, 
#' entretanto todas as colunas estão presentes
kptl_ajuste_sazonal <- function(dado_original, 
                                frequencia_data,
                                encoding_pc = "utf-8",
                                usa_dado_original = FALSE, 
                                spec_list_path = NULL, 
                                automethod =  "multiplicative",
                                country = "none",
                                var_xreg = NULL,
                                var_regression.usertype = "holiday",
                                var_arima.model = NULL,
                                var_xtrans = NULL,
                                var_seats.noadmiss = "yes",
                                var_regression.aictest = c("td", "easter"),
                                var_outlier = "",
                                var_automdl = "",
                                var_composite = NULL,
                                var_out = FALSE,
                                var_dir = NULL,
                                var_list = NULL,
                                var_regression.variables = NULL,
                                var_seats = NULL,
                                var_force.type = NULL,
                                extracao_de_series = "data",
                                var_roda_rapido = FALSE) {
  if(var_roda_rapido){
    var_arima.model <- "(0 1 1)(0 1 1)"
  }
  # VERIFICAÇÕES INICIAIS DA ENTRADA
  # _________________________________________________________________________________
  if(missing("dado_original")){ 
    stop("Falta a tabela de dados NSA. Insira na variável dado_original")}
  if(missing("frequencia_data")){ 
    stop("Voce deve definir uma frequência para o dado. Ex: 'month', 'quarter'")
    # a ser implementado no futuro
    if(is.null(frequencia_data)) {
      # funcao que busca frequencia de datas
      frequencia_data <- acha_frequencia_datas(dado_original_adj[[date_col]])
    }  
  }
  # verificacao de tibble ou DF
  if (!tibble::is_tibble(dado_original) && !is.data.frame(dado_original)) {
    stop("Sua entrada não é um tibble nem data frame.")
  }
  # _________________________________________________________________________________
  
  # obtem nome da coluna tipo data
  date_col <- obtem_coluna_de_data(dado_original)
  # verifica se todas as colunas são numericas
  tipos_culunas_numerico <- dado_original %>% 
                            summarise_all(class) %>% 
                            dplyr::select(-all_of(date_col)) %>% 
                            pivot_longer(cols = everything(), names_to = "var", values_to = "values") %>% 
                            dplyr::mutate(numerico = (values == "numeric")) %>% 
                            pull(numerico) 
  if(!all(tipos_culunas_numerico)){
    stop("Existem colunas que não são do tipo numérico. Você deve tratar essas colunas antes e transformar em formato numérico")
  }
  
  # AJUSTES DE DATAS E NOMES DAS COLUNAS
  # _________________________________________________________________________________
  # faremos uma transformação na coluna de datas para datas auxiliares. 
  # Ao passo que iremos armazenar as datas originais para retornar ao final do processo
  data_original <- tibble(data_orig = dado_original[[date_col]]) %>% 
    dplyr::mutate(data_adj = lubridate::floor_date(data_orig, unit = frequencia_data))
  names(data_original)[1] <- date_col
  
  
  dado_original_adj <- dado_original %>% 
    left_join(data_original, by = date_col) %>% 
    dplyr::mutate(!!as.symbol(date_col) := .$data_adj) %>% 
    dplyr::select(-data_adj)
  # Faremos um ajuste com os nomes das colunas para casos de nomes pouco usuais
  # dicionario de nomes velhos
  dicio_nomes_colunas <- tibble(nomes_velhos = names(dado_original_adj)) %>% 
    dplyr::mutate(nomes_novos = limpa_string_adj(nomes_velhos)) %>% 
    filter(nomes_velhos != date_col)
  # troca para nomes novos
  for(nome_col in dicio_nomes_colunas$nomes_velhos){
    # nome_col <-dicio_nomes_colunas$nomes_velhos[1]
    names(dado_original_adj)[which(names(dado_original_adj) == nome_col)] <- limpa_string_adj(nome_col)
  }
  dicio_shifts <- tibble()
  # Shift das séries a fim de rodar ajuste multiplicativo sem problemas
  for(nome_col in dicio_nomes_colunas$nomes_novos){ # nome_col <- dicio_nomes_colunas$nomes_novos[15]
    # constante aditiva para sempre usar dessaz multiplicativo
    const <- dado_original_adj %>% pull(nome_col) %>% min(na.rm = TRUE)
    if(const < 0){
        const <- abs(const)
        # ajuste dos dados
        dado_original_adj[[nome_col]] <- dado_original_adj[[nome_col]] + 2*const
    } else{ 
      if(const == 0){
          const <- 0.05*(dado_original_adj %>% pull(nome_col) %>% max(na.rm = TRUE))
          # ajuste dos dados
          dado_original_adj[[nome_col]] <- dado_original_adj[[nome_col]] + 2*const
      } else {
        if(const > 0){
            const <- 0
          }
        }
      }        
    # armazena ajuste
    dicio_shifts <- bind_rows(dicio_shifts,
                              tibble(nomes_novos = nome_col,
                                     shift = 2*const))
    }
  
  
  # _________________________________________________________________________________
  
  # Define specificação
  # _________________________________________________________________________________
  # Carrega de Modelo Spec pronto
  if(!is.null(spec_list_path)){
    # obtem primeira data para testar se data do spec.txt bate com data da entrada
    menor_data_entrada <- dado_original_adj[[date_col]] %>% 
      min()
    # obtém spec do arquivo .txt
    auto <- obtem_spec_list_do_path(spec_list_path = spec_list_path)
    # compara datas
    data_minima <- auto[["xreg"]] %>% as.xts() %>% index() %>% min() %>% as.Date()
    # se data proveniente do spec.txt for menor, emite mensagem e corta entrada para seguir com o processo
    if(menor_data_entrada < data_minima){
      base::message(glue::glue("A menor data aceita pelo seu arquivo SPEC é '{data_minima}', logo, o ajuste sazonal será feito a partir desta data."))
      dado_original_adj <- dado_original_adj %>% 
        filter((!!as.symbol(date_col)) >= data_minima )
    }
    
  } else {
    # Função para criar spec list desejado caso não haja spec pronto
    auto <- cria_auto(dado_original_adj, 
                      automethod,  
                      country, 
                      date_col, 
                      var_xreg, 
                      var_regression.usertype,
                      var_arima.model, 
                      encoding_pc,
                      var_xtrans,
                      var_seats.noadmiss,
                      var_regression.aictest,
                      var_outlier,
                      var_automdl,
                      var_composite,
                      var_out,
                      var_dir,
                      var_list,
                      var_regression.variables,
                      var_seats,
                      var_force.type,
                      frequencia_data)
  }
  # _________________________________________________________________________________
  
  # DETERMINA FREQUENCIA DE DATAS DOS DADOS
  # _________________________________________________________________________________
  
  # Se frequencia não for trimestral ou diaria, stopa e emite erro
  if (frequencia_data != "quarter" && frequencia_data != "month") {
    stop(paste0("Função não sabe como ajustar dados sazonalmente com frequencia das datas ", frequencia_data))
  }
  # _________________________________________________________________________________
  # COMPUTAR AJUSTE SAZONAL
  # _________________________________________________________________________________
  # Usamos dplyr::do para respeitar variáveis de agrupamento
  serie_ajustada <- dplyr::bind_rows(
    dplyr::group_map(
      .data = dado_original_adj %>% pivot_longer(cols = -all_of(date_col), names_to = "variavel", values_to = "value") %>% group_by(variavel),
          # .x <- dado_original_adj %>% pivot_longer(cols = -all_of(date_col), names_to = "variavel", values_to = "value") %>% group_by(variavel) %>% filter(variavel == "INDICE_DE_RECEITA_NOMINAL_DE_VENDAS_NO_COMERCIO_VAREJISTA")
          # .y <- tibble(variavel = "ALL_ITEMS")
          # guarda_auto <- auto
          # auto <- auto[which(!names(auto) %in% c("seats.noadmiss", "regression.aictest", "outlier", "automdl", "na.action", "out", "multimode", "regression.variables"))]
          # auto <- guarda_auto
          # auto <- auto[which(!names(auto) %in% c("na.action", "multimode"))]
          # "multimode",   "na.action"
      .f = ~ ajust_saz_serie_unica(
        original = .x,
        date_col = date_col,
        var = "value",
        frequencia_data = frequencia_data,
        group_vars = .y,
        usa_dado_original = usa_dado_original,
        spec_list = auto,
        extracao_de_series = extracao_de_series),
      .keep = TRUE)
  ) %>% 
    pivot_wider(values_from = value, names_from = variavel)
  
  by = join_by(!!as.symbol(date_col) == data_adj)
  # _________________________________________________________________________________________
  #  Desfaz os ajustes realizados
  # retorna com datas antes de ajustar novamente
  serie_ajustada <- serie_ajustada %>% 
    left_join(data_original, by = by, suffix = c(".x", "_orig")) %>% 
    dplyr::mutate(!!as.symbol(date_col) := pull(across(ends_with("_orig")))) %>% 
    dplyr::select(-!!as.symbol(paste0(date_col, "_orig")))
  
  # devolve os shifts realizados
  for(nome_col in dicio_nomes_colunas$nomes_novos){ # nome_col <- dicio_nomes_colunas$nomes_novos[1]
    shift <- dicio_shifts %>% filter(nomes_novos == nome_col) %>% pull(shift)
    
    serie_ajustada[[nome_col]] <- serie_ajustada[[nome_col]] - shift
  }
  
  # troca para nomes velhos
  for(nome_col in dicio_nomes_colunas$nomes_novos){
    # nome_col <-dicio_nomes_colunas$nomes_novos[1]
    nome_velho <- dicio_nomes_colunas %>% 
      filter(nomes_novos == nome_col) %>% 
      pull(nomes_velhos)
    names(serie_ajustada)[which(names(serie_ajustada) == nome_col)] <- nome_velho
  }
  # ordena para a ordem original
  serie_ajustada <- serie_ajustada[,names(dado_original)] %>% as_tibble()
  
  return(serie_ajustada)
}

obtem_spec_list_do_path <- function(spec_list_path){
  auto <- seasonal::import.spc(file = spec_list_path) %>% 
    pluck("seas") %>% as.list()
  # gambiarra para caso ele venha com erro
  names(auto) <- str_remove_all(names(auto), "\t")
  
  if(!is.null(auto[["regression.file"]])){
    path_genhol <- auto[["regression.file"]] %>% 
      str_remove_all("\\\"")
    genhol_file <- read.delim(file = path_genhol, header = FALSE)
    freq = max(genhol_file[,2])
    start = c(min(genhol_file[,1]), min(genhol_file[,2]))
    xreg = NULL
    for(k in 3:length(genhol_file)){
      xreg <- ts.union(
        xreg,
        ts(data = genhol_file[,k], start = start, frequency = freq))
    }
    auto[["xreg"]] <- xreg
    auto <- auto[!(names(auto) %in% c("",
                                      "regression.file"))]
    
  } else{
    auto <- auto[!(names(auto) %in% c("", 
                                      "regression.usertype",
                                      "regression.file"))
    ]
  }
  return(auto)
}


ajust_saz_serie_unica <- function(original, var, date_col, frequencia_data, group_vars, usa_dado_original, spec_list, extracao_de_series) {
  
  # debuga
  print(paste0("calcula variavel: ",original %>% pull(variavel) %>% unique()))
  
  
  # Regulariza time series
  original <- regularise(original, date_col, frequencia_data, group_vars)
  
  # obtem data inicial
  start <- obtem_data_inicio(original[[date_col]], frequencia_data)
  
  # Crie uma cópia apenas dos vars não ajustados.
  # Iremos mesclar os dados ajustados coluna por coluna depois de ajustá-los.
  seas_adjusted <- original[, date_col, drop = FALSE]
  
  
  if (all(is.na(original[[var]]))) {
    seas_adjusted[[var]] <- NA
    serie_ajustada <- dplyr::bind_cols(seas_adjusted, group_vars)
  } else {
    # obtém nome da série
    nome_var <- original %>% pull(variavel) %>% unique()
    # cria objeto ts
    tsversion <- stats::ts(original[[var]], start = start, frequency = fracao_anual_frequencia(frequencia_data))
    # calcula quantos NA's tem no começo e no fim da série
    omit_leading <- leading_nas(tsversion)
    omit_trailing <- trailing_nas(tsversion)
    # adiciona serie na spec_list
    spec_list[["x"]] <- tsversion
    
    # as tentativas de ajuste sazonal sequirão a ordem:
    # 1 - X-13 especifico
    # Se tiver arima especificado:
      # 2 - X-13 geralzão com ARIMA e com testes
      # 3 - X-13 geralzão com ARIMA e sem teste aic
      # 4 - X-13 geralzão com ARIMA e sem testes AIC e outlier
    # 5 - X-13 geralzão sem arima e com testes
    # 6 - X-13 geralzão sem arima e sem teste AIC
    # 7 - X-13 geralzão sem arima e sem testes AIC e outlier
    # 8 - X-13 geralzão com arima (1,1,0) e com testes
    # 9 - X-13 geralzão com arima (0 1 1)(0 1 1) e com testes
    # 10 - X-13 geralzão com arima (0 1 1)(0 1 1) e sem AIC Test
    # 11 - X-13 geralzão com arima (0 1 1)(0 1 1) e sem testes AIC e outlier
    # 12 - Decompose
    sucesso <- FALSE
    try(expr = {
      # 1 - X-13 especifico
      # _____________________________
      adjusted <- seasonal::seas(list = spec_list,
                                 estimate.maxiter = 10000)
      
      # captura os dados
      modelo_arima <- adjusted %>% pluck("model", "arima", "model")
      adjusted <- func_captura_serie(adjusted,
                                     extracao_de_series)
      
      
      # Insere as datas originais. Elimina aquelas que correspondem aos NA's iniciais ou finais
      original_dates <- original[[date_col]]
      original_dates <- original_dates[(1 + omit_leading):(length(original_dates) - omit_trailing)]
      adjusted$date <- original_dates
      
      # Obtem apenas as séries necessárias (e renomeia-as)
      adjusted <- adjusted[, c("date", "final")]
      
      
      # adjusted <- dplyr::rename(adjusted, {{ date_col }} := .data[["date"]], {{ var }} := .data[["final"]])
      
      
      adjusted <- dplyr::rename(adjusted, (!!as.symbol(date_col)) := .data[["date"]], (!!as.symbol(var)) := .data[["final"]])
      
      # Mesclar
      seas_adjusted <- dplyr::left_join(seas_adjusted, adjusted, by = date_col)
      
      serie_ajustada <- dplyr::bind_cols(seas_adjusted, group_vars)
      print(glue::glue('serie ajustada: {original %>% pull(variavel) %>% unique()}  arima encontrado: {modelo_arima}'))
      
      sucesso <- TRUE
      
    })
    if(!sucesso & !is.null(spec_list$arima.model)){
      try(expr = {
        # 2 - X-13 geralzão com ARIMA e com testes
        # _____________________________
        # mensagem de erro
        base::message(glue::glue("X-13 específico falhou para '{nome_var}': Tentando X-13 geral com testes e com arima fornecido"))
        # ajusta
        adjusted <- seasonal::seas(                    x = spec_list[["x"]], 
                                    regression.variables = c("td"),
                                      transform.function = spec_list[["transform.function"]],
                                             arima.model = spec_list[["arima.model"]],
                                        estimate.maxiter = 10000)
        # captura os dados
        modelo_arima <- adjusted %>% pluck("model", "arima", "model")
      adjusted <- func_captura_serie(adjusted,
                                     extracao_de_series)
        
        
        # Insere as datas originais. Elimina aquelas que correspondem aos NA's iniciais ou finais
        original_dates <- original[[date_col]]
        original_dates <- original_dates[(1 + omit_leading):(length(original_dates) - omit_trailing)]
        adjusted$date <- original_dates
        
        
        # Obtem apenas as séries necessárias (e renomeia-as)
        adjusted <- adjusted[, c("date", "final")]
        
        
        
        adjusted <- dplyr::rename(adjusted, (!!as.symbol(date_col)) := .data[["date"]], (!!as.symbol(var)) := .data[["final"]])
        
        # Mesclar
        seas_adjusted <- dplyr::left_join(seas_adjusted, adjusted, by = date_col)
        
        serie_ajustada <- dplyr::bind_cols(seas_adjusted, group_vars)
      print(glue::glue('serie ajustada: {original %>% pull(variavel) %>% unique()}  arima encontrado: {modelo_arima}'))
        
        sucesso = TRUE
      })
    }
    if(!sucesso & !is.null(spec_list$arima.model)){
      try(expr = {
        # 3 - X-13 geralzão com ARIMA e sem teste aic
        # _____________________________
        # mensagem de erro
        base::message(glue::glue("X-13 específico falhou para '{nome_var}': Tentando X-13 geral sem teste AIC e com arima fornecido"))
        # ajusta
        adjusted <- seasonal::seas(                  x = spec_list[["x"]], 
                                    transform.function = spec_list[["transform.function"]],
                                           arima.model = spec_list[["arima.model"]],
                                    regression.aictest = NULL,
                                               outlier = '',
                                      estimate.maxiter = 10000)
        # captura os dados
        modelo_arima <- adjusted %>% pluck("model", "arima", "model")
      adjusted <- func_captura_serie(adjusted,
                                     extracao_de_series)
        
        
        # Insere as datas originais. Elimina aquelas que correspondem aos NA's iniciais ou finais
        original_dates <- original[[date_col]]
        original_dates <- original_dates[(1 + omit_leading):(length(original_dates) - omit_trailing)]
        adjusted$date <- original_dates
        
        
        # Obtem apenas as séries necessárias (e renomeia-as)
        adjusted <- adjusted[, c("date", "final")]
        
        
        
        adjusted <- dplyr::rename(adjusted, (!!as.symbol(date_col)) := .data[["date"]], (!!as.symbol(var)) := .data[["final"]])
        
        # Mesclar
        seas_adjusted <- dplyr::left_join(seas_adjusted, adjusted, by = date_col)
        
        serie_ajustada <- dplyr::bind_cols(seas_adjusted, group_vars)
      print(glue::glue('serie ajustada: {original %>% pull(variavel) %>% unique()}  arima encontrado: {modelo_arima}'))
        
        sucesso = TRUE
      })
    }
    if(!sucesso & !is.null(spec_list$arima.model)){
      try(expr = {
        # 4 - X-13 geralzão com ARIMA e sem testes AIC e outlier
        # _____________________________
        # mensagem de erro
        base::message(glue::glue("X-13 específico falhou para '{nome_var}': Tentando X-13 geral sem testes AIC e outlier e com arima fornecido"))
        # ajusta
        adjusted <- seasonal::seas(                  x = spec_list[["x"]], 
                                                     transform.function = spec_list[["transform.function"]],
                                                     arima.model = spec_list[["arima.model"]],
                                                     regression.aictest = NULL,
                                                     outlier = NULL,
                                                     estimate.maxiter = 10000)
        # captura os dados
        modelo_arima <- adjusted %>% pluck("model", "arima", "model")
      adjusted <- func_captura_serie(adjusted,
                                       extracao_de_series)
        
        
        # Insere as datas originais. Elimina aquelas que correspondem aos NA's iniciais ou finais
        original_dates <- original[[date_col]]
        original_dates <- original_dates[(1 + omit_leading):(length(original_dates) - omit_trailing)]
        adjusted$date <- original_dates
        
        
        # Obtem apenas as séries necessárias (e renomeia-as)
        adjusted <- adjusted[, c("date", "final")]
        
        
        
        adjusted <- dplyr::rename(adjusted, (!!as.symbol(date_col)) := .data[["date"]], (!!as.symbol(var)) := .data[["final"]])
        
        # Mesclar
        seas_adjusted <- dplyr::left_join(seas_adjusted, adjusted, by = date_col)
        
        serie_ajustada <- dplyr::bind_cols(seas_adjusted, group_vars)
      print(glue::glue('serie ajustada: {original %>% pull(variavel) %>% unique()}  arima encontrado: {modelo_arima}'))
        
        sucesso = TRUE
      })
    }
    if(!sucesso){
      try(expr = {
        # 5 - X-13 geralzão sem arima e com testes
        # _____________________________
        # mensagem de erro
        base::message(glue::glue("X-13 específico falhou para '{nome_var}': Tentando X-13 geral sem arima fornecido e com testes"))
        # ajusta
        adjusted <- seasonal::seas(                 x   = spec_list[["x"]],  
                                   transform.function   = spec_list[["transform.function"]],
                                   regression.variables = c("td"),
                                       estimate.maxiter = 10000)
        # captura os dados
        modelo_arima <- adjusted %>% pluck("model", "arima", "model")
      adjusted <- func_captura_serie(adjusted,
                                     extracao_de_series)
        
        
        # Insere as datas originais. Elimina aquelas que correspondem aos NA's iniciais ou finais
        original_dates <- original[[date_col]]
        original_dates <- original_dates[(1 + omit_leading):(length(original_dates) - omit_trailing)]
        adjusted$date <- original_dates
        
        
        # Obtem apenas as séries necessárias (e renomeia-as)
        adjusted <- adjusted[, c("date", "final")]
        
        
        
        adjusted <- dplyr::rename(adjusted, (!!as.symbol(date_col)) := .data[["date"]], (!!as.symbol(var)) := .data[["final"]])
        
        # Mesclar
        seas_adjusted <- dplyr::left_join(seas_adjusted, adjusted, by = date_col)
        
        serie_ajustada <- dplyr::bind_cols(seas_adjusted, group_vars)
      print(glue::glue('serie ajustada: {original %>% pull(variavel) %>% unique()}  arima encontrado: {modelo_arima}'))
        
        sucesso = TRUE
      })
    }
    if(!sucesso){
      try(expr = {
        # 6 - X-13 geralzão sem arima e sem teste AIC
        # _____________________________
        # mensagem de erro
        base::message(glue::glue("X-13 específico falhou para '{nome_var}': Tentando X-13 geral sem arima fornecido e sem teste AIC"))
        # ajusta
        adjusted <- seasonal::seas(                  x = spec_list[["x"]],  
                                    transform.function = spec_list[["transform.function"]],
                                    regression.aictest = NULL,
                                               outlier = '',
                                      estimate.maxiter = 10000)
        # captura os dados
        modelo_arima <- adjusted %>% pluck("model", "arima", "model")
      adjusted <- func_captura_serie(adjusted,
                                     extracao_de_series)
        
        
        # Insere as datas originais. Elimina aquelas que correspondem aos NA's iniciais ou finais
        original_dates <- original[[date_col]]
        original_dates <- original_dates[(1 + omit_leading):(length(original_dates) - omit_trailing)]
        adjusted$date <- original_dates
        
        
        # Obtem apenas as séries necessárias (e renomeia-as)
        adjusted <- adjusted[, c("date", "final")]
        
        
        
        adjusted <- dplyr::rename(adjusted, (!!as.symbol(date_col)) := .data[["date"]], (!!as.symbol(var)) := .data[["final"]])
        
        # Mesclar
        seas_adjusted <- dplyr::left_join(seas_adjusted, adjusted, by = date_col)
        
        serie_ajustada <- dplyr::bind_cols(seas_adjusted, group_vars)
      print(glue::glue('serie ajustada: {original %>% pull(variavel) %>% unique()}  arima encontrado: {modelo_arima}'))
        
        sucesso = TRUE
      })
    }
    if(!sucesso){
      try(expr = {
        # 7 - X-13 geralzão sem arima e sem testes AIC e outlier
        # _____________________________
        # mensagem de erro
        base::message(glue::glue("X-13 específico falhou para '{nome_var}': Tentando X-13 geral sem arima fornecido e sem testes AIC e outlier"))
        # ajusta
        adjusted <- seasonal::seas(                  x = spec_list[["x"]],  
                                                     transform.function = spec_list[["transform.function"]],
                                                     regression.aictest = NULL,
                                                     outlier = NULL,
                                                     estimate.maxiter = 10000)
        # captura os dados
        modelo_arima <- adjusted %>% pluck("model", "arima", "model")
      adjusted <- func_captura_serie(adjusted,
                                       extracao_de_series)
        
        
        # Insere as datas originais. Elimina aquelas que correspondem aos NA's iniciais ou finais
        original_dates <- original[[date_col]]
        original_dates <- original_dates[(1 + omit_leading):(length(original_dates) - omit_trailing)]
        adjusted$date <- original_dates
        
        
        # Obtem apenas as séries necessárias (e renomeia-as)
        adjusted <- adjusted[, c("date", "final")]
        
        
        
        adjusted <- dplyr::rename(adjusted, (!!as.symbol(date_col)) := .data[["date"]], (!!as.symbol(var)) := .data[["final"]])
        
        # Mesclar
        seas_adjusted <- dplyr::left_join(seas_adjusted, adjusted, by = date_col)
        
        serie_ajustada <- dplyr::bind_cols(seas_adjusted, group_vars)
      print(glue::glue('serie ajustada: {original %>% pull(variavel) %>% unique()}  arima encontrado: {modelo_arima}'))
        
        sucesso = TRUE
      })
    }
    if(!sucesso){
      try(expr = {
        # 8 - X-13 geralzão com arima (1,1,0) e com testes
        # _____________________________
        # mensagem de erro
        base::message(glue::glue("X-13 específico falhou para '{nome_var}': Tentando X-13 geral com arima forçado (1, 1, 0) e com testes"))
        # ajusta
        adjusted <- seasonal::seas(                  x = spec_list[["x"]],  
                                    transform.function = spec_list[["transform.function"]],
                                           arima.model = c(1, 1, 0),
                                      estimate.maxiter = 10000)
        # captura os dados
        modelo_arima <- adjusted %>% pluck("model", "arima", "model")
      adjusted <- func_captura_serie(adjusted,
                                     extracao_de_series)
        
        
        # Insere as datas originais. Elimina aquelas que correspondem aos NA's iniciais ou finais
        original_dates <- original[[date_col]]
        original_dates <- original_dates[(1 + omit_leading):(length(original_dates) - omit_trailing)]
        adjusted$date <- original_dates
        
        
        # Obtem apenas as séries necessárias (e renomeia-as)
        adjusted <- adjusted[, c("date", "final")]
        
        
        
        adjusted <- dplyr::rename(adjusted, (!!as.symbol(date_col)) := .data[["date"]], (!!as.symbol(var)) := .data[["final"]])
        
        # Mesclar
        seas_adjusted <- dplyr::left_join(seas_adjusted, adjusted, by = date_col)
        
        serie_ajustada <- dplyr::bind_cols(seas_adjusted, group_vars)
      print(glue::glue('serie ajustada: {original %>% pull(variavel) %>% unique()}  arima encontrado: {modelo_arima}'))
        
        sucesso = TRUE
      })
    }
    if(!sucesso){
      try(expr = {
        # 9 - X-13 geralzão com arima (0 1 1)(0 1 1) e com testes
        # _____________________________
        # mensagem de erro
        base::message(glue::glue("X-13 específico falhou para '{nome_var}': Tentando X-13 geral com arima forçado (0, 1, 1, 0, 1, 1) e com testes"))
        # ajusta
        adjusted <- seasonal::seas(                  x = spec_list[["x"]],  
                                    transform.function = spec_list[["transform.function"]],
                                           arima.model = c(0, 1, 1, 0, 1, 1),
                                      estimate.maxiter = 10000)
        # captura os dados
        modelo_arima <- adjusted %>% pluck("model", "arima", "model")
      adjusted <- func_captura_serie(adjusted,
                                     extracao_de_series)
        
        
        # Insere as datas originais. Elimina aquelas que correspondem aos NA's iniciais ou finais
        original_dates <- original[[date_col]]
        original_dates <- original_dates[(1 + omit_leading):(length(original_dates) - omit_trailing)]
        adjusted$date <- original_dates
        
        
        # Obtem apenas as séries necessárias (e renomeia-as)
        adjusted <- adjusted[, c("date", "final")]
        
        
        
        adjusted <- dplyr::rename(adjusted, (!!as.symbol(date_col)) := .data[["date"]], (!!as.symbol(var)) := .data[["final"]])
        
        # Mesclar
        seas_adjusted <- dplyr::left_join(seas_adjusted, adjusted, by = date_col)
        
        serie_ajustada <- dplyr::bind_cols(seas_adjusted, group_vars)
      print(glue::glue('serie ajustada: {original %>% pull(variavel) %>% unique()}  arima encontrado: {modelo_arima}'))
        
        sucesso = TRUE
      })
    }
    if(!sucesso){
      try(expr = {
        # 10 - X-13 geralzão com arima (0 1 1)(0 1 1) e sem AIC Test
        # _____________________________
        # mensagem de erro
        base::message(glue::glue("X-13 específico falhou para '{nome_var}': Tentando X-13 geral com arima forçado (0, 1, 1, 0, 1, 1) e sem AIC Test"))
        # ajusta
        adjusted <- seasonal::seas(                  x = spec_list[["x"]],  
                                    transform.function = spec_list[["transform.function"]],
                                    regression.aictest = NULL,
                                               outlier = '',
                                           arima.model = c(0, 1, 1, 0, 1, 1),
                                      estimate.maxiter = 10000)
        # captura os dados
        modelo_arima <- adjusted %>% pluck("model", "arima", "model")
      adjusted <- func_captura_serie(adjusted,
                                     extracao_de_series)
        
        
        # Insere as datas originais. Elimina aquelas que correspondem aos NA's iniciais ou finais
        original_dates <- original[[date_col]]
        original_dates <- original_dates[(1 + omit_leading):(length(original_dates) - omit_trailing)]
        adjusted$date <- original_dates
        
        
        # Obtem apenas as séries necessárias (e renomeia-as)
        adjusted <- adjusted[, c("date", "final")]
        
        
        
        adjusted <- dplyr::rename(adjusted, (!!as.symbol(date_col)) := .data[["date"]], (!!as.symbol(var)) := .data[["final"]])
        
        # Mesclar
        seas_adjusted <- dplyr::left_join(seas_adjusted, adjusted, by = date_col)
        
        serie_ajustada <- dplyr::bind_cols(seas_adjusted, group_vars)
      print(glue::glue('serie ajustada: {original %>% pull(variavel) %>% unique()}  arima encontrado: {modelo_arima}'))
        
        sucesso = TRUE
      })
    }
    if(!sucesso){
      try(expr = {
        # 11 - X-13 geralzão com arima (0 1 1)(0 1 1) e sem testes
        # _____________________________
        # mensagem de erro
        base::message(glue::glue("X-13 específico falhou para '{nome_var}': Tentando X-13 geral com arima forçado (0, 1, 1, 0, 1, 1) e sem testes"))
        # ajusta
        adjusted <- seasonal::seas(                  x = spec_list[["x"]],  
                                    transform.function = spec_list[["transform.function"]],
                                    regression.aictest = NULL,
                                               outlier = NULL,
                                           arima.model = c(0, 1, 1, 0, 1, 1),
                                      estimate.maxiter = 10000)
        # captura os dados
        modelo_arima <- adjusted %>% pluck("model", "arima", "model")
      adjusted <- func_captura_serie(adjusted,
                                     extracao_de_series)
        
        
        # Insere as datas originais. Elimina aquelas que correspondem aos NA's iniciais ou finais
        original_dates <- original[[date_col]]
        original_dates <- original_dates[(1 + omit_leading):(length(original_dates) - omit_trailing)]
        adjusted$date <- original_dates
        
        
        # Obtem apenas as séries necessárias (e renomeia-as)
        adjusted <- adjusted[, c("date", "final")]
        
        
        
        adjusted <- dplyr::rename(adjusted, (!!as.symbol(date_col)) := .data[["date"]], (!!as.symbol(var)) := .data[["final"]])
        
        # Mesclar
        seas_adjusted <- dplyr::left_join(seas_adjusted, adjusted, by = date_col)
        
        serie_ajustada <- dplyr::bind_cols(seas_adjusted, group_vars)
      print(glue::glue('serie ajustada: {original %>% pull(variavel) %>% unique()}  arima encontrado: {modelo_arima}'))
        
        sucesso = TRUE
      })
    }
      if(!sucesso){
        try(expr = {
          # 12 - decompose
          # _____________________________
          # mensagem de erro
          base::message(glue::glue("X-13 geral também falhou para '{nome_var}': Tentando decompose (opção de ajuste mais naive possível)"))
          automethod <- ifelse(spec_list[["transform.function"]] == "log","multiplicative","additive")
          # ajusta
          adjusted <- decompose(x = spec_list[["x"]], 
                                type = automethod)
          if(automethod == "multiplicative") {
            adjusted <- tibble::tibble(final = as.numeric(tsversion/adjusted$seasonal))
          } else {
            adjusted <- tibble::tibble(final = as.numeric(tsversion - adjusted$seasonal))
          }
          
          # Insere as datas originais.
          adjusted$date <- original[[date_col]]
          
          
          
          adjusted <- dplyr::rename(adjusted, (!!as.symbol(date_col)) := .data[["date"]], (!!as.symbol(var)) := .data[["final"]])
          
          # Mesclar
          seas_adjusted <- dplyr::left_join(seas_adjusted, adjusted, by = date_col)
          serie_ajustada <- dplyr::bind_cols(seas_adjusted, group_vars)
          
          sucesso <- TRUE
          
        })
        }
    if(!sucesso){
          if (usa_dado_original) {
            base::message(paste0("Erro no ajuste sazonal da série (mantendo os dados originais): ", nome_var))
            seas_adjusted <- dplyr::left_join(seas_adjusted, original[, c(date_col, var)], by = date_col)
            serie_ajustada <- dplyr::bind_cols(seas_adjusted, group_vars)
          } else {
            base::message(paste0("Erro no ajuste sazonal da série (substituindo as séries por NAs): ", nome_var))
            seas_adjusted[[var]] <- NA
            serie_ajustada <- dplyr::bind_cols(seas_adjusted, group_vars)
          }
    }
  }
  return(serie_ajustada)
}


# conta quantos NA's tem no inicio
leading_nas <- function(data) {
  i <- 0
  while (is.na(data[i + 1])) {
    i <- i + 1
  }
  return(i)
}

# conta quantos NA's tem no final
trailing_nas <- function(data) {
  i <- 0
  while (is.na(data[length(data) - i])) {
    i <- i + 1
  }
  return(i)
}

obtem_data_inicio <- function(date_col_name, frequencia_data) {
  start <- min(date_col_name)
  if (frequencia_data == "quarter") {
    return(c(lubridate::year(start), lubridate::quarter(start)))
  } else if (frequencia_data == "month") {
    return(c(lubridate::year(start), lubridate::month(start)))
  } else {
    stop(paste0("Não sei como lidar com a frequência ", frequencia_data))
  }
}
# retorna numero da frequencia para criar ts
fracao_anual_frequencia <- function(frequencia_data) {
  if (frequencia_data == "quarter") {
    return(4)
  } else if (frequencia_data == "month") {
    return(12)
  }
}


obtem_coluna_de_data <- function(data) {
  date_col <- NULL
  # percorre para cada coluna verificando e armazenando se for tipo date
  for (col in colnames(data)) {
    # se for do tipo data armazena
    if (lubridate::is.Date(data[[col]])) {
      # se for a primeira tipo data armazena
      if (is.null(date_col)) {
        date_col <- col
        # se não for a primeira stopa e emite erro
      } else {
        stop("Mais de uma coluna de data nos seus dados.")
      }
    }
  }
  # Se percorreu tudo e achou coluna tipo date, retorna numero da coluna
  if (!is.null(date_col)) {
    return(date_col)
    # se não achou stopa e emite erro
  } else {
    stop("Não foi possível encontrar uma coluna de datas nos seus dados. É preciso que a coluna de datas seja do tipo 'date'")
  }
}

# acha_frequencia_datas <- function(datas) {
#   # captura menor data
#   start <- min(datas)
#   # captura maior data
#   end <- max(datas)
#   # para cada tipo de frequencia, gera sequencia de datas e 
#   # verifica se está alinhado com as datas que temos
#   for (posit in c("year", "quarter", "month", "day")) {
#     viable_seq <- seq.Date(from = start, to = end, by = posit)
#     # Se for compatível: retorna tipo de frequencia
#     if (all(datas %in% viable_seq)) { 
#       base::message(paste0("Frequencia das datas não fornecida, determinamos que é ", posit))
#       return(posit)
#     }
#   }
#   stop("Não é possível determinar a frequência de datas dos dados.")
# }
acha_frequencia_datas <- function(datas) {
  return(NULL)
}

regularise <- function(data, date_col, frequencia_data, group_vars) {
  if (any(duplicated(data[[date_col]]))) {
    stop("Você tem datas duplicadas em um grupo. Você agrupou corretamente seus dados?")
  }
  start <- min(data[[date_col]])
  end <- max(data[[date_col]])
  reg_seq <- seq.Date(from = start, to = end, by = frequencia_data)
  staging_tibble <- tibble::tibble(full_dates = reg_seq)
  join_names <- stats::setNames("full_dates", date_col)
  widened <- dplyr::right_join(data, staging_tibble, by = join_names)
  widened <- dplyr::arrange(widened, .data[[date_col]])
  
  # E agora garanta que quaisquer novos vars obtenham os valores corretos para cada uma das variáveis do grupo
  for (var in names(group_vars)) {
    val <- group_vars[[var]]
    widened[[var]] <- val
  }
  return(widened)
}

adiciona_feriados_pais <- function(spec_list, 
                                   country,
                                   encoding_pc,
                                   var_xreg = NULL,
                                   var_regression.usertype,
                                   frequencia_data){
  con <- odbc::dbConnect(odbc(),
                         Driver = "PostgreSQL ANSI(x64)",
                         Server = "PGKPTL01",
                         Database = "Macro",
                         UID = "k_macro",
                         PWD = "kmacro@01",
                         encoding = encoding_pc)
  
  nomes_paises <- dbSendQuery(
    con, 
    glue::glue("select distinct country_name from releases.feriados_mundo")) %>% 
    dbFetch() %>% pull(country_name)
  # Se tiver país e não tiver feriado específico definido, faça:
  if((country %in% nomes_paises) & (is.null(var_xreg))){
    
    # 1 - puxar dados de feriados do SQL
    # _______________________________________
    feriados <- dbSendQuery(
      con, 
      glue::glue("select * from releases.feriados_mundo where country_name = '{country}'")) %>% 
      dbFetch() %>% 
      dplyr::select(date, feriado, extra_days) %>% 
      plyr::dlply(.variables = "feriado") %>% 
      map(
        .f = ~ {
          date <-  .x %>% pull(date)
          
          extra_days <-  .x %>% pull(extra_days) %>% unique()
          
          holiday_genhol <- genhol(
            as.Date(date), 
            start = -extra_days, 
            end = extra_days, 
            frequency = ifelse(frequencia_data == "month", 12, 4), 
            center = 'calendar'
          ) %>% as.ts()
          
          if(any(holiday_genhol!=0)){ 
            return(holiday_genhol)
          } else {
            return(NULL)
          }
        }
      ) %>% 
      Filter(Negate(is.null), .)
    
    union_de_ts_feriados <- Reduce(ts.union, feriados)
    if(length(names(feriados))>1){
        colnames(union_de_ts_feriados) <- names(feriados)
        }
    
    
    spec_list[["xreg"]] <- union_de_ts_feriados
    # se tiver especificado regression.usertype, anexa, se não coloca como feriado
    ifelse(missing("var_regression.usertype") , 
           spec_list[["regression.usertype"]] <- "holiday", 
           spec_list[["regression.usertype"]] <- var_regression.usertype) 
    
    dbDisconnect(con)
    
    return(spec_list)
  } else {
    # Se tiver feriados especificos no input, adiciona
    if(!is.null(var_xreg)){
      base::message("Adicionando feriados específicos do input")
      # adiciona feriados específicos no formato ts.union
      spec_list[["xreg"]] <- var_xreg
      # se tiver especificado regression.usertype, anexa, se não coloca como feriado
      ifelse(missing("var_regression.usertype") , 
             spec_list[["regression.usertype"]] <- "holiday", 
             spec_list[["regression.usertype"]] <- var_regression.usertype) 
      # retorna feriado específico definido
      dbDisconnect(con)
      return(spec_list)
    } else{
        # Se entrar aqui é por que o problema está no nome do pais
        dbDisconnect(con)
        stop(glue::glue("O pais: {country} não está na  tabela  releases.feriados_mundo do SQL. Tente escrever o nome do país em inglês por extenso e com letra maiúscula nos nomes iniciais. Ex: 'United States', 'Brazil', 'Switzerland'"))
      }
    }
}


adiciona_level_shifts_e_temporary_changes <- function(spec_list, 
                                                      country,
                                                      encoding_pc,
                                                      var_regression.variables){
  con <- odbc::dbConnect(odbc(),
                         Driver = "PostgreSQL ANSI(x64)",
                         Server = "PGKPTL01",
                         Database = "Macro",
                         UID = "k_macro",
                         PWD = "kmacro@01",
                         encoding = encoding_pc)
  
  nomes_paises <- dbSendQuery(
    con, 
    glue::glue("select distinct country from releases.var_regressao_mundo")) %>% 
    dbFetch() %>% pull(country)
  
  if(country %in% nomes_paises){
    
    # 1 - puxar dados de feriados do SQL
    # _______________________________________
    variaveis_regressao <- dbSendQuery(
      con, 
      glue::glue("select * from releases.var_regressao_mundo where country = '{country}'")) %>% 
      dbFetch() %>% 
      pull(regression_var)
    if(is.null(var_regression.variables)){
        spec_list[["regression.variables"]] <- c("td", variaveis_regressao)
        }
    
    dbDisconnect(con)
    
    return(spec_list)
  } else {
    dbDisconnect(con)
    base::message(glue::glue("AVISO: O pais: {country} não está na tabela  releases.var_regressao_mundo do SQL. Tente escrever o nome do país em inglês por extenso e com letra maiúscula nos nomes iniciais. Ex: 'United States', 'Brazil', 'Switzerland'"))
    # ADICIONA OS TRADING DAYS e retorna
    if(is.null(var_regression.variables)){
        spec_list[["regression.variables"]] <- c("td")
        }
    return(spec_list)
  }
}

limpa_string_adj <- function(string){
  limpo <- string 
  # cria coluna com nomes completamente limpos
  # limpa espacos excedentes
  limpo <- str_squish(limpo)
  # substitui acentos
  limpo <- iconv(limpo, to = "ASCII//TRANSLIT")
  # maiúscula
  limpo <- toupper(limpo)
  # remove caracteres estranhos com excessão da vírgula
  limpo <- str_replace_all(limpo, "[^[:alnum:]\\,\\s]", " ")
  # limpa espacos excedentes 
  limpo <- str_squish(limpo)
  # substitui espacos por underline
  limpo <- str_replace_all(limpo, " ", "_")
  
  return(limpo)
} 

cria_auto <- function(dado_original_adj, 
                      automethod,  
                      country, 
                      date_col, 
                      var_xreg, 
                      var_regression.usertype,
                      var_arima.model, 
                      encoding_pc,
                      var_xtrans,
                      var_seats.noadmiss,
                      var_regression.aictest,
                      var_outlier,
                      var_automdl,
                      var_composite,
                      var_out,
                      var_dir,
                      var_list,
                      var_regression.variables,
                      var_seats,
                      var_force.type,
                      frequencia_data){
  
  # Define Spec Automatico
  auto <- list()
  if(automethod == "multiplicative"){
    auto[["transform.function"]] <- "log"
  } else if (automethod == "additive") {
    auto[["transform.function"]] <- "none"
  } else {
    # Emite erro
    stop("'automethod' deve ser 'multiplicative' ou 'additive'")
  }
  
  if(country != "none"){
    if(as.Date(min(dado_original_adj[[date_col]])) < as.Date("1995-01-01") & country != 'Brazil'){
      base::message(glue::glue("Não é possivel fazer ajuste sazonal com os feriados deste país, pois data inicial
                               ({min(dado_original_adj[[date_col]])}) vem de antes de '1995-01-01'. Modelo seguira para X-13 sem os feriados."))
    } else {
      auto <- suppressWarnings(
        adiciona_feriados_pais(spec_list = auto,
                               country=country,
                               encoding_pc = encoding_pc,
                               var_xreg = var_xreg,
                               var_regression.usertype = var_regression.usertype,
                               frequencia_data = frequencia_data))
    }
    
    auto <- suppressWarnings(
      adiciona_level_shifts_e_temporary_changes(spec_list = auto,
                                                country=country,
                                                encoding_pc = encoding_pc,
                                                var_regression.variables = var_regression.variables)
    )
  }
  auto[["x11"]] <- ""
  if(!is.null(var_arima.model)){
    auto[["arima.model"]] <- var_arima.model
  }
  if(!is.null(var_xtrans)){
    auto[["xtrans"]] <- var_xtrans
    }
  if(!is.null(var_seats.noadmiss)){
    auto[["seats.noadmiss"]] <- var_seats.noadmiss
    }
  if(!is.null(var_regression.aictest)){
    auto[["regression.aictest"]] <- var_regression.aictest
    }
  if(!is.null(var_outlier)){
    auto[["outlier"]] <- var_outlier
    }
  if(!is.null(var_automdl)){
    auto[["automdl"]] <- var_automdl
    }
  if(!is.null(var_composite)){
    auto[["composite"]] <- var_composite
    }
  if(!is.null(var_out)){
    auto[["out"]] <- var_out
    }
  if(!is.null(var_dir)){
    auto[["dir"]] <- var_dir
    }
  if(!is.null(var_list)){
    auto[["list"]] <- var_list
  }
  if(!is.null(var_regression.variables)){
    auto[["regression.variables"]] <- var_regression.variables
  }
  if(is.null(var_regression.variables)){
    auto[["regression.variables"]] <- c("td")
  }
  if(!is.null(var_seats)){
    auto[["seats"]] <- var_seats
  }
  if(!is.null(var_force.type)){
    auto[["force.type"]] <- var_force.type
  }
  return(auto)
}

func_captura_serie <- function(adjusted,
                               extracao_de_series){
  # Se for para extrair data, faça
  if(extracao_de_series == "data"){
  serie <- tibble::as_tibble(adjusted$data) %>%
    drop_na()
  } else {
    # Se não for verifica se série a ser extraída foi gerada
      series_geradas <- names(adjusted$series)
      # Se não foi, emite mensagem e extrai data
      if(!extracao_de_series %in% series_geradas){
        base::message(glue::glue("A série '{extracao_de_series}' não foi gerada. As séries geradas para está variável são: {paste(series_geradas, collapse = ', ')}. Retornando 'data'"))
        serie <- tibble::as_tibble(adjusted$data) %>%
          drop_na()
      } else {
      # Se foi, extrai serie e retorna
        serie <- tibble::as_tibble(adjusted %>% pluck("series", extracao_de_series)) %>%
          drop_na() %>% rename(final = x)
        }
    }
  return(serie)
}
