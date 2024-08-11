
``` r
pacman::p_load(
dplyr, # manipulação de dados
magrittr, # operador pipe line %>%
janitor, # tabela de contigencia => tabyl, adorn_pct_formatting, adorn_totals, adorn_percentages, adorn_ns
effsize # tamanho do efeito d'cohen
)
```



``` r
pacman::p_load(clipr) # captura dos dados => write_clip
capture = function(tabela, col_names=TRUE, pontuacao=','){
  tabela %>% print() %>% write_clip(dec = pontuacao, col.names = col_names)
}
```

# Valor de P


``` r
retorne_p = function(valor){
  valor_str = formatC(valor, format = "f", digits = 6)
  if (valor < 0.05){
    if (valor < 0.001 || valor == 0){"< 0.001"}
    else {
      if (valor < 0.01){substring(as.character(valor_str), 1, 5)}
      else {substring(as.character(valor_str), 1, 4)}
    }}
  else {
    if (valor < 0.06){substring(as.character(valor_str), 1, 5)}
    else {substring(as.character(valor_str), 1, 4)}}}

retorne_p(0.054)
```

```
## [1] "0.054"
```

``` r
retorne_p(0.050)
```

```
## [1] "0.050"
```

``` r
retorne_p(0.059)
```

```
## [1] "0.059"
```

``` r
retorne_p(0.045)
```

```
## [1] "0.04"
```

``` r
retorne_p(0.4)
```

```
## [1] "0.40"
```

``` r
retorne_p(0.3454149)
```

```
## [1] "0.34"
```

``` r
retorne_p(0.399949)
```

```
## [1] "0.39"
```

``` r
retorne_p(0.04)
```

```
## [1] "0.04"
```

``` r
retorne_p(0.002)
```

```
## [1] "0.002"
```

``` r
retorne_p(0.00002)
```

```
## [1] "< 0.001"
```



``` r
retorne_p_ajust = function(valor){
  if (valor == "< 0.001"){
    "P-Value < 0.001"
  }
  else{
    paste("P-Value =", valor)
  }
}

retorne_p_ajust(retorne_p(0.00002))
```

```
## [1] "P-Value < 0.001"
```

``` r
retorne_p_ajust(retorne_p(0.399949))
```

```
## [1] "P-Value = 0.39"
```

``` r
retorne_p_ajust(retorne_p(0.04))
```

```
## [1] "P-Value = 0.04"
```



``` r
# Aplicando a função a todos elementos da coluna
apply_retorne_p = function(df, coluna) {
  df[[coluna]] = sapply(df[[coluna]], function(x) {
    if (!is.na(x) && is.numeric(x)) {
      retorne_p(x)
    } else {
      x
    }
  })
  return(df)
}
```

# Arredondamento


``` r
rround = function(valor, digitos){
  if (abs(valor) < 0.01 || abs(valor) > 1000){
    if (abs(valor) < 0.01){formatC(0, format = "f", digits = digitos)}
    else{formatC(valor, format = "e", digits = digitos)}}
  else{formatC(valor, format = "f", digits = digitos)}}

rround(30, 2)
```

```
## [1] "30.00"
```

``` r
rround(0, 2)
```

```
## [1] "0.00"
```

``` r
rround(0.001, 2)
```

```
## [1] "0.00"
```

``` r
rround(0.0001, 2)
```

```
## [1] "0.00"
```

``` r
rround(0.041212, 2)
```

```
## [1] "0.04"
```

``` r
rround(0.1, 2)
```

```
## [1] "0.10"
```

``` r
rround(45.5151, 2)
```

```
## [1] "45.52"
```

``` r
rround(5115156, 2)
```

```
## [1] "5.12e+06"
```



``` r
rround = function(valor, digitos){
  if (abs(valor) < 0.01 && abs(valor) > 0.0009) {formatC(valor, format = "f", digits = 3)} 
  else if (valor == 0){"0.00"}
  else if (abs(valor) <= 0.0009 && valor != 0 || abs(valor) > 10000) {formatC(valor, format = "e", digits = digitos)} #{"0.00"}
  else {formatC(valor, format = "f", digits = digitos)}
}

rround(30, 2)
```

```
## [1] "30.00"
```

``` r
rround(0, 2)
```

```
## [1] "0.00"
```

``` r
rround(0.001, 2)
```

```
## [1] "0.001"
```

``` r
rround(0.0001, 2)
```

```
## [1] "1.00e-04"
```

``` r
rround(0.041212, 2)
```

```
## [1] "0.04"
```

``` r
rround(0.1, 2)
```

```
## [1] "0.10"
```

``` r
rround(45.5151, 2)
```

```
## [1] "45.52"
```

``` r
rround(5115156, 2)
```

```
## [1] "5.12e+06"
```

``` r
rround(0.0000000041212, 2)
```

```
## [1] "4.12e-09"
```



``` r
# Aplicando a função a todos elementos da coluna 
apply_rround = function(df, coluna, digitos = 2){
  df[[coluna]] = sapply(df[[coluna]], function(x) {
    if (!is.na(x) && is.numeric(x)) {
      rround(x, digitos)
    } else {
      x
    }
  })
  return(df)
}

apply_rround2 = function(vetor, digitos = 2){
  vetor = sapply(vetor, function(x) {
    if (!is.na(x) && is.numeric(x)) {
      rround(x, digitos)
    } else {
      x
    }
  })
  return(df)
}
```

# Tabelas


``` r
library(janitor) #tabela de contigencia => tabyl, adorn_pct_formatting, adorn_totals, adorn_percentages, adorn_ns
```



``` r
# Tabela contagem simples
cont = function(df, variavel){
  df %>% tabyl(.data[[variavel]], show_na = FALSE) %>% 
    adorn_pct_formatting(2) %>% as.data.frame()
}

cont(dff, 'desfecho')
```

```
##   desfecho  n percent
## 1        0 50  50.00%
## 2        1 50  50.00%
```

``` r
tabelinha_ajust = function(tabelinha){
  nomes_colunas = colnames(tabelinha)
  linha_nomes_colunas = as.data.frame(t(nomes_colunas), stringsAsFactors = FALSE)
  colnames(linha_nomes_colunas) = colnames(tabelinha)
  tabelinha = rbind(linha_nomes_colunas, tabelinha)
  colnames(tabelinha) = c('Variable', 'medida1', 'medida2')
  tabelinha$medida1[1] = NA
  tabelinha$medida2[1] = NA
  return(tabelinha)
}
```



``` r
# Função que verifica se é melhor aplicavel o teste de fisher para testar a hipotese entre duas variaveis categoricas
fisher_criterio = function(df, var1, var2){
  length1 = length(levels(as.factor(df[[var1]])))
  length2 = length(levels(as.factor(df[[var2]])))
  if (length1 >= 3 || length2 >= 3){
    return(FALSE)
  }
  else {
    # Criar tabela de contingência
    tabela = table(df[[var1]], df[[var2]])
    # Calcular as expectativas de frequência
    total_geral = sum(tabela)
    expectativas = outer(rowSums(tabela), colSums(tabela), FUN = "*") / total_geral
    # Verificar se alguma célula tem expectativa de frequência < 5
    return(any(expectativas < 5))
  }
}
```



``` r
conti = function(df, var_y, var_x, sentido_percent='col', apenas_fisher=F){
  #sentido_porcent => #col, row
  tabela = df %>% 
    tabyl(.data[[var_x]], .data[[var_y]], show_na = FALSE) %>% 
    adorn_totals(c("row", "col")) %>% 
    adorn_percentages(sentido_percent) %>% 
    adorn_pct_formatting(2) %>% adorn_ns
  
  #tabela = as.data.frame(tabela) %>% 
    #mutate(across(where(is.character), ~ str_replace_all(.x, "(\\d+\\.\\d+)\\% *\\(? *(\\d+)\\)?", "\\2 (\\1%)")))
  tabela = rename(tabela, "Variable" = var_x)
  tabela = rbind(NA, tabela)
  tabela[["Variable"]] = as.character(tabela[["Variable"]])
  tabela[["Variable"]][1] = var_x
  tabela = tabela[-nrow(tabela), ] #excluindo ultima linha
  
  #Aplicando teste de hipotese adequado
  tabela[["P-value"]] = NA
  tabela[["Test_Used"]] = NA
  if(fisher_criterio(df, var_y, var_x) == F){
    if (nrow(tabela) <= 3 && apenas_fisher == T){
      tabela[["P-value"]][1] = retorne_p(fisher.test(df[[var_x]],df[[var_y]])$p.value)
      tabela[["Test_Used"]][1] = "Fisher Exact"}
    else {
      tabela[["P-value"]][1] = retorne_p(chisq.test(df[[var_x]],df[[var_y]])$p.value)
      tabela[["Test_Used"]][1] = "Chi-squared"}}
  else{
    tabela[["P-value"]][1] = retorne_p(fisher.test(df[[var_x]],df[[var_y]])$p.value)
    tabela[["Test_Used"]][1] = "Fisher Exact"}
  
  # Reordenando as colunas, colocando coluna total para primeira posicao
  # Determina a sequência de índices das colunas
  indices = seq_len(ncol(tabela))
  # Altera a sequência para colocar a coluna -3 na posição 2
  indices = c(indices[1], indices[length(indices)-2], indices[2:(length(indices)-3)], indices[(length(indices)-1):length(indices)])
  # Reordena as colunas do dataframe de acordo com a sequência
  tabela = tabela[, indices]
  tabela[["Variable"]][1] = var_x
  
  # Ultimos Ajustes
  colnames(tabela)[colnames(tabela) == 'Total'] = 'Overall'
  tabela[] = lapply(tabela, function(x) gsub("%", "", x))
  tabela[] = lapply(tabela, function(x) gsub("  ", " ", x))
  
  return(tabela %>% as.data.frame())
}

conti(dff, "desfecho", "tratamentos")
```

```
##      Variable    Overall          0          1 P-value   Test_Used
## 1 tratamentos       <NA>       <NA>       <NA> < 0.001 Chi-squared
## 2           A 33.00 (33) 58.00 (29)   8.00 (4)    <NA>        <NA>
## 3           B 31.00 (31) 36.00 (18) 26.00 (13)    <NA>        <NA>
## 4           C 36.00 (36)   6.00 (3) 66.00 (33)    <NA>        <NA>
```



``` r
# Maneira antiga
# library(RVAideMemoire) # shapiro por grupo ==> byf.shapiro(numerico~categorico, df)
# esse pacote é bom procurar saber mais dps, contem teste de levene | comparações múltiplas usando o teste t de Student com correção para múltiplos testes (pairwise.t.test) | ANOVA ajustada para heterocedasticidade (anova.hetero())
# normalidade_por_grupo_shapiro = function(df, col_num, col_cat){
#   tabela = byf.shapiro(df[[col_num]]~df[[col_cat]])$tab
#   resultados = tabela$`p-value`
#   verificacao = any(resultados < 0.05)
#   
#   if (verificacao){ 
#     return(FALSE) # Alguma distribuição não segue a normal
#   } else{
#     return(TRUE)} # Todas as distribuições seguem a normal
# }

# Maneira Nova

shapiro_test <- function(x) {
    return(shapiro.test(x)$p.value)
}

normalidade_por_grupo_shapiro = function(df, col_num, col_cat){
  tabela = aggregate(df[[col_num]]~df[[col_cat]], data=df, shapiro_test) 
  resultados = tabela$`p-value`
  verificacao = any(resultados < 0.05)
  
  if (verificacao){ 
    return(FALSE) # Alguma distribuição não segue a normal
  } else{
    return(TRUE)} # Todas as distribuições seguem a normal
}

normalidade_por_grupo_ks = function(df, col_num, col_cat){
  resultados <- by(df[[col_num]], df[[col_cat]], function(subset) {
    ks.test(subset, "pnorm", mean(subset), sd(subset))
  })
  
  p.values <- sapply(resultados, function(test) test$p.value)
  verificacao = any(p.values < 0.05)
  
  if (verificacao){
    return(FALSE)  # Alguma distribuição não segue a normal
  } else {
    return(TRUE)  # Todas as distribuições seguem a normal
  }
}

normalidade_por_grupo_criterio = function(df, col_num, col_cat){
  tabela = df %>% filter(!is.na(!!sym(col_num)) & !is.na(!!sym(col_cat))) %>% 
    group_by(!!sym(col_cat)) %>% summarise(n = n())
  resultados = tabela$n
  
  # verificar se algum dos grupos é maior que 5000
  verificacao = any(resultados > 5000)
  
  if (verificacao){
    normalidade_por_grupo_ks(df, col_num, col_cat) %>% return()
  } else {
    normalidade_por_grupo_shapiro(df, col_num, col_cat) %>% return()
  }
}
```



``` r
summary_numerico_parametrico = function(df, col_num){
  tabela = df %>%
    filter(!is.na(!!sym(col_num))) %>%
    summarise(
      mean = round(mean(!!sym(col_num), na.rm = TRUE), 2),
      std = round(sd(!!sym(col_num), na.rm = TRUE), 2))
  
  tabela[["Variable"]] = col_num
  rownames(tabela) = NULL
  tabela = tabela[, c(ncol(tabela), 1:(ncol(tabela)-1))]
  
  return(tabela)
}

summary_numerico_parametrico(dff, 'var_num')
```

```
##   Variable  mean   std
## 1  var_num 56.07 29.14
```

``` r
library(effsize) #tamanho do efeito d'cohen

summary_numerico_por_grupo_parametrico = function(df, col_num, col_cat, teste_extra="F"){
  # Sumário por grupo
  sumario_grupo = df %>%
    filter(!is.na(!!sym(col_num)), !is.na(!!sym(col_cat))) %>%
    group_by(!!sym(col_cat)) %>%
    summarise(
      resumo = paste0(
        round(mean(!!sym(col_num), na.rm = TRUE), 2), 
        #" ± ", round(as.numeric(sd(!!sym(col_num), na.rm = TRUE)), 2)
        " (", round(as.numeric(sd(!!sym(col_num), na.rm = TRUE)), 2), ")")
      )
  sumario_grupo = rename(sumario_grupo, "coluna" = col_cat)
  
  # Sumário geral (total)
  sumario_geral = df %>%
    filter(!is.na(!!sym(col_num)), !is.na(!!sym(col_cat))) %>%
    summarise(
      coluna = 'Total',
      resumo = paste0(
        round(mean(!!sym(col_num), na.rm = TRUE), 2), 
        #" ± ", round(as.numeric(sd(!!sym(col_num), na.rm = TRUE)), 2)
        " (", round(as.numeric(sd(!!sym(col_num), na.rm = TRUE)), 2), ")")
      )
  
  sumario_final = rbind(sumario_geral, sumario_grupo) # Combinar os sumários
  tabela = as.data.frame(t(sumario_final)) # Transpor o dataframe
  colnames(tabela) = tabela[1, ] # Ajustar os nomes das colunas
  tabela = tabela[-1, ]  # Remover a primeira linha
  rownames(tabela)[1] = col_num
  
  tabela[["P-value"]] = NA
  tabela[["Test_Used"]] = NA
  
  if ( length(levels(as.factor(df[[col_cat]]))) <= 2 ){
    pvalor = retorne_p(t.test(df[[col_num]]~df[[col_cat]])$p.value)
    tabela[["Test_Used"]][1] = "T Test"
    
    if (teste_extra == "T"){
      niveis = df[[col_cat]] %>% as.factor() %>% levels()
      grupo1 = df[[col_num]][df[[col_cat]] == niveis[2]]
      grupo2 = df[[col_num]][df[[col_cat]] == niveis[1]]
      d_cohen = cohen.d(grupo1, grupo2)
      estimador = as.character(rround(d_cohen$estimate,2))
      IC_00 = as.character(rround(d_cohen$conf.int[1],2))
      IC_01 = as.character(rround(d_cohen$conf.int[2],2))
      d_cohen = paste0(estimador,' (',IC_00,' to ',IC_01,')')
      tabela[["teste_extra"]] = NA
      tabela[["teste_extra"]] = d_cohen}
  }else { 
    pvalor = summary(aov(df[[col_num]]~df[[col_cat]]))[[1]][["Pr(>F)"]][1]
    pvalor = retorne_p(pvalor)
    tabela[["Test_Used"]][1] = "Anova"
  }
  
  tabela[["P-value"]] = pvalor
  
  tabela[["Variable"]] = rownames(tabela)
  rownames(tabela) = NULL
  tabela = tabela[, c(ncol(tabela), 1:(ncol(tabela)-1))] #ultima coluna para primeira
  
  colnames(tabela)[colnames(tabela) == 'Total'] = 'Overall'
  
  return(tabela)  
}
# Exemplo de uso:
summary_numerico_por_grupo_parametrico(dff, "var_num", "desfecho", 'T')
```

```
##   Variable       Overall            0             1 P-value Test_Used          teste_extra
## 1  var_num 56.07 (29.14) 51.16 (31.2) 60.98 (26.33)    0.09    T Test 0.34 (-0.06 to 0.74)
```



``` r
summary_numerico_n_parametrico = function(df, col_num){
  tabela = df %>%
    filter(!is.na(!!sym(col_num))) %>%
    summarise(
      median = round(median(!!sym(col_num), na.rm = TRUE), 2),
      q1_q3 = paste0(
        '[', round(as.numeric(quantile(!!sym(col_num), 0.25, na.rm = TRUE)), 2), 
        " - ", 
        round(as.numeric(quantile(!!sym(col_num), 0.75, na.rm = TRUE)), 2), ']'))
  
  tabela[["Variable"]] = col_num
  rownames(tabela) = NULL
  tabela = tabela[, c(ncol(tabela), 1:(ncol(tabela)-1))]
  
  return(tabela)
}

summary_numerico_por_grupo_n_parametrico = function(df, col_num, col_cat, teste_extra="F"){
  # Sumário por grupo
  sumario_grupo = df %>%
    filter(!is.na(!!sym(col_num)), !is.na(!!sym(col_cat))) %>%
    group_by(!!sym(col_cat)) %>%
    summarise(
      resumo = paste0(
        round(median(!!sym(col_num), na.rm = TRUE), 2),
        " [", round(as.numeric(quantile(!!sym(col_num), 0.25, na.rm = TRUE)), 2), 
        " - ", 
        round(as.numeric(quantile(!!sym(col_num), 0.75, na.rm = TRUE)), 2),"]"
      )
    )
  sumario_grupo = rename(sumario_grupo, "coluna" = col_cat)
  
  # Sumário geral (total)
  sumario_geral = df %>%
    filter(!is.na(!!sym(col_num)), !is.na(!!sym(col_cat))) %>%
    summarise(
      coluna = 'Total',
      resumo = paste0(
        round(median(!!sym(col_num), na.rm = TRUE), 2), 
        " [", round(as.numeric(quantile(!!sym(col_num), 0.25, na.rm = TRUE)), 2), 
        " - ", 
        round(as.numeric(quantile(!!sym(col_num), 0.75, na.rm = TRUE)), 2),"]"
      )
    )
  
  sumario_final = rbind(sumario_geral, sumario_grupo) # Combinar os sumários
  tabela = as.data.frame(t(sumario_final)) # Transpor o dataframe
  colnames(tabela) = tabela[1, ] # Ajustar os nomes das colunas
  tabela = tabela[-1, ]  # Remover a primeira linha
  rownames(tabela)[1] = col_num
  
  tabela[["P-value"]] = NA
  tabela[["Test_Used"]] = NA
  if (length(levels(as.factor(df[[col_cat]]))) > 2){
    pvalor = retorne_p(kruskal.test(df[[col_num]]~df[[col_cat]])$p.value)
    tabela[["Test_Used"]][1] = "Kruskal-Wallis"}
  else{
    pvalor = retorne_p(wilcox.test(df[[col_num]]~df[[col_cat]])$p.value)
    tabela[["Test_Used"]][1] = "Mann-Whitney"
    
    if (teste_extra == "T"){
      niveis = df[[col_cat]] %>% as.factor() %>% levels()
      grupo1 = df[[col_num]][df[[col_cat]] == niveis[2]]
      grupo2 = df[[col_num]][df[[col_cat]] == niveis[1]]
      teste_hip = wilcox.test(grupo1, grupo2, conf.int = TRUE)
      
      #Estimador Hodges Lehmann
      estimador = as.character(rround(teste_hip$estimate,2))
      IC_00 = as.character(rround(teste_hip$conf.int[1],2))
      IC_01 = as.character(rround(teste_hip$conf.int[2],2))
      hodges_lehmann = paste0(estimador,' (',IC_00,' to ',IC_01,')')
      tabela[["teste_extra"]] = NA
      tabela[["teste_extra"]] = hodges_lehmann
      }
    }
  tabela[["P-value"]] = pvalor
  
  tabela[["Variable"]] = rownames(tabela)
  rownames(tabela) = NULL
  tabela = tabela[, c(ncol(tabela), 1:(ncol(tabela)-1))] #ultima coluna para primeira
  
  colnames(tabela)[colnames(tabela) == 'Total'] = 'Overall'
  
  return(tabela)  
}

summary_numerico_por_grupo_n_parametrico(dff, "var_num", "desfecho", 'T')
```

```
##   Variable           Overall              0                   1 P-value    Test_Used
## 1  var_num 57.5 [34.75 - 78] 50 [30 - 70.5] 60.5 [42.25 - 84.5]    0.10 Mann-Whitney
##              teste_extra
## 1 10.00 (-2.00 to 22.00)
```



``` r
summary_numerico_por_grupo = function(df, col_num, col_cat){
  if (normalidade_por_grupo_criterio(df, col_num, col_cat)){
    summary_numerico_por_grupo_parametrico(df, col_num, col_cat) %>% return()
  } else{
    summary_numerico_por_grupo_n_parametrico(df, col_num, col_cat) %>% return()
  }
}

summary_numerico_por_grupo(dff, "var_num", "desfecho")
```

```
##   Variable       Overall            0             1 P-value Test_Used
## 1  var_num 56.07 (29.14) 51.16 (31.2) 60.98 (26.33)    0.09    T Test
```


``` r
analise_mod = function(modelo){
  estimadores = as.data.frame(summary(modelo)$coefficients)
  odds = as.data.frame((exp(cbind(OR= coef(modelo), confint(modelo)))))
  
  estimadores = apply_retorne_p(estimadores, "Pr(>|z|)")
  #estimadores = estimadores[, "Pr(>|z|)", drop = FALSE]
  
  #odds = odds[rowSums(is.na(odds)) != ncol(odds), ] # Apagando as linhas quando um dado estiver NA
  odds = apply_rround(odds, "OR")
  odds = apply_rround(odds, "2.5 %")
  odds = apply_rround(odds, "97.5 %")
  #tabelona$OR = sapply(tabelona$OR, function(x) ifelse(is.na(x), NA, round(x, 2)))
  #tabelona$`2.5 %` = sapply(tabelona$`2.5 %`, function(x) ifelse(is.na(x), NA, round(x, 2)))
  #tabelona$`97.5 %` = sapply(tabelona$`97.5 %`, function(x) ifelse(is.na(x), NA, round(x, 2)))
  
  #odds$ODDS = paste0(odds$OR, " (", odds$`2.5 %`, " - ", odds$`97.5 %`, ")")
  #odds = odds[, "ODDS", drop = FALSE]
  
  tabela = cbind(odds, estimadores)
  tabela = tabela[-1, ] #apagando primeira linha
  
  tabela$Estimate = NULL
  tabela$`Std. Error` = NULL
  tabela$`z value` = NULL
  
  return(tabela)
}

# Versão antiga
analise_mod_antiga = function(modelo){
  estimadores = as.data.frame(summary(modelo)$coefficients)
  odds = as.data.frame((exp(cbind(OR= coef(modelo), confint(modelo)))))
  estimadores = apply_retorne_p(estimadores, "Pr(>|z|)")
  
  tabela = cbind(odds, estimadores)
  tabela = tabela[-1, ] #apagando primeira linha
  
  tabela$Estimate = NULL
  tabela$`Std. Error` = NULL
  tabela$`z value` = NULL
  
  tabela$variavel = row.names(tabela)
  tabela$variavel = factor(tabela$variavel, labels = tabela$variavel)
  
  tabela$vetor_or = tabela$OR
  tabela$vetor_ic0 = tabela$`2.5 %`
  tabela$vetor_ic1 = tabela$`97.5 %`
  tabela = apply_rround(tabela, 'vetor_or')
  tabela = apply_rround(tabela, 'vetor_ic0')
  tabela = apply_rround(tabela, 'vetor_ic1')
  tabela$OR_IC = paste0(tabela$vetor_or, ' (', tabela$vetor_ic0, ' to ', tabela$vetor_ic1, ')')
  tabela$vetor_or = NULL
  tabela$vetor_ic0 = NULL
  tabela$vetor_ic1 = NULL
  
  tabela$OR = tabela$OR %>% as.numeric() %>% round(digits = 2)
  tabela$`2.5 %`[is.na(tabela$`97.5 %`)] = NA
  tabela$`97.5 %`[is.na(tabela$`2.5 %`)] = NA
  tabela$`2.5 %` = tabela$`2.5 %` %>% as.numeric() %>% round(digits = 2)
  tabela$`97.5 %` = tabela$`97.5 %` %>% as.numeric() %>% round(digits = 2)
  
  colnames(tabela)[colnames(tabela) == 'Pr(>|z|)'] <- 'pvalor'
  
  return(tabela)
}
```



``` r
analise_fisher = function(teste){
  odds = rround(teste$estimate, 2)
  ic_0 = rround(teste$conf.int[1], 2)
  ic_1 = rround(teste$conf.int[2], 2)
  valores = paste0(odds, " (", ic_0, " - ", ic_1, ")")
  return(valores)
}
analise_fisher(fisher.test(table(dff$desfecho, dff$genero), conf.int = TRUE))
```

```
## [1] "2.64 (1.10 - 6.48)"
```



``` r
# Adicionar "\n" em frases muito longas
adicionar_quebra_de_linha = function(frase, comprimento_maximo = 20) {
  frase <- as.character(frase)
  # Verifica se frase é NA e retorna NA se for o caso
  if (is.na(frase)) {
    return(NA)
  }
  
  if (nchar(frase) > comprimento_maximo) {
    palavras = strsplit(frase, " ")[[1]]
    frase_final = ""
    linha_atual = ""
    
    for (palavra in palavras) {
      if (nchar(paste(linha_atual, palavra, sep = " ")) <= comprimento_maximo) {
        linha_atual = paste(linha_atual, palavra, sep = " ")
      } else {
        frase_final = paste(frase_final, linha_atual, "\n", sep = "")
        linha_atual = palavra
      }
    }
    frase_final = paste(frase_final, linha_atual, sep = "")
    frase <- as.factor(frase)
    return(frase_final)
  } else {
    return(frase)
  }
}


frase_longa = "Esta é uma frase muito longa que deve ser quebrada em várias linhas para melhor visualização."
adicionar_quebra_de_linha(frase_longa,50)
```

```
## [1] " Esta é uma frase muito longa que deve ser\nquebrada em várias linhas para melhor\nvisualização."
```

``` r
#df$coluna = sapply(df$coluna, function(x) adicionar_quebra_de_linha(x, 40))
```



``` r
#library(RcmdrMisc) #summary diferenciado ==> numSummary()
# #bibliotecas para extrair descrições das variaveis
# library(purrr)
# library(lmtest)
# 
# library(cowplot) #graficos lado a lado ==> plot_grid(graf1, graf2, graf3, graf4, ncol=2, nrow=2)
# library(MatchIt) #propensity score matchit
# 
# #forestplot
# library(sjPlot)
# library(sjlabelled)
# library(sjmisc)
# # ==> plot_model(modelo, show.values = TRUE, value.offset = .3)
# 
# library(epiR) #analise epidemiológica
# library(car) #"An R Companion to Applied Regression"
# library(rstatix) #pacote com testes de hipotese inutil
# library(emmeans) #pós-analise de modelos lineares e lineares generalizados
# library(exact2x2) # qui-quadrado pareado #ajuste para variaveis pequena
# library(vcd) #coefientes de tamanho de efeito para cruzamento categorico (CRAMER, PHI, COEF CONTI)
# library(effsize) #tamanho de efeito
# ###########
# library(DescTools) #==> PseudoR2(mod, which='Nagelkerke')
# library(QuantPsyc) #ClassLog
# library(rcompanion) # ==> compareGLM(mod3, mod4)
# library(pROC) #curva roc ==> roc()
# library(caret) #avaliação de ml (matrix confusão) ==> confusionMatrix()
# library(forestmodel) #grafico forestplot
# library(psych) # matrix de correlação==> pairs.panels(matrix)
# library(corrplot) #matriz/graficos de correlação
# 
# library(ggradar) #grafico de radar
# library(fmsb) #grafico de radar
# library(nnet) #treinar modelos; multinom
# 
# #################ANTIGO ABANDONADOS#######################
# library(tableone) #criação de tabelas
```



``` r
#library(gmodels) #analise de residuo em tabelas de cruzamentos categorica - TESTE PÓS HOC (de qui-quadrado)
```



``` r
library(caret)
library(DescTools)
library(car)
library(pROC)
library(MuMIn)

metricas_de_avaliacao_glm = function(modelo){
  data_df = model.frame(modelo)
  
  formula_modelo <- formula(modelo)
  variavel_dependente <- as.character(formula_modelo[[2]])
  vars_independentes <- as.character(formula_modelo[[3]])
  
  formula_texto = paste0(variavel_dependente, "~", vars_independentes)
  vars_independentes_lista <- unlist(strsplit(vars_independentes, split = "\\+"))
  
  tryCatch({
    # Previsões e dados reais
    df_clean = data_df#[complete.cases(data_df[, c(variavel_dependente, vars_independentes_lista)]), ]
    previsoes = predict(modelo, newdata = df_clean, type = "response")
    
    # Classificação binária das previsões
    previsoes_bin = ifelse(previsoes > 0.5, 1, 0)
    dados_reais = df_clean[[variavel_dependente]]
    
    # Metricas de Avaliação
    matrix = caret::confusionMatrix(as.factor(previsoes_bin), as.factor(dados_reais), positive = "1")
    acuracia = matrix$overall['Accuracy']
    precisao = matrix$byClass['Pos Pred Value']
    sensibilidade = matrix$byClass['Sensitivity']
    especificidade = matrix$byClass['Specificity']
    f1 = matrix$byClass['F1']
    auc = pROC::roc(dados_reais, previsoes)$auc # Curva roc
    
    # Extração dos valores de TP, TN, FP, FN
    tp = matrix$table[2, 2]
    tn = matrix$table[1, 1]
    fp = matrix$table[1, 2]
    fn = matrix$table[2, 1]
    
    # Critério de informação de (AKAIKE/BAYESIANO)
    aic = AIC(modelo)
    bic = BIC(modelo)
    
    # MODELANDO ESPECIFICAÇÕES DO MODELO
    if (inherits(modelo, "glm")) { # Modelo GLM
      # Pseudo R²
      pseudo_r2_McFadden = 1 - (modelo$deviance / modelo$null.deviance)
      pseudo_r2_Nagelkerke = DescTools::PseudoR2(modelo, which = "Nagelkerke")
      
      # null_model <- glm(target ~ 1, data = dfTrain, family = binomial)
      # pseudo_r2 <- 1 - logLik(model)/logLik(null_model)
      # print(paste("Pseudo R²: ", pseudo_r2))
      
      # VIF (Variance inflation factor) - Multicolinearidade
      if (length(vars_independentes_lista) > 1) {
        VIF = any(car::vif(modelo) > 10) %>% as.numeric()
      } else {
        VIF = 0
      }
    
      resultados = c(# tp=tp, tn=tn, fp=fp, fn=fn, 
        acuracia, precisao, sensibilidade, especificidade, 
        F1_Score = f1, AUC = auc,
        Pseudo_R2.McFadden = pseudo_r2_McFadden, Pseudo_R2 = pseudo_r2_Nagelkerke,
        AIC = aic, BIC = bic, VIF = VIF,
        Status = 1
      )
      return(resultados)
      
    } else if (inherits(modelo, "glmerMod")) { # Modelo Generalizado **Misto**
      warning("Você inseriu um modelo misto")
      
      # coeficiente de determinação condicional
      R2M = MuMIn::r.squaredGLMM(modelo)[1]
      # coeficiente marginal
      R2c = MuMIn::r.squaredGLMM(modelo)[2]
      
      resultados = c(# tp=tp, tn=tn, fp=fp, fn=fn, 
        acuracia, precisao, sensibilidade, especificidade, 
        F1_Score = f1, AUC = auc,
        R2M = R2M, R2c = R2c, 
        AIC = aic, BIC = bic, #VIF = VIF,
        Status = 1
      )
      return(resultados)
      
    } else {
      warning("O modelo fornecido não é suportado. Use um modelo 'glm' ou 'lmerMod'.")
      return(NULL)
    }
  }, error = function(e) {
    print(paste0('Erro com o modelo: ', formula_texto))
    return(c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 0))
  })
}

modelo = glm(desfecho ~ fixed_effects + group, data = dff, family = binomial())
metricas_de_avaliacao_glm(modelo) %>% round(4)
```

```
## Setting levels: control = 0, case = 1
```

```
## Setting direction: controls < cases
```

```
##             Accuracy       Pos Pred Value          Sensitivity          Specificity 
##               0.5900               0.5789               0.6600               0.5200 
##          F1_Score.F1                  AUC   Pseudo_R2.McFadden Pseudo_R2.Nagelkerke 
##               0.6168               0.6012               0.0207               0.0377 
##                  AIC                  BIC                  VIF               Status 
##             147.7580             163.3890               0.0000               1.0000
```

``` r
modelo_reduzido = glm(desfecho ~ fixed_effects, data = dff, family = binomial())
metricas_de_avaliacao_glm(modelo_reduzido) %>% round(4)
```

```
## Setting levels: control = 0, case = 1
## Setting direction: controls < cases
```

```
##             Accuracy       Pos Pred Value          Sensitivity          Specificity 
##               0.5900               0.5849               0.6200               0.5600 
##          F1_Score.F1                  AUC   Pseudo_R2.McFadden Pseudo_R2.Nagelkerke 
##               0.6019               0.5252               0.0019               0.0034 
##                  AIC                  BIC                  VIF               Status 
##             142.3709             147.5812               0.0000               1.0000
```

``` r
modelo_misto = glmer(desfecho ~ fixed_effects + (1|group), data=dff, family = binomial())
```

```
## boundary (singular) fit: see help('isSingular')
```

``` r
metricas_de_avaliacao_glm(modelo_misto) %>% round(4)
```

```
## Setting levels: control = 0, case = 1
## Setting direction: controls < cases
```

```
## Warning in doTryCatch(return(expr), name, parentenv, handler): Você inseriu um modelo misto
```

```
## Warning: the null model is only correct if all the variables it uses are identical 
## to those used in fitting the original model.
```

```
## boundary (singular) fit: see help('isSingular')
```

```
## Warning: the null model is only correct if all the variables it uses are identical 
## to those used in fitting the original model.
```

```
## boundary (singular) fit: see help('isSingular')
```

```
##       Accuracy Pos Pred Value    Sensitivity    Specificity    F1_Score.F1            AUC 
##         0.5900         0.5849         0.6200         0.5600         0.6019         0.5252 
##            R2M            R2c            AIC            BIC         Status 
##         0.0032         0.0026       144.3709       152.1864         1.0000
```

``` r
# Realizar o Teste de Deviance
# library(MASS)
# anova(modelo_reduzido, modelo, test = "Chisq")
# anova(modelo_reduzido, modelo)

# Teste de Hosmer-Lemeshow
# library(ResourceSelection)
# previsoes = predict(modelo, newdata = dff, type = "response")
# previsoes_bin = ifelse(previsoes > 0.5, 1, 0)
# hoslem.test(dff$desfecho, previsoes_bin, g = 10)

# Avaliação do modelo
# glm_diagnostic(modelo)
# modelo %>% stdres() %>% summary() # residuos padronizados
# Anova(modelo, type = 'II', test = "Wald") ## Overall effects
# Anova(modelo)
```



``` r
# library(lmerTest) # é melhor que library(lme4)
# library(MuMIn)
 # R quadrado para modelos mistos # 
# R2m = proporção explica apenas pelas variaveis fixas 
# R2c = proporção explica pelas variaveis fixas e aleatoria # R quadrado geral
# r.squaredGLMM(modelo) #código de aplicação

pacman::p_load(MuMIn, lmerTest)

metricas_de_avaliacao_regressao = function(modelo){
  data_df = model.frame(modelo)
  
  formula_modelo <- formula(modelo)
  variavel_dependente <- as.character(formula_modelo[[2]])
  vars_independentes <- as.character(formula_modelo[[3]])
  
  formula_texto = paste0(variavel_dependente, "~", vars_independentes)
  vars_independentes_lista <- unlist(strsplit(vars_independentes, split = "\\+"))
  
  tryCatch({
    # Previsões do modelo
    predictions = predict(modelo)
    
    # MAE - Mean Absolute Error
    MAE = mean(abs(data_df[[variavel_dependente]] - predictions))
    
    # MSE - Mean Squared Error
    MSE = mean((data_df[[variavel_dependente]] - predictions)^2)
    
    # RMSE - Root Mean Squared Error
    RMSE = sqrt(MSE)
    
    # MAPE - Mean Absolute Percentage Error
    MAPE = mean(abs((data_df[[variavel_dependente]] - predictions) / data_df[[variavel_dependente]])) * 100

    # Critério de informação de (AKAIKE/BAYESIANO)
    aic = AIC(modelo)
    bic = BIC(modelo)
      
    # MODELANDO ESPEFICAÇÕES DO MODELO
    # Verificando qual é o modelo
    if (inherits(modelo, "lm")) { # Modelo Linear
      #warning("vc inseriu um modelo linear")
      
      # VIF (Variance inflation factor) - Multicolinearidade
      # if (length(vars_independentes_lista) >= 2){
      #   tryCatch({
      #     VIF = any(car::vif(modelo) > 10) %>% as.numeric()
      #     }, error = function(e) {
      #       VIF = 1
      #       })
      # } else{ 
      #   VIF = 0
      # }
      VIF = 0 
      
      # Summary do modelo
      summary_LM = modelo %>% summary()
      # R-quadrado (R²)
      r_squared <- summary_LM$r.squared
      # R-quadrado ajustado
      r_squared_adj <- summary_LM$adj.r.squared
    
      resultados <- c(MAE = as.numeric(MAE), MSE = as.numeric(MSE), RMSE = as.numeric(RMSE), MAPE = as.numeric(MAPE), 
                      AIC = as.numeric(aic), BIC = as.numeric(bic), 
                      R2 = as.numeric(r_squared), R2_adj = as.numeric(r_squared_adj), 
                      VIF = as.numeric(VIF),
                      Status = 1)
      return(resultados)
    } else if (inherits(modelo, "lmerMod")) { # Modelo Linear **Misto**
      #warning("vc inseriu um modelo linear misto")

      # coeficiente de determinação condicional
      R2M = MuMIn::r.squaredGLMM(modelo)[1]
      # coeficiente marginal
      R2c = MuMIn::r.squaredGLMM(modelo)[2]
      
      resultados <- c(MAE = as.numeric(MAE), MSE = as.numeric(MSE), RMSE = as.numeric(RMSE), MAPE = as.numeric(MAPE), 
                      AIC = as.numeric(aic), BIC = as.numeric(bic), 
                      R2M = as.numeric(R2M), R2c = as.numeric(R2c), 
                      #VIF = as.numeric(VIF), 
                      Status = 1)
      return(resultados)
    } else {
      #warning("O modelo fornecido não é suportado. Use um modelo 'lm' ou 'lmerMod'.")
      
      resultados <- c(MAE = as.numeric(MAE), MSE = as.numeric(MSE), RMSE = as.numeric(RMSE), MAPE = as.numeric(MAPE), 
                      AIC = as.numeric(aic), BIC = as.numeric(bic), 
                      #VIF = as.numeric(VIF), 
                      Status = 1)
      return(resultados)
      }
    }, error = function(e) {
      print(paste0('erro com o modelo: ', formula_texto))
      
      return(c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 0))
  })
}

    # # Caso for apenas uma var dependente e numérica
    # if (length(vars_independentes_lista) == 1){
    #   var_independente = vars_independentes_lista[1]
    #   # Verificando se ela é numerica
    #   if (data_df[var_independente] %>% class() == 'numeric'){
    #     # Analise Univariada Correlação
    #     pearson_teste = cor.test(x=data_df[[var_independente]], 
    #                              y=data_df[[variavel_dependente]], 
    #                              method = 'pearson')
    #     pearson_estimate = pearson_teste$estimate
    #     pearson_P = pearson_teste$p.value %>% retorne_p()
    #     # Correlação de Spearman
    #     spearman_teste = cor.test(x=data_df[[var_independente]], 
    #                               y=data_df[[variavel_dependente]], 
    #                               method = 'spearman')
    #     spearman_estimate = spearman_teste$estimate
    #     spearman_P = spearman_teste$p.value %>% retorne_p()
    #   }
    # }
    

modelo_lm2 <- lm(score_esc ~ as.numeric(Ano) + Unidade + Regional + Estado, data = df_score)
```

```
## Error in eval(mf, parent.frame()): objeto 'df_score' não encontrado
```

``` r
metricas_de_avaliacao_regressao(modelo_lm2)
```

```
## Error in eval(expr, envir, enclos): objeto 'modelo_lm2' não encontrado
```

``` r
# modelo_lm0 <- lm(response ~ fixed_effects, data = dff)
# metricas_de_avaliacao_regressao(modelo_lm0) %>% round(4)
# 
# modelo_lm <- lm(response ~ fixed_effects + group, data = dff)
# metricas_de_avaliacao_regressao(modelo_lm) %>% round(4)
# 
# modelo <- lmer(response ~ fixed_effects + (1|group), data = dff)
# metricas_de_avaliacao_regressao(modelo) %>% round(4)
```


``` r
# Função de normalização
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# Aplicar a normalização à coluna
normalize(df$coluna)
```

```
## Error in df$coluna: objeto de tipo 'closure' não possível dividir em subconjuntos
```

``` r
scale(df$coluna)
```

```
## Error in df$coluna: objeto de tipo 'closure' não possível dividir em subconjuntos
```

``` r
# Ver o resultado
print(df)
```

```
## function (x, df1, df2, ncp, log = FALSE) 
## {
##     if (missing(ncp)) 
##         .Call(C_df, x, df1, df2, log)
##     else .Call(C_dnf, x, df1, df2, ncp, log)
## }
## <bytecode: 0x0000015468848730>
## <environment: namespace:stats>
```

