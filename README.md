# Aplicativo RShiny para Análise de Séries Temporais

Este aplicativo RShiny foi desenvolvido para realizar análises de séries temporais, permitindo a visualização, modelagem e diagnóstico de dados temporais. Ele utiliza diversas bibliotecas do R, como `shiny`, `ggplot2`, `dplyr`, `forecast`, `readxl`, `DT`, `openxlsx`, `yaml` e `plotly`.

## Funcionalidades Principais

### Interface do Usuário (UI)

A interface do usuário é composta por três abas principais:

1. **Visualização dos Dados**:
 - Permite o upload de arquivos CSV ou Excel.
 - Oferece opções para selecionar variáveis, aplicar transformações logarítmicas e escolher o tipo de gráfico (Linha, Histograma ou Box Plot).
 - Exibe gráficos interativos usando `plotly` e tabelas de resumo com `DT`.

2. **Ajuste do Modelo**:
 - Permite a seleção de variáveis para modelagem de séries temporais.
 - Oferece opções para ajustar modelos SARIMA, especificando ordens de AR, MA, e sazonalidade.
 - Exibe previsões e comparações entre valores observados e previstos.

3. **Diagnóstico**:
 - Fornece ferramentas para análise de resíduos, incluindo gráficos de resíduos, Q-Q plots, histogramas e ACF de resíduos.
 - Exibe tabelas de resumo dos resíduos.

### Servidor

O servidor do aplicativo gerencia a lógica de backend, incluindo:

- Carregamento e atualização de dados com base em uploads de arquivos.
- Transformação de dados conforme as seleções do usuário.
- Ajuste de modelos SARIMA e geração de previsões.
- Cálculo de estatísticas de previsão, como MSE e MAE.
- Geração de gráficos interativos e tabelas de dados.

### Estilo e Layout

O aplicativo utiliza CSS personalizado para estilizar a interface, garantindo uma aparência moderna e limpa. As cores predominantes são tons de azul e cinza, com um layout responsivo que se adapta a diferentes tamanhos de tela.

## Como Usar

1. **Carregar Dados**: Faça o upload de um arquivo CSV ou Excel contendo suas séries temporais.
2. **Visualizar Dados**: Selecione a variável de interesse e visualize os dados em diferentes formatos de gráfico.
3. **Ajustar Modelo**: Configure os parâmetros do modelo SARIMA e visualize as previsões.
4. **Diagnóstico**: Analise os resíduos do modelo para avaliar a qualidade do ajuste.

## Requisitos

Para executar este aplicativo, você precisará ter o R e as bibliotecas mencionadas instaladas em seu sistema. Além disso, o arquivo `function_aux.R` e o arquivo de configuração `config.yaml` devem estar presentes no diretório de trabalho.

## Execução

Para iniciar o aplicativo, execute o seguinte comando no R:

```{r}
shinyApp(ui = ui, server = server, enableBookmarking = "url")
```
Este aplicativo é uma ferramenta poderosa para analistas de dados que desejam explorar e modelar séries temporais de forma interativa e visual.
