# Aplicativo RShiny para Análise de Séries Temporais

Este aplicativo RShiny foi desenvolvido para realizar análises de séries temporais, permitindo a visualização, modelagem e diagnóstico de dados temporais. Ele utiliza diversas bibliotecas do R, como `shiny`, `ggplot2`, `dplyr`, `forecast`, `readxl`, `DT`, `openxlsx`, `yaml` e `plotly`.

## Funcionalidades Principais

### 1. **Visualização dos Dados**
- **Upload de Arquivos**: Permite o upload de arquivos CSV ou Excel contendo séries temporais.
- **Seleção de Variáveis**: Oferece opções para selecionar variáveis específicas para análise.
- **Transformações Logarítmicas**: Permite aplicar transformações logarítmicas nas variáveis selecionadas.
- **Gráficos Interativos**: Exibe gráficos interativos de série temporal, histogramas e box plots utilizando o pacote `plotly`.
- **Exibição de Tabelas**: Apresenta tabelas de resumo com o uso do `DT`, incluindo estatísticas descritivas das variáveis.

### 2. **Ajuste de Modelos de Séries Temporais**
- **Seleção de Variáveis para Modelagem**: Escolha variáveis de interesse para modelagem de séries temporais.
- **Modelos SARIMA**: Ajuste de modelos SARIMA, com especificação das ordens de AR (AutoRegressivo), MA (Média Móvel) e sazonalidade.
- **Previsões**: Exibe previsões feitas com o modelo ajustado e a comparação entre valores observados e previstos.

### 3. **Diagnóstico do Modelo**
- **Análise de Resíduos**: Ferramentas para análise de resíduos do modelo, incluindo:
  - Gráficos de resíduos.
  - Q-Q plots.
  - Histogramas de resíduos.
  - ACF (Autocorrelação) dos resíduos.
- **Estatísticas Descritivas**: Tabelas com estatísticas descritivas dos resíduos, ajudando a avaliar a qualidade do ajuste.

## Como Usar

1. **Carregar Dados**: Faça o upload de um arquivo CSV ou Excel com suas séries temporais. A primeira coluna do arquivo deve representar as datas ou o índice temporal.
   
2. **Visualizar Dados**: Após o upload, selecione a variável de interesse para visualização. O aplicativo permitirá que você visualize os dados em diferentes tipos de gráficos (série temporal, histograma, box plot).
   
3. **Ajustar Modelo**: Configure os parâmetros do modelo SARIMA, selecionando as ordens de AR, MA e sazonalidade. O aplicativo irá ajustar o modelo e exibir as previsões, além de permitir comparações entre os valores observados e previstos.

4. **Diagnóstico**: Após ajustar o modelo, utilize as ferramentas de diagnóstico para avaliar a qualidade do modelo, incluindo a análise dos resíduos.

### Banco de Dados Padrão
O banco de dados padrão do aplicativo contém dados mensais de umidade do ar, nebulosidade, precipitação, temperatura máxima, temperatura mínima e temperatura média, coletados a partir de estações meteorológicas em Porto Alegre entre janeiro de 1990 e dezembro de 2015. A primeira coluna do arquivo contém a data da série temporal.

### Configuração do Arquivo YAML
O arquivo `config.yaml` permite personalizar configurações do aplicativo, como o número de linhas a serem exibidas na aba de visualização de dados. Alterações nesse arquivo devem ser feitas conforme necessário para adaptar o comportamento do aplicativo.

## Requisitos

Para executar este aplicativo, você precisará de:

1. R instalado em seu sistema.
2. Bibliotecas necessárias:
   - `shiny`
   - `ggplot2`
   - `dplyr`
   - `forecast`
   - `readxl`
   - `DT`
   - `openxlsx`
   - `yaml`
   - `plotly`

3. O arquivo `function_aux.R` e o arquivo de configuração `config.yaml` devem estar presentes no diretório de trabalho.

## Execução

Para iniciar o aplicativo, execute o seguinte comando no R:


```{r}
shinyApp(ui = ui, server = server, enableBookmarking = "url")
```

Você também pode acessar o aplicativo diretamente através do seguinte link: https://ofzqfb-cau0-pereira0masseu.shinyapps.io/trabalho4_me918/

