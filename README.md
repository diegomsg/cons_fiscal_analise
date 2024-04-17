## cons_fiscal_analise

Este repositório é escrito em **R** e tem como objetivo automatizar a análise de balancetes mensais gerados a partir de dados exportados pelo sistema de gestão condominial **Superlógica**. Esses dados frequentemente estão em arquivos XLSX não estruturados, quando não apenas em PDFs como imagens não amigáveis à *machine reading* para automação de qualquer processo de análise de contas.


## Passos para Análise dos Balancetes:

1. **Ler Dados (Read Data)**:
    - O script lê os arquivos XLSX contendo os balancetes mensais.
    - Ele identifica as planilhas relevantes e extrai os dados necessários.

2. **Tabular Dados (Tidy Data)**:
    - Os dados extraídos são organizados em uma estrutura tabular.
    - Colunas são renomeadas e ajustadas para facilitar a análise.

3. **Limpar Dados (Clean Data)**:
    - O script trata valores ausentes, inconsistências e erros nos dados.
    - Remove duplicatas e formata as datas corretamente.

4. **Analisar Dados (Analyze Data)**:
    - São calculados indicadores financeiros, como saldo total, receitas, despesas, etc.
    - O script gera gráficos e visualizações para facilitar a interpretação.

5. **Classificar e Organizar (Classify and Organize)**:
    - Os dados são categorizados por tipo de transação (receita, despesa, investimento, etc.).
    - Os balancetes são organizados por período (mês, trimestre, ano).

6. **Comunicar (Communicate Findings)**:
    - O resultado da análise é apresentado em relatório *html* com diversas informações relevantes.
    - Os membros do conselho fiscal e consultivo, bem como os demais moradores, podem entender facilmente as informações financeiras do condomínio.


## Dados sensíveis

Para armazenar os dados sensíveis do empreendimento, síndicos, equipe gestora, caminho dos arquivos de dados, formato dos nomes dos arquivos, unidades, blocos, e demais informações necessáriasé utilizado um arquivo de configuração (`config.yml`) para armazená-los. Esse arquivo não deve ser divulgado publicamente, mas é necessário para a execução do script.

Além desse `config`, há ainda outra configuração, disponível, para uniformização das categorias de despesas, `src/despesas_categorias.csv`.


### config/config.yml

```yml
default:
  empreendimento:
    nome: 'Nome usual'
    nome_curto: 'Nome curto'
    nome_completo: 'Nome completo formal'
    endereco: 'logradouro'
    num: 1
    cep: 49000000
    bairro: 'Bairro'
    cidade: 'Cidade'
    uf: 'UF'
    cnpj: 'cnpj_só_núm'
  
  unidades:
    blocos: 2
    blocos_nomes: 'A, B'
    apart_andar: 4
    andares_tipo: 12
  
  path:
    prest_contas: 'C:/Users/diego/OneDrive/Documentos/casa/condomínio/Conselho Fiscal/prest_contas_xlsx'
    acordos: 'C:/Users/diego/OneDrive/Documentos/casa/condomínio/Conselho Fiscal/acordos_xlsx'

  conformidade:
    data_inicio: '2022-12-01'
    
    gestores:
    data_eleicao: '2022-10-08'
    principal:
      funcao: 'síndico'
      nome: 'nome'
      nome_completo: 'nome completo'
      cpf: 'cpf_num'
      rg: rg
      id: id_unidade
      paga: 0
    secundario:
      funcao: 'subsíndico'
      nome: 'nome'
      nome_completo: 'nome completo'
      cpf: 'cpf_num'
      rg: rg
      id: if_unidade
      paga: 0
```

## Outros usos

Provavelmente outras administradoras que utilizam sistemas Superlógica tenha customizações que impeçam o reaproveitamento direto deste código, mas já servirá de ponto de partida para usos semelhantes.
