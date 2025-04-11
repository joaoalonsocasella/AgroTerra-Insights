# 🛰️ AgroTerra Insights – Coleta Inteligente de Dados Imobiliários Rurais

## Visão Geral

**AgroTerra Insights** é um projeto de **inteligência de mercado voltado à compra estratégica de terrenos agrícolas no Centro-Oeste brasileiro** (Goiás, Mato Grosso, Mato Grosso do Sul e Distrito Federal). Utiliza **webscraping automatizado** para coletar dados de imóveis rurais anunciados em portais especializados, permitindo **análises comparativas**, **avaliações de valor de mercado** e cruzamentos com dados agronômicos, climáticos e logísticos.


## Objetivo

Desenvolver um sistema confiável e expansível que:

- Coleta periodicamente informações atualizadas de imóveis agrícolas;
- Armazena e padroniza os dados brutos em uma base tratada;
- Permite cruzamento com indicadores de solo, clima, logística e preço de commodities;
- Apoia decisões de compra de propriedades com alto potencial produtivo e bom custo-benefício.



## Escopo Geográfico

Foco inicial:

- Goiás
- Mato Grosso
- Mato Grosso do Sul
- Distrito Federal

Com corte por municípios com forte atuação agrícola, logística desenvolvida ou expansão recente de fronteira agrícola.



## Tecnologias Utilizadas

- **Linguagem:** Python
- **Webscraping:** `requests`, `BeautifulSoup`, `Selenium`
- **Armazenamento:** PostgreSQL (estrutura relacional e espacial com PostGIS)
- **ETL & Pipeline:** `pandas`, `sqlalchemy`, `luigi` ou `airflow`
- **Georreferenciamento & Mapas:** `geopandas`, `shapely`, `folium`
- **APIs Externas:**
  - EMBRAPA (solo)
  - INMET (clima histórico)
  - IBGE (malha e dados regionais)
  - SIGEF/SNCR (georreferenciamento e regularização fundiária)
  - MapBiomas (uso da terra)
- **Painel Interativo (futuro):** `streamlit` ou `dash`


## Fontes de Dados Coletadas

| Fonte                        | Tipo de Dado                                 | Observações                                                               |
|-----------------------------|----------------------------------------------|--------------------------------------------------------------------------|
| Portais Imobiliários        | Preço, área, localização, cultura, fotos      | Padronização via NLP devido à estrutura variável                         |
| SIGEF e SNCR                | Regularização fundiária, georreferenciamento | Avaliação de risco jurídico e formal da terra                            |
| EMBRAPA Solos               | Aptidão agrícola, textura, fertilidade        | Cruzamento via coordenadas geográficas                                   |
| INMET e NASA POWER          | Clima histórico (chuva, temperatura)          | Avaliação de risco climático                                             |
| MapBiomas                   | Uso da terra, desmatamento                    | Detecção de áreas degradadas ou recém-desmatadas                         |
| IBGE                        | Infraestrutura, densidade agrícola            | Dados municipais para análise de produtividade e contexto regional       |
| OpenStreetMap (OSM)         | Infraestrutura viária                         | Distância a rodovias, ferrovias e centros de escoamento                  |



## Indicadores Relevantes para Decisão

- Preço por hectare vs. média regional
- Aptidão agrícola vs. tipo de cultura anunciada
- Distância logística até centros de escoamento
- Histórico de uso da terra (preservação ou degradação)
- Status fundiário (georreferenciado, regularizado ou não)
- Clima histórico (precipitação e temperatura médias, risco climático)
- Possibilidades de valorização com base em infraestrutura futura (ex: PAC, Ferrogrão)

---

## Organização do Projeto

agroterra-insights/ │ ├── scraping/ │ ├── portals/ # Scripts por portal de anúncios │ ├── utils/ # Funções auxiliares (tratamento, NLP, etc.) │ └── scheduler.py # Rotina de scraping │ ├── data/ │ ├── raw/ # Dados brutos coletados │ ├── processed/ # Dados limpos e padronizados │ └── external/ # Downloads de APIs externas (solo, clima, etc.) │ ├── analysis/ │ ├── matching_engine.py # Script de cruzamento entre variáveis │ └── indicators.ipynb # Geração de dashboards e gráficos │ ├── database/ │ ├── schema.sql # Tabelas e relacionamentos │ └── connection.py # Conexão com PostgreSQL │ └── README.md
