# Skyrmion Dynamics: Transporte Topológico e Análise Estatística

Este repositório contém o ecossistema computacional desenvolvido para o estudo da dinâmica de skyrmions magnéticos em redes de potenciais periódicos. O projeto integra simulação física de alto desempenho com um pipeline de análise de dados focado em detecção de padrões e quantificação de estabilidade.

## Metodologia e Produção de Dados
A base do projeto é um simulador desenvolvido em **Fortran 90**, escolhido pela eficiência no processamento de cálculos intensivos. Ele resolve as equações de movimento para o modelo de partícula rígida, considerando as interações entre o skyrmion e potenciais Gaussianos (atrativos e repulsivos).

Diferente de abordagens puramente visuais, o código em Fortran exporta diretamente o **ângulo de Hall** e as coordenadas temporais, gerando um volume massivo de dados brutos que servem de entrada para a etapa de inteligência e pós-processamento.

## Pipeline de Análise Estatística (Python)
O diferencial deste trabalho está na camada de análise. Como os dados brutos possuem flutuações numéricas e comportamentos dinâmicos complexos, o pipeline em Python atua como um minerador estatístico para extrair conclusões físicas robustas.

### Detecção e Mineração de Travamentos (Directional Locking)
O algoritmo varre o amontoado de informações geradas pelo Fortran com foco em três pilares:

*   **Filtragem de Flutuações:** Identificação e isolamento de ruídos estatísticos, garantindo que a análise se concentre no comportamento estacionário real do sistema.
*   **Identificação de Estabilidades:** Através de testes de variância, o código detecta automaticamente os patamares onde o ângulo de Hall permanece constante, caracterizando o fenômeno de travamento direcional.
*   **Quantificação da Estabilidade:** O sistema calcula a "largura" de cada travamento, determinando o intervalo exato de forças de arraste em que aquela trajetória específica se mantém estável.

### Extração de Parâmetros e Correlação
Para cada estado de estabilidade identificado, o pipeline correlaciona automaticamente:
*   **Campos de Força:** Identifica quais intensidades de potenciais atrativos e repulsivos sustentam cada ângulo.
*   **Vetores Angulares:** Define com precisão os ângulos de condução (como $0^\circ, -45^\circ, -50^\circ$, entre outros).
*   **Mapeamento de Robustez:** Consolida milhares de simulações em estruturas de dados organizadas, permitindo a criação de diagramas de fase que evidenciam as janelas operacionais para dispositivos de memória e lógica.

## Objetivo Acadêmico e Profissional
Este fluxo de trabalho foi desenvolvido para transformar trajetórias complexas em dados objetivos e quantificáveis. A abordagem estatística automatizada elimina a necessidade de interpretação visual subjetiva, permitindo uma análise rigorosa do transporte topológico, sendo uma ferramenta essencial para o design de novos dispositivos de spintronics.

### Referência Científica
Este código foi a base para os resultados apresentados no artigo:
**L. Basseto**, N. P. Vizarim, J. C. Bellizotti Souza, and P. A. Venegas. *Skyrmion behavior in attractive-repulsive square array of pinning centers*. **Journal of Physics: Condensed Matter**, 2026 [DOI: 10.1088/1361-648X/ac35f9].

### Execução e Compilação

O núcleo das simulações foi desenvolvido em **Fortran 90** e executado em um servidor dedicado para garantir a escalabilidade dos cálculos.

Para compilar o código fonte, utilize o `gfortran`:

```bash
gfortran SKYRMIONS_FINAL.f90 -o skyrmion_sim
