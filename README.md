# Eficiência dos sistemas rurais de abastecimento de água de Marechal Cândido Rondon – PR (DEA)

Análise estatística e scripts em R do Trabalho de Conclusão de Curso de Jefferson Antônio Finger (Engenharia Civil, UTFPR – Câmpus Toledo, 2026).

| Arquivo | Conteúdo |
|---|---|
| 01_modelo_A.R | Modelo A (estrutural), robustez e supereficiência |
| 02_modelo_B.R | Modelo B (operacional) |
| 03_cruzamento_segunda_etapa.R | Cruzamento A × B e segunda etapa (Spearman) |

Pacotes: deaR, readxl, writexl (e lpSolve para a figura da fronteira).
Entrada: a planilha `Planilha_Estatistica_DEA_41.xlsx` (abas `Matriz_A_41`, `Modelo_B` e `Segunda_etapa`), que deve ser colocada na mesma pasta dos scripts.

Observações sobre a planilha:
- A Linha Três Voltas permanece na aba `Matriz_A_41` apenas como registro; o script do Modelo A a retira (rede e ligações cadastradas com a Linha Maracanã), de modo que o modelo roda com 40 linhas.
- Na coluna de fonte de captação, a Linha Ludwig deve constar como "Poço" (poço tubular de 88 m), para que entre na amostra homogênea de 36 linhas.

Como rodar: instale os pacotes (`install.packages(c("deaR", "readxl", "writexl", "lpSolve"))`), coloque a planilha na mesma pasta e rode os scripts na ordem 01 → 02 → 03. Cada script abre uma janela para escolher a planilha e grava os resultados (`.xlsx` e figuras) nessa mesma pasta.

Valores de conferência do Modelo A (BCC orientado ao produto, sem a Linha Três Voltas): 39 linhas com escore, 9 eficientes, média 0,7834 e mediana 0,8382.

O Modelo B (custo anual de energia elétrica × volume de água tratada) depende do levantamento de campo e será atualizado quando os dados estiverem completos.
