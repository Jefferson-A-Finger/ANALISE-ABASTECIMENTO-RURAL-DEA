# =====================================================================================
# SCRIPT DA SEGUNDA ANÁLISE (MODELO B, OPERACIONAL)
# DEA-BCC orientado ao produto — sistemas rurais de Marechal Cândido Rondon – PR
#
# Insumo:   X1 = consumo anual de energia elétrica (kWh/ano; soma das 12 contas de luz)
# Produtos: Y1 = volume anual medido no hidrômetro (m³/ano)   Y2 = famílias atendidas
#
# Entrada: Planilha_Estatistica_DEA_41.xlsx, aba "Modelo_B". Partida: as 41 linhas; entram
# as que têm os três valores preenchidos e positivos, e as demais são listadas com o motivo. Não se estimam
# valores deste modelo: ele roda apenas com dados de campo.
# =====================================================================================
 
library(deaR); library(readxl); library(writexl)
setwd(dirname(file.choose()))
 
b <- as.data.frame(read_excel("Planilha_Estatistica_DEA_41.xlsx", sheet = "Modelo_B", n_max = 41))   # 41 linhas; abaixo há notas
completa <- with(b, !is.na(X1_energia_kWh_ano) & !is.na(Y1_volume_medido_m3_ano) &
                   !is.na(Y2_familias) & X1_energia_kWh_ano > 0 &
                   Y1_volume_medido_m3_ano > 0 & Y2_familias > 0)
completa[is.na(completa)] <- FALSE
cat("Partida: 41 linhas | completas:", sum(completa), "| excluídas:", sum(!completa), "\n")
excl <- b[!completa, c(1, 2)]
excl$motivo <- ifelse(!is.na(b$Motivo_exclusao_B[!completa]) & b$Motivo_exclusao_B[!completa] != "",
                      b$Motivo_exclusao_B[!completa], "Faltam contas de luz e/ou de água (12 meses)")
print(excl, row.names = FALSE)                 # lista declarada das exclusões e do motivo
write_xlsx(excl, "Exclusoes_Modelo_B.xlsx")
b <- b[completa, ]
n <- nrow(b); cat("linhas com dados completos:", n, "\n")
if (n < 9) stop("Modelo B exige ao menos 9 DMUs (n >= 3(m+s) = 9). Aguardar mais dados de campo.")
 
dados <- data.frame(DMU = b[[2]], X1_energia = b$X1_energia_kWh_ano,
                    Y1_volume = b$Y1_volume_medido_m3_ano, Y2_familias = b$Y2_familias)
dea <- make_deadata(dados, dmus = 1, inputs = 2, outputs = 3:4)
 
res <- model_basic(dea, dmu_ref = 1:n, dmu_eval = 1:n, orientation = "oo", rts = "vrs")
escore <- round(1 / efficiencies(res), 6)
res_crs <- model_basic(dea, dmu_ref = 1:n, dmu_eval = 1:n, orientation = "oo", rts = "crs")
ccr <- round(1 / efficiencies(res_crs), 6)
 
saida <- data.frame(N = b[[1]], DMU = dados$DMU, escore = escore,
                    situacao = ifelse(escore >= 0.999999, "Eficiente", "Ineficiente"),
                    ccr = ccr, escala = round(ccr / escore, 6))
saida <- saida[order(-saida$escore), ]
print(saida, row.names = FALSE)
cat("eficientes:", sum(escore >= 0.999999), "| média:", round(mean(escore), 4),
    "| mediana:", round(median(escore), 4), "| mínimo:", round(min(escore), 4), "\n")
 
references(res); targets(res)
write_xlsx(saida, "Resultados_Modelo_B_R.xlsx")
