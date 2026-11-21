# =============================================================================
# Préparation des séries pour la prévision
# Entrée  : TimeSeries.csv (NAME, BEGIN, END, KWH_PERIOD)
# Sortie  : serie_corrigee.csv — une ligne par site et par mois, avec :
#   KWH_PER_DAY_CLEAN : consommation journalière corrigée (à utiliser pour la prévision)
#   FLAG              : correction appliquée (vide si aucune)
#   TRAIN             : TRUE si le mois fait partie de la période d'apprentissage
#
# Utilisation pour la prévision :
#   prev <- read.csv("serie_corrigee.csv") %>% filter(TRAIN)
#   -> prévoir KWH_PER_DAY_CLEAN, puis multiplier par 31 pour décembre 2021
# =============================================================================

library(dplyr)
library(lubridate)
library(readr)

# ---- 1. Lecture -------------------------------------------------------------
df <- read_csv("TimeSeries.csv", col_types = cols(.default = "c")) %>%
  mutate(BEGIN      = dmy(BEGIN),
         END        = dmy(END),
         KWH_PERIOD = as.numeric(gsub("\\s", "", KWH_PERIOD)),  # "2 500" -> 2500
         MONTH      = floor_date(BEGIN, "month")) %>%
  arrange(NAME, MONTH)

# ---- 2. Correction de date (section 3.1) ------------------------------------
# 0628BR, sept. 2021 : date de début saisie au 15/09 au lieu du 01/09 (volume inchangé)
df <- df %>%
  mutate(FIX_DATE    = NAME == "0628BR" & MONTH == as.Date("2021-09-01"),
         BEGIN       = if_else(FIX_DATE, as.Date("2021-09-01"), BEGIN),
         DAYS        = as.numeric(END - BEGIN) + 1,
         KWH_PER_DAY = KWH_PERIOD / DAYS)

# ---- 3. Valeurs remplacées (sections 3.3 et 3.4 du rapport) ----------------
# Factures remplacées par la médiane des 3 mois avant / 3 mois après :
#   - 0089KO, juin et août 2019 : sous-facturation puis régularisation
#   - 0690BR, 0274BR, 2106BB : écart compensé un ou deux mois plus tard (relevé décalé)
# Cette liste doit rester identique à celle du rapport (le rapport vérifie la cohérence).
replace_list <- tibble(
  NAME  = c("0089KO", "0089KO", "0690BR", "0690BR", "0274BR", "0274BR", "2106BB", "2106BB"),
  MONTH = as.Date(c("2019-06-01", "2019-08-01", "2021-03-01", "2021-04-01",
                    "2021-06-01", "2021-08-01", "2021-07-01", "2021-09-01")),
  REPLACE = TRUE)

local_median <- function(x, k = 3) {
  n <- length(x)
  sapply(seq_len(n), function(i) {
    idx <- setdiff(max(1, i - k):min(n, i + k), i)
    median(x[idx], na.rm = TRUE)
  })
}

df <- df %>%
  left_join(replace_list, by = c("NAME", "MONTH")) %>%
  mutate(REPLACE = coalesce(REPLACE, FALSE)) %>%
  group_by(NAME) %>%
  # les mois à remplacer sont exclus du calcul de la médiane de leurs voisins
  mutate(LOC_MED = local_median(if_else(REPLACE, NA_real_, KWH_PER_DAY))) %>%
  ungroup() %>%
  mutate(KWH_PER_DAY_CLEAN = if_else(REPLACE, LOC_MED, KWH_PER_DAY))

# ---- 4. Changements de niveau -> début de la période d'apprentissage --------
# Un mois t marque un changement de niveau si :
#   - la médiane des mois t..t+2 s'écarte de plus de 20 % de celle des mois t-3..t-1,
#     avec un écart d'au moins 100 kWh par mois ;
#   - le changement est durable : 12 mois plus tard (ou en fin de série), le niveau
#     reste à plus de 20 % de l'ancien, dans le même sens ;
#   - il n'est pas saisonnier : le même saut ne se répète pas un an avant ou après.
# L'apprentissage commence au dernier changement de niveau détecté.

# Début de l'apprentissage : premier mois de la dernière série de détections
# dont la valeur est déjà plus proche du nouveau niveau que de l'ancien
train_start_idx <- function(v, lc) {
  idx <- which(lc)
  if (length(idx) == 0) return(1)
  run <- idx[length(idx)]
  while ((run[1] - 1) %in% idx) run <- c(run[1] - 1, run)
  pre  <- median(v[max(1, run[1] - 3):(run[1] - 1)])
  post <- median(v[run[length(run)]:min(length(v), run[length(run)] + 2)])
  first_new <- run[abs(v[run] - post) < abs(v[run] - pre)][1]
  if (is.na(first_new)) max(run) else first_new
}

df <- df %>%
  group_by(NAME) %>%
  mutate(LEVEL_CHANGE = level_change(KWH_PER_DAY_CLEAN),
         TRAIN_START  = MONTH[train_start_idx(KWH_PER_DAY_CLEAN, LEVEL_CHANGE)],
         # 0254BB : toute l'année 2019 est jugée non fiable (section 3.3)
         TRAIN_START  = if (first(NAME) == "0254BB") as.Date("2020-01-01") else TRAIN_START,
         TRAIN        = MONTH >= TRAIN_START) %>%
  ungroup()

# ---- 5. Indicateurs ---------------------------------------------------------
# 0007BR, 1006BB et 1143BR : dernière facture (nov. 2021) suspecte, non confirmable
# -> valeur conservée, prévision à présenter avec une incertitude plus large
to_confirm <- c("0007BR", "1006BB", "1143BR")

serie_corrigee <- df %>%
  mutate(FLAG = case_when(
    FIX_DATE                                      ~ "Date corrigée",
    REPLACE                                       ~ "Valeur remplacée",
    NAME %in% to_confirm & MONTH == max(MONTH)    ~ "À confirmer",
    TRUE                                          ~ "")) %>%
  select(NAME, MONTH, DAYS, KWH_PERIOD, KWH_PER_DAY, KWH_PER_DAY_CLEAN,
         FLAG, TRAIN_START, TRAIN)

write_csv(serie_corrigee, "serie_corrigee.csv")

# ---- 6. Contrôles rapides ---------------------------------------------------
cat("Sites :", n_distinct(serie_corrigee$NAME), "\n")
cat("Valeurs remplacées :", sum(serie_corrigee$FLAG == "Valeur remplacée"), "\n")
cat("Sites avec un changement de niveau :", sum(tapply(!serie_corrigee$TRAIN, serie_corrigee$NAME, any)), "\n")
print(serie_corrigee %>% group_by(NAME) %>% summarise(TRAIN_START = first(TRAIN_START), MOIS = sum(TRAIN)) %>%
        filter(MOIS < 35) %>% arrange(MOIS), n = Inf)