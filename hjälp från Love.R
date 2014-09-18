

# ============================================================================ #
# Kommunerna SD där är störst
# ============================================================================ #
require(tidyr)
require(dplyr)
require(stringr)

localParties <- c()
for (col in names(kDat)) {
  if (grepl("KV_ROSTER", col) && !grepl("_ANDRING", col)) {
    localParties <- c(localParties, col) 
  }
}



partyResults <- kDat %>%
  tbl_df %>%
  select(name, contains("KV_ROSTER")) %>%
  gather(key = "cat", value = "votes", KV_ROSTER_V:KV_ROSTER_SP) %>%
  group_by(name) %>%
  arrange(name, desc(votes)) %>%
  mutate(ranking = rank(desc(votes), ties.method = "first")) %>%
  select(-votes) %>%
  spread(key = "cat", value = "ranking")

colnames(partyResults) <- c("name", str_replace_all(localParties, "ROSTER", "RANK"))
inner_join(kDat, partyResults, by = "name") %>% tbl_df

partyResults <- kDat %>%
  tbl_df %>%
  select(name, starts_with("ROSTER_")) %>% 
  gather(key = "cat", value = "votes", ROSTER_M:ROSTER_FI) %>%
  group_by(name) %>%
  arrange(name, desc(votes)) %>% 
  mutate(ranking = rank(desc(votes), ties.method = "first")) %>% 
  select(-votes) %>% 
  spread(key = "cat", value = "ranking")

colnames(partyResults) <- c("name", str_replace_all(localParties, "ROSTER", "RANK"))
inner_join(kDat, partyResults, by = "name") %>% tbl_df

cor(kDat$valdeltagande,kDat$PROCENT_SD)
cor(kDat$valdeltagande,kDat$PROCENT_S)