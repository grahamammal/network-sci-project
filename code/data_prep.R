
####################################
# Prepare Senate Vote List
####################################


senate_list <- vote_data %>% 
  filter(chamber == "Senate") %>% 
  group_by(congress) %>% 
  group_split()

get_vote_code_mat <- function(.data) {
  .data %>% 
    select(icpsr, rollnumber, cast_code) %>% 
    pivot_wider(names_from = rollnumber, 
                values_from = cast_code,
                values_fill = -1) %>% 
    arrange(icpsr) %>% 
    select(-icpsr) %>% 
    as.matrix()
}


###################################
# Calculate Adjacency Matrices
###################################

vote_mat_list <- senate_list %>% 
  map(get_vote_code_mat)

adj_mat_list <- vote_mat_list %>% 
  map(edges_for_session)

named_adj_mat <- map2(senate_list, adj_mat_list, function(x,y) {
                                       name <- select(x, icpsr) %>% 
                                         arrange(icpsr) %>% 
                                         pull(icpsr) %>% 
                                         unique()
                                       dimnames(y) <- list(name, name)
                                       
                                       y
                                     })



melted_adj_mats <- map(named_adj_mat, ~{.x[lower.tri(.x)] <- NA
                                        .x %>% 
  melt(na.rm = TRUE) %>% 
  rename(Source = Var1, Target = Var2, Weight = value)})

#####################################################
# Put in different data formats
# with different weights required ot count
#####################################################

final_format <- bind_rows(melted_adj_mats, .id = "Timestamp") %>% 
  mutate(Type = "Undirected") %>% 
  group_by(Timestamp) %>% 
  mutate(Weight = Weight/(max(Weight))) %>% 
  ungroup() %>% 
  relocate(Timestamp, .after = Type)

filtered_weights <- final_format %>% 
  group_by(Timestamp) %>% 
  mutate(Weight = ifelse(Weight > median(Weight), Weight/max(Weight), 0)) %>% 
  ungroup()

parties <- read_csv(here::here("data", "HSall_parties.csv")) %>% 
  select(party_code, party_name) %>% 
  distinct() %>% 
  mutate(party_name = ifelse(party_code == 200, "Republican", ifelse(party_code == 100, "Democrat", "Other")))


#####################################
# Start doing spectral projection
#####################################

svd_list <- map(adj_mat_list, svd)

projected_coords <- map2(svd_list, named_adj_mat,  ~{ mat <- apply(.y, 1, 
                                                      function(row) {
                                                              c(sum(row*.x$u[,2]),
                                                                sum(row*.x$u[,1]))
                                                             })
                                                      rbind(mat, rownames(.y))
                                                    }
  ) %>% 
  map(~as.data.frame(t(.x))) %>% 
  bind_rows(.id = "congress") %>% 
  mutate(congress = as.numeric(congress),
         V1 = as.numeric(V1),
         V2 = as.numeric(V2),
         V3 = as.numeric(V3)) %>% 
  arrange(congress) %>% 
  group_by(congress) %>% 
  mutate(V1 = V1/max(abs(V1)), 
         V2 = V2/max(abs(V2))) %>% 
  ungroup() %>% 
  rename(icpsr = V3)


projected_coords_party <- projected_coords %>% 
  left_join(member_data, by = "icpsr") %>% 
  select(congress.x, 
         V1, 
         V2, 
         icpsr, 
         state_abbrev,
         party_code, bioname) %>% 
  rename(congress = congress.x) %>% 
  left_join(parties, by = "party_code") %>% 
  select(-party_code) %>% 
  distinct()







##################################################
# Start ggraph network vis
##################################################


node_tbl <- member_data %>% 
  filter(chamber %in% c("Senate", "President")) %>% 
  select(congress, icpsr, state_abbrev, party_code, bioname) %>% 
  mutate(icpsr = as.character(icpsr),
         node_id = glue("{icpsr}_{congress}")) %>% 
  left_join(parties, by = "party_code") %>% 
  select(-party_code) %>% 
  distinct() %>% 
  arrange(node_id)

edge_tbl <- final_format %>% 
  mutate(to = as.character(Target),
         from = as.character(Source)) %>% 
  select(-Source, -Target, -Type) %>% 
  rename(weight = Weight, congress = Timestamp) %>% 
  rename(to_orig = to, from_orig = from) %>% 
  mutate(to = glue("{to_orig}_{congress}"),
         from = glue("{from_orig}_{congress}")) %>% 
  left_join(node_tbl, by = c("to" = "node_id")) %>% 
  select(weight:from, party_name) %>% 
  rename(congress = congress.x, party_name_to = party_name) %>% 
  left_join(node_tbl, by = c("from" = "node_id")) %>% 
  select(weight:from, party_name, party_name_to) %>% 
  rename(congress = congress.x, party_name_from = party_name) %>% 
  mutate(edge_party = ifelse(party_name_from == party_name_to, party_name_from, "Cross Party"),
         edge_party = factor(edge_party, levels = c("Democrat", "Republican", "Cross Party", "Other"))) %>% 
  select(-party_name_from, -party_name_to)


layout_coords <- projected_coords %>% 
  mutate(node_id = glue("{icpsr}_{congress}")) %>% 
  select(V1, V2, node_id)

node_tbl <- node_tbl %>% 
  left_join(layout_coords, by = "node_id") %>% 
  rename(x_svd = V1, y_svd = V2) %>% 
  mutate(party_name = factor(party_name, levels = c("Democrat", "Republican", "Other")))

  
network_data_full <- tbl_graph(nodes = node_tbl,
          edges = edge_tbl,
          directed = FALSE,
          node_key = "node_id")


