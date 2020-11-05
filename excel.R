source('excel.R')
# Read data in from excel files =========

# Change to YOUR path to Dropbox
root_dir_ <- "/Users/zitaaretz/Dropbox/ms/"

# Leave this alone
data_dir_ <- paste0(root_dir_,"data/") 
results_dir_ <-  paste0(root_dir_,"results/")
analysis_dir_ <- paste0(root_dir_,"analysis/") 

fl <- list.files(data_dir_)
fl <- fl[str_detect(fl,".xlsx")]
fl <- paste0(data_dir_,fl)


r_sheet <-  function(files_path){
  files_path  %>% purrr::set_names()  %>% map_df(read_excel, path = files_path)
}

Spectra <- read_excel(fl, sheet = 2)
df <- Spectra %>% dplyr::select(contains("ProteinMetrics"), contains("Log"), "Protein Name", contains("DB"))
  
df <- df %>% mutate(df, str_replace(df[,3], " \\[.*\\]","" ) )


df <-  fl %>%  map( ~ r_sheet()) %>%  bind_rows()

df %>%  distinct(sheet) %>% kable() %>% kable_styling(bootstrap_options = "striped", full_width = F)

#vvv(df)

df <- df %>% dplyr::select(sheet =1)
                           #time = 2,c(ends_with("Spectra"))) %>% gather("mol","abs", c(ends_with("M")))  %>%  
  mutate(mol = as.double(str_replace(mol, "[:alpha:]","")))

df <- df %>%  mutate(polymer = str_extract(sheet, "^[:alnum:]+"), condition = str_extract(sheet, "(?<=pH\\d\\s)[:alnum:]+"), ph = str_extract(sheet, "(?<=pH)\\d"), treatment = str_extract(sheet, "\\w{1,2}(?=\\s([[:digit:]]|[[:punct:]]){1,4}mg)"), conc = str_extract(sheet, "([[:digit:]]|[[:punct:]]){1,4}(?=mg)"), run = str_extract(sheet, "(?<=\\()\\d"))
df$conc <- as.numeric(df$conc)
#%>%  select(spolymer, condition, mol, ph, treatment, run, time, abs)




df <- df %>% mutate(P = as.double(str_extract(polymer, "(?<=P)\\d+")), Q = as.double(str_extract(polymer, "(?<=Q)\\d+")))



### Summary table of number of mol levles per run


ovw_mol <-df %>% filter(!is.na(abs)) %>%  dplyr::select(-sheet, -abs, -time, -P, -Q) %>% group_by(polymer, ph, treatment, run)  %>% mutate(mol_levels = n_distinct(mol)) %>% distinct(polymer, ph, treatment, run, mol_levels)



ovw_mol  %>%  kable() %>%
  kable_styling(bootstrap_options = "striped", full_width = F)  


