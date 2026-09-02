library(data.table)
library(tidyverse)

################################
#'## Read data
################################
name_output <- 'intermediate_files/cust_all_allactividades_fno_2013_2017.csv'
# from https://www.bcb.gov.br/estabilidadefinanceira/micrrural

cust <- read_csv('input/ABC/CusteioInvestimentoComercialIndustrialSemFiltros.csv',col_types = list(.default = col_character()))
cust$VlComercializacao <-as.numeric(gsub(",",".",cust$VlComercializacao))
cust$VlCusteio <-as.numeric(gsub(",",".",cust$VlCusteio))
cust$VlInvestimento <-as.numeric(gsub(",",".",cust$VlInvestimento))
cust$VlIndustrializacao <-as.numeric(gsub(",",".",cust$VlIndustrializacao))

cust$QtdComercializacao <-as.numeric(cust$QtdComercializacao)
cust$QtdCusteio <-as.numeric(cust$QtdCusteio)
cust$QtdInvestimento <-as.numeric(cust$QtdInvestimento)
cust$QtdIndustrializacao <-as.numeric(cust$QtdIndustrializacao)
cust$AreaCusteio <-as.numeric(cust$AreaCusteio)
cust$AreaInvestimento <-as.numeric(cust$AreaInvestimento)


cust <- cust%>% filter(AnoEmissao %in% c('2013','2014','2015', '2016','2017')) %>% filter(nomeUF %in% c('AC','AM',"RO",'PA')) %>%
  mutate(cdPrograma=as.character(as.numeric(cdPrograma)),cdSubPrograma=as.character(as.numeric(cdSubPrograma)) )
# 180 is the code for the fno program
cust_fno <- cust[cust$cdPrograma=='180',]
sum(cust_fno$VlInvestimento)
# 154 is the code for the ABC+ program
cust_abcp <- cust[cust$cdPrograma=='154',]


# Atividade == '2' is livestock 
sum_cust <- cust %>% filter(Atividade == '2')  %>% 
  group_by(cdPrograma) %>%
  summarize(VlCusteio = sum(VlCusteio),
            VlInvestimento = sum(VlInvestimento),
            VlIndustrializacao = sum(VlIndustrializacao))

ggplot(data = sum_cust, aes(x = cdPrograma, y = VlInvestimento)) +
  geom_bar(stat = "identity")

cust_all <- cust %>%
  #filter(Atividade == '2') %>%
  group_by(codMunicIbge) %>%
  reframe(
    credits_val_2013_2017 = sum(VlInvestimento)+sum(VlCusteio),#+(VlIndustrializacao)+ sum(cust$VlComercializacao),
    credits_num_2013_2017 = sum(QtdInvestimento),#+sum(QtdCusteio)+sum(QtdComercializacao)+ sum(QtdIndustrializacao),
    #credits_area_2013_2017 = sum(na.omit(AreaInvestimento)),#+sum(na.omit(AreaCusteio)),
    # inv_all = sum(VlInvestimento), 
    # cust_all = sum(VlCusteio),
    # pronaf_inv_all = sum(VlInvestimento[cdPrograma=='1']),
    # pronaf_cust_all = sum(VlCusteio[cdPrograma=='1']),
    #abc_inv_all_2013_2017 = sum(VlInvestimento[cdPrograma=='156']),
    abc_inv_all_2013_2017 = sum(VlInvestimento[cdPrograma%in%c('156','180')])+ sum(VlCusteio[cdPrograma%in%c('156','180')]),
    #abc_num_all_2013_2017 = sum(QtdInvestimento[cdPrograma=='156']),
    abc_num_all_2013_2017 = sum(QtdInvestimento[cdPrograma%in%c('156','180')]) + sum(QtdCusteio[cdPrograma%in%c('156','180')])
    #abc_area_2013_2017 = sum(na.omit(AreaInvestimento[cdProgram%in%c('156','180')])),
    # abc_recuperacao = sum(VlInvestimento[cdPrograma=='156' & cdSubPrograma=='32']),
    # abc_plantiodireto = sum(VlInvestimento[cdPrograma=='156' & cdSubPrograma=='34']),
    # abc_integracao = sum(VlInvestimento[cdPrograma=='156' & cdSubPrograma=='35'])
  ) %>%
  dplyr::rename(IBGE_CODE = codMunicIbge)%>% 
  ungroup()%>%
  mutate(IBGE_CODE = as.character(IBGE_CODE)) %>% distinct()

#cust_all%>% group_by(IBGE_CODE)%>%summarize(sum(.))

sum(cust_all$abc_inv_all_2013_2017)
hist(cust_all$credits_val_2013_2017)
length(unique(cust_all$IBGE_CODE))

write_csv(cust_all, name_output)

