#
#
#
#
# # condmora - condição de moradia #
# # If SE is needed
# condmora <- svyby(~condmora, ~zona, domi, svytotal)
# condmora <- clean_srv(condmora)
#
# # If SE is NOT needed (also handles NAs better)
# #domi_pod <- distinct(pod, id_dom, .keep_all = TRUE)
# #condmora <- xtabs(fe_dom ~ zona + condmora, data = domi_pod)
#
# # Número médio de automóveis #
# auto_medio <- svyby(~qt_auto, ~zona, domi, svymean, na.rm = T)
#
# auto_medio <- auto_medio %>%
#   rename(
#     code_zone = zona, point_estimate = qt_auto, confidence_interval = se) %>%
#   mutate(
#     criteria = "avg_automobile",
#     group_percentage = NA,
#     estimate_upper = point_estimate + confidence_interval,
#     estimate_lower = point_estimate - confidence_interval) %>%
#   select(
#     code_zone, criteria, point_estimate, confidence_interval,
#     estimate_upper, estimate_lower, group_percentage
#   )
#
# # Domi x auto #
# domi_auto <- domi %>%
#   as_survey() %>%
#   select(zona, qt_auto) %>%
#   filter(!is.na(qt_auto)) %>%
#   mutate(qt_auto = ifelse(qt_auto > 3, 3, qt_auto),
#          qt_auto = factor(qt_auto))
#
# auto_domi <- svyby(~qt_auto, ~zona, domi_auto, svytotal, na.rm = T)
# auto_domi <- clean_srv(auto_domi)
#
# # qt_banho - prop. sem banheiro #
# domi_banho <- domi %>%
#   as_survey() %>%
#   select(zona, qt_banho) %>%
#   filter(!is.na(qt_banho)) %>%
#   mutate(qt_banho = ifelse(qt_banho > 3, 3, qt_banho),
#          qt_banho = factor(qt_banho))
#
# banh_domi <- svyby(~qt_banho, ~zona, domi_banho, svytotal, na.rm = T)
# banh_domi <- clean_srv(banh_domi)
#
# # criteriobr #
# criterio <- svyby(~criteriobr, ~zona, domi, svytotal)
# criterio <- as.data.frame(criterio)
# criterio <- clean_srv(criterio)
#
# # no_marf -> numero de moradores por familia
# moraf <- svyby(~no_moraf, ~zona, fami, svytotal)
# moraf_medio <- svyby(~as.numeric(no_moraf), ~zona, fami, svymean)
# moraf <- clean_srv(moraf)
#
# # no_morad -> numero de moradores no domicilio
# morad <- svyby(~no_morad, ~zona, domi, svytotal)
# morad_medio <- svyby(~as.numeric(no_morad), ~zona, domi, svymean)
# morad <- clean_srv(morad)
# # agua -> possui agua encanada
# agua <- svyby(~agua, ~zona, domi, svytotal)
# agua <- clean_srv(agua)
# # estuda -> binario
# estuda <- svyby(~d_estuda, ~zona, pess, svytotal)
# estuda <- clean_srv(estuda)
#
# ### Export ####
#
# tables <- list(
#   house_type = condmora,
#   automobile = auto_domi,
#   automobile_avg = auto_medio,
#   house_bathroom = banh_domi,
#   criterio_brasil = criterio,
#   family_size = moraf,
#   household_size = morad,
#   household_water = agua,
#   studies = estuda
# )
