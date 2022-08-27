## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(ggsurvfit)
library(ggplot2)
library(patchwork)

## -----------------------------------------------------------------------------
gg_default <-
  survfit2(Surv(time, status) ~ surg, data = df_colon) %>%
  ggsurvfit() +
  add_confidence_interval() +
  labs(title = "Default")

gg_styled <-
  gg_default +
  coord_cartesian(xlim = c(0, 8)) +
  scale_y_continuous(
    limits = c(0, 1),
    labels = scales::percent, 
    expand = c(0.01, 0)
  ) +
  scale_x_continuous(breaks = 0:9, expand = c(0.02, 0)) +
  scale_color_manual(values = c('#54738E', '#82AC7C')) +
  scale_fill_manual(values = c('#54738E', '#82AC7C')) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(ncol = 1)) +
  labs(title = "Modified",
       y = "Percentage Survival")

gg_default + gg_styled

## -----------------------------------------------------------------------------
survfit2(Surv(time, status) ~ surg, data = df_colon) %>%
  ggsurvfit() +
  add_confidence_interval() +
  scale_color_grey() +
  scale_fill_grey() +
  labs(title = "Grey Scale")

## -----------------------------------------------------------------------------
survfit2(Surv(time, status) ~ surg, data = df_colon) %>%
  ggsurvfit(linetype_aes = TRUE) +
  add_confidence_interval() +
  add_risktable(
    risktable_stats = c("n.risk", "cum.censor", "cum.event")
  ) +
  theme_ggsurvfit_KMunicate() +
  scale_y_continuous(limits = c(0, 1)) +
  scale_x_continuous(expand = c(0.02, 0)) +
  theme(legend.position = c(0.85, 0.85))

## -----------------------------------------------------------------------------
survfit2(Surv(time, status) ~ surg, data = df_colon) %>%
  ggsurvfit() +
  add_confidence_interval() +
  add_risktable()

## -----------------------------------------------------------------------------
survfit2(Surv(time, status) ~ surg, data = df_colon) %>%
  ggsurvfit() +
  add_confidence_interval() +
  add_risktable(risktable_group = "risktable_stats") +
  scale_x_continuous(breaks = 0:9) +
  scale_y_continuous(limits = c(0, 1))

## -----------------------------------------------------------------------------
km_long_labels <-
  survfit2(Surv(AVAL, 1 - CNSR) ~ TRT01P, data = adtte) %>%
  ggsurvfit() +
  add_risktable(
    risktable_stats = "n.risk", 
    risktable_group = "risktable_stats",
    risktable_height = 0.2
  ) +
  scale_x_continuous(breaks = 0:5) +
  theme(legend.position = "right")
km_long_labels

## -----------------------------------------------------------------------------
km_long_labels +
  add_risktable_strata_symbol()

## -----------------------------------------------------------------------------
survfit2(Surv(time, status) ~ surg, data = df_colon) %>%
  ggsurvfit(size = 0.8) +
  add_risktable(
    risktable_height = 0.33,
    size = 4, # increase font size of risk table statistics
    theme =   # increase font size of risk table title and y-axis label
      list(
        theme_risktable_default(),
        theme(axis.text.y = element_text(size = 11),
              plot.title = element_text(size = 11, face = "bold"))
      )
  )

## -----------------------------------------------------------------------------
survfit2(Surv(time, status) ~ surg, data = df_colon) %>%
  ggsurvfit(size = 0.8) +
  add_censor_mark(size = 2, alpha = 0.2) +
  add_quantile(y_value = 0.5, linetype = "dotted", color = "grey50", size = 0.8) +
  add_quantile(y_value = 0.75,  color = "grey50", size = 0.8) 

## -----------------------------------------------------------------------------
survfit2(Surv(time, status) ~ surg, data = df_colon) %>%
  ggsurvfit(type = "risk", size = 0.8) +
  add_confidence_interval()

## -----------------------------------------------------------------------------
survfit2(Surv(time, status) ~ surg, data = df_colon) %>%
  ggsurvfit() +
  add_confidence_interval() +
  facet_wrap(~strata, nrow = 1) +
  theme(legend.position = "none") +
  scale_x_continuous(n.breaks = 6) +
  labs(title = "PFS by Duration between Surgery and Treatment")

