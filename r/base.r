suppressPackageStartupMessages({
    library(tidyverse)
    library(lme4)
    library(jtools)
    library(magrittr)
    library(purrr)
    # library(rmdformats)
    library(patchwork)
    library(jsonlite)
    # library(tidyjson)
    library(ggbeeswarm)
    library(stickylabeller)
    # library(ggeffects)
    library(rlang)
    # library(knitr)
    # library(ggside)
    library(broom.mixed)
    library(lmerTest)
    # library(optigrab)
    library(formula.tools)
    library(colorspace)
    # library(tune)
    library(infer)
    # library(ggrastr)
    # library(kableExtra)
})

MAKE_PDF <- TRUE


# %% ==================== Options ====================

RED <- "#E41A1C"
BLUE <- "#377EB8"
GREEN <- "#4DAF4A"
PURPLE <- "#984EA3"
ORANGE <- "#FF7F00"
YELLOW <- "#E9DB12"
BEIGE <- "#E9DDA1"
BLACK <- "#191919"
GRAY <- "#A8A8A8"


C_COMP <- "#E86623"
C_BESPOKE <- "#07A9C0"

cpal <- scale_colour_manual(values = c(
    bespoke = C_BESPOKE,
    compositional = C_COMP,
    `-1` = C_BESPOKE,
    `1` = C_COMP
), aesthetics = c("fill", "colour"), name = "")


default_palettes <- list(
    c(BLUE),
    c(ORANGE, BLUE),
    RColorBrewer::brewer.pal(6, "Set1")
)


facet_grid = function(form, labeller=purrr::partial(label_both, sep = " = "), ...) {
    ggplot2::facet_grid(form, labeller=labeller, ...)
}

facet_wrap = function(form, labeller=purrr::partial(label_both, sep = " = "), ...) {
    ggplot2::facet_wrap(form, labeller=labeller, ...)
}




yesno_pal <- scale_colour_manual(values = c(
    `FALSE` = "gray50",
    `TRUE` = BLUE
), labels = c("No", "Yes"))

teals_pal <- function(...) {
    scale_color_discrete_sequential(palette = "TealGrn", l1 = 40, l2 = 80, c2 = 40, aesthetics = c("color", "fill"), ...)
}


zissou_pal <- scale_fill_discrete_divergingx(
    palette = "Zissou 1", aesthetics = c("color", "fill"), rev = T, guide = guide_legend(reverse = TRUE)
)

options(
    "summ-model.info" = FALSE,
    "summ-model.fit" = FALSE,
    "summ-re.table" = FALSE,
    "summ-groups.table" = FALSE,
    "jtools-digits" = 3,
    "max.print" = 100,
    "ggplot2.discrete.colour" = default_palettes,
    "ggplot2.discrete.fill" = default_palettes,
    width = 100
)

kable <- knitr::kable
glue <- glue::glue
extract <- tidyr::extract


# %% ==================== Miscellany ====================

quantize <- function(x, q) q * round(x / q)

str_clip <- function(x, hi) {
    s_hi <- paste0(hi, "+")
    if_else(x >= hi, s_hi, as.character(x))
}

clip <- function(x, lo, hi) {
    pmin(pmax(x, lo), hi)
}
qclip <- function(x, q1, q2=1-q1) {
    clip(x, quantile(x, q1), quantile(x, q2))
}
zclip <- function(x, z1, z2) {
    if (missing(z2)) {
        stopifnot(z1 >= 0)
        z2 = z1
        z1 = -z1
    }
    lo <- mean(x) + z1*sd(x)
    hi <- mean(x) + z2*sd(x)
    clip(x, lo, hi)
}

numerize <- function(data, var) mutate(data, "{{var}}" := as.numeric({{ var }}))
orderize <- function(data, x, y, .fun = mean) mutate(data, "{{x}}" := fct_reorder({{ x }}, {{ y }}, .fun))
fctrize <- function(data, var, ...) mutate(data, "{{var}}" := factor({{ var }}, ...))
labelize <- function(data, var, t = as_string(ensym(var)), f = paste0("not ", t)) {
    mutate(data, "{{var}}" := if_else({{ var }}, t, f))
}

json_to_columns <- function(df, column) {
    json_df <- df %>%
        pull({{ column }}) %>%
        spread_all() %>%
        as_tibble() %>%
        select(-document.id)
    df %>%
        select(-{{ column }}) %>%
        bind_cols(json_df)
}

ensure_column <- function(data, col) {
    add <- col[!col %in% names(data)]
    if (length(add) != 0) data[add] <- NA
    data
}

zscore <- function(x) as.vector(scale(x))

n_pct <- function(x) {
    glue("{sum(x)} ({round(100*mean(x))}\\%)")
}

quibble <- function(x, q = c(0.25, 0.5, 0.75)) {
    tibble(x = quantile(x, q), q = q)
}

midbins <- function(x, breaks) {
    bin_ids <- cut(x, breaks, labels = FALSE)
    left <- breaks[-length(breaks)]
    right <- breaks[-1]
    ((left + right) / 2)[bin_ids]
}

maybe <- function(sym, .default = NULL) {
    env <- parent.frame()
    if (exists(deparse(substitute(sym)), env)) {
        eval(sym, env)
    } else {
        .default
    }
}


# e.g. read_csvs("{version}-{condition:[1-3]}", base_dir="data")
DATA_DIR <- maybe(DATA_DIR, ".")
read_csvs <- function(spec, base_dir = DATA_DIR) {
    # Quickly find candidate matches with a simple glob
    if (!dir.exists(base_dir)) {
        stop("Directory does not exist: ", base_dir)
    }

    glob_pattern <- spec %>% str_replace_all("\\{[^{}]+\\}", "*")
    all_files <- Sys.glob(file.path(base_dir, glob_pattern))
    if (length(all_files) == 0) {
        stop("No files found for pattern ", glob_pattern, " in directory ", base_dir)
    }

    # Pull out the slot names
    slot_names <- str_extract_all(spec, "\\{(\\w+)(?::[^}]+)?\\}")[[1]] %>%
        str_remove_all("[\\{\\}]") %>%
        str_extract("^\\w+")

    # Make tibble of filnames and extracted slot values
    regex_pattern <- spec %>%
        str_replace_all("\\{(\\w+):([^}]+)\\}", "(\\2)") %>% # {name:\\d+}
        str_replace_all("\\{(\\w+)\\}", "([^/]+)") # {name}
    
    annotated_files <- tibble(data_file = all_files) %>%
        extract(data_file, into = slot_names, regex = regex_pattern, remove = F) %>%
        drop_na()

    # Read the data!
    annotated_files %>%
        rowwise() %>%
        reframe(
            pick(everything()),
            read_csv(data_file, lazy = TRUE, show_col_types = FALSE, progress = FALSE)
        )
}

# %% ==================== Stats ====================

coeftable <- function(model, digits = 3, intercept = FALSE) {
    summ(model)$coeftable %>%
        as_tibble(rownames = "Term") %>%
        filter(intercept | (Term != "(Intercept)")) %>%
        rowwise() %>%
        mutate(p = pval(p)) %>%
        kable(digits = digits)
}

drop_extreme <- function(data, ..., q_lo = 0, q_hi = 0.95) {
    data %>%
        drop_na(...) %>%
        filter(if_all(c(...), ~
            between(.x, quantile(.x, q_lo), quantile(.x, q_hi))))
}

smart_print <- function(x, ...) {
    if (isTRUE(getOption("knitr.in.progress"))) {
        cat(knit_print(x, ...))
    } else {
        print(x, ...)
    }
}

is.binary <- function(x) {
    all(na.omit(x) %in% 0:1)
}

regress <- function(data, form, logistic = F, mixed = maybe(MIXED_EFFECTS, F), add_random = T, intercept = T, standardize = F, name = "", print_table = T) {
    preds <- paste(get.vars(rhs(form)), collapse = " + ")
    data <- tibble(data) # to not mutate
    if (standardize) {
        for (k in get.vars(form)) {
            if (!is.binary(data[[k]])) {
                data[[k]] <- zscore(data[[k]])
            }
        }
    }
    model <- if (mixed) {
        if (add_random) {
            form <- as.formula(glue("{form} + {1*intercept} + ({preds} + {1*intercept} || pid)"))
        }
        if (logistic) {
            glmer(form, family = binomial, data = data)
        } else {
            lmer(form, data = data)
        }
    } else {
        if (logistic) {
            glm(form, family = binomial, data = data)
        } else {
            lm(form, data = data)
        }
    }
    if (print_table) print(coeftable(model))
    if (name != "") write_model(model, name, logistic, standardize)
    model
}

print_regression <- function(model) {
    tidy(model) %>%
        filter(effect == "fixed", term != "(Intercept)") %>%
        rowwise() %>%
        group_walk(~ with(
            .x,
            print(fmt("$β = {estimate:.3},\\ {pval(p.value)}$"))
        ))
}

tidy <- function(model, ...) {
    d <- broom.mixed::tidy(model, conf.int = T, ...)
    if (typeof(model) == "list") {
        d$df <- model$df
    }
    d
}

# just like counts but proportions
props <- function(data, ...) {
    data %>%
        count(...) %>%
        mutate(prop = n / sum(n), .keep = "unused", by = ...)
}

# %% ==================== Plotting ====================


gg <- function(data, x, y, color, fill, order_x = F) {
    if (order_x) {
        data <- data %>%
            ungroup() %>%
            reorder({{ x }} := fct_reorder({{ x }}, {{ y }}, agg))
    }
    if (missing(fill)) {
        fill <- color
    }
    ggplot(aes(x, y, color, fill))
}

mute <- function(x, amt = .15) {
    x %>%
        colorspace::lighten(amt) %>%
        colorspace::desaturate(2 * amt)
}

concrete_palette <- function(pal, vals, name = waiver(), extra = c()) {
    scale_colour_manual(
        values = c(setNames(pal$palette(length(vals)), vals), extra),
        aesthetics = c("fill", "colour"), name
    )
}

# # plots under prevous plots
# `-.gg` <- function(plot, layer) {
#     if (missing(layer)) {
#         stop("Cannot use `-.gg()` with a single argument. Did you accidentally put - on a new line?")
#     }
#     if (!is.ggplot(plot)) {
#         stop('Need a plot on the left side')
#     }
#     plot$layers = c(layer, plot$layers)
#     plot
# }

join_limits <- function(...) {
    # map(list(...), ~ layer_scales(.x)$y$range$range)
    ylo <- list(...) %>%
        map(~ layer_scales(.x)$y$range$range[[1]]) %>%
        unlist() %>%
        min()
    yhi <- list(...) %>%
        map(~ layer_scales(.x)$y$range$range[[2]]) %>%
        unlist() %>%
        max()
    c(ylo, yhi)
}


patch_row <- function(tag, p1, ...) {
    wrap_plots(p1 + labs(tag = tag), ...)
}



geom_xdensity <- function() {
    list(
        geom_xsidedensity(aes(y = stat(density))),
        scale_xsidey_continuous(breaks = NULL, labels = "")
    )
}
geom_ydensity <- function() {
    list(
        geom_ysidedensity(aes(x = stat(density))),
        scale_ysidex_continuous(breaks = NULL, labels = "")
    )
}

robust <- function(f, min_n = 2) {
    function(x) {
        if (length(x) >= min_n) {
            f(x)
        } else {
            NaN
        }
    }
}

BOOTSTRAP_CI <- TRUE
get_mean_cl <- function() if (BOOTSTRAP_CI) mean_cl_boot else mean_cl_normal

point_line <- function(min_n = 2, ...) {
    list(
        stat_summary(fun.data = robust(get_mean_cl(), min_n), ...),
        stat_summary(fun = robust(mean, min_n), geom = "line", ...)
    )
}

points <- function(min_n = 2, ...) {
    stat_summary(fun.data = robust(get_mean_cl(), min_n), ...)
}

bars <- function(min_n = 2, err_width = .1, ...) {
    list(
        stat_summary(fun = robust(mean, min_n), geom = "bar", ...),
        stat_summary(fun.data = robust(get_mean_cl(), min_n), geom = "errorbar", width = err_width, ...)
    )
}

chance_lines <- function(data, var) {
    geom_errorbar(
        data = data, mapping = aes(y = {{ var }}, ymin = {{ var }}, ymax = {{ var }}),
        linetype = "dotted", linewidth = .3
    )
}

mean_line <- function(min_n = 2, ...) {
    stat_summary(fun = robust(mean, min_n), geom = "line", ...)
}

point_bin <- function(bins, min_n = 2, ...) {
    stat_summary_bin(fun.data = robust(get_mean_cl(), min_n), bins = bins, ...)
}

linear_fit <- function(..., alpha = 0.2) stat_smooth(method = "lm", alpha = alpha, ...)
logistic_fit <- function(...) stat_smooth(method = "glm", method.args = list(family = "binomial"), alpha = 0.2, ...)
gam_fit <- function(k = -1, ...) stat_smooth(method = "gam", formula = y ~ s(x, bs = "cs", k = k), ...)
gam_fit_ind <- function(k = -1, ...) {
    geom_line(
        stat = "smooth", mapping = aes(group = pid), alpha = 0.5, linewidth = .1,
        method = "gam", formula = y ~ s(x, bs = "cs", k = k), se = F
    )
}

point_line_bin <- function(bins, min_n = 2, ...) {
    list(
        stat_summary_bin(fun.data = robust(get_mean_cl(), min_n), bins = bins, ...),
        stat_summary_bin(fun = robust(mean, min_n), bins = bins, ..., geom = "line")
    )
}

point_smooth <- function(min_n = 2, ...) {
    list(
        stat_summary(fun.data = robust(get_mean_cl(), min_n)),
        stat_smooth(...)
    )
}

point_smooth_bin <- function(bins, min_n = 2, ...) {
    list(
        stat_summary_bin(fun.data = robust(get_mean_cl(), min_n), bins = bins),
        stat_smooth(...)
    )
}

no_legend <- theme(legend.position = "none")

no_gridlines <- theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
)

gridlines <- theme(
    panel.grid.major.x = element_line(color = "#EDEDED"),
    panel.grid.major.y = element_line(color = "#EDEDED"),
)

xgrid <- theme(
    panel.grid.major.x = element_line(color = "#EDEDED"),
)

ygrid <- theme(
    panel.grid.major.y = element_line(color = "#EDEDED"),
)

no_xaxis_ticks <- theme(
    # axis.title.x=element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
)

xbreaks <- function(...) scale_x_continuous(breaks = scales::pretty_breaks(...))
ybreaks <- function(...) scale_y_continuous(breaks = scales::pretty_breaks(...))

no_yaxis_ticks <- theme(
    # axis.title.x=element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
)

no_yaxis <- theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
)

no_xaxis <- theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
)

no_axes <- theme(
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    # panel.background=element_blank(),
    # panel.border=element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
)

stat_mean_and_quantiles <- function(rng = .9, ...) {
    q <- (1 - rng) / 2
    stat_summary(
        ...,
        fun = mean,
        fun.min = ~ quantile(.x, q),
        fun.max = ~ quantile(.x, 1 - q),
    )
}

theme_set(theme_bw(base_size = 12))
theme_update(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    # panel.grid.major.y = element_blank(),
    panel.grid.major.y = element_line(color = "#EDEDED"),
    strip.background = element_blank(),
    strip.text.x = element_text(size = 12),
    strip.text.y = element_text(size = 12),
    legend.position = "right",
    panel.spacing = unit(1, "lines"),
)

update_geom_defaults("vline", list(linetype = "dashed"))
update_geom_defaults("abline", list(linetype = "dashed"))
update_geom_defaults("hline", list(linetype = "dashed"))
update_geom_defaults("line", list(linewidth = 1.2))
update_geom_defaults("smooth", list(color = "black"))
update_geom_defaults("pointrange", list(size = .3))

tight_margin <- function(cm = .05) theme(plot.margin = unit(c(cm, cm, cm, cm), "cm"))


ensure_n <- function(data, ..., n = 10) filter(data, n() > n, .by = c(...))

collapse_participants <- function(data, f, y, ...) {
    data %>%
        group_by(pid, ...) %>%
        summarise("{{y}}" := f({{ y }}, na.rm = T)) %>%
        ungroup()
}

agg <- function(data, y, grp, fun = mean, na.rm = T) {
    data %>%
        group_by(pick({{ grp }}), .add = T) %>%
        summarise("{{y}}" := fun({{ y }}, na.rm = na.rm))
}

plot_coefs <- function(tbl) {
    tbl %>%
        mutate(term = gsub("TRUE", "", term)) %>%
        filter(term != "(Intercept)") %>%
        ggplot(aes(term, estimate, ymin = conf.low, ymax = conf.high)) +
        geom_pointrange() +
        geom_hline(yintercept = 0, linetype = "dashed") +
        coord_flip() +
        labs(x = "", y = "regression weight")
}

# %% ---------- formatting axis labels ---------- #

element_text_transform <- function(..., transform = identity) {
    elem <- element_text(...)
    elem$transform <- rlang::as_function(transform)
    class(elem) <- c("element_text_transform", class(elem))
    elem
}

element_grob.element_text_transform <- function(element, label = "", ...) {
    label <- element$transform(label)
    NextMethod()
}

fancy_names <- list(
    pid = "participant",
    nfix = "# fixations"
)

fancy_replacements <- c(
    "1 ?\\* ?" = "",
    "factor\\((.*)\\)" = "\\1",
    "_" = " "
)

fancy_word_replacements <- c(
    n = "#",
    dur = "duration",
    prop = "proportion",
    rel = "relative"
)

fancy_name <- function(lab) {
    words <- fancy_word_replacements
    if (is.na(lab)) {
        return("NA LABEL")
    }
    names(words) <- map_chr(names(words), ~ glue("\\b{.x}\\b"))
    if (lab %in% names(fancy_names)) {
        return(fancy_names[[lab]])
    }
    if (str_detect(lab, "β") || str_detect(lab, "γ")) {
        return(lab)
    }
    if (nchar(lab) == 1) {
        return(lab)
    }
    lab %>%
        str_replace_all(fancy_replacements) %>%
        str_replace_all(words) %>%
        # str_to_lower() %>%
        gsub("Rt", "RT", .) %>%
        gsub("(?!^)\\b(Of|In|On|The|Up|To|Vs|Per|A)\\b", "\\L\\1", ., perl = TRUE)
}

fancy_name_compact <- function(lab) {
    name <- fancy_name(lab)
    if (str_detect(name, " ") && str_length(name) > 10) {
        name <- str_replace_all(name, " ", "\n")
    }
    name
}

theme_update(
    axis.title.x = element_text_transform(transform = fancy_name),
    axis.title.y = element_text_transform(transform = fancy_name),
    # legend.text = element_text(),
    legend.title = element_text_transform(transform = fancy_name_compact),
    strip.text = element_text_transform(transform = fancy_name)
)

no_fancy <- theme(
    axis.title.x = element_text(),
    axis.title.y = element_text(),
    legend.title = element_text(),
    strip.text = element_text()
)


# %% ==================== Saving results ====================


TMP_PATH <- maybe(TMP_PATH, "tmp/")
STATS_PATH <- maybe(STATS_PATH, "stats/")
FIGS_PATH <- maybe(FIGS_PATH, "figs/")
MAKE_PDF <- maybe(MAKE_PDF, FALSE)
FIG_SCALE <- maybe(FIG_SCALE, 2.5)
WIDTH <- maybe(WIDTH, 1.4)
HEIGHT <- maybe(HEIGHT, 1)
DPI <- maybe(DPI, 320)

save_tmp <- function(obj, name = glue(ensym(obj)), path = TMP_PATH) {
    if (name == "." || name == "") {
        stop("no name found save_tmp")
    }
    file <- glue("{path}{name}.rds")
    dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
    saveRDS(obj, file)
    print(paste0(file))
}

read_tmp <- function(name, path = TMP_PATH) {
    file <- glue("{path}{name}.rds")
    readRDS(file)
}

sprintf_transformer <- function(text, envir) {
    m <- regexpr(":.+$", text)
    if (m != -1) {
        format <- substring(regmatches(text, m), 2)
        regmatches(text, m) <- ""
        res <- eval(parse(text = text, keep.source = FALSE), envir)
        do.call(sprintf, list(glue("%{format}f"), res))
    } else {
        eval(parse(text = text, keep.source = FALSE), envir)
    }
}

fmt <- function(..., .envir = parent.frame()) {
    glue(..., .transformer = sprintf_transformer, .envir = .envir)
}

pval <- function(p) {
    # if (p < .001) "p < .001" else glue("p = {str_sub(format(round(p, 3)), 2)}")
    if (p < .001) "p < .001" else glue("p = {str_sub(format(round(p, 3), nsmall=3), 2)}")
}



write_tex <- function(tex, name, path = STATS_PATH, format = T) {
    name <- glue(name, .envir = parent.frame()) %>% str_replace("[:*]", "-")
    if (format) {
        tex <- fmt(tex, .envir = parent.frame())
    }
    file <- glue("{path}{name}.tex")
    dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
    print(paste0(file, ": ", tex))
    writeLines(paste0(tex, "\\unskip"), file)
}

write_md <- function(md, name, path = STATS_PATH, format = T) {
    name <- glue(name, .envir = parent.frame()) %>% str_replace("[:*]", "-")
    if (format) {
        md <- fmt(md, .envir = parent.frame())
    }
    file <- glue("{path}{name}.md")
    dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
    writeLines(md, file)
    cat(glue("wrote {file}\n"))
    system(glue("cat {file}"))
}

fig <- function(name = "tmp", w = WIDTH, h = HEIGHT, s = FIG_SCALE, path = FIGS_PATH, dpi = DPI, pdf = MAKE_PDF, ...) {
    if (isTRUE(getOption("knitr.in.progress"))) {
        show(last_plot())
        return()
    }
    ggsave("/tmp/fig.png", width = w * s, height = h * s, dpi = dpi, ...)
    stamp <- format(Sys.time(), "%m-%d-%H-%M-%S")
    p <- glue('".fighist/{gsub("/", "-", name)}-{stamp}.png"')
    system(glue("mkdir -p `dirname {path}{name}.png`"))
    system(glue("mkdir -p .fighist"))
    system(glue("mv /tmp/fig.png {p}"))
    if (name != "tmp") {
        if (pdf) {
            ggsave(glue("{path}{name}.pdf"), width = w, height = h, ...)
            print(glue("{path}{name}.pdf"))
        } else {
            system(glue("cp {p} {path}{name}.png"))
            print(glue("{path}{name}.png"))
        }
    }
    # invisible(dev.off())
    # knitr::include_graphics(p)
}


figure <- function(name, gg, w = WIDTH, h = HEIGHT, panel_size = TRUE, no_defaults = FALSE) {
    if (!is_ggplot(gg)) {
        cat("'gg' parameter is not a ggplot object:\n")
        print(gg)
        return(invisible(NULL))
    }

    if (panel_size) {
        size <- get_panel_rows_cols(gg)
        w <- w + .7 * (size$nrow - 1)
        h <- h + .7 * (size$ncol - 1)
    }

    fig(name, w = w, h = h)
}

get_panel_rows_cols <- function(p) {
    g <- ggplotGrob(p)

    # Extract number of rows and columns of panels
    layout_panels <- g$layout[grepl("^panel", g$layout$name), ]
    list(
        nrow = length(unique(layout_panels$l)),
        ncol = length(unique(layout_panels$t))
    )
}



regression_tex <- function(logistic = F, standardized = T) {
    beta <- if (standardized) "$\\beta = {estimate:.3}$" else "$B = {estimate:.3}$"
    ci <- "95\\% CI [{conf.low:.3}, {conf.high:.3}]"
    stat <- if (logistic) "$z={statistic:.2}$" else "$t({df:.1})={statistic:.2}$"
    p <- "${pval(p.value)}$"
    paste(beta, ci, p, sep = ", ")
}

coef_tibble <- function(model, wide = F) {
    tbl <- model %>%
        tidy() %>%
        filter({
            if ("effect" %in% names(.)) effect == "fixed" else TRUE
        }) %>%
        mutate(term = str_replace(term, "\\(Intercept\\)", "Intercept"))

    if (wide) {
        tbl %>%
            select(term, estimate) %>%
            pivot_wider(names_from = term, values_from = estimate)
    } else {
        tbl
    }
}

write_model <- function(model, name, logistic = F, standardized = F) {
    model %>%
        coef_tibble() %>%
        rowwise() %>%
        group_walk(~ with(
            .x,
            regression_tex(logistic, standardized) %>%
                write_tex("{name}/{term}")
        ))
}

write_model_md <- function(model, name, ...) {
    regtype <- list(
        gaussian = "Linear",
        binomial = "Logistic"
    )[[family(model)$family]]
    write_md(c(
        glue("{regtype} regression: `{formula(model)}`"),
        "",
        coeftable(model, ...)
    ), name, format = F)
}

fmt_percent <- function(prop, digits = 0) {
    fmt(glue("{{100 * prop:.{digits}}}\\%"))
}

report_percent <- function(data, name, var) {
    data %>%
        summarise(y = mean({{ var }})) %>%
        with(y) %>%
        fmt_percent() %>%
        write_tex(name)
}

report_n_percent <- function(data, name, var) {
    data %>%
        summarise(pct = fmt_percent(mean({{ var }})), total = sum({{ var }})) %>%
        with(write_tex("{total} ({pct})", name))
}

exclude <- function(data, name, cond, report_cond = T) {
    data %>%
        filter({{ report_cond }}) %>%
        report_percent(glue("exclude/{name}"), {{ cond }})
    data %>% filter(!{{ cond }})
}

plot_regression_table <- function(model, term_prefix = NULL) {
    tbl <- tidy(model)
    if (!is.null(term_prefix)) {
        tbl <- tbl %>%
            filter(startsWith(term, term_prefix)) %>%
            mutate(term = str_remove(term, term_prefix))
    }

    tbl %>%
        ggplot(aes(term, estimate)) +
        geom_pointrange(aes(
            ymin = estimate - std.error,
            ymax = estimate + std.error,
            color = term
        )) +
        geom_hline(yintercept = 0, linetype = "dashed") +
        xlab("Coefficient") +
        theme(legend.position = "none")
}
