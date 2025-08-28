
plot_decisions <- function(d){

  as_num <- \(x){
    as.numeric(gsub(",", "", x))
  }
  j <- d |>
    mutate(`Catch year` = factor(as_num(`Catch year`)),
           `Catch (t)` = as_num(`Catch (t)`)) |>
    rename(`0.05` = `5\\%`,
           `0.5` = `50\\%`,
           `0.95` = `95\\%`) |>
    select(-value, -`start of year`) |>
      pivot_longer(cols = c(`0.05`, `0.5`, `0.95`),
                   names_to = "quantile")
  k <- j |>
    dplyr::filter(`Catch (t)` == 0)
  g <- ggplot(k,
              aes(x = `Catch year`,
                  y = value,
                  color = quantile,
                  group = quantile)) +
    geom_line() +
    geom_point(mapping = aes())

}
