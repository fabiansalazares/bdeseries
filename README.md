# bdeseries

Search and retrieval of [Banco de España](https://www.bde.es) timeseries data

# Installation

To install latest build available at github's fabiansalazares repo:

```
remotes::install_github("fabiansalazares/bdeseries")

```

# Usage

## Search series

`search_series()`will perform a search on field `descripcion` by default. I

```
bdeseries::search_series(search_str=c("string1", "string1"), field="descripcion")

```

If `search_str` argument is a vector with length > 1, results will be filtered applying a logical AND over each element in the vector. Should you want to apply an OR between strings, just include an empty space between string to be searched for. Argument `field` can be set to any valid column in the tibble returned by `bdeseries::get_catalog()`.

Example: retrieve all the available series containing unit value indices for Spain's exports.

```
bdeseries::search_series(search_str = c("IVU", "Exportaciones"))

```

## Retrieve timeseries

Example: retrieve portfolio investments' assets component of Spain's balance of payment's financial account:

```
bdeseries::get_series("DEEM.N.ES.W1.S1.S1.T.A.FA.P.F._Z.EUR._T.M.N.ALL")

````

Example: retrieve two random timeseries from the dataset:

```

bdeseries::get_catalog() |> dplyr::slice_sample(n=2) |> _$nombre |>  bdeseries::get_series()

```





