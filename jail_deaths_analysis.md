Analysis of Jail Deaths
================

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
    ## ✔ ggplot2 3.3.6     ✔ purrr   0.3.4
    ## ✔ tibble  3.1.8     ✔ dplyr   1.0.9
    ## ✔ tidyr   1.2.0     ✔ stringr 1.4.1
    ## ✔ readr   2.1.2     ✔ forcats 0.5.2
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'
    ## 
    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

``` r
library(readr)
library(dplyr)
library(knitr) 
```

``` r
# import from Micah's local computer
all_jails <- read.csv("C:\\Users\\Micah Clark Moody\\Desktop\\R\\2022-09 Natl Jail Crisis\\Allstatesinsurvey\\all_jails.csv")
```

# 2. 3-5 counties/jurisdictions with the largest \# of jail deaths

## 2a. Counties with the most jail deaths in 2019

d2019 = deaths in 2019

``` r
all_jails %>%
  slice_max(order_by = d2019, n = 20) %>%
  
  # printing table
  select(state, county, jail, d2019, adp2019) %>%
  kable()
```

| state        | county         | jail                                                                             | d2019 |  adp2019 |
|:-------------|:---------------|:---------------------------------------------------------------------------------|------:|---------:|
| California   | Los Angeles    | Los Angeles County Sheriff’s Department, all facilities                          |    32 | 17070.00 |
| Pennsylvania | Philadelphia   | Philadelphia Prison System Facilities                                            |    11 |  4741.00 |
| California   | Alameda        | Alameda County Santa Rita Jail                                                   |    10 |  2365.00 |
| California   | San Diego      | San Diego County Central Detention Facility                                      |    10 |  1018.00 |
| Texas        | Bexar          | Bexar County Adult Detention Center                                              |    10 |  4105.00 |
| Florida      | Broward        | Broward County Jails                                                             |     9 |  3659.00 |
| Illinois     | Cook           | Cook County Jail                                                                 |     9 |  5766.00 |
| Texas        | Harris         | Harris County Jails                                                              |     9 |  8652.00 |
| Florida      | Miami-Dade     | Miami-Dade Corrections Facilities                                                |     8 |  4329.00 |
| California   | Sacramento     | Sacramento County Main Jail                                                      |     7 |  1932.00 |
| Ohio         | Franklin       | Franklin County Correctional Center                                              |     7 |  1922.00 |
| California   | San Bernardino | San Bernardino County W. Valley Detention Center                                 |     6 |  3000.00 |
| California   | Santa Clara    | Santa Clara County Jail                                                          |     6 |   693.00 |
| Florida      | Duval          | John E. Goode Pretrial Detention Facility                                        |     6 |  2313.00 |
| Florida      | Polk           | South County Jail                                                                |     6 |  1883.00 |
| Georgia      | Cobb           | Cobb County Sheriffs Office Jail & Prison Unit                                   |     6 |  2018.00 |
| Indiana      | Marion         | Marion County Jail (includes the Main Jail, Hope Hall, and City/County Building) |     6 |  1120.49 |
| Nevada       | Clark          | Clark County Detention Center                                                    |     6 |  3718.00 |
| Oklahoma     | Oklahoma       | Oklahoma County Jail                                                             |     6 |  1744.94 |
| Pennsylvania | Dauphin        | Dauphin County Prison                                                            |     6 |  1002.00 |
| Utah         | Salt Lake      | Salt Lake County Metro Jail and the Oxbow Jail Facility                          |     6 |  2281.00 |

## 2b. Counties with the most jail deaths from 2008 to 2019

## 2c. Jail deaths by jail population in 2019

d2019 = deaths in 2019 adp2019 = average daily population in 2019

``` r
all_jails %>%
  mutate(d_by_adp = d2019 / adp2019) %>%
  slice_max(order_by = d_by_adp, n = 20) %>%
  
  # printing table
  select(state, county, jail, d2019, adp2019, d_by_adp) %>%
  kable()
```

| state         | county                                   | jail                                          | d2019 | adp2019 |  d_by_adp |
|:--------------|:-----------------------------------------|:----------------------------------------------|------:|--------:|----------:|
| New Hampshire | Cheshire                                 | Cheshire County Department of Corrections     |     2 |  100.00 | 0.0200000 |
| Nevada        | Elko                                     | Elko County Jail                              |     2 |  177.00 | 0.0112994 |
| Nevada        | Carson                                   | Carson City Sheriffs Detention Center         |     2 |  191.00 | 0.0104712 |
| Minnesota     | Crow Wing                                | Crow Wing County Jail                         |     2 |  193.41 | 0.0103407 |
| Nebraska      | Dawson                                   | Dawson County Jail                            |     1 |   99.00 | 0.0101010 |
| California    | San Diego                                | San Diego County Central Detention Facility   |    10 | 1018.00 | 0.0098232 |
| California    | Santa Clara                              | Santa Clara County Jail                       |     6 |  693.00 | 0.0086580 |
| Wyoming       | Sweetwater                               | Sweetwater County Detention Center            |     1 |  116.39 | 0.0085918 |
| New Mexico    | Santa Fe                                 | Santa Fe County Adult Corr. Facility          |     4 |  468.00 | 0.0085470 |
| Idaho         | Nez Perce                                | Nez Perce County Adult Detention Center       |     1 |  126.00 | 0.0079365 |
| Arizona       | Cochise                                  | Cochise County Jail                           |     2 |  255.74 | 0.0078204 |
| Colorado      | Mesa                                     | Mesa County Detention Facility                |     4 |  519.00 | 0.0077071 |
| New Hampshire | Hillsborough                             | Hillsborough County Department of Corrections |     2 |  263.00 | 0.0076046 |
| Idaho         | Bannock                                  | Bannock County Jail                           |     2 |  281.00 | 0.0071174 |
| New Hampshire | Rockingham                               | Rockingham County Department of Corrections   |     1 |  141.00 | 0.0070922 |
| Idaho         | Cassia                                   | Mini-Cassia Crim. Justice Center              |     1 |  154.00 | 0.0064935 |
| Washington    | Clark                                    | Clark County Main Jail                        |     4 |  634.55 | 0.0063037 |
| Maine         | York                                     | York County Jail                              |     1 |  162.00 | 0.0061728 |
| California    | Contra Costa                             | Contra Costa Martinez Detention Facility      |     3 |  493.00 | 0.0060852 |
| West Virginia | Brooke, Hancock, Marshall, Ohio & Wetzel | Northern Regional Jail                        |     2 |  329.00 | 0.0060790 |

## 2d. Jail deaths by jail population from 2008 to 2019

# 3. 3-5 counties/jurisdictions with the lowest \# of jail deaths

## 3a. Counties with the least jail deaths in 2019 (not instructive)

d2019 = deaths in 2019

Result: 217 of 523 jails with 0 reported deaths in 2019

## 3b. Jails in 2019 in the 100 highest dail population with the lowest number of jail deaths

d2019 = deaths in 2019

``` r
all_jails %>%
  slice_max(order_by = adp2019, n = 100) %>%
  slice_min(order_by = `d2019`) %>%
  
  # printing table
  select(state, county, jail, d2019, adp2019) %>%
  kable()
```

| state     | county                                                                       | jail                                                   | d2019 | adp2019 |
|:----------|:-----------------------------------------------------------------------------|:-------------------------------------------------------|------:|--------:|
| Kentucky  | Jefferson                                                                    | Jail Complex, Community Corrections, & Hall of Justice |     0 |    1814 |
| Florida   | Pasco                                                                        | Pasco County Land O’ Lakes Detention Facility          |     0 |    1717 |
| Texas     | Tarrant                                                                      | Tarrant County - Green Bay Maximum Security Facility   |     0 |    1540 |
| Texas     | Hidalgo                                                                      | Hidalgo County Adult Detention Center                  |     0 |    1353 |
| Virginia  | Stafford, Spotsylvania, King George counties, and the City of Fredericksburg | Rappahannock Regional Jail                             |     0 |    1342 |
| Michigan  | Oakland                                                                      | Oakland County Law Enforcement Complex                 |     0 |    1291 |
| Louisiana | Orleans Parish                                                               | Orleans Justice Center                                 |     0 |    1232 |
| Louisiana | Jackson Parish                                                               | Jackson Parish Correctional Facility                   |     0 |    1185 |
| Arkansas  | Pulaski                                                                      | Pulaski County Regional Detention Facility             |     0 |    1177 |
| Texas     | Denton                                                                       | Denton County Jail                                     |     0 |    1165 |
| Colorado  | Arapahoe                                                                     | Arapahoe County Sheriff’s Office Detention Facility    |     0 |    1082 |

## 3c. Counties with the least jail deaths from 2008 to 2019 (most instructive)

## 3d. Jails in 2019 in the 100 highest dail population with the lowest number of jail deaths by population

d2019 = deaths in 2019 adp2019 = average daily population in 2019

``` r
all_jails %>%
  mutate(d_by_adp = d2019 / adp2019) %>%
  slice_max(order_by = adp2019, n = 100) %>%
  slice_min(order_by = d_by_adp, n = 20) %>%
  
  # printing table
  select(state, county, jail, d2019, adp2019, d_by_adp) %>%
  kable()
```

| state          | county                                                                       | jail                                                   | d2019 | adp2019 |  d_by_adp |
|:---------------|:-----------------------------------------------------------------------------|:-------------------------------------------------------|------:|--------:|----------:|
| Kentucky       | Jefferson                                                                    | Jail Complex, Community Corrections, & Hall of Justice |     0 |  1814.0 | 0.0000000 |
| Florida        | Pasco                                                                        | Pasco County Land O’ Lakes Detention Facility          |     0 |  1717.0 | 0.0000000 |
| Texas          | Tarrant                                                                      | Tarrant County - Green Bay Maximum Security Facility   |     0 |  1540.0 | 0.0000000 |
| Texas          | Hidalgo                                                                      | Hidalgo County Adult Detention Center                  |     0 |  1353.0 | 0.0000000 |
| Virginia       | Stafford, Spotsylvania, King George counties, and the City of Fredericksburg | Rappahannock Regional Jail                             |     0 |  1342.0 | 0.0000000 |
| Michigan       | Oakland                                                                      | Oakland County Law Enforcement Complex                 |     0 |  1291.0 | 0.0000000 |
| Louisiana      | Orleans Parish                                                               | Orleans Justice Center                                 |     0 |  1232.0 | 0.0000000 |
| Louisiana      | Jackson Parish                                                               | Jackson Parish Correctional Facility                   |     0 |  1185.0 | 0.0000000 |
| Arkansas       | Pulaski                                                                      | Pulaski County Regional Detention Facility             |     0 |  1177.0 | 0.0000000 |
| Texas          | Denton                                                                       | Denton County Jail                                     |     0 |  1165.0 | 0.0000000 |
| Colorado       | Arapahoe                                                                     | Arapahoe County Sheriff’s Office Detention Facility    |     0 |  1082.0 | 0.0000000 |
| New York       | Queens                                                                       | New York City Anna M. Kross Center                     |     1 |  7345.0 | 0.0001361 |
| California     | Santa Clara                                                                  | Elmwood Complex (Santa Clara County)                   |     1 |  2588.0 | 0.0003864 |
| Ohio           | Cuyahoga                                                                     | Cuyahoga County Correctional Center                    |     1 |  2075.0 | 0.0004819 |
| Pennsylvania   | York                                                                         | York County Prison                                     |     1 |  1925.5 | 0.0005193 |
| Georgia        | Clayton                                                                      | Clayton County Detention Center                        |     1 |  1879.0 | 0.0005322 |
| Texas          | El Paso                                                                      | El Paso County Detention Facility - Annex              |     1 |  1567.0 | 0.0006382 |
| North Carolina | Mecklenburg                                                                  | Mecklenburg County Jail Central                        |     1 |  1526.0 | 0.0006553 |
| Florida        | Pinellas                                                                     | Pinellas County Jail                                   |     2 |  3009.0 | 0.0006647 |
| Texas          | Dallas                                                                       | Suzanne Kays South Tower                               |     1 |  1472.0 | 0.0006793 |

## 3e. Jail deaths by jail population from 2008 to 2019
