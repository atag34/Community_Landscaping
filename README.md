Community\_Landscaping
================
Anthony Tagliente
8/3/2020

## Community Landscaping

The project acted as a proof of concept to help identify local actors
attempt to cluster them together based on an analysis of their websites.
Hopefully this would inform work on the ground in identifying potential
partners and networks based on those findings.

## GDELT

GDELT is a great resource available for utilizing quick analysis of
large amounts of news data. In this case we are primarily interested in
looking at Organizations tagged with mentions in specific locations. I
will use Fresno, California as an example use case given its relvance at
the time to my work with the Heron Foundation.

You can read more about GDELT specifically on their website
<https://www.gdeltproject.org/>

We will specifically be using the Global Knowledge Graph data set from
google BigQuery <https://www.gdeltproject.org/data.html#googlebigquery>
in orger to build a base set of organizations and their number of
menttions.

## Google BigQuery

You will need an account with billing information attached to use google
BigQuery services. If you plan to make adjustments, I would recommend
testing them on tight date parameters to limit the amount of data you
are querying. This can and waste resources, which you may incur a cost
for at some point. There is more information on pricing etc. at
<https://cloud.google.com/bigquery/?utm_source=google&utm_medium=cpc&utm_campaign=na-US-all-en-dr-skws-all-all-trial-p-dr-1009135&utm_content=text-ad-none-any-DEV_c-CRE_288513563439-ADGP_Hybrid+%7C+AW+SEM+%7C+SKWS+%7C+US+%7C+en+%7C+PHR+~+Big+Data+~+BigQuery+~+bigquery-KWID_43700035823139005-kwd-301529154162&utm_term=KW_bigquery-ST_bigquery&gclid=Cj0KCQjw6575BRCQARIsAMp-ksNNLFJ0P1lbf6gjy_0Hvr2aGdsTm5UHPwgVeQ-HBZgaa9sgQhoPTQUaAkO2EALw_wcB#section-10>

As this is a proof of concept test, and an ongoing project, there is
still some work to be done on improving the search criteria. For now, I
have identified two ways for fresno to be tagged while trying to keep it
specifically to California. I have allso tried to exlude what I had
found to be common additional locations tags of articles creating extra
noise in the data.

This is an issue that will need to resolved, perhaps by increasing the
exlusions.

For now I am using stories from 2020, it would likely be more accurate
and inclusive with a longer time parameter.

We can query the database simply using SQL syntax

``` sql
SELECT  Locations, V2Locations, Organizations, V2Organizations, Persons, V2Persons
FROM `gdelt-bq.gdeltv2.gkg_partitioned` 
WHERE (DATE(_PARTITIONTIME) BETWEEN "2020-01-03" AND "2020-08-03") 
AND (Locations LIKE '%Fresno, CA%' OR 
     Locations LIKE '%Fresno, California%' OR 
     V2Locations LIKE '%Fresno, CA%' OR
     V2Locations LIKE '%Fresno%, California%')
AND (Locations NOT LIKE '%Los Angeles%' OR V2Locations NOT LIKE '%Los Angeles%')
AND (Locations NOT LIKE '%Sacramento%' OR V2Locations NOT LIKE '%Sacramento%')
AND (Locations NOT LIKE '%Texas%' OR V2Locations NOT LIKE '%Texas%')
AND (Locations NOT LIKE '%Arizona%' OR V2Locations NOT LIKE '%Arizona%')
AND (Locations NOT LIKE '%Oregon%' OR V2Locations NOT LIKE '%Oregon%')
AND (Locations NOT LIKE '%San Francisco%' OR V2Locations NOT LIKE '%San Francisco%')
AND (Locations NOT LIKE '%Iowa%' OR V2Locations NOT LIKE '%Iowa%')
AND (Locations NOT LIKE '%Nevada%' OR V2Locations NOT LIKE '%Nevada%')
AND (Locations NOT LIKE '%New Mexico%' OR V2Locations NOT LIKE '%New Mexico%')
LIMIT 10000
```

I saved the json output here for this demonstration which can be read
into R:

``` r
g_dat <- lapply(readLines("Gdelt_Results.json"), fromJSON)
g_dat <- bind_rows(g_dat)
```

## Formating

Since the output contains columns with multiple observations seperated
by ‘;’ we need to split them up into individual obervations, normalize
them a bit and remove id numbers added by GDELT where applicable. (EX:
‘Conway Medical Center,5906’)

``` r
g_dat$Organizations[1]
```

    ## [1] "pikeville medical center;saint agnes medical center;anne arundel medical center;baltimore sun"

``` r
g_dat$V2Organizations[101]
```

    ## [1] "University Of California,732;Conway Medical Center,5906;Coroner Office,4592;Kaiser Permanente Fresno Medical Center,4750;Kaiser Health News,315;Kaiser Health News,2157;Facebook,4788;Facebook,5738"

``` r
g_dat <- as.data.frame(
              table(
                  tolower(
                      gsub(
                          pattern = ",\\d+$",
                          replacement = "",
                          x = unlist(
                                  strsplit(x=c(g_dat$Organizations,g_dat$V2Organizations),
                                  split=";")),
                          perl = T
                      )
                  )
              )
          )
```

This will create a data frame of organizations and the frequency which
they appear in the data set.

``` r
head(arrange(.data = g_dat,desc(g_dat$Freq)),10)
```

    ##                           Var1 Freq
    ## 1                united states 4827
    ## 2                         espn 1142
    ## 3                     facebook  818
    ## 4           pine mountain club  814
    ## 5                          cnn  779
    ## 6  california state university  614
    ## 7                      twitter  559
    ## 8               sheriff office  453
    ## 9                    instagram  429
    ## 10                      disney  419
