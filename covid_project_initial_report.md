Report (Initial)
================
Heidi Lumish (hl2738), Louis Sharp (lzs2109), Philip Kim (pk2711), Kayla
Boyer (kb3066), Yujia Li (yl4923)

## Motivation

Considering the recent and ongoing COVID-19 pandemic, we are interested
in understanding how COVID-19 vaccination has impacted the trajectory of
the pandemic in the United States and in New York City specifically. To
that end, we will use data from the New York Times, Our World in Data,
the Centers for Disease Control and Prevention, and the World Health
Organization to explore the case rates and death rates attributable to
COVID-19 since January 2020 until November 2021.

We are interested in comparing trends in COVID-19 cases and deaths pre-
and post-vaccination in the United States. We would also like to
understand patterns of vaccine hesitancy and how this has impacted
vaccine uptake and COVID cases and mortality across the country. We will
focus in specifically on New York City and similarly describe trends in
COVID-19 disease and vaccinations. To achieve this, we will stratify our
gathered data into a pre- and post-vaccine rollout time period and
compare the increase in cases and deaths between these two strata. We’ll
use linear models to investigate whether the rollout of the vaccine had
an effect on the case and death rates. Using data from boroughs in New
York City, we will compare these models between areas with lower vaccine
uptake compared to higher vaccine uptake areas to determine the local
effects of vaccine availability. (Optional: To bolster the validity of
these results, we will compare case and death rates from the 3-5 states
with the lowest vaccine uptake to the 3-5 states with the highest
vaccine uptake and see if the relationships are similar to those in New
York City, in order to corroborate the trends on a larger scale.)

Similarly, we are interested in how the trends in case and fatality
rates differ when stratified by season/quarter. We know that they were
particularly high in winter, and unfortunately we have limited data for
winters pre- and post-vaccine release at this point. We do however have
more information for other seasons/quarters before and after vaccines
became available, and are interested in comparing these rates by time
period in addition to overall. We can use linear models for these strata
as well to investigate whether or not there was a large difference in
rates by season alone, and by season based on whether or not vaccines
were available.

(Additional optional: We thought it would be interesting to compare the
case and death rates from the United States with countries that took
different approaches to curbing the virus, as well as comparing these
rates pre- and post- vaccine release in those countries to investigate
the effect that the vaccine rollout had internationally. Examples of
countries that hit the hardest include India, the United States, Brazil,
Russia, and Mexico, and examples of regions that were not as devastated
by COVID-19 include Africa, Australia, Hong Kong.)

## Related Work

The inspiration for this exploratory data analysis was largely drawn
from the fact that we are still facing the effects of this pandemic
every day in many facets of life. Although school has largely returned
to in-person instruction, we are still living in a world polarized by
vaccine uptake and mask-wearing opinions, with online conference and
meeting tools still at the center of nearly everything we do. Because of
the life-altering forces of the pandemic, we have a wealth of data and
metrics related to the virus to sift through and attempt to make some
sense out of. Although there exist numerous dashboards and
interpretations of COVID-19 data online, we were interested in how
vaccine rollout has affected the spread of the virus and the case and
death rates and numbers across the country and globally. We were unable
to find this type of analysis easily online and figured it would be a
worthwhile question to ask and answer. Comparing trends between areas
with large vaccine uptake to those with lower vaccine uptake in addition
to stratifying data based on dates when vaccines became readily
available should shed some light on its practical effect in combating
this global pandemic.

## Initial Questions

How do trends in cases and deaths compare in the U.S. and NYC (and
globally) from January 2020 to November 2021?

How has the vaccine been distributed in these areas? What are the rates
of uptake/vaccination?

What are the differences in COVID-19 deaths by different subgroups (age,
sex, race, etc.)? Are these differences statistically significant?

What are the trends in mask usage, disease rates, and vaccine rates and
how are they related to one another or isolated by region?

What are trends in vaccination, vaccine hesitancy, and allocation of
vaccine type and how do they compare across NYC and the United States?

How did the release of vaccines affect the case and death rates in NYC
and across the country? Did they have an effect? What about in areas
with low vs high vaccination rates?

How do the seasonal/quarterly rates of COVID-19 infection differ and are
they significantly different for the same seasons/quarters pre- and
post-vaccine availability?

## Data (Preliminary)

-   [NY Times COVID-19 data
    set](https://github.com/nytimes/covid-19-data/)
-   [Our World COVID-19
    data](https://github.com/owid/covid-19-data/tree/master/public/data/vaccinations)
-   [CDC’s United States COVID-19 Cases and Deaths by State Over
    Time](https://data.cdc.gov/Case-Surveillance/United-States-COVID-19-Cases-and-Deaths-by-State-o/9mfq-cb36)
-   CDC’s COVID-19 deaths by: [Sex and
    age](https://data.cdc.gov/NCHS/Provisional-COVID-19-Deaths-by-Sex-and-Age/9bhg-hcku),
    [Race and Hispanic
    origin](https://data.cdc.gov/NCHS/Provisional-COVID-19-Deaths-Distribution-of-Deaths/pj7m-y5uh),
    [Week and
    Urbanicity](https://data.cdc.gov/NCHS/Provisional-COVID-19-Deaths-by-Week-and-Urbanicity/hkhc-f7hg),
    [Educational
    Attainment](https://data.cdc.gov/NCHS/AH-Provisional-COVID-19-Deaths-by-Race-and-Educati/i6ej-9eac)
-   [CDC’s Vaccine hesitancy for COVID-19: county and local
    estimates](https://data.cdc.gov/Vaccinations/Vaccine-Hesitancy-for-COVID-19-County-and-local-es/q9mh-h2tw)
-   CDC’s COVID-19 Vaccine Distribution Allocations by Jurisdiction -
    [Pfizer](https://data.cdc.gov/Vaccinations/COVID-19-Vaccine-Distribution-Allocations-by-Juris/saz5-9hgg),
    [Moderna](https://data.cdc.gov/Vaccinations/COVID-19-Vaccine-Distribution-Allocations-by-Juris/b7pe-5nws),
    [J&J](https://data.cdc.gov/Vaccinations/COVID-19-Vaccine-Distribution-Allocations-by-Juris/w9zu-fywh)
-   [CDC’s COVID-19 vaccinations in the United States,
    County](https://data.cdc.gov/Vaccinations/COVID-19-Vaccinations-in-the-United-States-County/8xkx-amqh)
