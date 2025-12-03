library(shiny)
library(tidyverse)

## Create a module for datasource tab server

datasource_server <- function(id){
  
  moduleServer(id = id, 
               module = function(input, output, session){
                 
                 ## Health - datasource copy
                 
                 output$datasource_health_copy <- renderUI({
                   HTML('<span class="datasource_copy">
                        <b>Life expectancy</b>: 
                        <ul>
                          <li>Local authority – Office for National Statistics, <i><a href="https://www.ons.gov.uk/peoplepopulationandcommunity/healthandsocialcare/healthandlifeexpectancies/datasets/lifeexpectancyforlocalareasofgreatbritain" target="_blank" rel="noopener noreferrer">Life expectancy for local areas of Great Britain</a></i>, GB, 2021–23.</li>
                          <li>MSOA – Office for Health Improvement & Disparities, <i><a href="https://fingertips.phe.org.uk/local-health#gid/1938133185/ati/3" target="_blank" rel="noopener noreferrer">Local Health: Public Health Data for small geographic areas</a></i>, England, 2016–20 – via Office for Health Improvement & Disparities, Fingertips.</li>
                        </ul>
                              
                        <b>Healthy life expectancy</b>: Office for National Statistics, <i><a href="https://www.ons.gov.uk/peoplepopulationandcommunity/healthandsocialcare/healthandlifeexpectancies/datasets/healthstatelifeexpectancyallagesuk" target="_blank" rel="noopener noreferrer">Health state life expectancy, all ages</a></i>, UK, 2021–2023.<br>
                              
                        <b>Indices of Multiple Deprivation</b>: Ministry of Housing, Communities & Local Government, <i><a href="https://www.gov.uk/government/statistics/english-indices-of-deprivation-2019" target="_blank" rel="noopener noreferrer">English indices of deprivation</a></i>, England, 2019.
                        </span>')
                 })
                 
                 
                 ## Work - datasource copy
                 
                 output$datasource_work_copy <- renderUI({
                   HTML('<span class="datasource_copy">
                        <b>Employment rate</b>: Office for National Statistics, <i><a href="https://beta.ukdataservice.ac.uk/datacatalogue/series/series?id=200002" target="_blank" rel="noopener noreferrer">Annual Population Survey</a></i>, England, 2024Q2–2025Q1 – via NOMIS.<br>
                        
                        <b>Unemployment rate</b>: Office for National Statistics, <i><a href="https://beta.ukdataservice.ac.uk/datacatalogue/series/series?id=200002" target="_blank" rel="noopener noreferrer">Annual Population Survey</a></i>, England, 2024Q2–2025Q1 – via NOMIS.<br>
                        
                        <b>Sickness and disability economic inactivity rate</b>: Office for National Statistics, <i><a href="https://beta.ukdataservice.ac.uk/datacatalogue/series/series?id=200002" target="_blank" rel="noopener noreferrer">Annual Population Survey</a></i>, England, 2024Q2–2025Q1 – via NOMIS.<br>
                        
                        <b>Other economic inactivity rate</b>: Office for National Statistics, <i><a href="https://beta.ukdataservice.ac.uk/datacatalogue/series/series?id=200002" target="_blank" rel="noopener noreferrer">Annual Population Survey</a></i>, England, 2024Q2–2025Q1 – via NOMIS.<br>
                        
                        <b>Unemployed claimant count</b>: Office for National Statistics, <i><a href="https://www.ons.gov.uk/employmentandlabourmarket/peoplenotinwork/outofworkbenefits/datasets/claimantcountcla01" target="_blank" rel="noopener noreferrer">CLA01: Claimant Count</a></i>, UK, June 2025 – via NOMIS.<br>
                        
                        <b>Employees by industry</b>: Office for National Statistics, <i><a href="https://beta.ukdataservice.ac.uk/datacatalogue/series/series?id=200002" target="_blank" rel="noopener noreferrer">Annual Population Survey</a></i>, England, 2024Q2–2025Q1 – via NOMIS.<br>
                        
                        <b>Employees by occupation</b>: Office for National Statistics, <i><a href="https://beta.ukdataservice.ac.uk/datacatalogue/series/series?id=200002" target="_blank" rel="noopener noreferrer">Annual Population Survey</a></i>, England, 2024Q2–2025Q1 – via NOMIS.<br>
                        </span>')
                 })
                 
                 ## Money - datasource copy
                 
                 output$datasource_money_copy <- renderUI({
                   HTML('<span class="datasource_copy">
                        <b>Median monthly pay (Local authority)</b>: Office for National Statistics, <i><a href="https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/earningsandworkinghours/datasets/realtimeinformationstatisticsreferencetableseasonallyadjusted/current" target="_blank" rel="noopener noreferrer">Earnings and employment from Pay As You Earn Real Time Information, seasonally adjusted</a></i>, UK, May 2025.<br>
                        
                        <b>Child poverty rate</b>: End Child Poverty, <i><a href="https://endchildpoverty.org.uk/child-poverty-2025/" target="_blank" rel="noopener noreferrer">Local Child Poverty Statistics 2025</a></i>, UK, 2023/24.<br>
                        
                        <b>Low pay rate</b>: Office for National Statistics, <i><a href="https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/earningsandworkinghours/datasets/numberandproportionofemployeejobswithhourlypaybelowthelivingwage" target="_blank" rel="noopener noreferrer">Number and proportion of employee jobs with hourly pay below the living wage</a></i>, UK, 2023/24.<br>
                        
                        <b>Out-of-work benefits rate</b>: Department for Work and Pensions, <i><a href="https://www.gov.uk/government/publications/dwp-statistical-summary-policies-and-statements/benefit-combination-statistics-background-information-note" target="_blank" rel="noopener noreferrer">Benefit Combinations</a></i>, Great Britain, November 2024.<br>
                        
                        <b>Median annual income (MSOA)</b>: Office for National Statistics, <i><a href="https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/earningsandworkinghours/datasets/smallareaincomeestimatesformiddlelayersuperoutputareasenglandandwales" target="_blank" rel="noopener noreferrer">Income estimates for small areas</a></i>, England and Wales, 2019/20. 
                        </span>')
                 })
                 
                 ## Housing - datasource copy
                 
                 output$datasource_housing_copy <- renderUI({
                   HTML('<span class="datasource_copy">
                        <b>Median monthly rent</b>: Office for National Statistics, <i><a href="https://www.ons.gov.uk/economy/inflationandpriceindices/datasets/priceindexofprivaterentsukmonthlypricestatistics" target="_blank" rel="noopener noreferrer">Price Index of Private Rents: monthly price statistics </a></i>, UK, 2025 Q2.<br>
                        
                        <b>Housing stock by tenure</b>: Office for National Statistics, <i><a href="https://www.ons.gov.uk/peoplepopulationandcommunity/housing/datasets/subnationaldwellingstockbytenureestimates" target="_blank" rel="noopener noreferrer">Subnational estimates of dwellings by tenure</a></i>, England, 2021.<br>
                        
                        <b>Non-decent homes</b>: Department for Levelling Up, Housing and Communities, <i><a href="https://www.gov.uk/government/collections/english-housing-survey-local-authority-stock-condition-modelling" target="_blank" rel="noopener noreferrer">English Housing Survey: local authority housing stock condition modelling</a></i>, England, 2023.<br>
                        
                        <b>Housing deprivation</b>: Office for National Statistics, <i><a href="https://www.ons.gov.uk/census" target="_blank" rel="noopener noreferrer">Census 2021</a></i>, England and Wales, 2021. 
                        </span>')
                 })
                 
                 
                 ## Transport - datasource copy
                 
                 output$datasource_transport_copy <- renderUI({
                   HTML('<span class="datasource_copy">
                        <b>Walking and cycling</b>: Department for Transport, <i><a href="https://www.gov.uk/government/statistical-data-sets/walking-and-cycling-statistics-cw" target="_blank" rel="noopener noreferrer">Participation in walking and cycling (local authority rates)</a></i>, England, 2023.<br>
                        
                        <b>Commuting methods</b>: Office for National Statistics, <i><a href="https://www.ons.gov.uk/census" target="_blank" rel="noopener noreferrer">Census 2021</a></i>, England and Wales, 2021.<br>
                        
                        <b>Commuting distance</b>: Office for National Statistics, <i><a href="https://www.ons.gov.uk/census" target="_blank" rel="noopener noreferrer">Census 2021</a></i>, England and Wales, 2021.<br>
                        
                        <b>Road fatalities</b>: Department for Transport, <i><a href="https://www.data.gov.uk/dataset/cb7ae6f0-4be6-4935-9277-47e5ce24a11f/road-safety-data" target="_blank" rel="noopener noreferrer">Road Safety Data</a></i>, UK, 2023.<br>
                        </span>')
                 })
                 
                 ## Education - datasource copy
                 
                 output$datasource_education_copy <- renderUI({
                   HTML('<span class="datasource_copy">
                        <b>Residents with no, Level 3+ and Level 4+ qualifications</b>: Office for National Statistics, <i><a href="https://www.ons.gov.uk/census" target="_blank" rel="noopener noreferrer">Census 2021</a></i>, England and Wales, 2021.<br>
                        
                        <b>Pupils receiving free school meals</b>: Department for Education, <i><a href="https://explore-education-statistics.service.gov.uk/find-statistics/special-educational-needs-in-england" target="_blank" rel="noopener noreferrer">Special educational needs in England</a></i>, 2023/24.<br>
                        
                        <b>Pupils with SEN support or EHC plans</b>: Department for Education, <i><a href="https://explore-education-statistics.service.gov.uk/find-statistics/special-educational-needs-in-england" target="_blank" rel="noopener noreferrer">Special educational needs in England</a></i>, England and Wales, 2023/24.<br>
                        
                        <b>Pupils persistently absent</b>: Department for Education, <i><a href="https://explore-education-statistics.service.gov.uk/find-statistics/pupil-absence-in-schools-in-england/" target="_blank" rel="noopener noreferrer">Pupil absence in schools in England</a></i>, 2022/23.<br>
                        
                        <b>Highest level of qualification</b>: Office for National Statistics, <i><a href="https://www.ons.gov.uk/census" target="_blank" rel="noopener noreferrer">Census 2021</a></i>, England and Wales, 2021.<br>
                        
                        <b>Level 2 and 3 attainment by age 19</b>: Department for Education, <i><a href="https://explore-education-statistics.service.gov.uk/find-statistics/level-2-and-3-attainment-by-young-people-aged-19/" target="_blank" rel="noopener noreferrer">Level 2 and 3 attainment age 16 to 25</a></i>, England, 2022/23.<br>
                        
                        <b>Progression to higher education</b>: Department for Education, <i><a href="https://explore-education-statistics.service.gov.uk/find-statistics/progression-to-higher-education-or-training/" target="_blank" rel="noopener noreferrer">Progression to higher education or training</a></i>, England, 2020/21.<br>
                        </span>')
                 })
                 
                 ## Community - datasource copy
                 
                 output$datasource_community_copy <- renderUI({
                   HTML('<span class="datasource_copy">
                        <b>Loneliness</b>: Department for Culture, Media and Sport, <i><a href="https://www.gov.uk/government/statistics/community-life-survey-202324-annual-publication" target="_blank" rel="noopener noreferrer">Community Life Survey, 2023/24</a></i>, England, 2023/24.<br>
                        
                        <b>Neighbourhood belonging</b>: Department for Culture, Media and Sport, <i><a href="https://www.gov.uk/government/statistics/community-life-survey-202324-annual-publication" target="_blank" rel="noopener noreferrer">Community Life Survey, 2023/24</a></i>, England, 2023/24.<br>
                        
                        <b>Chatting to neighbours</b>: Department for Culture, Media and Sport, <i><a href="https://www.gov.uk/government/statistics/community-life-survey-202324-annual-publication" target="_blank" rel="noopener noreferrer">Community Life Survey, 2023/24</a></i>, England, 2023/24.<br>
                        
                        <b>Having people there if needing help</b>: Department for Culture, Media and Sport, <i><a href="https://www.gov.uk/government/statistics/community-life-survey-202324-annual-publication" target="_blank" rel="noopener noreferrer">Community Life Survey, 2023/24</a></i>, England, 2023/24.<br>
                        
                        <b>Single-person households</b>: Office for National Statistics, <i><a href="https://www.ons.gov.uk/census" target="_blank" rel="noopener noreferrer">Census 2021</a></i>, England and Wales, 2021.<br>
                        </span>')
                 })
                 
                 ## Surroundings - datasource copy
                 
                 output$datasource_surroundings_copy <- renderUI({
                   HTML('<span class="datasource_copy">
                        <b>Air Quality</b>: Department for Environment, Food & Rural Affairs, <i><a href="https://uk-air.defra.gov.uk/data/pcm-data" target="_blank" rel="noopener noreferrer">Modelled background polution data, Population-weighted annual mean PM2.5</a></i>, UK, 2023.<br>
                        
                        <b>Crime rate</b>: Police UK, <i><a href="https://data.police.uk/" target="_blank" rel="noopener noreferrer">Street-level crime data</a></i>, England, 2024/25.<br>
                        
                        <b>Access to green space</b>: Department for Environment, Food & Rural Affairs, <i><a href="https://www.gov.uk/government/statistics/access-to-green-space-in-england" target="_blank" rel="noopener noreferrer">Access to green space in England</a></i>, England, 2024.<br>
                        
                        <b>Distance from harms</b>: Consumer Data Research Centre, <i><a href="https://data.geods.ac.uk/dataset/access-to-healthy-assets-hazards-ahah" target="_blank" rel="noopener noreferrer">Access to Healthy Assets & Hazards (AHAH)</a></i>, Great Britain, 2022.<br>
                        </span>')
                 })
               })
}