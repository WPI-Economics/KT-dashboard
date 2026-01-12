library(shiny)
library(tidyverse)

## Create a module for glossary tab server

glossary_server <- function(id){
  
  moduleServer(id = id, 
               module = function(input, output, session){
                 
                 ## General - glossary copy
                 
                 output$glossary_general_copy <- renderUI({
                   HTML("<span class='glossary_copy'>
                        <b>Local authorities</b>: Upper-tier local authorities in England (Counties, Unitary authorities, London boroughs and Metropolitan boroughs)<br>
                        
                        <b>Neighbourhoods</b>: Middle layer super output areas (2011) with names provided by the <a href='https://houseofcommonslibrary.github.io/msoanames/' target='_blank' rel='noopener noreferrer'>House of Commons Library</a>.<br>
                        
                        <b>Deciles</b>: Data in rank order split into 10 equally sized groups, so the first decile will be the 10% lowest, and the 10th the 10% highest.<br>
                        
                        <b>Quintiles</b>: Data in rank order split into 5 equally sized groups, so the first quintile will be the 20% lowest, and the 5th the 20% highest.<br>
                        </span>")
                 })
                 
                 ## Health - glossary copy
                 
                 output$glossary_health_copy <- renderUI({
                   HTML("<span class='glossary_copy'>
                        <b>Life expectancy</b>: The average number of years a person would expect to live based on contemporary mortality rates. For a particular area and time period, it is an estimate of the average number of years a newborn baby would survive if he or she experienced the age specific mortality rates for that area and time period throughout his or her life.<br>
                        
                        <b>Healthy life expectancy</b>: A measure of the average number of years a person would expect to live in good health based on contemporary mortality rates and prevalence of self reported good health. For a particular area and time period, it is an estimate of the average number of years a newborn baby would live in good general health if he or she experienced the age-specific mortality rates and prevalence of good health for that area and time period throughout his or her life.<br>
                        
                        <b>Good health</b>: The proportion of people reporting their general health as good or very good in response in response to the question 'How is your health in general?' from the Annual Population Survey. Responses 'very good' and 'good' are categorised as ‘good’ health and 'fair', 'bad' or 'very bad' as ‘not good’ health.
                        </span>")
                 })
                 
                 ## Work - glossary copy
                 
                 output$glossary_work_copy <- renderUI({
                   HTML('<span class="glossary_copy">
                        <b>Employment rate</b>: The proportion of the working-age population (aged 16–64) that is either an employee or self-employed.<br>
                        
                        <b>Unemployment rate</b>: Proportion of the economically active population (aged 16+) who are without a job.<br>
                        
                        <b>Economically active population</b>: People who are either in work or have been actively seeking work within the last four weeks and are available to start work within the next two weeks.<br>
                        
                        <b>Sickness and disability economic inactivity rate</b>: Proportion of the working age population (aged 16–64) who are economically inactive because of long term sickness or disability.<br>
                        
                        <b>Other economic inactivity rate</b>: Proportion of the working age population (aged 16–64) who are economically inactive because of being a student, looking after the family home, being retired or other reasons.<br>

                        <b>Industry groups</b>: Industries are classified according to the top-level Sections of the Standard Industrial Classification (2007).<br>
                        
                        <b>Occupation groups</b>: Occupations are classified according to the top-level Major Groups of the Standard Occupational Classification (2020)<br>
                        </span>')
                 })
                 
                 ## Money and resources - glossary copy
                 
                 output$glossary_money_copy <- renderUI({
                   HTML('<span class="glossary_copy">
                        <b>Low pay rate</b>: Proportion of employees earning below the real Living Wage as set by the Living Wage Foundation. In 2023–24, the real Living Wage was set at £13.15 in London and £12.00 in the rest of the UK.<br>
                        
                        <b>Out of work benefits</b>: People in receipt of any of the following benefits:<br>
                        <ul>
                          <li>Jobseekers Allowance (JSA)</li>
                          <li>Employment and Support Allowance (ESA)</li>
                          <li>Incapacity Benefit (IB)</li>
                          <li>Severe Disablement Allowance (SDA)</li>
                          <li>Income Support (IS) where Carers Allowance (CA) not also in payment</li>
                          <li>Pension Credit (PC) where Carers Allowance (CA) not also in payment</li>
                          <li>Universal Credit Out of Work (UC OOW) where conditionality regime is one of Searching for Work, Preparing for Work or Planning for Work</li>
                          <li>Universal Credit No Work Requirements (UC NWR) where conditionality regime is No Work Requirements</li>
                        </ul>
                        </span>')
                 })
                 
                 ## Housing - glossary copy
                 
                 output$glossary_housing_copy <- renderUI({
                   HTML("<span class='glossary_copy'>
                        <b>Monthly rent (1 bedroom)</b>: The median monthly rent for a one bedroom home.<br>
                        
                        <b>Monthly rent (1 bedroom) as % of median pay</b>: The median monthly rent for a one bedroom home as a percentage of median monthly pay.<br>
                        
                        <b>Privately rented homes</b>: The percentage of homes in an area that are privately rented.<br>
                        
                        <b>Socially rented homes</b>: The percentage of homes in an area that are rented from either a local authority or a local housing association.<br>
                        
                        <b>Non-decent homes</b>: Non-decent homes are defined as those with a Category 1 hazard – as assessed by the Housing Health and Safety Rating System (HHSRS) – that are not in a reasonable state of repair, lack reasonably modern facilities or are not warm enough (do not provide a reasonable degree of thermal comfort).<br>
                        
                        <b>Housing deprivation</b>: A household is classified as experiencing housing deprivation if the household's accommodation is either overcrowded, in a shared dwelling, or has no central heating.
                        </span>")
                 })
                 
                 ## Transport - glossary copy
                 
                 output$glossary_transport_copy <- renderUI({
                   HTML("<span class='glossary_copy'>
                        <b>Active travel/commuting</b>: People who travel or commute on foot or by bicycle.<br>
                        
                        <b>Public transport commuters</b>: People who travel to work by:<br>
                        <ul>
                          <li>Underground, metro, light rail, tram,</li>
                          <li>Train, or</li>
                          <li>Bus, minibus or coach.</li>
                        </ul>
                        
                        <b>Non-driving commuters</b>: People who travel to work either by public transport or using active travel methods – on foot or by bicycle.<br>
                        </span>")
                 })
                 
                 ## Education - glossary copy
                 
                 output$glossary_education_copy <- renderUI({
                   HTML("<span class='glossary_copy'>
                        <b>Qualification levels</b>: Qualifications refer to levels on the Regulated Qualifications Framework (RQF).<br>
                        
                        <b>Persistent absence</b>: Pupils who miss more than 10% of days with unauthorised absences.<br>
                        
                        <b>Progression to Level 4+ study</b>: Proportion of pupils in a cohort of Level 3 attainess who sustain at least 2 years of Level 4+ (higher education) study.<br>
                        </span>")
                 })
                 
                 ## Community - glossary copy
                 
                 output$glossary_community_copy <- renderUI({
                   HTML("<span class='glossary_copy'>
                        <b>Loneliness</b>: Proportion of people who respond to the question 'How often do you feel lonely?' with 'Often' or 'Always'.<br>
                        
                        <b>Neighbourhood belonging</b>: Proportion of people who respond to the question 'How strongly do you feel you belong to your immediate neighbourhood?' with 'Very strongly' or 'Fairly strongly'.<br>
                        
                        <b>Chatting to neighbours</b>: Proportion of people who chat to their neighbours to say more than just hello at least once per month.<br>
                        
                        <b>Having someone there for you</b>: Proportion of people who agree that they would have someone there for them if they needed help.<br>
                        
                        <b>Single-person households</b>: Proportion of people who live alone without any dependent children.<br>
                        </span>")
                 })
                 
                 ## Surroundings - glossary copy
                 
                 output$glossary_surroundings_copy <- renderUI({
                   HTML('<span class="glossary_copy">
                        <b>Air quality</b>: Concentration of PM2.5 particles, measured in micrograms per cubic meter of air (μg/m3).<br>
                        <b>WHO air quality guideline level</b>: <a href="https://www.who.int/publications/i/item/9789240034228" target="_blank" rel="noopener noreferrer">The World Health Organisation (WHO) quality guideline (AQG)</a> states that annual average concentrations of PM2.5 should not exceed 5 µg/m3<br>
                        
                        <b>Crime rate</b>: The number of street-level crimes committed in an area per 100 people who live in the area. They have been calculated for each local authority for the whole calendar year.<br>
                        
                        <b>Access to green space</b>: Natural England Accessible Greenspace Neighbourhood Standard, which is defined as &#39;Accessible Greenspace of at least 10 ha within 1 km from home. &#39;Accessible Greenspace is defined as <a href="https://designatedsites.naturalengland.org.uk/GreenInfrastructure/AccessibleGreenspaceStandard.aspx" target="_blank" rel="noopener noreferrer">&#39;greenspace that is specifically provided for public access, or one to which the public would usually expect to access, or one over which there is a public to open access.&#39;</a><br>
                        
                        <b>Distance from harms</b>: Average time in minutes for residents of a local authority to drive by car to the following retail outlets:<br>
                        <ul>
                          <li>Tobaconnists/Vape stores</li>
                          <li>Betting shops and gambling outlets</li>
                          <li>Off-licences</li>
                        </ul>
                        </span>')
                 })
               })
}