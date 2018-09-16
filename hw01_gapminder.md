Explore dataset gapminder
================

First, load the dataset.

``` r
require(gapminder)
require(dplyr)
```

Take a glance at the head of the dataset.

``` r
head(gapminder)
```

    ## # A tibble: 6 x 6
    ##   country     continent  year lifeExp      pop gdpPercap
    ##   <fct>       <fct>     <int>   <dbl>    <int>     <dbl>
    ## 1 Afghanistan Asia       1952    28.8  8425333      779.
    ## 2 Afghanistan Asia       1957    30.3  9240934      821.
    ## 3 Afghanistan Asia       1962    32.0 10267083      853.
    ## 4 Afghanistan Asia       1967    34.0 11537966      836.
    ## 5 Afghanistan Asia       1972    36.1 13079460      740.
    ## 6 Afghanistan Asia       1977    38.4 14880372      786.

What is the size of the dataset?

``` r
nrow(gapminder)
```

    ## [1] 1704

How many columns are there in this dataset?

``` r
ncol(gapminder)
```

    ## [1] 6

How about the last few rows of the datset?

``` r
tail(gapminder)
```

    ## # A tibble: 6 x 6
    ##   country  continent  year lifeExp      pop gdpPercap
    ##   <fct>    <fct>     <int>   <dbl>    <int>     <dbl>
    ## 1 Zimbabwe Africa     1982    60.4  7636524      789.
    ## 2 Zimbabwe Africa     1987    62.4  9216418      706.
    ## 3 Zimbabwe Africa     1992    60.4 10704340      693.
    ## 4 Zimbabwe Africa     1997    46.8 11404948      792.
    ## 5 Zimbabwe Africa     2002    40.0 11926563      672.
    ## 6 Zimbabwe Africa     2007    43.5 12311143      470.

What is the time-span of this dataset?

``` r
range(gapminder$year)
```

    ## [1] 1952 2007

How far can lifeExpectancy go?

``` r
range(gapminder$gdpPercap)
```

    ## [1]    241.1659 113523.1329

What are the countries here?

``` r
unique(gapminder$country)
```

    ##   [1] Afghanistan              Albania                 
    ##   [3] Algeria                  Angola                  
    ##   [5] Argentina                Australia               
    ##   [7] Austria                  Bahrain                 
    ##   [9] Bangladesh               Belgium                 
    ##  [11] Benin                    Bolivia                 
    ##  [13] Bosnia and Herzegovina   Botswana                
    ##  [15] Brazil                   Bulgaria                
    ##  [17] Burkina Faso             Burundi                 
    ##  [19] Cambodia                 Cameroon                
    ##  [21] Canada                   Central African Republic
    ##  [23] Chad                     Chile                   
    ##  [25] China                    Colombia                
    ##  [27] Comoros                  Congo, Dem. Rep.        
    ##  [29] Congo, Rep.              Costa Rica              
    ##  [31] Cote d'Ivoire            Croatia                 
    ##  [33] Cuba                     Czech Republic          
    ##  [35] Denmark                  Djibouti                
    ##  [37] Dominican Republic       Ecuador                 
    ##  [39] Egypt                    El Salvador             
    ##  [41] Equatorial Guinea        Eritrea                 
    ##  [43] Ethiopia                 Finland                 
    ##  [45] France                   Gabon                   
    ##  [47] Gambia                   Germany                 
    ##  [49] Ghana                    Greece                  
    ##  [51] Guatemala                Guinea                  
    ##  [53] Guinea-Bissau            Haiti                   
    ##  [55] Honduras                 Hong Kong, China        
    ##  [57] Hungary                  Iceland                 
    ##  [59] India                    Indonesia               
    ##  [61] Iran                     Iraq                    
    ##  [63] Ireland                  Israel                  
    ##  [65] Italy                    Jamaica                 
    ##  [67] Japan                    Jordan                  
    ##  [69] Kenya                    Korea, Dem. Rep.        
    ##  [71] Korea, Rep.              Kuwait                  
    ##  [73] Lebanon                  Lesotho                 
    ##  [75] Liberia                  Libya                   
    ##  [77] Madagascar               Malawi                  
    ##  [79] Malaysia                 Mali                    
    ##  [81] Mauritania               Mauritius               
    ##  [83] Mexico                   Mongolia                
    ##  [85] Montenegro               Morocco                 
    ##  [87] Mozambique               Myanmar                 
    ##  [89] Namibia                  Nepal                   
    ##  [91] Netherlands              New Zealand             
    ##  [93] Nicaragua                Niger                   
    ##  [95] Nigeria                  Norway                  
    ##  [97] Oman                     Pakistan                
    ##  [99] Panama                   Paraguay                
    ## [101] Peru                     Philippines             
    ## [103] Poland                   Portugal                
    ## [105] Puerto Rico              Reunion                 
    ## [107] Romania                  Rwanda                  
    ## [109] Sao Tome and Principe    Saudi Arabia            
    ## [111] Senegal                  Serbia                  
    ## [113] Sierra Leone             Singapore               
    ## [115] Slovak Republic          Slovenia                
    ## [117] Somalia                  South Africa            
    ## [119] Spain                    Sri Lanka               
    ## [121] Sudan                    Swaziland               
    ## [123] Sweden                   Switzerland             
    ## [125] Syria                    Taiwan                  
    ## [127] Tanzania                 Thailand                
    ## [129] Togo                     Trinidad and Tobago     
    ## [131] Tunisia                  Turkey                  
    ## [133] Uganda                   United Kingdom          
    ## [135] United States            Uruguay                 
    ## [137] Venezuela                Vietnam                 
    ## [139] West Bank and Gaza       Yemen, Rep.             
    ## [141] Zambia                   Zimbabwe                
    ## 142 Levels: Afghanistan Albania Algeria Angola Argentina ... Zimbabwe

What is the mean life expectancy in 1952?

``` r
mgapminder1952 = aggregate(na.omit(gapminder[gapminder$year==1952,]),
                           by=list(gapminder[gapminder$year==1952,]$continent),
                           FUN=mean)
mgapminder1952$country = NULL
mgapminder1952$continent = NULL
names(mgapminder1952)[1] = "continent"
mgapminder1952
```

    ##   continent year  lifeExp      pop gdpPercap
    ## 1    Africa 1952 39.13550  4570010  1252.572
    ## 2  Americas 1952 53.27984 13806098  4079.063
    ## 3      Asia 1952 46.31439 42283556  5195.484
    ## 4    Europe 1952 64.40850 13937362  5661.057
    ## 5   Oceania 1952 69.25500  5343003 10298.086

How does it compare to now (2007)?

``` r
mgapminder2007 = aggregate(na.omit(gapminder[gapminder$year==2007,]), 
                           by=list(gapminder[gapminder$year==1952,]$continent), 
                           FUN=mean)
mgapminder2007$country = NULL
mgapminder2007$continent = NULL
names(mgapminder2007)[names(mgapminder2007) == "Group.1"] = "continent"
mgapminder2007
```

    ##   continent year  lifeExp       pop gdpPercap
    ## 1    Africa 2007 54.80604  17875763  3089.033
    ## 2  Americas 2007 73.60812  35954847 11003.032
    ## 3      Asia 2007 70.72848 115513752 12473.027
    ## 4    Europe 2007 77.64860  19536618 25054.482
    ## 5   Oceania 2007 80.71950  12274974 29810.188

Life expectancy in all continents growed at different levels.

``` r
dlgapminder = cbind(mgapminder2007["continent"],mgapminder2007["lifeExp"]-mgapminder1952["lifeExp"])
names(dlgapminder)[2] = "dLifeExp"
dlgapminder
```

    ##   continent dLifeExp
    ## 1    Africa 15.67054
    ## 2  Americas 20.32828
    ## 3      Asia 24.41409
    ## 4    Europe 13.24010
    ## 5   Oceania 11.46450
