Who Brexited? Mapping the 2016 EU Referendum Vote
================
Todd K. Hartman
2016-12-07

Housekeeping

``` r
## Load packages via 'pacman' package manager
pacman::p_load(broom, data.table, ggplot2, googleVis, rgdal)
  
## Set the working directory
# setwd("ENTER YOUR WORKING DIRECTORY HERE")
```

2016 EU Referendum (Brexit) vote share data

``` r
## Download and import brexit data from the web (or get from Github directly)
url.df <- "http://www.electoralcommission.org.uk/__data/assets/file/0014/212135/EU-referendum-result-data.csv"
file.df <- basename(url.df)  # Extract the filename
if (!file.exists(file.df))   # Only download the file if it doesn't already exist in our working directory
    download.file(url = url.df, destfile = file.df, mode = "wb")
brexit <- fread(file.df)  # Read the dataset quickly
head(brexit)  # Check the data
```

    ##     id Region_Code Region Area_Code                 Area Electorate
    ## 1: 108   E12000006   East E06000031         Peterborough     120892
    ## 2: 109   E12000006   East E06000032                Luton     127612
    ## 3: 112   E12000006   East E06000033      Southend-on-Sea     128856
    ## 4: 113   E12000006   East E06000034             Thurrock     109897
    ## 5: 110   E12000006   East E06000055              Bedford     119530
    ## 6: 111   E12000006   East E06000056 Central Bedfordshire     204004
    ##    ExpectedBallots VerifiedBallotPapers Pct_Turnout Votes_Cast Valid_Votes
    ## 1:           87474                87469       72.35      87469       87392
    ## 2:           84633                84636       66.31      84616       84481
    ## 3:           93948                93939       72.90      93939       93870
    ## 4:           79969                79954       72.75      79950       79916
    ## 5:           86136                86136       72.06      86135       86066
    ## 6:          158904               158896       77.89     158894      158804
    ##    Remain Leave Rejected_Ballots No_official_mark Voting_for_both_answers
    ## 1:  34176 53216               77                0                      32
    ## 2:  36708 47773              135                0                      85
    ## 3:  39348 54522               69                0                      21
    ## 4:  22151 57765               34                0                       8
    ## 5:  41497 44569               69                0                      26
    ## 6:  69670 89134               90                0                      34
    ##    Writing_or_mark Unmarked_or_void Pct_Remain Pct_Leave Pct_Rejected
    ## 1:               7               38      39.11     60.89         0.09
    ## 2:               0               50      43.45     56.55         0.16
    ## 3:               0               48      41.92     58.08         0.07
    ## 4:               3               23      27.72     72.28         0.04
    ## 5:               1               42      48.22     51.78         0.08
    ## 6:               1               55      43.87     56.13         0.06

``` r
## Remove Gibraltar from the dataset for mapping purposes
brexit <- brexit[!(brexit$Area == "Gibraltar"), ]

## Create a common identifier for mapping in ggplot
brexit <- brexit[order(Area_Code)]
brexit$id <- 0:380

## Display 'Leave' vote share from highest to lowest
brexit$Pct_Leave <- as.numeric(brexit$Pct_Leave)  # Percentage of leave vote share by administrative area
brexit2 <- subset(brexit, select = c("Area", "Pct_Remain", "Pct_Leave"))  # Subset the data
brexit2 <- brexit2[order(-Pct_Leave), ]  # Display the 'Leave' vote by area
print(brexit2[order(-Pct_Leave), ], nrows = 382)  # Display the 'Leave' vote by area
```

    ##                              Area Pct_Remain Pct_Leave
    ##   1:                       Boston      24.44     75.56
    ##   2:                South Holland      26.41     73.59
    ##   3:                 Castle Point      27.30     72.70
    ##   4:                     Thurrock      27.72     72.28
    ##   5:               Great Yarmouth      28.50     71.50
    ##   6:                      Fenland      28.61     71.39
    ##   7:                    Mansfield      29.14     70.86
    ##   8:                     Bolsover      29.17     70.83
    ##   9:                 East Lindsey      29.35     70.65
    ##  10:      North East Lincolnshire      30.13     69.87
    ##  11:                     Ashfield      30.16     69.84
    ##  12:                     Havering      30.34     69.66
    ##  13:                   Hartlepool      30.43     69.57
    ##  14:                     Tendring      30.50     69.50
    ##  15:               Stoke-on-Trent      30.64     69.36
    ##  16:                    Doncaster      31.04     68.96
    ##  17:                Cannock Chase      31.14     68.86
    ##  18:                     Basildon      31.38     68.62
    ##  19:                     Barnsley      31.69     68.31
    ##  20:                       Harlow      31.90     68.10
    ##  21:                    Rotherham      32.11     67.89
    ##  22:                      Walsall      32.14     67.86
    ##  23:                    Bassetlaw      32.17     67.83
    ##  24:  Kingston upon Hull, City of      32.38     67.62
    ##  25:                       Dudley      32.40     67.60
    ##  26:                     Tamworth      32.53     67.47
    ##  27:                    Blackpool      32.54     67.46
    ##  28:           North Warwickshire      33.12     66.88
    ##  29:                     Sandwell      33.28     66.72
    ##  30:                     Rochford      33.39     66.61
    ##  31:                      Burnley      33.39     66.61
    ##  32: King's Lynn and West Norfolk      33.60     66.40
    ##  33:                    Wakefield      33.64     66.36
    ##  34:           North Lincolnshire      33.70     66.30
    ##  35:                   Broxbourne      33.74     66.26
    ##  36:         Redcar and Cleveland      33.81     66.19
    ##  37:                     Hyndburn      33.81     66.19
    ##  38:        Nuneaton and Bedworth      33.99     66.01
    ##  39:                Middlesbrough      34.52     65.48
    ##  40:                    Gravesham      34.62     65.38
    ##  41:                 Forest Heath      35.03     64.97
    ##  42:          South Staffordshire      35.15     64.85
    ##  43:      Staffordshire Moorlands      35.27     64.73
    ##  44:                        Corby      35.75     64.25
    ##  45:                     Dartford      35.78     64.22
    ##  46:                    Breckland      35.78     64.22
    ##  47:                       Medway      35.92     64.08
    ##  48:                        Wigan      36.10     63.90
    ##  49:                      Gosport      36.14     63.86
    ##  50:                       Thanet      36.15     63.85
    ##  51:                         Wyre      36.23     63.77
    ##  52:           Telford and Wrekin      36.78     63.22
    ##  53:           East Staffordshire      36.79     63.21
    ##  54:                       Torbay      36.84     63.16
    ##  55:                       Pendle      36.85     63.15
    ##  56:                  Wyre Forest      36.85     63.15
    ##  57:         Newcastle-under-Lyme      36.96     63.04
    ##  58:                       Bexley      37.05     62.95
    ##  59:                      Waveney      37.10     62.90
    ##  60:        North East Derbyshire      37.22     62.78
    ##  61:                Epping Forest      37.31     62.69
    ##  62:                       Maldon      37.42     62.58
    ##  63:                Wolverhampton      37.43     62.57
    ##  64:                         Arun      37.52     62.48
    ##  65:                        Swale      37.54     62.46
    ##  66:         Barking and Dagenham      37.56     62.44
    ##  67:               Wellingborough      37.58     62.42
    ##  68:                       Havant      37.64     62.36
    ##  69:                     Redditch      37.71     62.29
    ##  70:               North Kesteven      37.74     62.26
    ##  71:                      Shepway      37.75     62.25
    ##  72:                        Dover      37.85     62.15
    ##  73:               South Tyneside      37.95     62.05
    ##  74:                Blaenau Gwent      37.97     62.03
    ##  75:                     Copeland      38.00     62.00
    ##  76:                  Scarborough      38.01     61.99
    ##  77:                Isle of Wight      38.05     61.95
    ##  78:                 West Lindsey      38.18     61.82
    ##  79:             Stockton-on-Tees      38.27     61.73
    ##  80:                   Sunderland      38.66     61.34
    ##  81:                      Erewash      38.77     61.23
    ##  82:                    Sedgemoor      38.80     61.20
    ##  83:                     Tameside      38.86     61.14
    ##  84:                    Braintree      38.87     61.13
    ##  85:        Weymouth and Portland      38.96     61.04
    ##  86:                    Kettering      39.01     60.99
    ##  87:                 Peterborough      39.11     60.89
    ##  88:                       Oldham      39.14     60.86
    ##  89:                     Torridge      39.17     60.83
    ##  90:    North West Leicestershire      39.30     60.70
    ##  91:                   Rossendale      39.32     60.68
    ##  92:            Barrow-in-Furness      39.38     60.62
    ##  93:                West Somerset      39.41     60.59
    ##  94:     East Riding of Yorkshire      39.60     60.40
    ##  95:          Newark and Sherwood      39.61     60.39
    ##  96:             South Derbyshire      39.65     60.35
    ##  97:        Hinckley and Bosworth      39.67     60.33
    ##  98:                   Spelthorne      39.70     60.30
    ##  99:                 Amber Valley      39.72     60.28
    ## 100:                     Carlisle      39.86     60.14
    ## 101:                     Rochdale      39.93     60.07
    ## 102:                 Chesterfield      39.96     60.04
    ## 103:                     Plymouth      40.06     59.94
    ## 104:               South Kesteven      40.07     59.93
    ## 105:                      Torfaen      40.22     59.78
    ## 106:                        Blaby      40.53     59.47
    ## 107:                      Ashford      40.57     59.43
    ## 108:                    Stevenage      40.75     59.25
    ## 109:     Herefordshire, County of      40.78     59.22
    ## 110:                        Selby      40.83     59.17
    ## 111:                    Brentwood      40.85     59.15
    ## 112:                      Purbeck      40.93     59.07
    ## 113:                      Wrexham      40.96     59.04
    ## 114:                North Norfolk      41.09     58.91
    ## 115:                 Christchurch      41.17     58.83
    ## 116:                    Lichfield      41.19     58.81
    ## 117:        East Northamptonshire      41.24     58.76
    ## 118:                    Maidstone      41.25     58.75
    ## 119:                    Allerdale      41.35     58.65
    ## 120:                     Daventry      41.40     58.60
    ## 121:               Forest of Dean      41.42     58.58
    ## 122:                 South Ribble      41.44     58.56
    ## 123:                       Rother      41.47     58.53
    ## 124:                   Gloucester      41.50     58.50
    ## 125:                      Crawley      41.59     58.41
    ## 126:                  Northampton      41.62     58.38
    ## 127:                       Bolton      41.71     58.29
    ## 128:                      Ipswich      41.74     58.26
    ## 129:                     Rushmoor      41.79     58.21
    ## 130:                        Poole      41.83     58.17
    ## 131:                       Melton      41.89     58.11
    ## 132:              Southend-on-Sea      41.92     58.08
    ## 133:                   Portsmouth      41.92     58.08
    ## 134:                   St. Helens      41.98     58.02
    ## 135:                     Wychavon      42.14     57.86
    ## 136:                   New Forest      42.24     57.76
    ## 137:                   Caerphilly      42.37     57.63
    ## 138:                  East Dorset      42.38     57.62
    ## 139:                County Durham      42.45     57.55
    ## 140:                       Halton      42.58     57.42
    ## 141:                   Eastbourne      42.67     57.33
    ## 142:               South Somerset      42.75     57.25
    ## 143:                        Derby      42.78     57.22
    ## 144:                Pembrokeshire      42.86     57.14
    ## 145:                  North Devon      42.96     57.04
    ## 146:                        Fylde      43.04     56.96
    ## 147:                      Lincoln      43.06     56.94
    ## 148:                   Shropshire      43.13     56.87
    ## 149:                    Gateshead      43.15     56.85
    ## 150:            Neath Port Talbot      43.16     56.84
    ## 151:                      Chorley      43.17     56.83
    ## 152:                      Salford      43.19     56.81
    ## 153:                Richmondshire      43.22     56.78
    ## 154:                        Rugby      43.30     56.70
    ## 155:               St Edmundsbury      43.38     56.62
    ## 156:                        Luton      43.45     56.55
    ## 157:                     Cornwall      43.48     56.52
    ## 158:               Merthyr Tydfil      43.56     56.44
    ## 159:                 North Dorset      43.60     56.40
    ## 160:                Ribble Valley      43.61     56.39
    ## 161:                   Hillingdon      43.63     56.37
    ## 162:                   Flintshire      43.63     56.37
    ## 163:        Blackburn with Darwen      43.66     56.34
    ## 164:                   Darlington      43.82     56.18
    ## 165:                     Solihull      43.84     56.16
    ## 166:         Central Bedfordshire      43.87     56.13
    ## 167:                     Stafford      44.01     55.99
    ## 168:                      Newport      44.01     55.99
    ## 169:        Tonbridge and Malling      44.30     55.70
    ## 170:                   Calderdale      44.32     55.68
    ## 171:                     Coventry      44.40     55.60
    ## 172:                      Gedling      44.45     55.55
    ## 173:                   Bromsgrove      44.63     55.37
    ## 174:              West Lancashire      44.69     55.31
    ## 175:                      Ryedale      44.74     55.26
    ## 176:                  Mid Suffolk      44.77     55.23
    ## 177:                      Fareham      44.90     55.10
    ## 178:                  Bournemouth      45.12     54.88
    ## 179:                     Hastings      45.12     54.88
    ## 180:                     Kirklees      45.33     54.67
    ## 181:                      Swindon      45.34     54.66
    ## 182:                     Broxtowe      45.35     54.65
    ## 183:                     Bridgend      45.36     54.64
    ## 184:            Oadby and Wigston      45.42     54.58
    ## 185:                         Adur      45.43     54.57
    ## 186:                      Wealden      45.50     54.50
    ## 187:                    Sevenoaks      45.62     54.38
    ## 188:                    Broadland      45.63     54.37
    ## 189:       South Northamptonshire      45.66     54.34
    ## 190:                       Slough      45.67     54.33
    ## 191:                   Warrington      45.73     54.27
    ## 192:                    Runnymede      45.74     54.26
    ## 193:              Huntingdonshire      45.76     54.24
    ## 194:                     Bradford      45.77     54.23
    ## 195:                      Babergh      45.81     54.19
    ## 196:                         Bury      45.88     54.12
    ## 197:               Northumberland      45.89     54.11
    ## 198:                   East Devon      45.89     54.11
    ## 199:                 Denbighshire      46.00     54.00
    ## 200:                        Conwy      46.02     53.98
    ## 201:             Bracknell Forest      46.06     53.94
    ## 202:                  Teignbridge      46.10     53.90
    ## 203:                    Charnwood      46.19     53.81
    ## 204:                  Southampton      46.20     53.80
    ## 205:              Carmarthenshire      46.25     53.75
    ## 206:                        Powys      46.26     53.74
    ## 207:                       Sutton      46.28     53.72
    ## 208:            Rhondda Cynon Taf      46.30     53.70
    ## 209:                    Worcester      46.32     53.68
    ## 210:                    Hambleton      46.34     53.66
    ## 211:                   Colchester      46.40     53.60
    ## 212:               North Tyneside      46.60     53.40
    ## 213:                    Mid Devon      46.66     53.34
    ## 214:                         Eden      46.68     53.32
    ## 215:                      Preston      46.69     53.31
    ## 216:                   Tewkesbury      46.75     53.25
    ## 217:                   West Devon      46.80     53.20
    ## 218:              Suffolk Coastal      47.00     53.00
    ## 219:                     Worthing      47.01     52.99
    ## 220:              Welwyn Hatfield      47.01     52.99
    ## 221:                Taunton Deane      47.08     52.92
    ## 222:                    Tandridge      47.16     52.84
    ## 223:                   Chelmsford      47.17     52.83
    ## 224:                       Craven      47.17     52.83
    ## 225:        South Gloucestershire      47.32     52.68
    ## 226:                    Wiltshire      47.51     52.49
    ## 227:                    Eastleigh      47.55     52.45
    ## 228:               North Somerset      47.83     52.17
    ## 229:                Malvern Hills      47.84     52.16
    ## 230:                  Test Valley      48.06     51.94
    ## 231:        Basingstoke and Deane      48.10     51.90
    ## 232:                      Bedford      48.22     51.78
    ## 233:                South Norfolk      48.31     51.69
    ## 234:             Derbyshire Dales      48.44     51.56
    ## 235:            Stratford-on-Avon      48.44     51.56
    ## 236:                     Knowsley      48.44     51.56
    ## 237:                      Swansea      48.49     51.51
    ## 238:                Milton Keynes      48.59     51.41
    ## 239:                 Three Rivers      48.73     51.27
    ## 240:                Cheshire East      48.82     51.18
    ## 241:                    Lancaster      48.92     51.08
    ## 242:                   Canterbury      48.96     51.04
    ## 243:                  West Dorset      48.97     51.03
    ## 244:                    Sheffield      49.01     50.99
    ## 245:                 Surrey Heath      49.02     50.98
    ## 246:             Isle of Anglesey      49.06     50.94
    ## 247:          East Cambridgeshire      49.08     50.92
    ## 248:                   Chichester      49.08     50.92
    ## 249:                   Nottingham      49.16     50.84
    ## 250:                    Hertsmere      49.16     50.84
    ## 251:                   Harborough      49.25     50.75
    ## 252:                  South Bucks      49.30     50.70
    ## 253:    Cheshire West and Chester      49.32     50.68
    ## 254:                   Uttlesford      49.32     50.68
    ## 255:                      Dacorum      49.33     50.67
    ## 256:                      Rutland      49.43     50.57
    ## 257:                    High Peak      49.45     50.55
    ## 258:               Aylesbury Vale      49.50     50.50
    ## 259:         Reigate and Banstead      49.51     50.49
    ## 260:                   Birmingham      49.58     50.42
    ## 261:           East Hertfordshire      49.64     50.36
    ## 262:                     Cherwell      49.69     50.31
    ## 263:                      Watford      49.73     50.27
    ## 264:                        Moray      50.13     49.87
    ## 265:                        Leeds      50.31     49.69
    ## 266:                Monmouthshire      50.44     49.56
    ## 267:               East Hampshire      50.52     49.48
    ## 268:                      Bromley      50.65     49.35
    ## 269:          Newcastle upon Tyne      50.70     49.30
    ## 270:            Vale of Glamorgan      50.73     49.27
    ## 271:                    Harrogate      50.97     49.03
    ## 272:                     Hounslow      51.06     48.94
    ## 273:                       Mendip      51.07     48.93
    ## 274:                    Leicester      51.08     48.92
    ## 275:                     Cotswold      51.10     48.90
    ## 276:                      Horsham      51.46     48.54
    ## 277:                       Wirral      51.70     48.30
    ## 278:               West Berkshire      51.78     48.22
    ## 279:                       Sefton      51.87     48.13
    ## 280:                      Wycombe      51.97     48.03
    ## 281:                        Lewes      52.07     47.93
    ## 282:              Epsom and Ewell      52.08     47.92
    ## 283:                    Stockport      52.33     47.67
    ## 284:                         Hart      52.40     47.60
    ## 285:                       Newham      52.84     47.16
    ## 286:                   South Hams      52.85     47.15
    ## 287:               South Lakeland      52.86     47.14
    ## 288:        Dumfries and Galloway      53.06     46.94
    ## 289:                  Mole Valley      53.08     46.92
    ## 290:                   Mid Sussex      53.09     46.91
    ## 291:             West Oxfordshire      53.66     46.34
    ## 292:       Windsor and Maidenhead      53.90     46.10
    ## 293:                    Redbridge      53.97     46.03
    ## 294:                      Croydon      54.29     45.71
    ## 295:          North Hertfordshire      54.37     45.63
    ## 296:                       Stroud      54.61     45.39
    ## 297:                       Harrow      54.63     45.37
    ## 298:                   Ceredigion      54.63     45.37
    ## 299:              Tunbridge Wells      54.89     45.11
    ## 300:            South Oxfordshire      54.98     45.02
    ## 301:                Aberdeenshire      55.01     44.99
    ## 302:                     Chiltern      55.02     44.98
    ## 303:                  Eilean Siar      55.24     44.76
    ## 304:                        Angus      55.26     44.74
    ## 305:                       Exeter      55.28     44.72
    ## 306:                    Greenwich      55.59     44.41
    ## 307:             Northern Ireland      55.78     44.22
    ## 308:                      Enfield      55.82     44.18
    ## 309:                     Highland      55.95     44.05
    ## 310:                       Woking      56.15     43.85
    ## 311:                   Cheltenham      56.17     43.83
    ## 312:                    Guildford      56.17     43.83
    ## 313:                      Norwich      56.24     43.76
    ## 314:              Isles of Scilly      56.39     43.61
    ## 315:             Shetland Islands      56.51     43.49
    ## 316:                    Wokingham      56.69     43.31
    ## 317:          Vale of White Horse      56.70     43.30
    ## 318:                      Falkirk      56.76     43.24
    ## 319:               North Ayrshire      56.88     43.12
    ## 320:                   Rushcliffe      57.55     42.45
    ## 321:                     Trafford      57.69     42.31
    ## 322:             Clackmannanshire      57.78     42.22
    ## 323: Bath and North East Somerset      57.85     42.15
    ## 324:                      Reading      58.03     41.97
    ## 325:                         York      58.04     41.96
    ## 326:                      Gwynedd      58.05     41.95
    ## 327:                    Liverpool      58.19     41.81
    ## 328:                 West Lothian      58.25     41.75
    ## 329:                     Waverley      58.39     41.61
    ## 330:             Scottish Borders      58.47     41.53
    ## 331:                         Fife      58.59     41.41
    ## 332:                East Ayrshire      58.60     41.40
    ## 333:                      Warwick      58.78     41.22
    ##  [ reached getOption("max.print") -- omitted 49 rows ]

``` r
bar.plot <- gvisBarChart(brexit2, xvar = "Area", yvar = "Pct_Leave", 
                         options = list(legend = "none",
                                        vAxes = "[{textStyle:{fontSize: '16'}}]",
                                        chartArea = "{left:250,top:10,bottom:10}",
                                        width= 800, height = 10000) )
plot(bar.plot)
```

UK ESRI shapefile

``` r
## Download and import from the web (or get from Github)
url.shp <- "https://t.co/GYqX2PCqJ0"
file.zip <- "esri.zip"
if (!file.exists(file.zip)) 
    download.file(url = url.shp, destfile = file.zip, mode = "wb")
file.shp <- grep("shp", unzip(file.zip), ignore.case=TRUE, value=TRUE)  # Unzip the shapefile
uk.shp <- readOGR(file.shp, ogrListLayers(file.shp)[1])  # Load the shapefile
```

    ## OGR data source with driver: ESRI Shapefile 
    ## Source: "./Geotheory-UK-2016-EU-Referendum/Geotheory-UK-2016-EU-Referendum.shp", layer: "Geotheory-UK-2016-EU-Referendum"
    ## with 381 features
    ## It has 21 fields
    ## Integer64 fields read as strings:  id Electrt ExpctdB VrfdBlP Vts_Cst Vld_Vts Remain Leave Rjctd_B N_ffcl_ Vtng___ Wrtng__ Unmrk__

``` r
## Confirm that the brexit data matches the shapefile
names(uk.shp)  # Get the variables in the shapefile
```

    ##  [1] "id"      "Regn_Cd" "Region"  "Area_Cd" "Area"    "Electrt" "ExpctdB"
    ##  [8] "VrfdBlP" "Pct_Trn" "Vts_Cst" "Vld_Vts" "Remain"  "Leave"   "Rjctd_B"
    ## [15] "N_ffcl_" "Vtng___" "Wrtng__" "Unmrk__" "Pct_Rmn" "Pct_Lev" "Pct_Rjc"

``` r
sort(brexit$Area_Code) == sort(uk.shp@data$Area_Cd)  # Are the admin areas the same?
```

    ##   [1] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ##  [15] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ##  [29] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ##  [43] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ##  [57] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ##  [71] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ##  [85] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ##  [99] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ## [113] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ## [127] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ## [141] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ## [155] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ## [169] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ## [183] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ## [197] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ## [211] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ## [225] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ## [239] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ## [253] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ## [267] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ## [281] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ## [295] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ## [309] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ## [323] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ## [337] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ## [351] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ## [365] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ## [379] TRUE TRUE TRUE

``` r
## Convert the raw data to a data.frame for ggplot
uk.shp@data$id <- rownames(uk.shp@data)  # Create an id to match the brexit data
uk.f <- tidy(uk.shp)  # Convert to a dataframe
```

    ## Regions defined for each Polygons

``` r
## If desired, merge the map with brexit data
# uk.map <- merge(uk.f, brexit, by="id", all.x=TRUE)
# uk.map <- uk.map[order(uk.map$order), ]  # Order for plotting purposes
# head(uk.map)
```

Mapping Brexit vote share using ggplot

``` r
map <- ggplot() +
    geom_map(data = brexit, 
             aes(map_id = id, fill = Pct_Leave), 
             map = uk.f) + 
    expand_limits(x = uk.f$long, y = uk.f$lat) + 
    scale_fill_distiller(palette = "Spectral") +
    # scale_fill_gradient(low = "white", high = "#000034") +  Alternative colour option for printing
    ggtitle("Brexit Map: 2016 EU Referendum Vote Share") + 
    labs(fill = "Leave (%)") +
    theme(plot.title = element_text(lineheight = .8, face = "bold"),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank())

plot(map)
```

![](mapping_2016_brexit_vote_files/figure-markdown_github/unnamed-chunk-4-1.png)
