# Set working directory
setwd("C:/Textmining GCMs 2.0")

# Define required packages
required_packages <- c(
  "data.table", "tidyverse", "janitor", "stringr", 
  "ggplot2", "reshape2", "cowplot", "patchwork", 
  "ggpubr", "lubridate", "knitr", "sf", 
  "rnaturalearth", "readr", "ggforce", "dplyr", "randomForest", "rpart.plot", "writexl"
)

# Install missing packages only
missing_packages <- setdiff(required_packages, rownames(installed.packages()))
if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

# Load all required packages
invisible(lapply(required_packages, library, character.only = TRUE))

# Set knitr options if knitr is loaded (for RMarkdown use)
if ("knitr" %in% loadedNamespaces()) {
  knitr::opts_chunk$set(echo = TRUE, dev = "tikz", cache = TRUE)
}

# Load Scopus metadata
full.dt <- fread("C:/Textmining GCMs 2.0/Familynames metadata.csv", encoding = "UTF-8", quote = '"', na.strings = "", data.table = TRUE)
full.dt <- clean_names(full.dt)

# --------------------------------------------------------
# Institution and Country Extraction from Affiliations
# --------------------------------------------------------

# Define alias mappings and lists (paste your full alias + institution_list + country_aliases + country list block here)
# -- shortened for brevity --
institution_aliases <- c(
  # Existing aliases
  "barcelona supercomputing center" = "barcelona supercomputing center (bsc)",
  "bsc" = "barcelona supercomputing center (bsc)",
  "oxford university" = "university of oxford",
  "universiteit utrecht" = "utrecht university",
  "inpe" = "national institute for space research",
  "eccc" = "environment and climate change canada",
  "environment and climate change canada (eccc)" = "environment and climate change canada",
  "eth zürich" = "eth zurich",
  "eth zurich" = "eth zurich",
  "eth zÃ¼rich" = "eth zurich",
  "swiss federal institute of technology" = "eth zurich",
  "japan agency for marine-earth science and technology (jamstec)" = "jamstec",
  "jamstec" = "japan agency for marine-earth science and technology (jamstec)",
  "kma" = "korea meteorological administration (kma)",
  "kiost" = "korea institute of ocean science and technology (kiost)",
  "met office hadley centre" = "met office",
  "mri" = "meteorological research institute (mri)",
  "meteorological research institute" = "meteorological research institute (mri)",
  "nus" = "national university of singapore (nus)",
  "national university of singapore" = "national university of singapore",
  "université paris-saclay" = "sorbonne université",
  "universités upmc-cnrs-ird-mnhn" = "sorbonne université",
  "psl research university" = "sorbonne université",
  "ras" = "russian academy of sciences (ras)",
  "russian academy of sciences" = "russian academy of sciences (ras)",
  "sorbonne universitÃ©" = "sorbonne université",
  "universidad nacional autónoma de méxico" = "universidad nacional autonoma de mexico",
  "universidad nacional autÃ³noma de mÃ©xico" = "universidad nacional autónoma de méxico",
  "université catholique de louvain" = "uclouvain",
  "uclouvain" = "uclouvain",
  "université de liège" = "university of liège",
  "uc berkeley" = "university of california",
  "university of california berkeley" = "university of california",
  "university of california, berkeley" = "university of california",
  "lamont-doherty earth observatory of columbia university" = "columbia university",
  "laboratoire des sciences du climat et de l'environnement (lsce)" = "laboratoire des sciences du climat et de l’environnement",
  "ncar" = "national center for atmospheric research",
  "national center for atmospheric research, boulder" = "national center for atmospheric research",
  "university of chinese academy of sciences" = "chinese academy of sciences (cas)",
  "state key laboratory of numerical modeling for atmospheric sciences and geophysical fluid dynamics" = "chinese academy of sciences (cas)",
  "chinese academy of sciences" = "chinese academy of sciences (cas)",
  "chinese academy of sciences / chinese academy of sciences (cas)" = "chinese academy of sciences (cas)",
  "cas" = "chinese academy of sciences (cas)",
  "iit roorkee" = "indian institute of technology roorkee",
  "iit delhi" = "indian institute of technology delhi",
  "iit palakkad" = "indian institute of technology palakkad",
  "cra" = "council for agricultural research and economics",
  "fio" = "first institute of oceanography",
  "csiro" = "commonwealth scientific and industrial research organisation",
  "max-planck-institut für meteorologie" = "max planck institute",
  "max planck institute for meteorology" = "max planck institute",
  "mpi-m" = "max planck institute",
  "nansen centre" = "nansen environmental and remote sensing centre (nersc)",
  "nersc" = "nansen environmental and remote sensing centre (nersc)",
  "uhh" = "university of hamburg",
  "universität hamburg" = "university of hamburg",
  "universidad de sao paulo" = "university of são paulo",
  "universidad de sã£o paulo" = "university of são paulo",
  "university of sao paulo" = "university of são paulo",
  "universidade federal de itajubÃ¡" = "university of itajubá",
  "university of liège" = "university of liège",
  "university of liege" = "university of liège",
  "university of liÃ¨ge" = "university of liège",
  "university of information scienceand technology" = "university of information science and technology",
  "university of informatioscience and technology" = "university of information science and technology",
  "university of information science and technology" = "university of sciences and technology",
  "victoria university" = "university of victoria",
  "cccma" = "university of victoria",
  "university of new york" = "new york university",
  "york university" = "university of york",
  "university of hawai" = "university of hawaii",
  "university of colorado at boulder" = "university of colorado boulder",
  "university of engineering & technology" = "university of engineering and technology",
  "university unbc" = "university of northern british columbia",
  "university of north british columbia" = "university of northern british columbia",
  "university of northern british columbia (unbc)" = "university of northern british columbia",
  
  # NEWLY ADDED GLOBAL UNIVERSITY ABBREVIATIONS
  "mit" = "massachusetts institute of technology",
  "ucb" = "university of california",
  "caltech" = "california institute of technology",
  "ucla" = "university of california, los angeles",
  "uq" = "the university of queensland",
  "umich" = "university of michigan-ann arbor",
  "mcgill" = "mcgill university",
  "unimelb" = "the university of melbourne",
  "utoronto" = "university of toronto",
  "epfl" = "école polytechnique fédérale de lausanne",
  "anu" = "australian national university",
  "unsw" = "the university of new south wales",
  "utokyo" = "the university of tokyo",
  "dtu" = "technical university of denmark",
  "gatech" = "georgia institute of technology",
  "hku" = "the university of hong kong",
  "hkust" = "the hong kong university of science and technology",
  "unam" = "universidad nacional autónoma de méxico",
  "psl" = "université psl",
  "ntu" = "national taiwan university",
  "um" = "universiti malaya",
  "tu berlin" = "technische universität berlin",
  "sorbonne" = "sorbonne university",
  "ntnu" = "norwegian university of science and technology",
  "radboud" = "radboud university",
  "ncku" = "national cheng kung university",
  "rwth" = "rwth aachen university",
  "uclouvain" = "université catholique de louvain",
  "vub" = "vrije universiteit brussel",
  "uab" = "universitat autònoma de barcelona",
  "tuebingen" = "eberhard karls universität tübingen",
  "lmu" = "lmu munich",
  "mpi-m" = "max planck institute for meteorology",
  "kit" = "karlsruhe institute of technology",
  "ucsd" = "university of california, san diego",
  "osu" = "the ohio state university",
  "upc" = "universitat politècnica de catalunya · barcelonatech",
  "upm" = "universiti putra malaysia",
  "utm" = "universiti teknologi malaysia",
  "upenn" = "university of pennsylvania",
  "umd" = "university of maryland",
  "uiuc" = "university of illinois at urbana-champaign",
  "uw-madison" = "university of wisconsin-madison",
  "uci" = "university of california, irvine",
  "cmu" = "carnegie mellon university",
  "polyu" = "the hong kong polytechnic university",
  "uwaterloo" = "university of waterloo",
  "notredame" = "university of notre dame",
  "tum" = "technical university of munich",
  "tokyo tech" = "tokyo institute of technology",
  "vu amsterdam" = "vrije universiteit amsterdam",
  "wustl" = "washington university in st. louis",
  
  
  "université" = NA,
  "universidad" = NA,
  "universiteit" = NA,
  "universitÃ©s" = NA
)

# Define institution list
institution_list <- tolower(c(
  "academia sinica", "al-farahidi university", "alfred wegener institute (awi)", "andhra university", "anna university", "applied physics laboratory",
  "arctic and antarctic research institute (aari)", "argonne national laboratory", "army engineering university of pla", "atmospheric and environmental research (aer)", "atmospheric physics group",
  "atmospheric science and meteorological research center (asmerc)", "australian national university",
  "banaras hindu university", "bangladesh university of engineering and technology",
  "barcelona supercomputing center (bsc)", "begum rokeya university", "beijing climate center",
  "beijing forestry university", "beijing normal university", "brookhaven national laboratory", "bureau of meteorology (bom)", "california institute of technology",
  "center for ocean-land-atmosphere studies", "canadian forest service", "center of advanced studies in arid zones (ceaza)", "central university",
  "centre national de recherches météorologiques (cnrm)", "centre national de la recherche scientifique (cnrs)", "central weather administration",
  "cmcc", "cheikh anta diop university", "chiba university",
  "china electric power research institute (cepri)", "china university of geosciences", "chinese academy of agricultural sciences",
  "chinese academy of meteorological sciences", "chinese academy of sciences (cas)", "chung yuan christian university",
  "colorado state university", "columbia university", "commonwealth scientific and industrial research organisation",
  "cornell university", "danish meteorological institute (dmi)", "delft university of technology",
  "deutsches zentrum für luft- und raumfahrt (dlr)", "dicle university", "dong nai technology university",
  "duke university", "durham university", "eth zurich", "east china normal university", "ec-earth consortium", "egerton university",
  "environment and climate change canada","european centre for medium-range weather forecasts (ecmwf)" ,"ewha womans university","first institute of oceanography", "flanders marine institute (vliz)",
  "florida institute of technology", "florida state university", "fudan university",
  "geological survey of denmark and greenland (geus)", "geophysical institute", "george mason university", "german aerospace center (dlr)",
  "goethe university frankfurt", "griffith university", "guangdong ocean university", "harvard university",
  "heriot-watt university", "hiroshima university", "hohai university", "hunan normal university",
  "illinois state museum", "indian institute of technology delhi", "indian institute of technology palakkad",
  "indian institute of technology roorkee", "indian institute of tropical meteorology", "indian institute of science",
  "indiana university", "institut pierre-simon laplace (ipsl)", "institut teknologi bandung", "universitat autonoma de barcelona",
  "institute of technology of cambodia", "integrated climate system modeling laboratory",
  "international pacific research center", "ipb university", "islamic azad university", "jadavpur university",
  "japan agency for marine-earth science and technology (jamstec)", "jawaharlal nehru university",
  "jeonbuk national university", "korea advanced institute of science and technology", "korea institute of ocean science and technology (kiost)", "korea meteorological administration (kma)", "kyoto university",
  "laboratoire des sciences du climat et de l'environnement (lsce)", "lanzhou university", "lawrence livermore national laboratory (llnl)", "lawrence berkeley national laboratory", "lehigh university",
  "lomonosov moscow state university", "los alamos national laboratory", "lund university", "mahidol university",
  "massachusetts institute of technology", "max planck institute", "mcgill university", "mediterranean consortium",
  "memorial university", "met office", "meteorological research institute (mri)", "middlesex university",
  "ministry of energy water research institute", "monash university", "montana state university",
  "moscow state university", "météo-france", "nanjing normal university", "nanjing university",
  "nansen environmental and remote sensing centre (nersc)", "national aeronautics and space administration (nasa)", "national center for atmospheric research (ncar)", "national central university",
  "national climate center", "national climatic data center", "national institute for space research", "national institute for environmental studies (nies)",
  "national institute of advanced industrial science and technology (aist)", "national oceanic and atmospheric administration (noaa)",
  "national renewable energy laboratory", "national research and innovation agency", "national research council (cnr-isac)", "national taiwan normal university",
  "national taiwan university", "national university of singapore (nus)", "new york university", "newcastle university", "nirma university",
  "north carolina a and t state university", "northeast agricultural university", "northeastern university",
  "northern arizona university", "northwest a&f university", "norwegian climate service service centre (ncss)", "norwegian meteorological institute (met)", "norwegian research centre (norce)", "oak ridge national laboratory", "old dominion university", "omdurman islamic university",
  "open university", "pacific northwest national laboratory", "peking university", "portland state university", "postech", "princeton university",
  "pukyong national university", "punjab agricultural university", "purdue university", "ramkhamhaeng university",
  "ritsumeikan university", "royal netherlands meteorological institute (knmi)", "russian academy of sciences (ras)",
  "samara university", "san diego state university", "sandia national laboratory",
  "sapienza university of rome", "sari agricultural sciences and natural resources university",
  "sejong university", "seoul national university (snu)", "shanghai regional climate center",
  "shaoxing university", "sichuan university", "sorbonne université",
  "southern ocean carbon-climate observatory (socco)", "sri venkateswara university", "stanford university",
  "state grid corporation of china", "stockholm university", "stony brook university",
  "sun yat-sen university", "swedish meteorological and hydrological institute (smhi)", "syracuse university",
  "tamil nadu agricultural university", "texas a&m university", "the pennsylvania state university",
  "thu dau mot university", "thuyloi university", "tohoku university", "tribhuvan university", "tsinghua university",
  "ucl department of geography", "us geological survey", "usda- natural resource conservation center", "uclouvain",
  "unist-gil", "united states cellular corporation", "univ coimbra", "universidad católica del norte",
  "universidad de la habana", "universidad de valladolid", "universidad de vigo", "universidad del norte",
  "universidad indoamérica", "universidad nacional autonoma de mexico",
  "universidad de granada", "universidade de lisboa", "universidade do estado da amazonas",
  "universidade estadual de ponta grossa", "universidade federal de itajubá",
  "universidade federal do maranhão", "universidade federal do rio de janeiro",
  "universidade federal do rio grande do sul", "universidade federal do tocantins",
  "universitas muhammadiyah semarang", "universitat de barcelona", "universiteit utrecht",
  "universiti kebangsaan malaysia", "universiti malaysia terengganu", "universiti sains malaysia",
  "universiti teknologi malaysia", "university avenue", "university college london",
  "university corporation for atmospheric research", "university federal of são paulo",
  "university of agriculture", "university of alberta", "university of alexandria",
  "university of allahabad", "university of arizona", "university of aveiro", "university of baghdad",
  "university of baja california sur", "university of baroda", "university of bergen",
  "university of bern", "university of bristol", "university of british columbia",
  "university of buenos aires", "university of burdwan", "university of calcutta",
  "university of california", "university of california irvine", "university of campina grande",
  "university of chicago", "university of china", "university of cologne", "university of colorado",
  "university of colorado boulder", "university of copenhagen", "university of defense technology",
  "university of east anglia", "university of eastern finland", "university of edinburgh",
  "university of engineering", "university of engineering and technology",
  "university of engineering and information technology", "university of exeter",
  "university of geosciences", "university of ghana", "university of guelph", "university of gujrat","university of hamburg",
  "university of hawaii", "university of helsinki", "university of hertfordshire",
  "university of hong kong", "university of hormozgan", "university of houston",
  "university of hyderabad", "university of illinois at urbana-champaign",
  "university of information science and technology", "university of information technology",
  "university of iowa", "university of itajubá", "university of kerbala", "university of leeds",
  "university of liege", "university of malaya", "university of malaysia", "university of manitoba",
  "university of maryland", "university of mashhad", "university of melbourne",
  "university of miami", "university of michigan", "university of montana",
  "university of moratuwa", "university of new hampshire", "university of new jersey",
  "university of new mexico", "university of new south wales", "university of new york",
  "university of northern british columbia", "university of oslo", "university of oxford",
  "university of paraná", "university of rajasthan", "university of reading",
  "university of rhode island", "university of rio de janeiro", "university of rostock",
  "university of rwanda", "university of saskatchewan",
  "university of science and technology", "university of science and technology of china",
  "university of science and technology of hanoi", "university of sciences and technology",
  "university of singapore", "university of south bihar", "university of southampton",
  "university of são paulo", "university of technology", "university of technology thonburi",
  "university of texas at austin", "university of texas at san antonio",
  "university of the philippines-diliman", "university of the west indies",
  "university of tokyo", "university of toronto", "university of toulouse", "university of trento",
  "university of trier", "university of tromsø", "university of tsukuba", "university of turin",
  "university of utah", "university of venice", "university of victoria", "university of warwick",
  "university of washington", "university of wisconsin", "university of wisconsin-madison",
  "university of yaounde", "university of york", "university of zurich", "university putra malaysia",
  "university of amsterdam", "university of cambridge", "university of cape town",
  "university of kwazulu-natal", "university of maine", "university of queensland",
  "university of sydney", "university of the free state", "università di bologna",
  "università di cagliari", "universität berlin", "universität bonn",
  "universität wien", "université - cnrs-ird-mnhn", "université catholique de louvain",
  "université de bretagne occidentale (ubo)", "université du québec à montréal",
  "université d’abomey-calavi", "université laval", "université paris-saclay",
  "université pierre et marie curie", "université de toulouse", "uppsala university",
  "us geological survey", "usda forest service", "utah state university", "utrecht university",
  "victoria university", "virginia polytechnic institute and state university",
  "wageningen university", "walailak university", "waseda university", "washington state university",
  "westfield state university", "wuhan university", "wuxi university", "xiamen university",
  "yale university", "yonsei university", "york university", "yunnan university",
  "zhejiang university", "aalborg university",
  ##########      missing universities of the top 450 (environment and technology)
  "aalto university",
  "aarhus university",
  "aix-marseille university",
  "albert-ludwigs-universitaet freiburg",
  "asian institute of technology",
  "bangor university",
  "bauhaus university, weimar",
  "beijing institute of technology",
  "boston university",
  "catholic university of eichstätt-ingolstadt",
  "cardiff university",
  "chinese academy of sciences (cas)",
  "christian-albrechts-university zu kiel",
  "city university of hong kong (cityuhk)",
  "clausthal university of technology",
  "complutense university of madrid",
  "cranfield university",
  "czech university of life sciences in prague",
  "dalhousie university",
  "darmstadt university of applied sciences",
  "deakin university",
  "durham university",
  "eberhard karls universität tübingen",
  "florida international university",
  "florida state university",
  "frankfurt university of applied sciences",
  "friedrich alexander university erlangen-nürnberg",
  "goethe-university frankfurt am main",
  "griffith university",
  "gwangju institute of science and technology (gist)",
  "hamburg university of applied sciences",
  "hannover medical school",
  "heinrich heine university düsseldorf",
  "helmholtz zentrum münchen",
  "heriot-watt university",
  "hildesheim university",
  "huazhong university of science and technology",
  "ilmenau university of technology",
  "indian institute of science",
  "indian institute of technology kharagpur (iit-kgp)",
  "indiana university bloomington",
  "iowa state university",
  "istanbul technical university",
  "jacobs university bremen",
  "kasetsart university",
  "karlsruhe institute of technology (kit)",
  "karlsruhe university of applied sciences",
  "kyushu university",
  "leibniz university hannover",
  "leiden university",
  "linköping university",
  "linnaeus university",
  "lomonosov moscow state university",
  "loughborough university",
  "ludwig-maximilians-universität münchen",
  "luleå university of technology",
  "macquarie university (sydney, australia)",
  "malmö university",
  "martin luther university of halle-wittenberg",
  "masaryk university",
  "max planck institute for meteorology",
  "mid sweden university",
  "nagoya university",
  "nankai university",
  "national cheng kung university (ncku)",
  "norwegian university of science and technology (ntnu)",
  "örebro university",
  "otto von guericke university magdeburg",
  "philipps university of marburg",
  "politecnico di milano",
  "pusan national university",
  "qatar university",
  "queen mary university of london",
  "queen's university at kingston",
  "queensland university of technology (qut)",
  "radboud university",
  "rice university",
  "rmit university",
  "rwth aachen university",
  "saarland university",
  "sapienza university of rome",
  "simon fraser university",
  "södertörn university",
  "south china university of technology",
  "stockholm university",
  "sun yat-sen university",
  "sungkyunkwan university (skku)",
  "technical university of braunschweig",
  "technical university of denmark",
  "technische universität berlin (tu berlin)",
  "tohoku university",
  "tongji university",
  "trinity college dublin",
  "tu berlin (technische universität berlin)",
  "tu dortmund university",
  "tu dresden (technische universität dresden)",
  "tu hamburg",
  "tu ilmenau (technische universität ilmenau)",
  "universidad nacional autónoma de méxico (unam)",
  "universidad politécnica de madrid (upm)",
  "universidade estadual de campinas (unicamp)",
  "universidade federal do rio grande do sul",
  "universitat politècnica de catalunya · barcelonatech (upc)",
  "universitat de valencia",
  "université catholique de louvain",
  "université de liège",
  "université de strasbourg",
  "université grenoble alpes",
  "université laval",
  "universität bremen",
  "universität der bundeswehr münchen",
  "universität hamburg",
  "university of alabama",
  "university of augsburg",
  "university of bari aldo moro",
  "university of bayreuth",
  "university of bologna",
  "university of bonn",
  "university of bremen",
  "university of canterbury",
  "university of canterbury",
  "university of cincinnati",
  "university of cologne",
  "university of connecticut",
  "university of delaware",
  "university of duisburg-essen",
  "university of eastern finland",
  "university of erlangen-nuremberg",
  "university of essex",
  "university of florence",
  "university of geneva",
  "university of georgia",
  "university of gothenburg",
  "university of granada",
  "university of greifswald",
  "university of göttingen",
  "university of guelph",
  "university of hagen",
  "university of hawaiʻi at mānoa",
  "university of heidelberg",
  "university of hohenheim",
  "university of hildesheim",
  "university of illinois at chicago",
  "university of illinois chicago (uic)",
  "university of innsbruck",
  "university of jena",
  "university of kaiserslautern",
  "university of kansas",
  "university of kassel",
  "university of kiel",
  "university of koblenz-landau",
  "university of konstanz",
  "university of lausanne",
  "university of leicester",
  "university of liège",
  "university of lisbon",
  "university of ljubljana",
  "university of mainz",
  "university of mannheim",
  "university of marburg",
  "university of milan",
  "university of montreal",
  "university of nebraska",
  "university of new mexico",
  "university of nottingham",
  "university of oldenburg",
  "university of osnabrück",
  "university of oulu",
  "university of padua",
  "university of palermo",
  "university of parma",
  "university of passau",
  "university of pennsylvania",
  "university of pisa",
  "university of porto",
  "university of potsdam",
  "university of pretoria",
  "university of regensburg",
  "university of renes",
  "university of rome tor vergata",
  "university of rostock",
  "university of santiago de compostela",
  "university of sassari",
  "university of seville",
  "university of siegen",
  "university of south carolina",
  "university of south florida",
  "university of split",
  "university of st andrews",
  "university of stuttgart",
  "university of surrey",
  "university of szeged",
  "university of tartu",
  "university of tasmania",
  "university of texas at dallas",
  "university of thessaloniki",
  "university of torino",
  "university of trento",
  "university of trieste",
  "university of tromsø",
  "university of trier",
  "university of tübingen",
  "university of turin",
  "university of turku",
  "university of ulm",
  "university of vaasa",
  "university of valencia",
  "university of valladolid",
  "university of vancouver",
  "university of vechta",
  "university of vienna",
  "university of vigo",
  "university of vilnius",
  "university of virginia",
  "university of waikato",
  "university of warsaw",
  "university of waterloo",
  "university of western australia",
  "university of wisconsin",
  "university of wrocław",
  "university of wuppertal",
  "university of würzburg",
  "university of zagreb",
  "university of zaragoza",
  "ural federal university",
  "virginia polytechnic institute and state university",
  "vrije universiteit amsterdam",
  "vrije universiteit brussel (vub)",
  "washington state university",
  "washington university in st. louis",
  "western university",
  "whu – otto beisheim school of management",
  "witten/herdecke university",
  "xiamen university",
  "xiangtan university",
  "yokohama national university",
  "yonsei university",
  "zeppelin university",
  "école des ponts paristech"
))

# Country alias corrections
country_aliases <- c(
  "usa" = "united states",
  "england" = "united kingdom",
  "republic of korea" = "south korea",
  "korea,? south" = "south korea",
  "pr china" = "china",
  "russian federation" = "russia",
  "viet nam" = "vietnam",
  "czechia" = "czech republic",
  "slovak republic" = "slovakia",
  "taiwan,? province of china" = "taiwan",
  "brasil" = "brazil",
  "holland" = "netherlands"
)

country_list <- tolower(c(
  "Afghanistan", "Albania", "Algeria", "Andorra", "Angola", "Antigua and Barbuda", "Argentina", 
  "Armenia", "Australia", "Austria", "Azerbaijan", "Bahamas", "Bahrain", "Bangladesh", "Barbados", 
  "Belarus", "Belgium", "Belize", "Benin", "Bhutan", "Bolivia", "Bosnia and Herzegovina", "Botswana", 
  "Brazil", "Brunei", "Bulgaria", "Burkina Faso", "Burundi", "Cabo Verde", "Cambodia", "Cameroon", 
  "Canada", "Central African Republic", "Chad", "Chile", "China", "Colombia", "Comoros", "Congo", 
  "Costa Rica", "Croatia", "Cuba", "Cyprus", "Czech Republic", "Democratic Republic of the Congo", 
  "Denmark", "Djibouti", "Dominica", "Dominican Republic", "Ecuador", "Egypt", "El Salvador", 
  "Equatorial Guinea", "Eritrea", "Estonia", "Eswatini", "Ethiopia", "Fiji", "Finland", "France", 
  "Gabon", "Gambia", "Georgia", "Germany", "Ghana", "Greece", "Grenada", "Guatemala", "Guinea", 
  "Guinea-Bissau", "Guyana", "Haiti", "Honduras", "Hungary", "Iceland", "India", "Indonesia", 
  "Iran", "Iraq", "Ireland", "Israel", "Italy", "Ivory Coast", "Jamaica", "Japan", "Jordan", 
  "Kazakhstan", "Kenya", "Kiribati", "Kuwait", "Kyrgyzstan", "Laos", "Latvia", "Lebanon", 
  "Lesotho", "Liberia", "Libya", "Liechtenstein", "Lithuania", "Luxembourg", "Madagascar", 
  "Malawi", "Malaysia", "Maldives", "Mali", "Malta", "Marshall Islands", "Mauritania", 
  "Mauritius", "Mexico", "Micronesia", "Moldova", "Monaco", "Mongolia", "Montenegro", 
  "Morocco", "Mozambique", "Myanmar", "Namibia", "Nauru", "Nepal", "Netherlands", 
  "New Zealand", "Nicaragua", "Niger", "Nigeria", "North Korea", "North Macedonia", 
  "Norway", "Oman", "Pakistan", "Palau", "Palestine", "Panama", "Papua New Guinea", 
  "Paraguay", "Peru", "Philippines", "Poland", "Portugal", "Qatar", "Republic of the Congo", 
  "Romania", "Russia", "Rwanda", "Saint Kitts and Nevis", "Saint Lucia", 
  "Saint Vincent and the Grenadines", "Samoa", "San Marino", "Sao Tome and Principe", 
  "Saudi Arabia", "Senegal", "Serbia", "Seychelles", "Sierra Leone", "Singapore", 
  "Slovakia", "Slovenia", "Solomon Islands", "Somalia", "South Africa", "South Korea", 
  "South Sudan", "Spain", "Sri Lanka", "Sudan", "Suriname", "Sweden", "Switzerland", 
  "Syria", "Taiwan", "Tajikistan", "Tanzania", "Thailand", "Timor-Leste", "Togo", 
  "Tonga", "Trinidad and Tobago", "Tunisia", "Turkey", "Turkmenistan", "Tuvalu", 
  "Uganda", "Ukraine", "United Arab Emirates", "United Kingdom", "United States", 
  "Uruguay", "Uzbekistan", "Vanuatu", "Vatican City", "Venezuela", "Vietnam", 
  "Yemen", "Zambia", "Zimbabwe", "Hong Kong", "Kosovo"
))

# Function to extract core institution(s) from affiliation
extract_institutions <- function(affiliation) {
  if (is.na(affiliation) || str_trim(affiliation) == "") return(NA)
  parts <- unlist(str_split(affiliation, ",|;|/"))
  parts <- str_trim(tolower(parts))
  found <- table(sapply(institution_list, function(inst) {
    if (any(str_detect(parts, fixed(inst)))) return(inst)
    return(NA)
  }))
  found <- found[names(found) != "NA"]
  found <- found[found > 0]
  if (length(found) == 0) return(NA)
  return(paste(rep(names(found), found), collapse = " / "))
}

# Function to extract country from affiliation
extract_country <- function(affiliation) {
  if (is.na(affiliation) || str_trim(affiliation) == "") return(NA)
  
  affil_clean <- tolower(affiliation)
  
  # Apply alias replacements
  for (alias in names(country_aliases)) {
    affil_clean <- str_replace_all(affil_clean, alias, country_aliases[[alias]])
  }
  
  # Match full country names in text in the order they appear
  for (cntry in country_list) {
    if (str_detect(affil_clean, fixed(cntry))) {
      return(cntry)
    }
  }
  
  return(NA)
}


# Apply extraction
full.dt[, institution := sapply(affiliations, extract_institutions)]
full.dt[, country := sapply(affiliations, extract_country)]

# --------------------------------------------------------
# Continue with original analysis
# --------------------------------------------------------

# Define GCM model family list
gcms <- c(
  "ACCESS", "AWI", "BCC", "BNU", "CAMS", "CAS", "CIESM", "CCSM", "CESM",
  "CFSv2", "CMCC", "CNRM", "CanESM", "E3SM", "EC-Earth", "ECMWF-IFS", "FGOALS", "FIO",
  "GFDL", "GEOS", "GISS", "HadGEM", "HiRAM", "ICON", "IITM", "INM", "IPSL", "KACE",
  "KIOST", "MCM-UA", "MIROC", "MPI-ESM", "MRI", "NESM", "NICAM", "NorESM", "SAM0", "TaiESM", "UKESM"
)

gcm_origin_country <- data.frame(
  GCM = c(
    "ACCESS", "AWI", "BCC", "BNU", "CAMS", "CAS", "CIESM", "CCSM", "CESM",
    "CFSv2", "CMCC", "CNRM", "CanESM", "E3SM", "EC-Earth", "ECMWF-IFS", "FGOALS", "FIO",
    "GFDL", "GEOS", "GISS", "HadGEM", "HiRAM", "ICON", "IITM", "INM", "IPSL", "KACE",
    "KIOST", "MCM-UA", "MIROC", "MPI-ESM", "MRI", "NESM", "NICAM", "NorESM", "SAM0", "TaiESM", "UKESM"
  ),
  Origin_Country = c(
    "australia",             # ACCESS
    "germany",               # AWI
    "china",                 # BCC
    "china",                 # BNU
    "china",                 # CAMS
    "china",                 # CAS
    "italy",                 # CIESM
    "united states",         # CCSM
    "united states",         # CESM
    "united states",         # CFSv2
    "italy",                 # CMCC
    "france",                # CNRM
    "canada",                # CanESM
    "united states",         # E3SM 
    "europe (Consortium)",   # EC-Earth
    "europe (Consortium)",   # ECMWF-IFS
    "china",                 # FGOALS
    "china",                 # FIO
    "united states",         # GFDL
    "united states",         # GEOS 
    "united states",         # GISS 
    "united kingdom",        # HadGEM
    "united states",         # HiRAM 
    "germany",               # ICON
    "india",                 # IITM
    "russia",                # INM
    "france",                # IPSL
    "south korea",           # KACE
    "south korea",           # KIOST
    "mexico",                # MCM-UA 
    "japan",                 # MIROC
    "germany",               # MPI-ESM
    "japan",                 # MRI
    "china",                 # NESM
    "japan",                 # NICAM
    "norway",                # NorESM
    "south korea",           # SAM0 
    "taiwan",                # TaiESM
    "united kingdom"         # UKESM
  ),
  
  
  stringsAsFactors = FALSE)


############## helper
extract_gcm_mentions <- function(text_raw) {
  if (is.na(text_raw) || stringr::str_trim(text_raw) == "") return(NA_character_)
  
  hits <- character(0)
  
  for (m in gcms) {
    if (stringr::str_detect(text_raw,
                            stringr::regex(paste0("\\b", m, "\\b"), ignore_case = FALSE))) {
      hits <- c(hits, m)
    }
  }
  
  if (length(hits) == 0) return(NA_character_)
  paste(sort(unique(hits)), collapse = " / ")
}









# Combine text fields
# Create full_text_raw (preserves original case) and full_text (lowercase)
full.dt[, full_text_raw := paste(title, abstract, author_keywords, index_keywords, sep = " ")]
full.dt[, full_text := tolower(full_text_raw)]

# ------------------------------------------------------------------
# Exact-case, word-boundary search for *every* model in `gcms`
# ------------------------------------------------------------------
for (model in gcms) {
  colname  <- gsub("-", "_", model)
  pattern  <- paste0("\\b", model, "\\b")
  
  full.dt[, (colname) :=
            fifelse(str_detect(full_text_raw,
                               regex(pattern, ignore_case = FALSE)),
                    1L, 0L)]
}


# Melt and summarise
melted <- melt(full.dt, id.vars = "institution", measure.vars = gsub("-", "_", gcms), variable.name = "GCM", value.name = "Mention")

summary_table <- melted %>%
  group_by(institution, GCM) %>%
  summarise(mentions = sum(Mention, na.rm = TRUE)) %>%
  pivot_wider(names_from = GCM, values_from = mentions, values_fill = 0)

fwrite(summary_table, "GCM_mentions_by_institution.csv")

# GCM popularity
# --------------------------------------------
# Step 1: Normalize and split institutions
# --------------------------------------------
# Split institutions into separate rows
melted_institution_split <- melted %>%
  filter(!is.na(institution)) %>%
  mutate(institution = str_split(institution, "/")) %>%
  unnest(institution) %>%
  mutate(institution = str_trim(institution))

melted_institution_split <- melted_institution_split %>%
  mutate(
    GCM = str_replace_all(GCM, "_", "-")
  )


# --------------------------------------------
# Step 2: Count GCM mentions per institution
# --------------------------------------------
institution_gcm_ranking <- melted_institution_split %>%
  group_by(institution, GCM) %>%
  summarise(frequency = sum(Mention, na.rm = TRUE), .groups = "drop") %>%
  filter(frequency > 0)

cm_origin_institution <- data.frame(
  GCM = c(
    "ACCESS", "AWI", "BCC", "BNU", "CAMS", "CAS", "CIESM", "CCSM", "CESM",
    "CFSv2", "CMCC", "CNRM", "CanESM", "E3SM", "EC-Earth", "ECMWF-IFS", "FGOALS", "FIO",
    "GFDL", "GEOS", "GISS", "HadGEM", "HiRAM", "ICON", "IITM", "INM", "IPSL", "KACE",
    "KIOST", "MCM-UA", "MIROC", "MPI-ESM", "MRI", "NESM", "NICAM", "NorESM","SAM0", "TaiESM", "UKESM"
  ),
  Origin_Institution = c(
    "commonwealth scientific and industrial research organisation / bureau of meteorology (bom)",
    "alfred wegener institute (awi)",
    "beijing climate center",
    "beijing normal university",
    "chinese academy of meteorological sciences",
    "chinese academy of sciences (cas)",
    "mediterranean consortium",
    "national center for atmospheric research (ncar)",
    "national center for atmospheric research (ncar)",
    "center for ocean-land-atmosphere studies / national oceanic and atmospheric administration (noaa)",
    "cmcc",
    "météo-france / centre national de la recherche scientifique (cnrs)",
    "environment and climate change canada",
    "lawrence livermore national laboratory (llnl) / los alamos national laboratory, argonne national laboratory / oak ridge national laboratory / sandia national laboratories / brookhaven national laboratory / lawrence berkeley national laboratory / pacific northwest national laboratory / national renewable energy laboratory",
    "ec-earth consortium",
    "european centre for medium-range weather forecasts (ecmwf)",
    "chinese academy of sciences (cas)",
    "first institute of oceanography",
    "national oceanic and atmospheric administration (noaa)",
    "nasa",
    "nasa",
    "met office",
    "national oceanic and atmospheric administration (noaa)",
    "max planck institute",
    "indian institute of tropical meteorology",
    "russian academy of sciences (ras)",
    "institut pierre-simon laplace (ipsl)",
    "korea meteorological administration (kma)",
    "korea institute of ocean science and technology (kiost)",
    "university of arizona",
    "university of tokyo / national institute for environmental studies (nies) / japan agency for marine-earth science and technology (jamstec)",
    "max planck institute",
    "meteorological research institute (mri)",
    "national climate center",
    "japan agency for marine-earth science and technology (jamstec)",
    "nansen environmental and remote sensing centre (nersc) / norwegian meteorological institute (met) / university of bergen / norwegian research centre (norce)",
    "seoul national university (snu)",
    "academia sinica / national central university / central weather administration",
    "met office"
  ),
  stringsAsFactors = FALSE
)
# Show models without institutions (NA or empty string)


"Mediterranean Consortium"
# --------------------------------------------
# Step 4: Join with origin information
# --------------------------------------------
# Make GCM casing uniform (lowercase for both tables)
institution_gcm_ranking <- institution_gcm_ranking %>%
  mutate(GCM = tolower(GCM))

cm_origin_institution <- cm_origin_institution %>%
  mutate(GCM = tolower(GCM))

# Join based on lowercase GCM
institution_gcm_ranking <- institution_gcm_ranking %>%
  left_join(cm_origin_institution, by = "GCM") %>%
  arrange(institution, desc(frequency))


# --------------------------------------------
# Step 5: Export final dataset
# --------------------------------------------
fwrite(institution_gcm_ranking, "GCM_mentions_ranking_by_institution.csv")



# GCM ranking per country
melted_country <- melt(full.dt, id.vars = "country", measure.vars = gsub("-", "_", gcms), variable.name = "GCM", value.name = "Mention")
# Split countries into separate rows
melted_country_split <- melted_country %>%
  filter(!is.na(country)) %>%
  mutate(country = str_split(country, "/")) %>%
  unnest(country) %>%
  mutate(country = str_trim(country))

# Now group by individual countries
# Group by individual countries and count mentions
country_gcm_ranking <- melted_country_split %>%
  group_by(country, GCM) %>%
  summarise(frequency = sum(Mention, na.rm = TRUE), .groups = "drop") %>%
  filter(frequency > 0) %>%
  mutate(GCM = str_replace_all(GCM, "_", "-")) %>%
  left_join(gcm_origin_country, by = "GCM")

# Handle duplicate columns if they exist
if ("Origin_Country.x" %in% names(country_gcm_ranking) && "Origin_Country.y" %in% names(country_gcm_ranking)) {
  country_gcm_ranking <- country_gcm_ranking %>%
    mutate(Origin_Country = coalesce(Origin_Country.x, Origin_Country.y)) %>%
    select(-Origin_Country.x, -Origin_Country.y)
}

# Sort and export
country_gcm_ranking <- country_gcm_ranking %>%
  arrange(country, desc(frequency))

fwrite(country_gcm_ranking, "GCM_mentions_ranking_by_country.csv")


# GCM mention frequency by research goal
goal_keywords <- list(
  "carbon cycle" = "(?i)carbon cycle",
  "ITCZ" = "(?i)ITCZ|Intertropical Convergence Zone",
  "sea level rise" = "(?i)sea level rise",
  "El Niño" = "(?i)El Niño|ENSO|El Nino|El Niño|El Niño–Southern Oscillation",
  "monsoon" = "(?i)monsoon|Asian monsoon|Indian monsoon|West African monsoon|monsoonal",
  "tropical cyclone" = "(?i)tropical cyclone",
  "Hadley circulation" = "(?i)Hadley circulation",
  "Arctic amplification" = "(?i)Arctic amplification",
  "ocean heat content" = "(?i)ocean heat content",
  "jet stream" = "(?i)jet stream"
)

full.dt[, article_id := .I]
goal_links <- rbindlist(lapply(names(goal_keywords), function(goal) {
  pattern <- goal_keywords[[goal]]
  full.dt[grepl(pattern, full_text), .(article_id, goal)]
}))

goal_merged <- merge(goal_links, full.dt, by = "article_id")

melted_goals <- melt(goal_merged,
                     id.vars = c("goal"),
                     measure.vars = gsub("-", "_", gcms),
                     variable.name = "GCM",
                     value.name = "Mention")

goal_gcm_ranking <- melted_goals %>%
  group_by(goal, GCM) %>%
  summarise(frequency = sum(Mention, na.rm = TRUE)) %>%
  filter(frequency > 0) %>%
  arrange(goal, desc(frequency)) %>%
  ungroup()
fwrite(goal_gcm_ranking, "GCM_mentions_ranking_by_research_goal.csv")


######################
###   Model Popularity
######################



model_popularity <- melted %>%
  group_by(GCM) %>%
  summarise(total_mentions = sum(Mention, na.rm = TRUE)) %>%
  arrange(desc(total_mentions))

# View the top few models
print(head(model_popularity, 10))

# Save to CSV
fwrite(model_popularity, "GCM_model_popularity.csv")


######### for papers with multiple institutions


# --------------------------------------------
# 1. Extract all institutions per paper
# --------------------------------------------

extract_all_institutions <- function(affiliation) {
  if (is.na(affiliation) || str_trim(affiliation) == "") return(NA)
  parts <- unlist(str_split(affiliation, ",|;|/"))
  parts <- str_trim(tolower(parts))
  found <- unique(sapply(institution_list, function(inst) {
    if (any(str_detect(parts, fixed(inst)))) return(inst)
    return(NA)
  }))
  found <- found[!is.na(found)]
  if (length(found) == 0) return(NA)
  return(paste(sort(found), collapse = " / "))
}

full.dt[, institutions_combined := sapply(affiliations, extract_all_institutions)]

# --------------------------------------------
# 2. Melt GCM columns to long format
# --------------------------------------------

gcm_cols <- gsub("-", "_", gcms)

gcm_long <- data.table::melt(
  full.dt,
  id.vars = c("title", "institutions_combined"),
  measure.vars = gcm_cols,
  variable.name = "GCM",
  value.name = "Mention"
)

setDT(gcm_long)

# Standardize GCM format
gcm_long[, GCM := str_to_upper(str_replace_all(GCM, "_", "-"))]

# --------------------------------------------
# 3. Filter and summarise
# --------------------------------------------

gcm_mentions_by_paper <- gcm_long[
  !is.na(institutions_combined) & Mention > 0
][
  , .(frequency = sum(Mention)), by = .(title, institutions_combined, GCM)
][
  order(-frequency)
]

# --------------------------------------------
# 4. Add Originating Institution
# --------------------------------------------

# Standardize GCM names in origin table
cm_origin_institution$GCM <- str_to_upper(str_replace_all(cm_origin_institution$GCM, "_", "-"))

# Ensure character type before merge
gcm_mentions_by_paper$GCM <- as.character(gcm_mentions_by_paper$GCM)
cm_origin_institution$GCM <- as.character(cm_origin_institution$GCM)

# Merge with origin institution
gcm_mentions_by_paper <- merge(
  gcm_mentions_by_paper,
  cm_origin_institution,
  by = "GCM",
  all.x = TRUE
)

# --------------------------------------------
# 5. Export to CSV
# --------------------------------------------

fwrite(gcm_mentions_by_paper, "GCM_mentions_by_combined_institutions.csv")



################################
# ──────────────────────────────────────────────────────────────
# Show 10 random publications with all extracted countries
# ──────────────────────────────────────────────────────────────
# ────────────────────────────────────────────────────────────
# Show 10 random publications with title + 4 extracted fields
# ────────────────────────────────────────────────────────────
library(data.table)

## 1. If needed, generate the helper columns ----
if (!("gcms_mentioned" %in% names(full.dt))) {
  gcm_cols <- gsub("-", "_", gcms)  # convert names like "MPI-ESM" to "MPI_ESM"
  
  full.dt[, gcms_mentioned := apply(.SD, 1, function(x) {
    present <- names(which(x == 1))
    present <- toupper(gsub("_", "-", present))
    if (length(present) == 0) NA_character_ else paste(sort(present), collapse = " / ")
  }), .SDcols = gcm_cols]
}

if (!("goals_extracted" %in% names(full.dt))) {
  full.dt[, goals_extracted := sapply(full_text, function(txt) {
    goals <- names(goal_keywords)[
      vapply(goal_keywords, function(pat) grepl(pat, txt, perl = TRUE), logical(1))
    ]
    if (length(goals) == 0) NA_character_ else paste(sort(goals), collapse = " / ")
  })]
}

## 2. Sample 10 publications ----
set.seed(42)
sample_rows <- full.dt[sample(.N, 20)]

## 3. Create summary table ----
random_summary <- sample_rows[, .(
  title,
  institutions_extracted   = institutions_combined,
  country_extracted        = country,
  research_goals_extracted = goals_extracted,
  gcms_extracted           = gcms_mentioned
)]

## 4. Show in console and save to file
print(random_summary)
fwrite(random_summary, "random_10_publications_table.csv")







fwrite(gcm_mentions_by_paper, "GCM_mentions_by_combined_institutions.csv")


