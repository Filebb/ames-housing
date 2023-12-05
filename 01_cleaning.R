library(tidyverse)
library(AmesHousing)

calc_mode <- function(x){

    # from
    # https://www.codingprof.com/how-to-replace-nas-with-the-mode-most-frequent-value-in-r/

    # List the distinct / unique values
    distinct_values <- unique(x)

    # Count the occurrence of each distinct value
    distinct_tabulate <- tabulate(match(x, distinct_values))

    # Return the value with the highest occurrence
    distinct_values[which.max(distinct_tabulate)]

}

ames <- ames_raw |>

    rename(order                     = Order,
           pid                       = PID,
           ms_subclass               = `MS SubClass`,
           ms_zoning                 = `MS Zoning`,
           lot_frontage              = `Lot Frontage`,
           lot_area                  = `Lot Area`,
           street                    = Street,
           alley                     = Alley,
           lot_shape                 = `Lot Shape`,
           land_contour              = `Land Contour`,
           utilities                 = Utilities,
           lot_configuration         = `Lot Config`,
           land_slope                = `Land Slope`,
           neighborhood              = Neighborhood,
           condition_1               = `Condition 1`,
           condition_2               = `Condition 2`,
           building_type             = `Bldg Type`,
           house_style               = `House Style`,
           overall_quality           = `Overall Qual`,
           overall_condition         = `Overall Cond`,
           year_built                = `Year Built`,
           year_remod_add            = `Year Remod/Add`,
           roof_style                = `Roof Style`,
           roof_material             = `Roof Matl`,
           exterior_1                = `Exterior 1st`,
           exterior_2                = `Exterior 2nd`,
           masonry_veneer_type       = `Mas Vnr Type`,
           masonry_veneer_area       = `Mas Vnr Area`,
           exterior_quality          = `Exter Qual`,
           exterior_condition        = `Exter Cond`,
           foundation                = Foundation,
           basement_quality          = `Bsmt Qual`,
           basement_condition        = `Bsmt Cond`,
           basement_exposure         = `Bsmt Exposure`,
           basement_fin_type_1       = `BsmtFin Type 1`,
           basement_area_type_1      = `BsmtFin SF 1`,
           basement_fin_type_2       = `BsmtFin Type 2`,
           basement_area_type_2      = `BsmtFin SF 2`,
           basement_unfinished_area  = `Bsmt Unf SF`,
           basement_total_area       = `Total Bsmt SF`,
           heating                   = Heating,
           heating_quality           = `Heating QC`,
           central_air               = `Central Air`,
           electrical                = Electrical,
           first_floor_area          = `1st Flr SF`,
           second_floor_area         = `2nd Flr SF`,
           low_quality_finished_area = `Low Qual Fin SF`,
           ground_living_area        = `Gr Liv Area`,
           basement_full_bathrooms   = `Bsmt Full Bath`,
           basement_half_bathrooms   = `Bsmt Half Bath`,
           full_bathrooms            = `Full Bath`,
           half_bathrooms            = `Half Bath`,
           bedroom_above_ground      = `Bedroom AbvGr`,
           kitchen_above_ground      = `Kitchen AbvGr`,
           kitchen_quality           = `Kitchen Qual`,
           total_rooms_above_ground  = `TotRms AbvGrd`,
           functional                = Functional,
           fireplaces                = Fireplaces,
           fireplace_quality         = `Fireplace Qu`,
           garage_type               = `Garage Type`,
           garage_year_built         = `Garage Yr Blt`,
           garage_finish             = `Garage Finish`,
           garage_cars               = `Garage Cars`,
           garage_area               = `Garage Area`,
           garage_quality            = `Garage Qual`,
           garage_condition          = `Garage Cond`,
           paved_drive               = `Paved Drive`,
           wood_deck_area            = `Wood Deck SF`,
           open_porch_area           = `Open Porch SF`,
           enclosed_porch_area       = `Enclosed Porch`,
           three_season_porch_area   = `3Ssn Porch`,
           screen_porch_area         = `Screen Porch`,
           pool_area                 = `Pool Area`,
           pool_quality              = `Pool QC`,
           fence                     = Fence,
           misc_feature              = `Misc Feature`,
           misc_value                = `Misc Val`,
           month_sold                = `Mo Sold`,
           year_sold                 = `Yr Sold`,
           sale_type                 = `Sale Type`,
           sale_condition            = `Sale Condition`,
           sale_price                =  SalePrice) |>

    mutate(

        # Order (Discrete): Observation number
        # will be removed

        # PID (Nominal): Parcel identification number  - can be used with city web site for parcel review.
        # will be removed

        # MS SubClass (Nominal): Identifies the type of dwelling involved in the sale.

        ms_subclass = recode_factor(

            factor(ms_subclass),

            "020" = "One Story 1946 and Newer All Styles",
            "030" = "One Story 1945 and Older",
            "040" = "One Story with Finished Attic All Ages",
            "045" = "One and Half Story Unfinished All Ages",
            "050" = "One and Half Story Finished All Ages",
            "060" = "Two Story 1946 and Newer",
            "070" = "Two Story 1945 and Older",
            "075" = "Two and Half Story All Ages",
            "080" = "Split or Multilevel",
            "085" = "Split Foyer",
            "090" = "Duplex All Styles and Ages",
            "120" = "One Story PUD 1946 and Newer",
            "150" = "One and Half Story PUD All Ages",
            "160" = "Two Story PUD 1946 and Newer",
            "180" = "PUD Multilevel Split Level Foyer",
            "190" = "Two Family Conversion All Styles and Ages"),

        # MS Zoning (Nominal): Identifies the general zoning classification of the sale.

        ms_zoning = recode_factor(

            factor(ms_zoning),

            "A (agr)" = "Agriculture",
            "C (all)" = "Commercial",
            "FV"      = "Floating Village Residential",
            "I (all)" = "Industrial",
            "RH"      = "Residential High Density",
            "RL"      = "Residential Low Density",
            "RM"      = "Residential Medium Density"),

        # Lot Frontage (Continuous): Linear feet of street connected to property
        # When values are missing we assume that the value is zero

        lot_frontage = replace_na(lot_frontage, 0L),

        # Lot Area (Continuous): Lot size in square feet.

        # Street (Nominal): Type of road access to property

        street = recode_factor(

            factor(street),

            "Grvl" = "Gravel",
            "Pave" = "Paved"),

        # Alley (Nominal): Type of alley access to property
        # Missing values are coded as "No_Alley_Access"

        alley = replace_na(alley, "No Alley Access"),

        alley = recode_factor(

            factor(alley),

            "Grvl"            = "Gravel",
            "Pave"            = "Paved",
            "No Alley Access" = "No Alley Access"),

        # Lot Shape (Ordinal): General shape of property

        lot_shape = recode_factor(

            factor(lot_shape),

            "IR3" = "Irregular",
            "IR2" = "Moderately Irregular",
            "IR1" = "Slightly Irregular",
            "Reg" = "Regular"),

        # Land Contour (Nominal): Flatness of the property

        land_contour = recode_factor(

            factor(land_contour),

            "Lvl" = "Near Flat Level",
            "Bnk" = "Banked",
            "HLS" = "Hillside",
            "Low" = "Depression"),

        # Utilities (Ordinal): Type of utilities available

        utilities = recode_factor(

            factor(utilities),

            "NoSeWa" = "Electricity Gas",
            "NoSewr" = "Electricity Gas Water",
            "AllPub" = "All Public Utilities"),

        # Lot Config (Nominal): Lot configuration

        lot_configuration = recode_factor(

            factor(lot_configuration),

            "Inside"  = "Inside Lot",
            "Corner"  = "Corner Lot",
            "CulDSac" = "Cul de Sac",
            "FR2"     = "Frontage on 2 Sides",
            "FR3"     = "Frontage on 3 Sides"),

        # Land Slope (Ordinal): Slope of property

        land_slope = recode_factor(

            factor(land_slope),

            "Gtl" = "Gentle Slope",
            "Mod" = "Moderate Slope",
            "Sev" = "Severe Slope"),

        # Neighborhood (Nominal): Physical locations within Ames city limits

        neighborhood = recode_factor(

            factor(neighborhood),

            "Blmngtn"     = "Bloomington Heights",
            "Blueste"     = "Bluestem",
            "BrDale"      = "Briardale",
            "BrkSide"     = "Brookside",
            "ClearCr"     = "Clear Creek",
            "CollgCr"     = "College Creek",
            "Crawfor"     = "Crawford",
            "Edwards"     = "Edwards",
            "Gilbert"     = "Gilbert",
            "Greens"      = "Greens",
            "GrnHill"     = "Green Hills",
            "IDOTRR"      = "Iowa DOT and Rail Road",
            "Landmrk"     = "Landmark",
            "MeadowV"     = "Meadow Village",
            "Mitchel"     = "Mitchell",
            "NAmes"       = "North Ames",
            "NoRidge"     = "Northridge",
            "NPkVill"     = "Northpark Villa",
            "NridgHt"     = "Northridge Heights",
            "NWAmes"      = "Northwest Ames",
            "OldTown"     = "Old Town",
            "SWISU"       = "South and West of Iowa State University",
            "Sawyer"      = "Sawyer",
            "SawyerW"     = "Sawyer West",
            "Somerst"     = "Somerset",
            "StoneBr"     = "Stone Brook",
            "Timber"      = "Timberland",
            "Veenker"     = "Veenker",
            "Hayden Lake" = "Hayden Lake"),

        # Condition 1 (Nominal): Proximity to various conditions

        condition_1 = recode_factor(

            factor(condition_1),

            "Artery" = "Adjacent Arterial Street",
            "Feedr"  = "Adjacent Feeder Street",
            "Norm"   = "Normal",
            "RRNn"   = "Within 200 North South Railroad",
            "RRAn"   = "Adjacent North South Railroad",
            "PosN"   = "Near Positive off Site Feature",
            "PosA"   = "Adjacent Postive off Site Feature",
            "RRNe"   = "Within 200 East West Railroad",
            "RRAe"   = "Adjacent East West Railroad"),

        # Condition 2 (Nominal): Proximity to various conditions (if more than one is present)

        condition_2 = recode_factor(

            factor(condition_2),

            "Artery" = "Adjacent Arterial Street",
            "Feedr"  = "Adjacent Feeder Street",
            "Norm"   = "Normal",
            "RRNn"   = "Within 200 North South Railroad",
            "RRAn"   = "Adjacent North South Railroad",
            "PosN"   = "Near Positive off Site Feature",
            "PosA"   = "Adjacent Postive off Site Feature",
            "RRNe"   = "Within 200 East West Railroad",
            "RRAe"   = "Adjacent East West Railroad"),

        # Bldg Type (Nominal): Type of dwelling

        building_type = recode_factor(

            factor(building_type),

            "1Fam"   = "One Family Detached",
            "2FmCon" = "Two Family Conversion",
            "Duplx"  = "Duplex",
            "TwnhsE" = "Townhouse End Unit",
            "TwnhsI" = "Townhouse Inside Unit"),

        # House Style (Nominal): Style of dwelling

        house_style = recode_factor(

            factor(house_style),

            "1Story" = "One Story",
            "1.5Fin" = "One and Half Story 2nd Level Finished",
            "1.5Unf" = "One and Half Story 2nd Level Unfinished",
            "2Story" = "Two Story",
            "2.5Fin" = "Two and Half Story 2nd Level Finished",
            "2.5Unf" = "Two and Half Story 2nd Level Unfinished",
            "SFoyer" = "Split Foyer",
            "SLvl"   = "Split Level"),

        # Overall Qual (Ordinal): Rates the overall material and finish of the house

        overall_quality = recode_factor(

            factor(overall_quality),

            "1"  = "Very Poor",
            "2"  = "Poor",
            "3"  = "Fair",
            "4"  = "Below Average",
            "5"  = "Average",
            "6"  = "Above Average",
            "7"  = "Good",
            "8"  = "Very Good",
            "9"  = "Excellent",
            "10" = "Very Excellent"),

        # Overall Cond (Ordinal): Rates the overall condition of the house

        overall_condition = recode_factor(

            factor(overall_condition),

            "1"  = "Very Poor",
            "2"  = "Poor",
            "3"  = "Fair",
            "4"  = "Below Average",
            "5"  = "Average",
            "6"  = "Above Average",
            "7"  = "Good",
            "8"  = "Very Good",
            "9"  = "Excellent",
            "10" = "Very Excellent"),

        # Year Built (Discrete): Original construction date

        # Year Remod/Add (Discrete): Remodel date (same as construction date if no remodeling or additions)

        # Roof Style (Nominal): Type of roof

        roof_style = recode_factor(

            factor(roof_style),

            "Flat"    = "Flat",
            "Gable"   = "Gable",
            "Gambrel" = "Gabrel Barn",
            "Hip"     = "Hip",
            "Mansard" = "Mansard",
            "Shed"    = "Shed"),

        # Roof Matl (Nominal): Roof material

        roof_material = recode_factor(

            factor(roof_material),

            "ClyTile" = "Clay or Tile",
            "CompShg" = "Composite Shingle",
            "Membran" = "Membrane",
            "Metal"   = "Metal",
            "Roll"    = "Roll",
            "Tar&Grv" = "Gravel and Tar",
            "WdShake" = "Wood Shakes",
            "WdShngl" = "Wood Shingles"),

        # Exterior 1 (Nominal): Exterior covering on house

        exterior_1 = recode_factor(

            factor(exterior_1),

            "AsbShng" = "Asbestos Shingles",
            "AsphShn" = "Asphalt Shingles",
            "BrkComm" = "Brick Common",
            "BrkFace" = "Brick Face",
            "CBlock"  = "Cinder Block",
            "CemntBd" = "Cement Board",
            "HdBoard" = "Hard Board",
            "ImStucc" = "Imitation Stucco",
            "MetalSd" = "Metal Siding",
            "Other"   = "Other",
            "Plywood" = "Plywood",
            "PreCast" = "PreCast",
            "Stone"   = "Stone",
            "Stucco"  = "Stucco",
            "VinylSd" = "Vinyl Siding",
            "Wd Sdng" = "Wood Siding",
            "WdShing" = "Wood Shingles"),

        # Exterior 2 (Nominal): Exterior covering on house (if more than one material)

        exterior_2 = recode_factor(

            factor(exterior_2),

            "AsbShng" = "Asbestos Shingles",
            "AsphShn" = "Asphalt Shingles",
            "BrkComm" = "Brick Common",
            "BrkFace" = "Brick Face",
            "CBlock"  = "Cinder Block",
            "CemntBd" = "Cement Board",
            "HdBoard" = "Hard Board",
            "ImStucc" = "Imitation Stucco",
            "MetalSd" = "Metal Siding",
            "Other"   = "Other",
            "Plywood" = "Plywood",
            "PreCast" = "PreCast",
            "Stone"   = "Stone",
            "Stucco"  = "Stucco",
            "VinylSd" = "Vinyl Siding",
            "Wd Sdng" = "Wood Siding",
            "WdShing" = "Wood Shingles"),

        # Mas Vnr Type (Nominal): Masonry veneer type
        # Missing values are coded as "None"

        masonry_veneer_type = replace_na(masonry_veneer_type, "None"),

        masonry_veneer_type = recode_factor(

            factor(masonry_veneer_type),

            "BrkCmn"  = "Brick Common",
            "BrkFace" = "Brick Face",
            "CBlock"  = "Cinder Block",
            "None"    = "None",
            "Stone"   = "Stone",
            "None"    = "None"),

        # Mas Vnr Area (Continuous): Masonry veneer area in square feet

        masonry_veneer_area = replace_na(masonry_veneer_area, 0L),

        # Exter Qual (Ordinal): Evaluates the quality of the material on the exterior

        exterior_quality = recode_factor(

            factor(exterior_quality),

            "Po" = "Poor",
            "Fa" = "Fair",
            "TA" = "Average Typical",
            "Gd" = "Good",
            "Ex" = "Excellent"),

        # Exter Cond (Ordinal): Evaluates the present condition of the material on the exterior

        exterior_condition = recode_factor(

            factor(exterior_condition),

            "Po" = "Poor",
            "Fa" = "Fair",
            "TA" = "Average Typical",
            "Gd" = "Good",
            "Ex" = "Excellent"),

        # Foundation (Nominal): Type of foundation

        foundation = recode_factor(

            factor(foundation),

            "BrkTil" = "Brick and Tile",
            "CBlock" = "Cinder Block",
            "PConc"  = "Poured Contrete",
            "Slab"   = "Slab",
            "Stone"  = "Stone",
            "Wood"   = "Wood"),

        # Bsmt Qual (Ordinal): Evaluates the height of the basement

        basement_quality = replace_na(basement_quality, "No Basement"),

        basement_quality = recode_factor(

            factor(basement_quality),

            "No Basement" = "No Basement",
            "Po"          = "Poor",
            "Fa"          = "Fair",
            "TA"          = "Average Typical",
            "Gd"          = "Good",
            "Ex"          = "Excellent"),

        # Bsmt Cond (Ordinal): Evaluates the general condition of the basement

        basement_condition = replace_na(basement_condition, "No Basement"),

        basement_condition = recode_factor(

            factor(basement_condition),

            "No Basement" = "No Basement",
            "Po"          = "Poor",
            "Fa"          = "Fair",
            "TA"          = "Average Typical",
            "Gd"          = "Good",
            "Ex"          = "Excellent"),

        # Bsmt Exposure	(Ordinal): Refers to walkout or garden level walls

        basement_exposure = replace_na(basement_exposure, "No Basement"),

        basement_exposure = recode_factor(

            factor(basement_exposure),

            "No Basement" = "No Basement",
            "No"          = "No Exposure",
            "Mn"          = "Minimum Exposure",
            "Av"          = "Average Exposure",
            "Gd"          = "Good Exposure"),

        # BsmtFin Type 1	(Ordinal): Rating of basement finished area

        basement_fin_type_1 = replace_na(basement_fin_type_1, "No Basement"),

        basement_fin_type_1 = recode_factor(

            factor(basement_fin_type_1),

            "No Basement" = "No Basement",
            "Unf"         = "Unfinshed",
            "LwQ"         = "Low Quality",
            "Rec"         = "Average Rec Room",
            "BLQ"         = "Below Average Living Quarters",
            "ALQ"         = "Average Living Quarters",
            "GLQ"         = "Good Living Quarters"),

        # BsmtFin SF 1 (Continuous): Type 1 finished square feet

        basement_area_type_1 = replace_na(basement_area_type_1, 0L),

        # BsmtFinType 2	(Ordinal): Rating of basement finished area (if multiple types)

        basement_fin_type_2 = replace_na(basement_fin_type_2, "No Basement"),

        basement_fin_type_2 = recode_factor(

            factor(basement_fin_type_2),

            "No Basement" = "No Basement",
            "Unf"         = "Unfinshed",
            "LwQ"         = "Low Quality",
            "Rec"         = "Average Rec Room",
            "BLQ"         = "Below Average Living Quarters",
            "ALQ"         = "Average Living Quarters",
            "GLQ"         = "Good Living Quarters"),

        # BsmtFin SF 2 (Continuous): Type 2 finished square feet

        basement_area_type_2 = replace_na(basement_area_type_2, 0L),

        # Bsmt Unf SF (Continuous): Unfinished square feet of basement area

        basement_unfinished_area = replace_na(basement_unfinished_area, 0L),

        # Total Bsmt SF (Continuous): Total square feet of basement area

        basement_total_area = replace_na(basement_total_area, 0L),

        # Heating (Nominal): Type of heating

        heating = recode_factor(

            factor(heating),

            "Floor" = "Floor Furnace",
            "GasA"  = "Gas Forced Warm Air Furnace",
            "GasW"  = "Gas Hot Water or Steam Heat",
            "Grav"  = "Gravity Furnace",
            "OthW"  = "Hot Water or Steam Heat other than Gas",
            "Wall"  = "Wall Furnace"),

        # HeatingQC (Ordinal): Heating quality and condition

        heating_quality = recode_factor(

            factor(heating_quality),

            "Po" = "Poor",
            "Fa" = "Fair",
            "TA" = "Average Typical",
            "Gd" = "Good",
            "Ex" = "Excellent"),

        # Central Air (Nominal): Central air conditioning

        central_air = recode_factor(

            factor(central_air),

            "N" = "No",
            "Y" = "Yes"),

        # Electrical (Ordinal): Electrical system
        # One missing value will be imputed using the mode

        electrical = factor(electrical),

        # impute mode
        electrical = replace_na(electrical, calc_mode(electrical)),

        electrical = recode_factor(

            electrical,

            "Mix"   = "Mixed",
            "FuseP" = "Fuse Box Poor",
            "FuseF" = "Fuse Box Fair",
            "FuseA" = "Fuse Box Average",
            "SBrkr" = "Standard Circuit Breakers and Romex"),

        # 1st Flr SF (Continuous): First Floor square feet

        # 2nd Flr SF (Continuous)	: Second floor square feet

        # Low Qual Fin SF (Continuous): Low quality finished square feet (all floors)

        # Gr Liv Area (Continuous): Above grade (ground) living area square feet

        # Bsmt Full Bath (Discrete): Basement full bathrooms

        basement_full_bathrooms = replace_na(basement_full_bathrooms, 0L),

        # Bsmt Half Bath (Discrete): Basement half bathrooms

        basement_half_bathrooms = replace_na(basement_half_bathrooms, 0L),

        # Full Bath (Discrete): Full bathrooms above grade

        # Half Bath (Discrete): Half baths above grade

        # Bedroom (Discrete): Bedrooms above grade (does NOT include basement bedrooms)

        # Kitchen (Discrete): Kitchens above grade

        # KitchenQual (Ordinal): Kitchen quality

        kitchen_quality = recode_factor(

            factor(kitchen_quality),

            "Po" = "Poor",
            "Fa" = "Fair",
            "TA" = "Average Typical",
            "Gd" = "Good",
            "Ex" = "Excellent"),

        # TotRmsAbvGrd (Discrete): Total rooms above grade (does not include bathrooms)

        # Functional (Ordinal): Home functionality (Assume typical unless deductions are warranted)

        functional = recode_factor(

            factor(functional),

            "Sal"  = "Salvage Only",
            "Sev"  = "Severely Damaged",
            "Maj2" = "Major Deductions 2",
            "Maj1" = "Major Deductions 1",
            "Mod"  = "Moderate Deductions",
            "Min2" = "Minor Deductions 2",
            "Min1" = "Minor Deductions 1",
            "Typ"  = "Typical Functionality"),

        # Fireplaces (Discrete): Number of fireplaces

        # FireplaceQu (Ordinal): Fireplace quality

        fireplace_quality = replace_na(fireplace_quality, "No Fireplace"),

        fireplace_quality = recode_factor(

            factor(fireplace_quality),

            "No Fireplace" = "No Fireplace",
            "Po"           = "Poor",
            "Fa"           = "Fair",
            "TA"           = "Average Typical",
            "Gd"           = "Good",
            "Ex"           = "Excellent"),

        # Garage Type (Nominal): Garage location

        garage_type = replace_na(garage_type, "No Garage"),

        garage_type = recode_factor(

            factor(garage_type),

            "2Types"    = "More than One Type",
            "Attchd"	= "Attached to Home",
            "Basment"   = "Basement Garage",
            "BuiltIn"   = "Built-In",
            "CarPort"   = "Car Port",
            "Detchd"    = "Detached from Home",
            "No Garage"	= "No Garage"),

        # Garage Yr Blt (Discrete): Year garage was built
        # there are some missing values for this variable, and it is not clear
        # how to deal with them. This variable was removed

        # Garage Finish (Ordinal): Interior finish of the garage

        garage_finish = replace_na(garage_finish, "No Garage"),

        garage_finish = recode_factor(

            factor(garage_finish),

            "No Garage" = "No Garage",
            "Fin"       = "Finished",
            "RFn"       = "Rough Finished",
            "Unf"       = "Unfinished"),

        # Garage Cars (Discrete): Size of garage in car capacity

        garage_cars = replace_na(garage_cars, 0L),

        # Garage Area (Continuous): Size of garage in square feet

        garage_area = replace_na(garage_area, 0L),

        # Garage Qual (Ordinal): Garage quality

        garage_quality = replace_na(garage_quality, "No Garage"),

        garage_quality = recode_factor(

            factor(garage_quality),

            "No Garage" = "No Garage",
            "Po"        = "Poor",
            "Fa"        = "Fair",
            "TA"        = "Average Typical",
            "Gd"        = "Good",
            "Ex"        = "Excellent"),

        # Garage Cond (Ordinal): Garage condition

        garage_condition = replace_na(garage_condition, "No Garage"),

        garage_condition = recode_factor(

            factor(garage_condition),

            "No Garage" = "No Garage",
            "Po"        = "Poor",
            "Fa"        = "Fair",
            "TA"        = "Average Typical",
            "Gd"        = "Good",
            "Ex"        = "Excellent"),

        # Paved Drive (Ordinal): Paved driveway

        paved_drive = replace_na(paved_drive, "No Garage"),

        paved_drive = recode_factor(

            factor(paved_drive),

            "Y" = "Paved",
            "P" = "Partial Pavement",
            "N" = "Dirt Gravel"),

        # Wood Deck SF (Continuous): Wood deck area in square feet

        # Open Porch SF (Continuous): Open porch area in square feet

        # Enclosed Porch (Continuous): Enclosed porch area in square feet

        # 3-Ssn Porch (Continuous): Three season porch area in square feet

        # Screen Porch (Continuous): Screen porch area in square feet

        # Pool Area (Continuous): Pool area in square feet

        # Pool QC (Ordinal): Pool quality

        pool_quality = replace_na(pool_quality, "No Pool"),

        pool_quality = recode_factor(

            factor(pool_quality),

            "No Pool" = "No Pool",
            "Fa"      = "Fair",
            "TA"      = "Average Typical",
            "Gd"      = "Good",
            "Ex"      = "Excellent"),

        # Fence (Ordinal): Fence quality

        fence = replace_na(fence, "No Fence"),

        fence = recode_factor(

            factor(fence),

            "No Fence" = "No Fence",
            "MnWw"     = "Minimum Wood Wire",
            "GdWo"     = "Good Wood",
            "MnPrv"    = "Minimum Privacy",
            "GdPrv"    = "Good Privacy"),

        # Misc Feature (Nominal): Miscellaneous feature not covered in other categories

        misc_feature = replace_na(misc_feature, "None"),

        misc_feature = recode_factor(

            factor(misc_feature),

            "Elev" = "Elevator",
            "Gar2" = "Second Garage if not Described in Garage Section",
            "Othr" = "Other",
            "Shed" = "Shed over 100 SF",
            "TenC" = "Tennis Court",
            "None" = "None"),

        # Misc Val (Continuous): $Value of miscellaneous feature

        # Mo Sold (Discrete): Month Sold (MM)

        month_sold = recode_factor(

            factor(month_sold),

            "1"  = "Jan",
            "2"  = "Feb",
            "3"  = "Mar",
            "4"  = "Apr",
            "5"  = "May",
            "6"  = "Jun",
            "7"  = "Jul",
            "8"  = "Aug",
            "9"  = "Sep",
            "10" = "Oct",
            "11" = "Nov",
            "12" = "Dec"),

        # Yr Sold (Discrete): Year Sold (YYYY)

        # Sale Type (Nominal): Type of sale

        sale_type = recode_factor(

            factor(sale_type),

            "WD "   = "Warranty Deed Conventional",
            "CWD"   = "Warranty Deed Cash",
            "VWD"   = "Warranty Deed VA Loan",
            "New"   = "Home just Constructed and Sold",
            "COD"   = "Court Officer Deed Estate",
            "Con"   = "Contract 15 Down Payment Regular Terms",
            "ConLw" = "Contract Low Down Payment and Low Interest",
            "ConLI" = "Contract Low Interest",
            "ConLD" = "Contract Low Down",
            "Oth"   = "Other"),

        # Sale Condition (Nominal): Condition of sale

        sale_condition = recode_factor(

            factor(sale_condition),

            "Normal"  = "Normal Sale",
            "Abnorml" = "Abnormal Sale",
            "AdjLand" = "Adjoining Land Purchase",
            "Alloca"  = "Allocation",
            "Family"  = "Sale between Family Members",
            "Partial" = "Home was not Completed when Last Assessed"),

        # SalePrice (Continuous): Sale price $$

    )

rm(calc_mode)

ames |> write_rds(file = "ames.RDS")
