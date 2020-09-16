
label define ER10001L  ///
       1 "Release number 1 - April 1999"  ///
       2 "Release number 2 - May 1999"  ///
       3 "Release number 3- June 1999"  ///
       4 "Release number 4 - May 2008"  ///
       5 "Release number 5 - November 2013"  ///
       6 "Release number 6 - January 2016"  ///
       7 "Release number 7 - March 2016"
forvalues n = 1/20 {
    label define ER10008L `n' "Actual number"  , modify
}

label define ER10010L  ///
       1 "Male"  ///
       2 "Female"
label define ER10032L        0 "None; FU shares room"  , modify
forvalues n = 1/97 {
    label define ER10032L `n' "Actual number"  , modify
}
label define ER10032L       98 "DK"  , modify
label define ER10032L       99 "NA; refused"  , modify

label define ER10035L  ///
       1 "Owns or is buying home, either fully or jointly; mobile home owners who rent lots are included here"  ///
       5 "Pays rent"  ///
       8 "Neither owns nor rents"  ///
       9 "NA; DK; refused"

label define ER10072L  ///
       1 "Yes"  ///
       5 "No"  ///
       8 "DK"  ///
       9 "NA; refused"
label define ER12222L        0 "Completed no grades of school"  , modify
forvalues n = 1/16 {
    label define ER12222L `n' "Actual number"  , modify
}
label define ER12222L       17 "At least some post-graduate work"  , modify
label define ER12222L       99 "NA; DK"  , modify

label define ER13001L  ///
       1 "Release number 1 - August 2001"  ///
       2 "Release number 2 - October 2001"  ///
       3 "Release number 3 - January 2002"  ///
       4 "Release number 4 - May 2008"  ///
       5 "Release number 5 - November 2013"  ///
       6 "Release number 6 - February 2014"  ///
       7 "Release number 7 - January 2016"
forvalues n = 1/20 {
    label define ER13009L `n' "Actual number"  , modify
}

label define ER13011L  ///
       1 "Male"  ///
       2 "Female"
label define ER13037L        0 "None; FU shares room"  , modify
forvalues n = 1/97 {
    label define ER13037L `n' "Actual number"  , modify
}
label define ER13037L       98 "DK"  , modify
label define ER13037L       99 "NA; refused"  , modify

label define ER13040L  ///
       1 "Owns or is buying home, either fully or jointly; mobile home owners who rent lots are included here"  ///
       5 "Pays rent"  ///
       8 "Neither owns nor rents"

label define ER13077L  ///
       1 "Yes"  ///
       5 "No"  ///
       8 "DK"  ///
       9 "NA; refused"
label define ER16516L        0 "Completed no grades of school"  , modify
forvalues n = 1/16 {
    label define ER16516L `n' "Actual number"  , modify
}
label define ER16516L       17 "At least some post-graduate work"  , modify
label define ER16516L       99 "NA; DK"  , modify

label define ER17001L  ///
       1 "Release number 1 - November 2002"  ///
       2 "Release number 2 - May 2008"  ///
       3 "Release number 3 - November 2013"  ///
       4 "Release number 4 - February 2014"  ///
       5 "Release number 5 - January 2016"
forvalues n = 1/20 {
    label define ER17012L `n' "Actual number"  , modify
}

label define ER17014L  ///
       1 "Male"  ///
       2 "Female"
label define ER17040L        0 "None; FU shares room"  , modify
forvalues n = 1/97 {
    label define ER17040L `n' "Actual number"  , modify
}
label define ER17040L       98 "DK"  , modify
label define ER17040L       99 "NA; refused"  , modify

label define ER17043L  ///
       1 "Owns or is buying home, either fully or jointly; mobile home owners who rent lots are included here"  ///
       5 "Pays rent"  ///
       8 "Neither owns nor rents"

label define ER17088L  ///
       1 "Yes"  ///
       5 "No"  ///
       8 "DK"  ///
       9 "NA; refused"

label define ER2001L  ///
       1 "Release number 1 - August 1995"  ///
       2 "Release number 2 - January 2003"  ///
       3 "Release number 3 - March 2004"  ///
       4 "Release number 4 - May 2008"  ///
       5 "Release number 5 - November 2013"  ///
       6 "Release number 6 - January 2016"
forvalues n = 1/20 {
    label define ER2006L `n' "Actual number"  , modify
}
forvalues n = 14/96 {
    label define ER2007L `n' "Actual age"  , modify
}
label define ER2007L       97 "Ninety-seven years of age or older"  , modify
label define ER2007L        0 "Wild code"  , modify
label define ER2007L       99 "NA; DK"  , modify

label define ER2008L  ///
       1 "Male"  ///
       2 "Female"  ///
       0 "Wild code"
label define ER2029L        0 "None; FU shares room"  , modify
forvalues n = 1/97 {
    label define ER2029L `n' "Actual number"  , modify
}
label define ER2029L       98 "DK"  , modify
label define ER2029L       99 "NA; refused"  , modify

label define ER2032L  ///
       1 "Owns or is buying home, either fully or jointly; mobile home owners who rent lots are included here"  ///
       5 "Pays rent"  ///
       8 "Neither owns nor rents"  ///
       9 "DK; NA; refused"  ///
       0 "Inap."
label define ER20457L        0 "Completed no grades of school"  , modify
forvalues n = 1/16 {
    label define ER20457L `n' "Actual number"  , modify
}
label define ER20457L       17 "At least some post-graduate work"  , modify
label define ER20457L       99 "NA; DK"  , modify

label define ER2062L  ///
       1 "Yes"  ///
       5 "No"  ///
       8 "DK"  ///
       9 "NA; refused"

label define ER21001L  ///
       1 "Release number 1 - December 2004"  ///
       2 "Release number 2 - October 2005"  ///
       3 "Release number 3 - November 2005"  ///
       4 "Release number 4 - May 2008"  ///
       5 "Release number 5 - November 2013"  ///
       6 "Release number 6 - February 2014"  ///
       7 "Release number 7 - January 2016"
forvalues n = 1/20 {
    label define ER21016L `n' "Actual number"  , modify
}

label define ER21018L  ///
       1 "Male"  ///
       2 "Female"
label define ER21039L        0 "None; FU shares room"  , modify
forvalues n = 1/19 {
    label define ER21039L `n' "Actual number"  , modify
}
label define ER21039L       20 "20 or more rooms"  , modify
label define ER21039L       98 "DK"  , modify
label define ER21039L       99 "NA; refused"  , modify

label define ER21042L  ///
       1 "Owns or is buying home, either fully or jointly; mobile home owners who rent lots are included here"  ///
       5 "Pays rent"  ///
       8 "Neither owns nor rents"

label define ER21117L  ///
       1 "Yes"  ///
       5 "No"  ///
       8 "DK"  ///
       9 "NA; refused"
label define ER24148L        0 "Completed no grades of school"  , modify
forvalues n = 1/16 {
    label define ER24148L `n' "Actual number"  , modify
}
label define ER24148L       17 "At least some post-graduate work"  , modify
label define ER24148L       99 "NA; DK"  , modify

label define ER25001L  ///
       1 "Release number 1, March 2007"  ///
       2 "Release number 2, May 2007"  ///
       3 "Release number 3, November 2013"  ///
       4 "Release number 4, February 2014"  ///
       5 "Release number 5, January 2016"
forvalues n = 1/20 {
    label define ER25016L `n' "Actual number"  , modify
}

label define ER25018L  ///
       1 "Male"  ///
       2 "Female"
label define ER25027L        0 "None; FU shares room"  , modify
forvalues n = 1/19 {
    label define ER25027L `n' "Actual number"  , modify
}
label define ER25027L       20 "20 or more rooms"  , modify
label define ER25027L       98 "DK"  , modify
label define ER25027L       99 "NA; refused"  , modify

label define ER25028L  ///
       1 "Owns or is buying home, either fully or jointly; mobile home owners who rent lots are included here"  ///
       5 "Pays rent"  ///
       8 "Neither owns nor rents"

label define ER25098L  ///
       1 "Yes"  ///
       5 "No"  ///
       8 "DK"  ///
       9 "NA; refused"
label define ER28047L        0 "Completed no grades of school"  , modify
forvalues n = 1/16 {
    label define ER28047L `n' "Actual number"  , modify
}
label define ER28047L       17 "At least some post-graduate work"  , modify
label define ER28047L       99 "NA; DK"  , modify

label define ER36001L  ///
       1 "Release number 1, June 2009"  ///
       2 "Release number 2, October 2009"  ///
       3 "Release number 3, January 2012"  ///
       4 "Release number 4, December 2013"  ///
       5 "Release number 5, February 2014"  ///
       6 "Release number 6, January 2016"
forvalues n = 1/20 {
    label define ER36016L `n' "Actual number"  , modify
}

label define ER36018L  ///
       1 "Male"  ///
       2 "Female"
label define ER36027L        0 "None; FU shares room"  , modify
forvalues n = 1/19 {
    label define ER36027L `n' "Actual number"  , modify
}
label define ER36027L       20 "20 or more rooms"  , modify
label define ER36027L       98 "DK"  , modify
label define ER36027L       99 "NA; refused"  , modify

label define ER36028L  ///
       1 "Owns or is buying home, either fully or jointly; mobile home owners who rent lots are included here"  ///
       5 "Pays rent"  ///
       8 "Neither owns nor rents"  ///
       9 "Wild code"

label define ER36103L  ///
       1 "Yes"  ///
       5 "No"  ///
       8 "DK"  ///
       9 "NA; refused"
label define ER41037L        0 "Completed no grades of school"  , modify
forvalues n = 1/16 {
    label define ER41037L `n' "Actual number"  , modify
}
label define ER41037L       17 "At least some post-graduate work"  , modify
label define ER41037L       99 "NA; DK"  , modify
label define ER4158L        0 "Completed no grades of school"  , modify
forvalues n = 1/16 {
    label define ER4158L `n' "Actual number"  , modify
}
label define ER4158L       17 "At least some post-graduate work"  , modify
label define ER4158L       99 "NA; DK"  , modify

label define ER42001L  ///
       1 "Release number 1, July 2011"  ///
       2 "Release number 2, November 2013"  ///
       3 "Release number 3, February 2014"  ///
       4 "Release number 4, January 2016"
forvalues n = 1/20 {
    label define ER42016L `n' "Actual number"  , modify
}

label define ER42018L  ///
       1 "Male"  ///
       2 "Female"
label define ER42028L        0 "None; FU shares room"  , modify
forvalues n = 1/19 {
    label define ER42028L `n' "Actual number"  , modify
}
label define ER42028L       20 "20 or more rooms"  , modify
label define ER42028L       98 "DK"  , modify
label define ER42028L       99 "NA; refused"  , modify

label define ER42029L  ///
       1 "Owns or is buying home, either fully or jointly; mobile home owners who rent lots are included here"  ///
       5 "Pays rent"  ///
       8 "Neither owns nor rents"

label define ER42132L  ///
       1 "Yes"  ///
       5 "No"  ///
       8 "DK"  ///
       9 "NA; refused"
label define ER46981L        0 "Completed no grades of school"  , modify
forvalues n = 1/16 {
    label define ER46981L `n' "Actual number"  , modify
}
label define ER46981L       17 "At least some post-graduate work"  , modify
label define ER46981L       99 "DK; NA"  , modify

label define ER47301L  ///
       1 "Release number 1, July 2013"  ///
       2 "Release number 2, November 2013"  ///
       3 "Release number 3, February 2014"  ///
       4 "Release number 4, January 2016"
forvalues n = 1/20 {
    label define ER47316L `n' "Actual number"  , modify
}

label define ER47318L  ///
       1 "Male"  ///
       2 "Female"
label define ER47328L        0 "None; FU shares room"  , modify
forvalues n = 1/19 {
    label define ER47328L `n' "Actual number"  , modify
}
label define ER47328L       20 "20 or more rooms"  , modify
label define ER47328L       98 "DK"  , modify
label define ER47328L       99 "NA; refused"  , modify

label define ER47329L  ///
       1 "Owns or is buying home, either fully or jointly; mobile home owners who rent lots are included here"  ///
       5 "Pays rent"  ///
       8 "Neither owns nor rents"

label define ER47440L  ///
       1 "Yes"  ///
       5 "No"  ///
       8 "DK"  ///
       9 "NA; refused"

label define ER5001L  ///
       1 "Release number 1 - August 1995"  ///
       2 "Release number 2 - January 2003"  ///
       3 "Release number 3 - March 2004"  ///
       4 "Release number 4 - May 2008"  ///
       5 "Release number 5 - November 2013"  ///
       6 "Release number 6 - January 2016"
forvalues n = 1/20 {
    label define ER5005L `n' "Actual number"  , modify
}
forvalues n = 14/96 {
    label define ER5006L `n' "Actual age"  , modify
}
label define ER5006L       97 "Ninety-seven years of age or older"  , modify
label define ER5006L        0 "Wild code"  , modify
label define ER5006L       98 "NA; DK"  , modify
label define ER5006L       99 "NA; DK"  , modify

label define ER5007L  ///
       1 "Male"  ///
       2 "Female"  ///
       0 "Wild code"
label define ER5028L        0 "None; FU shares room"  , modify
forvalues n = 1/97 {
    label define ER5028L `n' "Actual number"  , modify
}
label define ER5028L       98 "DK"  , modify
label define ER5028L       99 "NA; refused"  , modify

label define ER5031L  ///
       1 "Owns or is buying home, either fully or jointly; mobile home owners who rent lots are included here"  ///
       5 "Pays rent"  ///
       8 "Neither owns nor rents"  ///
       9 "DK; NA; refused"

label define ER5061L  ///
       1 "Yes"  ///
       5 "No"  ///
       8 "DK"  ///
       9 "NA; refused"
label define ER52405L        0 "Completed no grades of school"  , modify
forvalues n = 1/16 {
    label define ER52405L `n' "Actual number"  , modify
}
label define ER52405L       17 "At least some post-graduate work"  , modify
label define ER52405L       99 "DK; NA"  , modify

label define ER53001L  ///
       1 "Release number 1, May 2015"  ///
       2 "Release number 2, January 2016"
forvalues n = 1/20 {
    label define ER53016L `n' "Actual number"  , modify
}

label define ER53018L  ///
       1 "Male"  ///
       2 "Female"
label define ER53028L        0 "None; FU shares room"  , modify
forvalues n = 1/19 {
    label define ER53028L `n' "Actual number"  , modify
}
label define ER53028L       20 "20 or more rooms"  , modify
label define ER53028L       98 "DK"  , modify
label define ER53028L       99 "NA; refused"  , modify

label define ER53029L  ///
       1 "Owns or is buying home, either fully or jointly; mobile home owners who rent lots are included here"  ///
       5 "Pays rent"  ///
       8 "Neither owns nor rents"

label define ER53140L  ///
       1 "Yes"  ///
       5 "No"  ///
       8 "DK"  ///
       9 "NA; refused"
label define ER58223L        0 "Completed no grades of school"  , modify
forvalues n = 1/17 {
    label define ER58223L `n' "Actual number"  , modify
}
label define ER58223L       99 "DK; NA"  , modify
label define ER6998L        0 "Completed no grades of school"  , modify
forvalues n = 1/16 {
    label define ER6998L `n' "Actual number"  , modify
}
label define ER6998L       17 "At least some post-graduate work"  , modify
label define ER6998L       99 "NA; DK"  , modify

label define ER7001L  ///
       1 "Release number 1 - August 1996"  ///
       2 "Release number 2 - January 2003"  ///
       3 "Release number 3 - March 2004"  ///
       4 "Release number 4 - May 2008"  ///
       5 "Release number 5 - November 2013"  ///
       6 "Release number 6 - January 2016"
forvalues n = 1/20 {
    label define ER7005L `n' "Actual number"  , modify
}
forvalues n = 14/96 {
    label define ER7006L `n' "Actual age"  , modify
}
label define ER7006L       97 "Ninety-seven years of age or older"  , modify
label define ER7006L        0 "Wild code"  , modify
label define ER7006L       98 "DK"  , modify
label define ER7006L       99 "NA"  , modify

label define ER7007L  ///
       1 "Male"  ///
       2 "Female"  ///
       0 "Wild code"
label define ER7028L        0 "None; FU shares room"  , modify
forvalues n = 1/97 {
    label define ER7028L `n' "Actual number"  , modify
}
label define ER7028L       98 "DK"  , modify
label define ER7028L       99 "NA; refused"  , modify

label define ER7031L  ///
       1 "Owns or is buying home, either fully or jointly; mobile home owners who rent lots are included here"  ///
       5 "Pays rent"  ///
       8 "Neither owns nor rents"  ///
       9 "DK; NA; refused"

label define ER7155L  ///
       1 "Yes"  ///
       5 "No"  ///
       8 "DK"  ///
       9 "NA; refused"
label define ER9249L        0 "Completed no grades of school"  , modify
forvalues n = 1/16 {
    label define ER9249L `n' "Actual number"  , modify
}
label define ER9249L       17 "At least some post-graduate work"  , modify
label define ER9249L       99 "NA; DK"  , modify

label define V1L  ///
       2 "Release number 2 - May 2008"  ///
       3 "Release number 3 - December 2013"

label define V10001L  ///
       2 "Release number 2 - May 2008"  ///
       3 "Release number 3 - December 2013"
forvalues n = 17/97 {
    label define V1008L `n' "Actual age of Head"  , modify
}
label define V1008L       98 "Ninety-eight years or older"  , modify
label define V1008L       99 "NA"  , modify

label define V1010L  ///
       1 "Male"  ///
       2 "Female"

label define V102L  ///
       1 "One"  ///
       2 "Two"  ///
       3 "Three"  ///
       4 "Four"  ///
       5 "Five"  ///
       6 "Six"  ///
       7 "Seven"  ///
       8 "Eight or more"  ///
       9 "NA; DK"  ///
       0 "None, shares room"

label define V103L  ///
       1 "Owns home (or trailer, fully or jointly)"  ///
       5 "Rents (or shares rent)"  ///
       8 "Neither (owns nor rents)"
forvalues n = 1/20 {
    label define V10418L `n' "Actual number of people"  , modify
}
forvalues n = 17/97 {
    label define V10419L `n' "Actual age"  , modify
}
label define V10419L       98 "98 years of age or older"  , modify
label define V10419L       99 "NA"  , modify

label define V10420L  ///
       1 "Male"  ///
       2 "Female"

label define V10432L  ///
       0 "None; FU shares room"  ///
       1 "One room"  ///
       2 "Two rooms"  ///
       3 "Three rooms"  ///
       4 "Four rooms"  ///
       5 "Five rooms"  ///
       6 "Six rooms"  ///
       7 "Seven rooms"  ///
       8 "Eight rooms or more"  ///
       9 "NA; DK"

label define V10437L  ///
       1 "Owns or is buying home, either fully or jointly; mobile home owners who rent lots are included here"  ///
       5 "Pays rent"  ///
       8 "Neither owns nor rents"

label define V10447L  ///
       1 "Yes"  ///
       5 "No"  ///
       9 "NA; DK"

label define V10996L  ///
       0 "None"  ///
       1 "One"  ///
       2 "Two"  ///
       3 "Three"  ///
       4 "Four"  ///
       5 "Five"  ///
       6 "Six"  ///
       7 "Seven"  ///
       8 "Eight"  ///
       9 "Nine"  ///
      10 "Ten"  ///
      11 "Eleven"  ///
      12 "Twelve; GED"  ///
      13 "First year of college"  ///
      14 "Second year of college, with or without Associate`=char(146)'s degree"  ///
      15 "Third year of college"  ///
      16 "Fourth year of college; college graduate"  ///
      17 "At least some postgraduate work"  ///
      99 "NA; DK"

label define V1101L  ///
       2 "Release number 2 - May 2008"  ///
       3 "Release number 3 - December 2013"

label define V11101L  ///
       2 "Release number 2 - May 2008"  ///
       3 "Release number 3 - December 2013"
forvalues n = 1/16 {
    label define V115L `n' "Actual number of people"  , modify
}
label define V115L       99 "NA"  , modify
forvalues n = 1/20 {
    label define V11605L `n' "Actual number of people"  , modify
}
forvalues n = 16/97 {
    label define V11606L `n' "Actual age"  , modify
}
label define V11606L       98 "Ninety-eight years of age or older"  , modify
label define V11606L       99 "NA"  , modify

label define V11607L  ///
       1 "Male"  ///
       2 "Female"
label define V11614L        0 "None; FU shares room"  , modify
forvalues n = 1/98 {
    label define V11614L `n' "Actual number of rooms"  , modify
}
label define V11614L       99 "NA; DK"  , modify

label define V11618L  ///
       1 "Owns or is buying home, either fully or jointly; mobile home owners who rent lots are included here"  ///
       5 "Pays rent"  ///
       8 "Neither owns nor rents"

label define V11628L  ///
       1 "Yes"  ///
       5 "No"  ///
       9 "NA; DK"
forvalues n = 1/96 {
    label define V117L `n' "Actual age"  , modify
}
label define V117L       97 "Ninety-seven years or older"  , modify
label define V117L       98 "DK"  , modify
label define V117L       99 "NA"  , modify

label define V119L  ///
       1 "Male"  ///
       2 "Female"  ///
       9 "NA"
forvalues n = 1/19 {
    label define V1238L `n' "Actual number in FU"  , modify
}
forvalues n = 16/93 {
    label define V1239L `n' "Actual age"  , modify
}
label define V1239L       99 "NA age"  , modify

label define V1240L  ///
       1 "Male"  ///
       2 "Female"

label define V12501L  ///
       2 "Release number 2 - May 2008"  ///
       3 "Release number 3 - December 2013"

label define V1263L  ///
       0 "None, shares room"  ///
       1 "1 room"  ///
       2 "2 rooms"  ///
       3 "3 rooms"  ///
       4 "4 rooms"  ///
       5 "5 rooms"  ///
       6 "6 rooms"  ///
       7 "7 rooms"  ///
       8 "8 rooms"  ///
       9 "NA"

label define V1264L  ///
       1 "Owns or is buying house or trailer (fully or jointly)"  ///
       5 "Pays rent"  ///
       8 "Neither owns nor rents"

label define V1274L  ///
       1 "Yes"  ///
       5 "No"  ///
       9 "NA"
forvalues n = 1/20 {
    label define V13010L `n' "Actual number of people"  , modify
}
forvalues n = 17/97 {
    label define V13011L `n' "Actual age"  , modify
}
label define V13011L       98 "Ninety-eight years of age or older"  , modify
label define V13011L       99 "NA"  , modify

label define V13012L  ///
       1 "Male"  ///
       2 "Female"
label define V13019L        0 "None; FU shares room"  , modify
forvalues n = 1/98 {
    label define V13019L `n' "Actual number"  , modify
}
label define V13019L       99 "NA; DK"  , modify

label define V13023L  ///
       1 "Owns or is buying home, either fully or jointly; mobile home owners who rent lots are included here"  ///
       5 "Pays rent"  ///
       8 "Neither owns nor rents"

label define V13037L  ///
       1 "Yes"  ///
       5 "No"  ///
       9 "NA; DK"

label define V13701L  ///
       2 "Release number 2 - May 2008"  ///
       3 "Release number 3 - December 2013"
forvalues n = 1/20 {
    label define V14113L `n' "Actual number of people"  , modify
}
forvalues n = 17/97 {
    label define V14114L `n' "Actual age"  , modify
}
label define V14114L       98 "Ninety-eight years of age or older"  , modify
label define V14114L       99 "NA"  , modify

label define V14115L  ///
       1 "Male"  ///
       2 "Female"
label define V14122L        0 "None; FU shares room"  , modify
forvalues n = 1/98 {
    label define V14122L `n' "Actual number of rooms"  , modify
}
label define V14122L       99 "NA; DK"  , modify

label define V14126L  ///
       1 "Owns or is buying home, either fully or jointly; mobile home owners who rent lots are included here"  ///
       5 "Pays rent"  ///
       8 "Neither owns nor rents"

label define V14140L  ///
       1 "Yes"  ///
       5 "No"  ///
       9 "NA; DK"

label define V14801L  ///
       2 "Release number 2 - May 2008"  ///
       3 "Release number 3 - December 2013"
forvalues n = 1/20 {
    label define V15129L `n' "Actual number of people"  , modify
}
forvalues n = 17/97 {
    label define V15130L `n' "Actual age"  , modify
}
label define V15130L       98 "Ninety-eight years of age or older"  , modify
label define V15130L       99 "NA"  , modify

label define V15131L  ///
       1 "Male"  ///
       2 "Female"
label define V15138L        0 "None; FU shares room"  , modify
forvalues n = 1/98 {
    label define V15138L `n' "Actual number of rooms"  , modify
}
label define V15138L       99 "NA; DK"  , modify

label define V15140L  ///
       1 "Owns or is buying home, either fully or jointly; mobile home owners who rent lots are included here"  ///
       5 "Pays rent"  ///
       8 "Neither owns nor rents"

label define V15148L  ///
       1 "Yes"  ///
       5 "No"  ///
       9 "NA; DK"

label define V16301L  ///
       2 "Release number 2 - May 2008"  ///
       3 "Release number 3 - December 2013"
forvalues n = 1/20 {
    label define V16630L `n' "Actual number of people"  , modify
}
forvalues n = 18/97 {
    label define V16631L `n' "Actual age"  , modify
}
label define V16631L       98 "Ninety-eight years of age or older"  , modify
label define V16631L       99 "NA"  , modify

label define V16632L  ///
       1 "Male"  ///
       2 "Female"
label define V16639L        0 "None; FU shares room"  , modify
forvalues n = 1/98 {
    label define V16639L `n' "Actual number"  , modify
}
label define V16639L       99 "NA; DK"  , modify

label define V16641L  ///
       1 "Owns or is buying home, either fully or jointly; mobile home owners who rent lots are included here"  ///
       5 "Pays rent"  ///
       8 "Neither owns nor rents"

label define V16649L  ///
       1 "Yes"  ///
       5 "No"  ///
       9 "NA; DK"

label define V17701L  ///
       2 "Release Number 2 - May 2008"  ///
       3 "Release Number 3 - December 2013"

label define V1801L  ///
       2 "Release number 2 - May 2008"  ///
       3 "Release number 3 - December 2013"
forvalues n = 1/20 {
    label define V18048L `n' "Actual number of people"  , modify
}
forvalues n = 17/97 {
    label define V18049L `n' "Actual age"  , modify
}
label define V18049L       98 "Ninety-eight years of age or older"  , modify
label define V18049L       99 "NA"  , modify

label define V18050L  ///
       1 "Male"  ///
       2 "Female"
label define V18070L        0 "None; FU shares room"  , modify
forvalues n = 1/98 {
    label define V18070L `n' "Actual number"  , modify
}
label define V18070L       99 "NA; DK"  , modify

label define V18072L  ///
       1 "Owns or is buying home, either fully or jointly; mobile home owners who rent lots are included here"  ///
       5 "Pays rent"  ///
       8 "Neither owns nor rents"

label define V18087L  ///
       1 "Yes"  ///
       5 "No"  ///
       9 "NA; DK"

label define V19001L  ///
       2 "Release number 2 - May 2008"  ///
       3 "Release number 3 - December 2013"
forvalues n = 1/20 {
    label define V19348L `n' "Actual number of people"  , modify
}
forvalues n = 17/97 {
    label define V19349L `n' "Actual age"  , modify
}
label define V19349L       98 "Ninety-eight years of age or older"  , modify
label define V19349L       99 "NA"  , modify

label define V19350L  ///
       1 "Male"  ///
       2 "Female"
label define V19370L        0 "None; FU shares room"  , modify
forvalues n = 1/98 {
    label define V19370L `n' "Actual number"  , modify
}
label define V19370L       99 "NA; DK"  , modify

label define V19372L  ///
       1 "Owns or is buying home, either fully or jointly; mobile home owners who rent lots are included here"  ///
       5 "Pays rent"  ///
       8 "Neither owns nor rents"

label define V19387L  ///
       1 "Yes"  ///
       5 "No"  ///
       9 "NA; DK"
forvalues n = 1/17 {
    label define V1941L `n' "Actual number of persons currently in the FU"  , modify
}
forvalues n = 15/97 {
    label define V1942L `n' "Actual age"  , modify
}
label define V1942L       98 "Ninety-eight years of age or older"  , modify
label define V1942L       99 "NA, DK"  , modify

label define V1943L  ///
       1 "Male"  ///
       2 "Female"

label define V1966L  ///
       0 "None, shares room"  ///
       1 "One room"  ///
       2 "Two"  ///
       3 "Three"  ///
       4 "Four"  ///
       5 "Five"  ///
       6 "Six"  ///
       7 "Seven"  ///
       8 "Eight or more rooms"  ///
       9 "NA, DK"

label define V1967L  ///
       1 "Owns or is buying house or trailer (fully or jointly)"  ///
       5 "Pays rent"  ///
       8 "Neither owns nor rents"

label define V1979L  ///
       1 "Yes"  ///
       5 "No"  ///
       9 "NA, DK"
forvalues n = 1/16 {
    label define V20198L `n' "Actual grade of school completed"  , modify
}
label define V20198L       17 "Completed at least some postgraduate work"  , modify
label define V20198L       99 "NA; DK"  , modify
label define V20198L        0 "Inap: completed no grades of school"  , modify

label define V20301L  ///
       3 "Release number 3 - May 2008"  ///
       4 "Release number 4 - December 2013"
forvalues n = 1/20 {
    label define V20650L `n' "Actual number"  , modify
}
forvalues n = 14/97 {
    label define V20651L `n' "Actual age"  , modify
}
label define V20651L       98 "Ninety-eight years of age or older"  , modify
label define V20651L       99 "NA"  , modify

label define V20652L  ///
       1 "Male"  ///
       2 "Female"
label define V20670L        0 "None; FU shares room"  , modify
forvalues n = 1/98 {
    label define V20670L `n' "Actual number"  , modify
}
label define V20670L       99 "NA; DK"  , modify

label define V20672L  ///
       1 "Owns or is buying home, either fully or jointly; mobile home owners who rent lots are included here"  ///
       5 "Pays rent"  ///
       8 "Neither owns nor rents"

label define V20687L  ///
       1 "Yes"  ///
       5 "No"  ///
       9 "NA; DK"
forvalues n = 1/16 {
    label define V21504L `n' "Actual grade of school completed"  , modify
}
label define V21504L       17 "At least some post-graduate work"  , modify
label define V21504L       99 "NA; DK"  , modify
label define V21504L        0 "Inap: completed no grades of school"  , modify

label define V21601L  ///
       1 "Release number 1 - January 1998"  ///
       2 "Release number 2 - February 1998"  ///
       3 "Release number 3 - April 2000"  ///
       4 "Release number 4 - May 2008"
forvalues n = 1/20 {
    label define V22405L `n' "Actual number"  , modify
}
forvalues n = 14/97 {
    label define V22406L `n' "Actual age"  , modify
}
label define V22406L       98 "Ninety-eight years of age or older"  , modify
label define V22406L       99 "NA"  , modify

label define V22407L  ///
       1 "Male"  ///
       2 "Female"
label define V22425L        0 "None; FU shares room"  , modify
forvalues n = 1/98 {
    label define V22425L `n' "Actual number"  , modify
}
label define V22425L       99 "NA; DK"  , modify

label define V22427L  ///
       1 "Owns or is buying home, either fully or jointly; mobile home owners who rent lots are included here"  ///
       5 "Pays rent"  ///
       8 "Neither owns nor rents"

label define V22441L  ///
       1 "Yes"  ///
       5 "No"  ///
       9 "NA; DK"
forvalues n = 1/16 {
    label define V23333L `n' "Actual number"  , modify
}
label define V23333L       17 "At least some post-graduate work"  , modify
label define V23333L       99 "NA; DK"  , modify
label define V23333L        0 "Inap: completed no grades of school"  , modify

label define V2401L  ///
       2 "Release number 2 -- May 2008"  ///
       3 "Release number 3 - December 2013"
forvalues n = 1/19 {
    label define V2541L `n' "Actual number of people"  , modify
}
forvalues n = 17/96 {
    label define V2542L `n' "Actual age"  , modify
}
label define V2542L       99 "NA age"  , modify

label define V2543L  ///
       1 "Male"  ///
       2 "Female"

label define V2565L  ///
       0 "None, R shares room"  ///
       1 "One room"  ///
       2 "Two rooms"  ///
       3 "Three rooms"  ///
       4 "Four rooms"  ///
       5 "Five rooms"  ///
       6 "Six rooms"  ///
       7 "Seven rooms"  ///
       8 "Eight rooms or more"  ///
       9 "NA"

label define V2566L  ///
       1 "Owns or is buying house or trailer (fully or jointly)"  ///
       5 "Pays rent"  ///
       8 "Neither owns nor rents"

label define V2577L  ///
       1 "Yes"  ///
       5 "No"  ///
       9 "NA; DK"

label define V3001L  ///
       2 "Release number 2 - May 2008"  ///
       3 "Release number 3 - December 2013"
forvalues n = 1/17 {
    label define V3094L `n' "Actual number of people"  , modify
}
forvalues n = 17/97 {
    label define V3095L `n' "Actual age"  , modify
}
label define V3095L       99 "NA"  , modify

label define V3096L  ///
       1 "Male"  ///
       2 "Female"

label define V3107L  ///
       0 "None; R shares room"  ///
       1 "One room"  ///
       2 "Two rooms"  ///
       3 "Three rooms"  ///
       4 "Four rooms"  ///
       5 "Five rooms"  ///
       6 "Six rooms"  ///
       7 "Seven rooms"  ///
       8 "Eight or more rooms"  ///
       9 "NA; DK"

label define V3108L  ///
       1 "Owns or is buying house or trailer (fully or jointly)"  ///
       5 "Pays rent"  ///
       8 "Neither owns nor rents"

label define V3110L  ///
       1 "Yes"  ///
       5 "No"  ///
       9 "NA; DK"

label define V3401L  ///
       2 "Release number 2 - May 2008"  ///
       3 "Release number 3 - December 2013"
forvalues n = 1/17 {
    label define V3507L `n' "Actual number of people"  , modify
}
forvalues n = 17/93 {
    label define V3508L `n' "Actual age"  , modify
}
label define V3508L       99 "NA; DK"  , modify

label define V3509L  ///
       1 "Male"  ///
       2 "Female"

label define V3521L  ///
       0 "None; R shares room"  ///
       1 "One room"  ///
       2 "Two rooms"  ///
       3 "Three rooms"  ///
       4 "Four rooms"  ///
       5 "Five rooms"  ///
       6 "Six rooms"  ///
       7 "Seven rooms"  ///
       8 "Eight rooms or more"  ///
       9 "NA; DK"

label define V3522L  ///
       1 "Owns or is buying house or trailer (fully or jointly)"  ///
       5 "Pays rent"  ///
       8 "Neither owns nor rents"

label define V3524L  ///
       1 "Yes"  ///
       5 "No"  ///
       9 "NA; DK"

label define V3801L  ///
       2 "Release number 2 - May 2008"  ///
       3 "Release number 3 - December 2013"
forvalues n = 1/16 {
    label define V3920L `n' "Actual number of people"  , modify
}
forvalues n = 16/97 {
    label define V3921L `n' "Actual age"  , modify
}
label define V3921L       98 "Ninety-eight years of age or older"  , modify
label define V3921L       99 "NA; DK age"  , modify

label define V3922L  ///
       1 "Male"  ///
       2 "Female"

label define V3937L  ///
       0 "None; R shares room"  ///
       1 "One"  ///
       2 "Two"  ///
       3 "Three"  ///
       4 "Four"  ///
       5 "Five"  ///
       6 "Six"  ///
       7 "Seven"  ///
       8 "Eight or more"  ///
       9 "NA; DK"

label define V3939L  ///
       1 "Owns or is buying house or trailer (fully or jointly)"  ///
       5 "Pays rent"  ///
       8 "Neither owns nor rents"

label define V3941L  ///
       1 "Yes"  ///
       5 "No"  ///
       9 "NA; DK"

label define V4093L  ///
       0 "None"  ///
       1 "One"  ///
       2 "Two"  ///
       3 "Three"  ///
       4 "Four"  ///
       5 "Five"  ///
       6 "Six"  ///
       7 "Seven"  ///
       8 "Eight"  ///
       9 "Nine"  ///
      10 "Ten"  ///
      11 "Eleven"  ///
      12 "Twelve"  ///
      13 "Thirteen"  ///
      14 "Fourteen"  ///
      15 "Fifteen"  ///
      16 "Sixteen"  ///
      17 "Seventeen or more"  ///
      99 "NA; DK"

label define V4301L  ///
       2 "Release number 2 - May 2008"  ///
       3 "Release number 3 - December 2013"

label define V441L  ///
       2 "Release number 2 - May 2008"  ///
       3 "Release number 3 - December 2013"
forvalues n = 1/20 {
    label define V4435L `n' "Actual number of people"  , modify
}
forvalues n = 16/97 {
    label define V4436L `n' "Actual age"  , modify
}
label define V4436L       98 "Ninety-eight years of age or older"  , modify
label define V4436L       99 "NA; DK"  , modify

label define V4437L  ///
       1 "Male"  ///
       2 "Female"

label define V4448L  ///
       0 "None; R shares room"  ///
       1 "One room"  ///
       2 "Two"  ///
       3 "Three"  ///
       4 "Four"  ///
       5 "Five"  ///
       6 "Six"  ///
       7 "Seven"  ///
       8 "Eight or more"  ///
       9 "NA; DK"

label define V4450L  ///
       1 "Owns or is buying house or trailer (fully or jointly)"  ///
       5 "Pays rent"  ///
       8 "Neither owns nor rents"

label define V4452L  ///
       1 "Yes"  ///
       5 "No"  ///
       9 "NA; DK"

label define V4684L  ///
       0 "None"  ///
       1 "One"  ///
       2 "Two"  ///
       3 "Three"  ///
       4 "Four"  ///
       5 "Five"  ///
       6 "Six"  ///
       7 "Seven"  ///
       8 "Eight"  ///
       9 "Nine"  ///
      10 "Ten"  ///
      11 "Eleven"  ///
      12 "Twelve; GED"  ///
      13 "Thirteen"  ///
      14 "Fourteen"  ///
      15 "Fifteen"  ///
      16 "Sixteen"  ///
      17 "Seventeen or more"  ///
      99 "NA; DK"

label define V5201L  ///
       2 "Release number 2 - May 2008"  ///
       3 "Release number 3 - December 2013"
forvalues n = 1/20 {
    label define V5349L `n' "Actual number of people"  , modify
}
forvalues n = 17/95 {
    label define V5350L `n' "Actual age of Head"  , modify
}
label define V5350L       99 "NA"  , modify

label define V5351L  ///
       1 "Male"  ///
       2 "Female"

label define V5362L  ///
       0 "None; R shares room"  ///
       1 "One room"  ///
       2 "Two"  ///
       3 "Three"  ///
       4 "Four"  ///
       5 "Five"  ///
       6 "Six"  ///
       7 "Seven"  ///
       8 "Eight or more"  ///
       9 "NA; DK"

label define V5364L  ///
       1 "Owns or is buying house or trailer (fully or jointly)"  ///
       5 "Pays rent"  ///
       8 "Neither owns nor rents"

label define V5366L  ///
       1 "Yes"  ///
       5 "No"  ///
       9 "NA; DK"
forvalues n = 1/18 {
    label define V549L `n' "Actual number of people"  , modify
}

label define V5608L  ///
       0 "None"  ///
       1 "One"  ///
       2 "Two"  ///
       3 "Three"  ///
       4 "Four"  ///
       5 "Five"  ///
       6 "Six"  ///
       7 "Seven"  ///
       8 "Eight"  ///
       9 "Nine"  ///
      10 "Ten"  ///
      11 "Eleven"  ///
      12 "Twelve; GED"  ///
      13 "Thirteen"  ///
      14 "Fourteen"  ///
      15 "Fifteen"  ///
      16 "Sixteen"  ///
      17 "Seventeen or more"  ///
      99 "NA; DK"

label define V5701L  ///
       2 "Release number 2 - May 2008"  ///
       3 "Release number 3 - December 2013"
forvalues n = 1/15 {
    label define V5849L `n' "Actual number of people"  , modify
}
forvalues n = 16/96 {
    label define V5850L `n' "Actual age"  , modify
}
label define V5850L       99 "NA"  , modify

label define V5851L  ///
       1 "Male"  ///
       2 "Female"

label define V5862L  ///
       0 "None; R shares room"  ///
       1 "One room"  ///
       2 "Two"  ///
       3 "Three"  ///
       4 "Four"  ///
       5 "Five"  ///
       6 "Six"  ///
       7 "Seven"  ///
       8 "Eight rooms or more"  ///
       9 "NA; DK"

label define V5864L  ///
       1 "Owns or is buying house or trailer (fully or jointly)"  ///
       5 "Pays rent"  ///
       8 "Neither owns nor rents"

label define V5866L  ///
       1 "Yes"  ///
       5 "No"  ///
       9 "NA; DK"

label define V592L  ///
       0 "None, shares room"  ///
       1 "One"  ///
       2 "Two"  ///
       3 "Three"  ///
       4 "Four"  ///
       5 "Five"  ///
       6 "Six"  ///
       7 "Seven"  ///
       8 "Eight"  ///
       9 "NA"

label define V593L  ///
       1 "Owns home (or trailer, fully or jointly)"  ///
       5 "Rents"  ///
       8 "Neither owns nor rents"

label define V603L  ///
       1 "Yes"  ///
       5 "No"  ///
       9 "NA"
label define V6157L        0 "None"  , modify
forvalues n = 1/16 {
    label define V6157L `n' "Actual number of grades"  , modify
}
label define V6157L       17 "Seventeen or more"  , modify
label define V6157L       99 "NA; DK"  , modify

label define V6301L  ///
       2 "Release number 2 - May 2008"  ///
       3 "Release number 3 - December 2013"
forvalues n = 1/15 {
    label define V6461L `n' "Actual number of people"  , modify
}
forvalues n = 17/97 {
    label define V6462L `n' "Actual age"  , modify
}
label define V6462L       99 "NA"  , modify

label define V6463L  ///
       1 "Male"  ///
       2 "Female"

label define V6477L  ///
       0 "None; R shares room"  ///
       1 "One room"  ///
       2 "Two rooms"  ///
       3 "Three rooms"  ///
       4 "Four rooms"  ///
       5 "Five rooms"  ///
       6 "Six rooms"  ///
       7 "Seven rooms"  ///
       8 "Eight rooms or more"  ///
       9 "NA; DK"

label define V6479L  ///
       1 "Owns or is buying house or trailer (fully or jointly)"  ///
       5 "Pays rent"  ///
       8 "Neither owns nor rents"

label define V6484L  ///
       1 "Yes"  ///
       5 "No"  ///
       9 "NA; DK"

label define V6754L  ///
       0 "None"  ///
       1 "One"  ///
       2 "Two"  ///
       3 "Three"  ///
       4 "Four"  ///
       5 "Five"  ///
       6 "Six"  ///
       7 "Seven"  ///
       8 "Eight"  ///
       9 "Nine"  ///
      10 "Ten"  ///
      11 "Eleven"  ///
      12 "Twelve; GED"  ///
      13 "Thirteen"  ///
      14 "Fourteen"  ///
      15 "Fifteen"  ///
      16 "Sixteen"  ///
      17 "Seventeen or more"  ///
      99 "NA; DK"

label define V6901L  ///
       2 "Release number 2 - May 2008"  ///
       3 "Release number 3 - December 2013"
forvalues n = 1/16 {
    label define V7066L `n' "Actual number of people"  , modify
}
forvalues n = 16/97 {
    label define V7067L `n' "Actual age"  , modify
}
label define V7067L       98 "Ninety-eight years of age or older"  , modify
label define V7067L       99 "NA"  , modify

label define V7068L  ///
       1 "Male"  ///
       2 "Female"

label define V7080L  ///
       0 "None; R shares room"  ///
       1 "One room"  ///
       2 "Two rooms"  ///
       3 "Three rooms"  ///
       4 "Four rooms"  ///
       5 "Five rooms"  ///
       6 "Six rooms"  ///
       7 "Seven rooms"  ///
       8 "Eight rooms or more"  ///
       9 "NA; DK"

label define V7084L  ///
       1 "Owns or is buying house or trailer (fully or jointly)"  ///
       5 "Pays rent"  ///
       8 "Neither owns nor rents"

label define V7089L  ///
       1 "Yes"  ///
       5 "No"  ///
       9 "NA; DK"

label define V7387L  ///
       0 "None"  ///
       1 "One"  ///
       2 "Two"  ///
       3 "Three"  ///
       4 "Four"  ///
       5 "Five"  ///
       6 "Six"  ///
       7 "Seven"  ///
       8 "Eight"  ///
       9 "Nine"  ///
      10 "Ten"  ///
      11 "Eleven"  ///
      12 "Twelve; GED"  ///
      13 "Thirteen"  ///
      14 "Fourteen"  ///
      15 "Fifteen"  ///
      16 "Sixteen"  ///
      17 "Seventeen or more"  ///
      99 "NA; DK"

label define V7501L  ///
       2 "Release number 2 - May 2008"  ///
       3 "Release number 3 - December 2013"
forvalues n = 1/15 {
    label define V7657L `n' "Actual number of people"  , modify
}
forvalues n = 17/97 {
    label define V7658L `n' "Actual age"  , modify
}
label define V7658L       98 "98 years of age or older"  , modify
label define V7658L       99 "NA"  , modify

label define V7659L  ///
       1 "Male"  ///
       2 "Female"

label define V7671L  ///
       0 "None; R shares room"  ///
       1 "One room"  ///
       2 "Two rooms"  ///
       3 "Three rooms"  ///
       4 "Four rooms"  ///
       5 "Five rooms"  ///
       6 "Six rooms"  ///
       7 "Seven rooms"  ///
       8 "Eight rooms or more"  ///
       9 "NA; DK"

label define V7675L  ///
       1 "Owns or is buying house or trailer (fully or jointly)"  ///
       5 "Pays rent"  ///
       8 "Neither owns nor rents"

label define V7700L  ///
       1 "Yes"  ///
       5 "No"  ///
       9 "NA; DK"

label define V8039L  ///
       0 "None"  ///
       1 "One"  ///
       2 "Two"  ///
       3 "Three"  ///
       4 "Four"  ///
       5 "Five"  ///
       6 "Six"  ///
       7 "Seven"  ///
       8 "Eight"  ///
       9 "Nine"  ///
      10 "Ten"  ///
      11 "Eleven"  ///
      12 "Twelve; GED"  ///
      13 "Thirteen"  ///
      14 "Fourteen"  ///
      15 "Fifteen"  ///
      16 "Sixteen"  ///
      17 "Seventeen or more"  ///
      99 "NA; DK"

label define V8201L  ///
       2 "Release number 2 - May 2008"  ///
       3 "Release number 3 - December 2013"
forvalues n = 1/20 {
    label define V8351L `n' "Actual number of people"  , modify
}
forvalues n = 17/97 {
    label define V8352L `n' "Actual age"  , modify
}
label define V8352L       98 "Ninety-eight years of age or older"  , modify
label define V8352L       99 "NA"  , modify

label define V8353L  ///
       1 "Male"  ///
       2 "Female"

label define V8360L  ///
       0 "None; FU shares room"  ///
       1 "One room"  ///
       2 "Two rooms"  ///
       3 "Three rooms"  ///
       4 "Four rooms"  ///
       5 "Five rooms"  ///
       6 "Six rooms"  ///
       7 "Seven rooms"  ///
       8 "Eight rooms or more"  ///
       9 "NA; DK"

label define V8364L  ///
       1 "Owns or is buying home, either fully or jointly; mobile home owners who rent lots are included here"  ///
       5 "Pays rent"  ///
       8 "Neither owns nor rents"

label define V8369L  ///
       1 "Yes"  ///
       5 "No"  ///
       9 "NA; DK"

label define V8663L  ///
       0 "None"  ///
       1 "One"  ///
       2 "Two"  ///
       3 "Three"  ///
       4 "Four"  ///
       5 "Five"  ///
       6 "Six"  ///
       7 "Seven"  ///
       8 "Eight"  ///
       9 "Nine"  ///
      10 "Ten"  ///
      11 "Eleven"  ///
      12 "Twelve; GED"  ///
      13 "First year of college"  ///
      14 "Second year of college, with or without Associate`=char(146)'s degree"  ///
      15 "Third year of college"  ///
      16 "Fourth year of college; college graduate"  ///
      17 "At least some postgraduate work"  ///
      99 "NA; DK"

label define V8801L  ///
       2 "Release number 2 - May 2008"  ///
       3 "Release number 3 - December 2013"
forvalues n = 1/20 {
    label define V8960L `n' "Actual number of people"  , modify
}
forvalues n = 14/97 {
    label define V8961L `n' "Actual age"  , modify
}
label define V8961L       98 "Ninety-eight years of age or older"  , modify
label define V8961L       99 "NA"  , modify

label define V8962L  ///
       1 "Male"  ///
       2 "Female"

label define V8969L  ///
       0 "None; FU shares room"  ///
       1 "One room"  ///
       2 "Two rooms"  ///
       3 "Three rooms"  ///
       4 "Four rooms"  ///
       5 "Five rooms"  ///
       6 "Six rooms"  ///
       7 "Seven rooms"  ///
       8 "Eight rooms or more"  ///
       9 "NA; DK"

label define V8974L  ///
       1 "Owns or is buying home, either fully or jointly; mobile home owners who rent lots are included here"  ///
       5 "Pays rent"  ///
       8 "Neither owns nor rents"

label define V8999L  ///
       1 "Yes"  ///
       5 "No"  ///
       9 "NA; DK"

label define V9349L  ///
       0 "None"  ///
       1 "One"  ///
       2 "Two"  ///
       3 "Three"  ///
       4 "Four"  ///
       5 "Five"  ///
       6 "Six"  ///
       7 "Seven"  ///
       8 "Eight"  ///
       9 "Nine"  ///
      10 "Ten"  ///
      11 "Eleven"  ///
      12 "Twelve; GED"  ///
      13 "First year of college"  ///
      14 "Second year of college, with or without Associate`=char(146)'s degree"  ///
      15 "Third year of college"  ///
      16 "Fourth year of college; college graduate"  ///
      17 "At least some postgraduate work"  ///
      99 "NA; DK"

label values ER10001    ER10001L
label values ER10008    ER10008L
label values ER10010    ER10010L
label values ER10032    ER10032L
label values ER10035    ER10035L
label values ER10072    ER10072L
label values ER12222    ER12222L
label values ER13001    ER13001L
label values ER13009    ER13009L
label values ER13011    ER13011L
label values ER13037    ER13037L
label values ER13040    ER13040L
label values ER13077    ER13077L
label values ER16516    ER16516L
label values ER17001    ER17001L
label values ER17012    ER17012L
label values ER17014    ER17014L
label values ER17040    ER17040L
label values ER17043    ER17043L
label values ER17088    ER17088L
label values ER2001     ER2001L
label values ER2006     ER2006L
label values ER2007     ER2007L
label values ER2008     ER2008L
label values ER2029     ER2029L
label values ER2032     ER2032L
label values ER20457    ER20457L
label values ER2062     ER2062L
label values ER21001    ER21001L
label values ER21016    ER21016L
label values ER21018    ER21018L
label values ER21039    ER21039L
label values ER21042    ER21042L
label values ER21117    ER21117L
label values ER24148    ER24148L
label values ER25001    ER25001L
label values ER25016    ER25016L
label values ER25018    ER25018L
label values ER25027    ER25027L
label values ER25028    ER25028L
label values ER25098    ER25098L
label values ER28047    ER28047L
label values ER36001    ER36001L
label values ER36016    ER36016L
label values ER36018    ER36018L
label values ER36027    ER36027L
label values ER36028    ER36028L
label values ER36103    ER36103L
label values ER41037    ER41037L
label values ER4158     ER4158L
label values ER42001    ER42001L
label values ER42016    ER42016L
label values ER42018    ER42018L
label values ER42028    ER42028L
label values ER42029    ER42029L
label values ER42132    ER42132L
label values ER46981    ER46981L
label values ER47301    ER47301L
label values ER47316    ER47316L
label values ER47318    ER47318L
label values ER47328    ER47328L
label values ER47329    ER47329L
label values ER47440    ER47440L
label values ER5001     ER5001L
label values ER5005     ER5005L
label values ER5006     ER5006L
label values ER5007     ER5007L
label values ER5028     ER5028L
label values ER5031     ER5031L
label values ER5061     ER5061L
label values ER52405    ER52405L
label values ER53001    ER53001L
label values ER53016    ER53016L
label values ER53018    ER53018L
label values ER53028    ER53028L
label values ER53029    ER53029L
label values ER53140    ER53140L
label values ER58223    ER58223L
label values ER6998     ER6998L
label values ER7001     ER7001L
label values ER7005     ER7005L
label values ER7006     ER7006L
label values ER7007     ER7007L
label values ER7028     ER7028L
label values ER7031     ER7031L
label values ER7155     ER7155L
label values ER9249     ER9249L
label values V1         V1L
label values V10001     V10001L
label values V1008      V1008L
label values V1010      V1010L
label values V102       V102L
label values V103       V103L
label values V10418     V10418L
label values V10419     V10419L
label values V10420     V10420L
label values V10432     V10432L
label values V10437     V10437L
label values V10447     V10447L
label values V10996     V10996L
label values V1101      V1101L
label values V11101     V11101L
label values V115       V115L
label values V11605     V11605L
label values V11606     V11606L
label values V11607     V11607L
label values V11614     V11614L
label values V11618     V11618L
label values V11628     V11628L
label values V117       V117L
label values V119       V119L
label values V1238      V1238L
label values V1239      V1239L
label values V1240      V1240L
label values V12501     V12501L
label values V1263      V1263L
label values V1264      V1264L
label values V1274      V1274L
label values V13010     V13010L
label values V13011     V13011L
label values V13012     V13012L
label values V13019     V13019L
label values V13023     V13023L
label values V13037     V13037L
label values V13701     V13701L
label values V14113     V14113L
label values V14114     V14114L
label values V14115     V14115L
label values V14122     V14122L
label values V14126     V14126L
label values V14140     V14140L
label values V14801     V14801L
label values V15129     V15129L
label values V15130     V15130L
label values V15131     V15131L
label values V15138     V15138L
label values V15140     V15140L
label values V15148     V15148L
label values V16301     V16301L
label values V16630     V16630L
label values V16631     V16631L
label values V16632     V16632L
label values V16639     V16639L
label values V16641     V16641L
label values V16649     V16649L
label values V17701     V17701L
label values V1801      V1801L
label values V18048     V18048L
label values V18049     V18049L
label values V18050     V18050L
label values V18070     V18070L
label values V18072     V18072L
label values V18087     V18087L
label values V19001     V19001L
label values V19348     V19348L
label values V19349     V19349L
label values V19350     V19350L
label values V19370     V19370L
label values V19372     V19372L
label values V19387     V19387L
label values V1941      V1941L
label values V1942      V1942L
label values V1943      V1943L
label values V1966      V1966L
label values V1967      V1967L
label values V1979      V1979L
label values V20198     V20198L
label values V20301     V20301L
label values V20650     V20650L
label values V20651     V20651L
label values V20652     V20652L
label values V20670     V20670L
label values V20672     V20672L
label values V20687     V20687L
label values V21504     V21504L
label values V21601     V21601L
label values V22405     V22405L
label values V22406     V22406L
label values V22407     V22407L
label values V22425     V22425L
label values V22427     V22427L
label values V22441     V22441L
label values V23333     V23333L
label values V2401      V2401L
label values V2541      V2541L
label values V2542      V2542L
label values V2543      V2543L
label values V2565      V2565L
label values V2566      V2566L
label values V2577      V2577L
label values V3001      V3001L
label values V3094      V3094L
label values V3095      V3095L
label values V3096      V3096L
label values V3107      V3107L
label values V3108      V3108L
label values V3110      V3110L
label values V3401      V3401L
label values V3507      V3507L
label values V3508      V3508L
label values V3509      V3509L
label values V3521      V3521L
label values V3522      V3522L
label values V3524      V3524L
label values V3801      V3801L
label values V3920      V3920L
label values V3921      V3921L
label values V3922      V3922L
label values V3937      V3937L
label values V3939      V3939L
label values V3941      V3941L
label values V4093      V4093L
label values V4301      V4301L
label values V441       V441L
label values V4435      V4435L
label values V4436      V4436L
label values V4437      V4437L
label values V4448      V4448L
label values V4450      V4450L
label values V4452      V4452L
label values V4684      V4684L
label values V5201      V5201L
label values V5349      V5349L
label values V5350      V5350L
label values V5351      V5351L
label values V5362      V5362L
label values V5364      V5364L
label values V5366      V5366L
label values V549       V549L
label values V5608      V5608L
label values V5701      V5701L
label values V5849      V5849L
label values V5850      V5850L
label values V5851      V5851L
label values V5862      V5862L
label values V5864      V5864L
label values V5866      V5866L
label values V592       V592L
label values V593       V593L
label values V603       V603L
label values V6157      V6157L
label values V6301      V6301L
label values V6461      V6461L
label values V6462      V6462L
label values V6463      V6463L
label values V6477      V6477L
label values V6479      V6479L
label values V6484      V6484L
label values V6754      V6754L
label values V6901      V6901L
label values V7066      V7066L
label values V7067      V7067L
label values V7068      V7068L
label values V7080      V7080L
label values V7084      V7084L
label values V7089      V7089L
label values V7387      V7387L
label values V7501      V7501L
label values V7657      V7657L
label values V7658      V7658L
label values V7659      V7659L
label values V7671      V7671L
label values V7675      V7675L
label values V7700      V7700L
label values V8039      V8039L
label values V8201      V8201L
label values V8351      V8351L
label values V8352      V8352L
label values V8353      V8353L
label values V8360      V8360L
label values V8364      V8364L
label values V8369      V8369L
label values V8663      V8663L
label values V8801      V8801L
label values V8960      V8960L
label values V8961      V8961L
label values V8962      V8962L
label values V8969      V8969L
label values V8974      V8974L
label values V8999      V8999L
label values V9349      V9349L
