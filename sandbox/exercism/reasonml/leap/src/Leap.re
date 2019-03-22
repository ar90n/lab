let isLeapYear: int => bool  = (year: int) => ((year mod 4 == 0) && (year mod 100 != 0)) || (year mod 400 == 0);
