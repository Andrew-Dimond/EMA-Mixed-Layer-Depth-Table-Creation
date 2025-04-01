# EMA Mixed Layer Depth Table Creation
This code was written to take data exports from the CTD and CAT data tables within EMA's oceanography databases and generate mixed layer
depth values for every station in which a cast is conducted.  It then generates summary fields to be associated with those calculations
that details whether calculations were successful, a reason if they were unsuccessful, or if the station was well mixed.  Lastly, it also
calculates maximum gear depth for each gear.  For many purposes when using MLD, max gear depth serves as the mixed layer depth at stations
that are well mixed.

This code needs 2 data files to run.  These are full field exports from the CTD and CAT tables.  Note that if you are only running this to
append to a current year, you only need that years' data in order to run this code successfully.  The export format of this code matches
the table structure in EMA's database for the Mixed Layer Depth table.

The code is set up to run if you simply save the CAT and CTD data files to the same location as the R script.