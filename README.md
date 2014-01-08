eimarGsodTools
==============

### What it is all about

Every person dealing with long-term climatological data (e.g. of daily air
temperature, relative humidity, and precipitation amounts) will sooner or later
stumble across the Global Summary Of Day (GSOD) climate data collection 
provided by the National Oceanic and Atmospheric Association (NOAA). In the 
course of my PhD thesis, I've been recently looking for available GSOD 
stations in close vicinity to Mt. Kilimanjaro, Tanzania, and as I am trying to
realize most of my coding work using R, I quickly noticed that there are only a 
few packages that provide convenient tools for processing GSOD data from within
R. Hence, I started to write this package that includes both downloading 
data sets of selected climate stations for a given time span as well as 
some processing steps (detection of statistic outliers, gap-filling based upon
linear interpolation, linear models and singular spectrum analysis (SSA)) for 
quality assurance.