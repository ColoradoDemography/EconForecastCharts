#######################################################################
#                     Data Prep                                       
#######################################################################

library(readxl)
library(dplyr)
library(tidyr)

# Reads in the Excel File (I had to convert it to an xlsx because it wasn't reading as an Excel)
v13=read_excel("CNTYDET_2015.xlsx", sheet=3)
v14=read_excel("CNTYDET_2015.xlsx", sheet=2)
v15=read_excel("CNTYDET_2015.xlsx", sheet=4)
v16=read_excel("CNTYDET_2016.xlsx", sheet=1)
v17=read_excel("CNTYDET_2017.xlsx", sheet=1)
v18=read_excel("CNTYDET_2018.xlsx", sheet=1)

rv13=read_excel("REGDET_13v_14v_2015.xlsx", sheet=1)
rv14=read_excel("REGDET_13v_14v_2015.xlsx", sheet=2)
rv15=read_excel("REGDET_13v_14v_2015.xlsx", sheet=4)
rv16=read_excel("REGDET_2016.xlsx", sheet=1)
rv17=read_excel("REGDET_2017.xlsx", sheet=1)
rv18=read_excel("REGDET_2018.xlsx", sheet=1)

# Pipe of functions that parses the data
j13=v13%>% #Original Data to be passed
  gather(year, value, -OBS)%>% # takes original data and reshapes it long from wide
  filter(grepl("JOBSI0C", OBS))%>% # takes long data and filters obs without "JOBSI0C" in the OBS column
  separate(OBS, c("variable", "countyfips"), sep=7 ) #Splits the OBS column into the variable name and county number in separate columns

#Writes out the parsed data to a csv
write.csv(j13, "totalJobs_v13.csv", row.names = FALSE)


# Pipe of functions that parses the data
j14=v14%>% #Original Data to be passed
  gather(year, value, -OBS)%>% # takes original data and reshapes it long from wide
  filter(grepl("JOBSI0C", OBS))%>% # takes long data and filters obs without "JOBSI0C" in the OBS column
  separate(OBS, c("variable", "countyfips"), sep=7 ) #Splits the OBS column into the variable name and county number in separate columns

#Writes out the parsed data to a csv
write.csv(j14, "totalJobs_v14.csv", row.names = FALSE)


# Pipe of functions that parses the data
j15=v15%>% #Original Data to be passed
  gather(year, value, -OBS)%>% # takes original data and reshapes it long from wide
  filter(grepl("JOBSI0C", OBS))%>% # takes long data and filters obs without "JOBSI0C" in the OBS column
  separate(OBS, c("variable", "countyfips"), sep=7 ) #Splits the OBS column into the variable name and county number in separate columns

#Writes out the parsed data to a csv
write.csv(j15, "totalJobs_v15.csv", row.names = FALSE)

# Pipe of functions that parses the data
j16=v16%>% #Original Data to be passed
  gather(year, value, -OBS)%>% # takes original data and reshapes it long from wide
  filter(grepl("JOBSI0C", OBS))%>% # takes long data and filters obs without "JOBSI0C" in the OBS column
  separate(OBS, c("variable", "countyfips"), sep=7 ) #Splits the OBS column into the variable name and county number in separate columns

#Writes out the parsed data to a csv
write.csv(j16, "totalJobs_v16.csv", row.names = FALSE)

# Pipe of functions that parses the data
p13=v13%>% #Original Data to be passed
  gather(year, value, -OBS)%>% # takes original data and reshapes it long from wide
  filter(grepl("ADJPOPC", OBS))%>% # takes long data and filters obs without "JOBSI0C" in the OBS column
  separate(OBS, c("variable", "countyfips"), sep=7 ) #Splits the OBS column into the variable name and county number in separate columns

#Writes out the parsed data to a csv
write.csv(p15, "totalPop_v13.csv", row.names = FALSE)

# Pipe of functions that parses the data
p14=v14%>% #Original Data to be passed
  gather(year, value, -OBS)%>% # takes original data and reshapes it long from wide
  filter(grepl("ADJPOPC", OBS))%>% # takes long data and filters obs without "JOBSI0C" in the OBS column
  separate(OBS, c("variable", "countyfips"), sep=7 ) #Splits the OBS column into the variable name and county number in separate columns

#Writes out the parsed data to a csv
write.csv(p15, "totalPop_v14.csv", row.names = FALSE)

# Pipe of functions that parses the data
p15=v15%>% #Original Data to be passed
  gather(year, value, -OBS)%>% # takes original data and reshapes it long from wide
  filter(grepl("ADJPOPC", OBS))%>% # takes long data and filters obs without "JOBSI0C" in the OBS column
  separate(OBS, c("variable", "countyfips"), sep=7 ) #Splits the OBS column into the variable name and county number in separate columns

#Writes out the parsed data to a csv
write.csv(p15, "totalPop_v15.csv", row.names = FALSE)

# Pipe of functions that parses the data
p16=v16%>% #Original Data to be passed
  gather(year, value, -OBS)%>% # takes original data and reshapes it long from wide
  filter(grepl("ADJPOPC", OBS))%>% # takes long data and filters obs without "JOBSI0C" in the OBS column
  separate(OBS, c("variable", "countyfips"), sep=7 ) #Splits the OBS column into the variable name and county number in separate columns

#Writes out the parsed data to a csv
write.csv(p16, "totalPop_v16.csv", row.names = FALSE)

# Pipe of functions that parses the data
l16=v16%>% #Original Data to be passed
  gather(year, value, -OBS)%>% # takes original data and reshapes it long from wide
  filter(grepl("LFRESC", OBS))%>% # takes long data and filters obs without "JOBSI0C" in the OBS column
  separate(OBS, c("variable", "countyfips"), sep=6 ) #Splits the OBS column into the variable name and county number in separate columns

#Writes out the parsed data to a csv
write.csv(l16, "totalLabor_v16.csv", row.names = FALSE)

# Pipe of functions that parses the data
l17=v17%>% #Original Data to be passed
  gather(year, value, -OBS)%>% # takes original data and reshapes it long from wide
  filter(grepl("LFRESC", OBS))%>% # takes long data and filters obs without "JOBSI0C" in the OBS column
  separate(OBS, c("variable", "countyfips"), sep=6 ) #Splits the OBS column into the variable name and county number in separate columns

#Writes out the parsed data to a csv
write.csv(l17, "totalLabor_v17.csv", row.names = FALSE)

# Pipe of functions that parses the data
l18=v18%>% #Original Data to be passed
  gather(year, value, -OBS)%>% # takes original data and reshapes it long from wide
  filter(grepl("LFRESC", OBS))%>% # takes long data and filters obs without "JOBSI0C" in the OBS column
  separate(OBS, c("variable", "countyfips"), sep=6 ) #Splits the OBS column into the variable name and county number in separate columns

#Writes out the parsed data to a csv
write.csv(l18, "totalLabor_v18.csv", row.names = FALSE)

##2017 added:
# Pipe of functions that parses the data
j17=v17%>% #Original Data to be passed
  gather(year, value, -OBS)%>% # takes original data and reshapes it long from wide
  filter(grepl("JOBSI0C", OBS))%>% # takes long data and filters obs without "JOBSI0C" in the OBS column
  separate(OBS, c("variable", "countyfips"), sep=7 ) #Splits the OBS column into the variable name and county number in separate columns

#Writes out the parsed data to a csv
write.csv(j17, "totalJobs_v17.csv", row.names = FALSE)

# Pipe of functions that parses the data
p17=v17%>% #Original Data to be passed
  gather(year, value, -OBS)%>% # takes original data and reshapes it long from wide
  filter(grepl("ADJPOPC", OBS))%>% # takes long data and filters obs without "JOBSI0C" in the OBS column
  separate(OBS, c("variable", "countyfips"), sep=7 ) #Splits the OBS column into the variable name and county number in separate columns

#Writes out the parsed data to a csv
write.csv(p17, "totalPop_v17.csv", row.names = FALSE)

##2018 added:
# Pipe of functions that parses the data
j18=v18%>% #Original Data to be passed
  gather(year, value, -OBS)%>% # takes original data and reshapes it long from wide
  filter(grepl("JOBSI0C", OBS))%>% # takes long data and filters obs without "JOBSI0C" in the OBS column
  separate(OBS, c("variable", "countyfips"), sep=7 ) #Splits the OBS column into the variable name and county number in separate columns

#Writes out the parsed data to a csv
write.csv(j18, "totalJobs_v18.csv", row.names = FALSE)

# Pipe of functions that parses the data
p18=v18%>% #Original Data to be passed
  gather(year, value, -OBS)%>% # takes original data and reshapes it long from wide
  filter(grepl("ADJPOPC", OBS))%>% # takes long data and filters obs without "JOBSI0C" in the OBS column
  separate(OBS, c("variable", "countyfips"), sep=7 ) #Splits the OBS column into the variable name and county number in separate columns

#Writes out the parsed data to a csv
write.csv(p18, "totalPop_v18.csv", row.names = FALSE)


# Pipe of functions that parses the data
rj13=rv13%>% #Original Data to be passed
  gather(year, value, -OBS)%>% # takes original data and reshapes it long from wide
  filter(grepl("JOBSI0R", OBS))%>% # takes long data and filters obs without "JOBSI0C" in the OBS column
  separate(OBS, c("variable", "regionnumber"), sep=7 ) #Splits the OBS column into the variable name and county number in separate columns

#Writes out the parsed data to a csv
write.csv(rj13, "totalJobsReg_v13.csv", row.names = FALSE)



# Pipe of functions that parses the data
rj14=rv14[,-1]%>% #Original Data to be passed
  gather(year, value, -OBS)%>% # takes original data and reshapes it long from wide
  filter(grepl("JOBSI0R", OBS))%>% # takes long data and filters obs without "JOBSI0C" in the OBS column
  separate(OBS, c("variable", "regionnumber"), sep=7 ) #Splits the OBS column into the variable name and county number in separate columns

#Writes out the parsed data to a csv
write.csv(rj14, "totalJobsReg_v14.csv", row.names = FALSE)


# Pipe of functions that parses the data
rj15=rv15%>% #Original Data to be passed
  gather(year, value, -OBS)%>% # takes original data and reshapes it long from wide
  filter(grepl("JOBSI0R", OBS))%>% # takes long data and filters obs without "JOBSI0C" in the OBS column
  separate(OBS, c("variable", "regionnumber"), sep=7 ) #Splits the OBS column into the variable name and county number in separate columns
#Writes out the parsed data to a csv
write.csv(rj15, "totalJobsReg_v15.csv", row.names = FALSE)

# Pipe of functions that parses the data
rj16=rv16%>% #Original Data to be passed
  gather(year, value, -OBS)%>% # takes original data and reshapes it long from wide
  filter(grepl("JOBSI0R", OBS))%>% # takes long data and filters obs without "JOBSI0C" in the OBS column
  separate(OBS, c("variable", "regionnumber"), sep=7 ) #Splits the OBS column into the variable name and county number in separate columns
#Writes out the parsed data to a csv
write.csv(rj16, "totalJobsReg_v16.csv", row.names = FALSE)

# Pipe of functions that parses the data
rj17=rv17%>% #Original Data to be passed
  gather(year, value, -OBS)%>% # takes original data and reshapes it long from wide
  filter(grepl("JOBSI0R", OBS))%>% # takes long data and filters obs without "JOBSI0C" in the OBS column
  separate(OBS, c("variable", "regionnumber"), sep=7 ) #Splits the OBS column into the variable name and county number in separate columns
#Writes out the parsed data to a csv
write.csv(rj17, "totalJobsReg_v17.csv", row.names = FALSE)

# Pipe of functions that parses the data
rj18=rv18%>% #Original Data to be passed
  gather(year, value, -OBS)%>% # takes original data and reshapes it long from wide
  filter(grepl("JOBSI0R", OBS))%>% # takes long data and filters obs without "JOBSI0C" in the OBS column
  separate(OBS, c("variable", "regionnumber"), sep=7 ) #Splits the OBS column into the variable name and county number in separate columns
#Writes out the parsed data to a csv
write.csv(rj18, "totalJobsReg_v18.csv", row.names = FALSE)


# rp15=rv15[,-1]%>% #Original Data to be passed
#   gather(year, value, -OBS)%>% # takes original data and reshapes it long from wide
#   filter(grepl("ADJPOPR", OBS))%>% # takes long data and filters obs without "JOBSI0C" in the OBS column
#   separate(OBS, c("variable", "regionnumber"), sep=7 ) #Splits the OBS column into the variable name and county number in separate columns
# 
# #Writes out the parsed data to a csv
# write.csv(rp15, "totalPopReg_v15.csv", row.names = FALSE)
# 
# rl15=rv15[,-1]%>% #Original Data to be passed
#   gather(year, value, -OBS)%>% # takes original data and reshapes it long from wide
#   filter(grepl("LFRESR", OBS))%>% # takes long data and filters obs without "JOBSI0C" in the OBS column
#   separate(OBS, c("variable", "regionnumber"), sep=6 ) #Splits the OBS column into the variable name and county number in separate columns
# 
# #Writes out the parsed data to a csv
# write.csv(rl15, "totalLaborReg_v15.csv", row.names = FALSE)


#Removes the objects from the environment
rm(j14,v14, j15, v15, j13, v13)