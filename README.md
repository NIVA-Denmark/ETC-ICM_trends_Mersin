# ETC-ICM_trends_Mersin

### Preprocessing

The folder [data](/data/) contains the following files:

The file received from METU was a text file `data_from_Mersin.txt`. This is data exported from an odv file. Before importing into R, I used a visual basic script `preprocess.wsf` to manipulate the data into a format more easily read by R. In theory this could all be done directly in R but it was quicker for me to add this extra step outside R.
The script file was run using the windows batch file `preprocess.bat`. The restructured data is saved as the file  `data_from_Mersin_for_R.txt`.

### import to R
The file received from METU was a text file `data_from_Mersin.txt`. This is data exported from an odv file. Before 