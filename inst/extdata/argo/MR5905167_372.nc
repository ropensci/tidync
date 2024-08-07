CDF       
      N_PROF        N_LEVELS  �   N_CALIB       STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         	DATE_TIME         N_PARAM       	N_HISTORY                title         &Argo float merged core and bio profile     institution       US GDAC    source        
Argo float     history       2019-02-08T07:31:30Z   
references        (http://www.argodatamgt.org/Documentation   user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         M   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                     D�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    D�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    D�   REFERENCE_DATE_TIME       
         	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    D�   DATE_CREATION         
         	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    D�   DATE_UPDATE       
         	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    D�   PLATFORM_NUMBER                    	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    E   PROJECT_NAME                   	long_name         Name of the project    
_FillValue                  @  E   PI_NAME                    	long_name         "Name of the principal investigator     
_FillValue                  @  EP   STATION_PARAMETERS                        conventions       Argo reference table 3     	long_name         ,List of available parameters for the station   
_FillValue                 �  E�   CYCLE_NUMBER                	long_name         Float cycle number     
_FillValue         ��   conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle           GP   	DIRECTION                   	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    GT   DATA_CENTRE                    	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    GX   DC_REFERENCE                   	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     G\   DATA_STATE_INDICATOR                   	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    G|   	DATA_MODE                   	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    G�   PLATFORM_TYPE                      	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     G�   PARAMETER_DATA_MODE                    	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    G�   FLOAT_SERIAL_NO                    	long_name         Serial number of the float     
_FillValue                     G�   FIRMWARE_VERSION                   	long_name         Instrument firmware version    
_FillValue                     G�   WMO_INST_TYPE                      	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    G�   JULD                standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    conventions       8Relative julian days with decimal part (as parts of day)   units         "days since 1950-01-01 00:00:00 UTC     
resolution        >�����h�   
_FillValue        A.�~       axis      T           G�   JULD_QC                 	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    G�   JULD_LOCATION                   	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           G�   LATITUDE                	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   	valid_min         �V�        	valid_max         @V�        axis      Y      
_FillValue        @�i�            H   	LONGITUDE                   	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    	valid_min         �f�        	valid_max         @f�        axis      X      
_FillValue        @�i�            H   POSITION_QC                 	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    H   POSITIONING_SYSTEM                     	long_name         Positioning system     
_FillValue                    H   VERTICAL_SAMPLING_SCHEME          	         	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    H    CONFIG_MISSION_NUMBER                   	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        I    PROFILE_PRES_QC                 	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    I$   PROFILE_TEMP_QC                 	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    I(   PROFILE_PSAL_QC                 	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    I,   PROFILE_DOXY_QC                 	long_name         #Global quality flag of DOXY profile    conventions       Argo reference table 2a    
_FillValue                    I0   PROFILE_CHLA_QC                 	long_name         #Global quality flag of CHLA profile    conventions       Argo reference table 2a    
_FillValue                    I4   PROFILE_BBP700_QC                   	long_name         %Global quality flag of BBP700 profile      conventions       Argo reference table 2a    
_FillValue                    I8   PROFILE_BBP532_QC                   	long_name         %Global quality flag of BBP532 profile      conventions       Argo reference table 2a    
_FillValue                    I<   PRES                
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    axis      Z      
_FillValue        G�O�   	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  I@   PRES_QC                    	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Q    PRES_ADJUSTED                   	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    
_FillValue        G�O�   	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  R�   PRES_ADJUSTED_QC                   	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Z�   PRES_ADJUSTED_ERROR                    	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   units         decibar    
_FillValue        G�O�     �  \�   TEMP                	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  d`   TEMP_QC                    	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  l    TEMP_ADJUSTED                   	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  n   TEMP_ADJUSTED_QC                   	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  u�   TEMP_ADJUSTED_ERROR                    	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   
_FillValue        G�O�     �  w�   PSAL                	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_QC                    	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �@   PSAL_ADJUSTED                   	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �0   PSAL_ADJUSTED_QC                   	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR                    	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   
_FillValue        G�O�     �  ��   DOXY                
   	long_name         Dissolved oxygen   standard_name         *moles_of_oxygen_per_unit_mass_in_sea_water     units         micromole/kg   axis      Z      
_FillValue        G�O�   	valid_min         ��     	valid_max         D     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   DOXY_QC                    	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �`   DOXY_ADJUSTED                   	   	long_name         Dissolved oxygen   standard_name         *moles_of_oxygen_per_unit_mass_in_sea_water     units         micromole/kg   
_FillValue        G�O�   	valid_min         ��     	valid_max         D     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �P   DOXY_ADJUSTED_QC                   	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   DOXY_ADJUSTED_ERROR                    	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         micromole/kg   C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o   
_FillValue        G�O�     �  �    CHLA                   	long_name         Chlorophyll-A      standard_name         0mass_concentration_of_chlorophyll_a_in_sea_water   units         mg/m3      
_FillValue        G�O�   C_format      %9.5f      FORTRAN_format        F9.5   
resolution        7'Ŭ     �  ��   CHLA_QC                    	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   CHLA_ADJUSTED                      	long_name         Chlorophyll-A      standard_name         0mass_concentration_of_chlorophyll_a_in_sea_water   units         mg/m3      
_FillValue        G�O�   C_format      %9.5f      FORTRAN_format        F9.5   
resolution        7'Ŭ     �  �p   CHLA_ADJUSTED_QC                   	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �0   CHLA_ADJUSTED_ERROR                    	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         mg/m3      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        =���   
_FillValue        G�O�     �  �    BBP700                     	long_name         )Particle backscattering at 700 nanometers      units         m-1    
_FillValue        G�O�   C_format      %10.8f     FORTRAN_format        F10.8      
resolution        2+�w     �  ��   	BBP700_QC                      	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ؠ   BBP700_ADJUSTED                    	long_name         )Particle backscattering at 700 nanometers      units         m-1    
_FillValue        G�O�   C_format      %10.8f     FORTRAN_format        F10.8      
resolution        2+�w     �  ڐ   BBP700_ADJUSTED_QC                     	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �P   BBP700_ADJUSTED_ERROR                      	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         m-1    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        =���   
_FillValue        G�O�     �  �@   BBP532                     	long_name         )Particle backscattering at 532 nanometers      units         m-1    
_FillValue        G�O�   C_format      %10.8f     FORTRAN_format        F10.8      
resolution        2+�w     �  �    	BBP532_QC                      	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   BBP532_ADJUSTED                    	long_name         )Particle backscattering at 532 nanometers      units         m-1    
_FillValue        G�O�   C_format      %10.8f     FORTRAN_format        F10.8      
resolution        2+�w     �  ��   BBP532_ADJUSTED_QC                     	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �p   BBP532_ADJUSTED_ERROR                      	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         m-1    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        =���   
_FillValue        G�O�     �  �`   	PARAMETER                            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                 �     SCIENTIFIC_CALIB_EQUATION                   	         	long_name         'Calibration equation for this parameter    
_FillValue                   �   SCIENTIFIC_CALIB_COEFFICIENT                	         	long_name         *Calibration coefficients for this equation     
_FillValue                   �   SCIENTIFIC_CALIB_COMMENT                	         	long_name         .Comment applying to this parameter calibration     
_FillValue                   �   SCIENTIFIC_CALIB_DATE                   
         	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  d �Argo profile merged             3.1 1.2 19500101000000  20190208073130  20190208073130  5905167 Argo Australia                                                  Nick Hardman-Mountford                                          PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            CHLA                                                            BBP700                                                          BBP532                                                            tA   CS  5905167/372                     2B  A   NAVIS_A                         AAARRRR 389                             BGCiA_060314                    863 @�h��k�1   @�i%�	��D�$tS�@b��'RTa1   GPS     Primary sampling: averaged []                                                                                                                                                                                                                                      A   F   A   A               @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D/��D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Da��Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx` 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��H@�z�@�z�A=qA>=qA^=qA~=qA��A��A��A��A��A��A��A��B�\B�\B�\B�\B'�\B/�\B7�\B?�\BG�\BO�\BW�\B_�\Bg�\Bo�\Bw�\B�\B�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮC��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D x�D ��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��D	x�D	��D
x�D
��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��D x�D ��D!x�D!��D"x�D"��D#x�D#��D$x�D$��D%x�D%��D&x�D&��D'x�D'��D(x�D(��D)x�D)��D*x�D*��D+x�D+��D,x�D,��D-x�D-��D.x�D.��D/x�D/�D0x�D0��D1x�D1��D2x�D2��D3x�D3��D4x�D4��D5x�D5��D6x�D6��D7x�D7��D8x�D8��D9x�D9��D:x�D:��D;x�D;��D<x�D<��D=x�D=��D>x�D>��D?x�D?��D@x�D@��DAx�DA��DBx�DB��DCx�DC��DDx�DD��DEx�DE��DFx�DF��DGx�DG��DHx�DH��DIx�DI��DJx�DJ��DKx�DK��DLx�DL��DMx�DM��DNx�DN��DOx�DO��DPx�DP��DQx�DQ��DRx�DR��DSx�DS��DTx�DT��DUx�DU��DVx�DV��DWx�DW��DXx�DX��DYx�DY��DZx�DZ��D[x�D[��D\x�D\��D]x�D]��D^x�D^��D_x�D_��D`x�D`��Dax�Da�Dbx�Db��Dcx�Dc��Ddx�Dd��Dex�De��Dfx�Df��Dgx�Dg��Dhx�Dh��Dix�Di��Djx�Dj��Dkx�Dk��Dlx�Dl��Dmx�Dm��Dnx�Dn��Dox�Do��Dpx�Dp��Dqx�Dq��Drx�Dr��Dsx�Ds��Dtx�Dt��Dux�Du��Dvx�Dv��Dwx�Dw��DxX�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A���A���A���A���A���A���A�ȴA�ƨA�A�A��jA��9A��9A��A���A���A���A�XA�A��DA�VA�O�A�bA��;A��A���A�A�ĜA�ȴA���A���A���A���A���A���A��
A���A��9A���A��+A�x�A��yA���A���A�VA�A��;A�oA}�A{�TAz�jAz(�Axn�AwS�Av1'AuS�AtVAr�/Aq�Ap�RAp�\Ap$�Ao�Ao/Am�FAmVAl~�Ak��AkC�AkVAj-Ai�-Ai��Ai�PAip�Ai?}Ah�`Ahn�AhJAg�wAgp�Ag�Af��AeAedZAd  Ac|�AcC�Ac�Ab�Ab�9AbJAa�-Aa%A_�wA^�jA]��A]|�A]`BA]7LA]�A\ZA[�#A[��AZ�jAZ�AZM�AY�AY��AY��AY��AY�hAY|�AYdZAYK�AY�AXr�AW�hAV��AV$�AU�ATĜATZAT$�AS�TAS�7AS|�AS%AQ�TAQ�AP�9AP�AO�AOhsAOK�AOoANĜAN9XAM/AL��ALQ�AK�AKp�AJE�AIC�AI�AH�/AHZAHE�AHffAH=qAGdZAG;dAF�/AF9XAE7LAC��AC33ACAB�jAB�\ABffAB�AA�TAA��AA?}A@�A?\)A?�A>��A>$�A=��A<��A<��A<{A;XA:z�A9�;A9�hA933A8��A7��A6��A6 �A6  A5�A5�PA5XA5K�A4��A3A2=qA1ƨA1�FA1�PA1hsA0��A0n�A0 �A/�TA.5?A,��A+�
A+�PA+XA*��A*z�A)�-A(z�A&�A&�!A&n�A&=qA&-A&  A%�mA%�;A%��A%�A%dZA%&�A$1'A"��A"JA!�A!p�A VAn�A �A�FA�-A��A��A��A��A��A�yA�A��Al�AVA�DAbNAM�A5?A9XAA��A`BA�9A�RAjA��AƨA|�A�9AVA��A��A��A��AbNA�FA��A��AO�A�!AVA��A"�AVA��A��A5?A��AhsA�A�An�A5?AA��A�hA;dA��AM�A�
Al�AoA
~�A
A�A
{A	�^A	�PA	x�A	l�A	XA	C�A	�A	oA��A�HA�jA�DA~�AZA(�A-A-A(�A�A{AJA1A  A�mA�hA7LA7LA/A"�AVA�`A��A��A�+Av�AffAQ�A1'A�PA��A�
A��AQ�A5?A1'A5?A-AJA��AS�A%A �A -@�S�@�^5@��h@��m@���@�ff@�-@���@��-@��@�V@��9@��D@�(�@��w@��@�\)@�"�@��T@�@�7@���@�b@�1@� �@��;@�@�l�@���@�M�@�h@�&�@�o@��@��@�(�@�l�@噚@��/@�9X@���@㕁@�K�@�;d@�o@�5?@ᙚ@�7L@���@���@�33@��@�ȴ@�$�@��T@���@��@�@�%@܃@�ƨ@���@�~�@�M�@���@���@֧�@�?}@�(�@�dZ@��H@�$�@���@�K�@���@˾w@�K�@�
=@��T@��/@ȓu@�Q�@���@���@��@���@�hs@ģ�@� �@þw@��H@�@�$�@���@��@�Ĝ@�  @���@�|�@�l�@�K�@��@�o@��@��-@�G�@��@��`@�j@�l�@�@�?}@�Z@��@�33@�E�@���@�bN@���@�t�@�|�@�dZ@�C�@�"�@�{@�x�@�O�@�/@�V@�j@��R@�^5@��@��@��@�bN@��F@��y@���@��@�dZ@�ff@��^@��h@�/@�(�@�+@���@��!@���3333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333A���A���A���A���A���A���A���A�ȴA�ƨA�A�A��jA��9A��9A��A���A���A���A�XA�A��DA�VA�O�A�bA��;A��A���A�A�ĜA�ȴA���A���A���A���A���A���A��
A���A��9A���A��+A�x�A��yA���A���A�VA�A��;A�oA}�A{�TAz�jAz(�Axn�AwS�Av1'AuS�AtVAr�/Aq�Ap�RAp�\Ap$�Ao�Ao/Am�FAmVAl~�Ak��AkC�AkVAj-Ai�-Ai��Ai�PAip�Ai?}Ah�`Ahn�AhJAg�wAgp�Ag�Af��AeAedZAd  Ac|�AcC�Ac�Ab�Ab�9AbJAa�-Aa%A_�wA^�jA]��A]|�A]`BA]7LA]�A\ZA[�#A[��AZ�jAZ�AZM�AY�AY��AY��AY��AY�hAY|�AYdZAYK�AY�AXr�AW�hAV��AV$�AU�ATĜATZAT$�AS�TAS�7AS|�AS%AQ�TAQ�AP�9AP�AO�AOhsAOK�AOoANĜAN9XAM/AL��ALQ�AK�AKp�AJE�AIC�AI�AH�/AHZAHE�AHffAH=qAGdZAG;dAF�/AF9XAE7LAC��AC33ACAB�jAB�\ABffAB�AA�TAA��AA?}A@�A?\)A?�A>��A>$�A=��A<��A<��A<{A;XA:z�A9�;A9�hA933A8��A7��A6��A6 �A6  A5�A5�PA5XA5K�A4��A3A2=qA1ƨA1�FA1�PA1hsA0��A0n�A0 �A/�TA.5?A,��A+�
A+�PA+XA*��A*z�A)�-A(z�A&�A&�!A&n�A&=qA&-A&  A%�mA%�;A%��A%�A%dZA%&�A$1'A"��A"JA!�A!p�A VAn�A �A�FA�-A��A��A��A��A��A�yA�A��Al�AVA�DAbNAM�A5?A9XAA��A`BA�9A�RAjA��AƨA|�A�9AVA��A��A��A��AbNA�FA��A��AO�A�!AVA��A"�AVA��A��A5?A��AhsA�A�An�A5?AA��A�hA;dA��AM�A�
Al�AoA
~�A
A�A
{A	�^A	�PA	x�A	l�A	XA	C�A	�A	oA��A�HA�jA�DA~�AZA(�A-A-A(�A�A{AJA1A  A�mA�hA7LA7LA/A"�AVA�`A��A��A�+Av�AffAQ�A1'A�PA��A�
A��AQ�A5?A1'A5?A-AJA��AS�A%A �A -@�S�@�^5@��h@��m@���@�ff@�-@���@��-@��@�V@��9@��D@�(�@��w@��@�\)@�"�@��T@�@�7@���@�b@�1@� �@��;@�@�l�@���@�M�@�h@�&�@�o@��@��@�(�@�l�@噚@��/@�9X@���@㕁@�K�@�;d@�o@�5?@ᙚ@�7L@���@���@�33@��@�ȴ@�$�@��T@���@��@�@�%@܃@�ƨ@���@�~�@�M�@���@���@֧�@�?}@�(�@�dZ@��H@�$�@���@�K�@���@˾w@�K�@�
=@��T@��/@ȓu@�Q�@���@���@��@���@�hs@ģ�@� �@þw@��H@�@�$�@���@��@�Ĝ@�  @���@�|�@�l�@�K�@��@�o@��@��-@�G�@��@��`@�j@�l�@�@�?}@�Z@��@�33@�E�@���@�bN@���@�t�@�|�@�dZ@�C�@�"�@�{@�x�@�O�@�/@�V@�j@��R@�^5@��@��@��@�bN@��F@��y@���@��@�dZ@�ff@��^@��h@�/@�(�@�+@���@��!@���3333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BbBbBbBbBbBbBbBbBhBoBoBoBoBoBoBoBhBhBDBB��B��B��B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�yB�/BB�9B��B��Bu�BG�B��B�mB�)B�
B��BɺBÖB�qB�RB�!B�B��B��B��B��B��B��B�{B�oB�\B�JB�DB�1B�B�B�B�B�B� B}�B{�Bz�Bx�Bw�Bu�Bq�Bp�Bl�BiyBiyBhsBgmBffBdZBbNB_;BYBP�BK�BG�BE�BC�BA�B<jB9XB8RB1'B/B.B+B(�B(�B(�B(�B(�B'�B&�B%�B!�B�B�B�BhBPB	7B1B%BBB��B��B�B�B�yB�`B�NB�HB�;B�/B�B��B��BȴBĜB�}B�FB�B�'B�B�B�B�B�FB�'B�B�B��B��B�hB�PB�DB�1B�%B�B�B� B|�Bz�Bt�BiyBhsBcTB_;B[#BS�BQ�BM�BF�B>wB9XB6FB2-B-B%�B�B�BuBhB\BPBJB+B��B�B�B�B�B�B�fB�NB�;B�#B��B��B�XB�FB�9B�!B�B��B��B�PB�DB�7B�1B�+B�%B�B�B�B� B~�B{�Bt�BgmBaHB`BBZBO�BA�B=qB:^B9XB9XB8RB8RB9XB7LB33B)�B'�B#�B�B�B�B�B�B�B�B�B�BoBhB\BDB	7B%B
��B
��B
��B
�B
�B
�B
�yB
�TB
�NB
�NB
�;B
�B
�
B
��B
��B
��B
��B
��B
ǮB
ĜB
B
��B
�wB
�jB
�^B
�XB
�XB
�LB
�?B
�'B
�B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�uB
�hB
�JB
�1B
�B
� B
� B
� B
� B
~�B
}�B
{�B
z�B
x�B
w�B
t�B
r�B
o�B
m�B
k�B
gmB
ffB
ffB
e`B
e`B
e`B
dZB
cTB
cTB
aHB
aHB
`BB
_;B
^5B
YB
S�B
O�B
M�B
K�B
K�B
K�B
K�B
J�B
I�B
H�B
G�B
E�B
C�B
A�B
@�B
?}B
>wB
<jB
:^B
9XB
9XB
7LB
6FB
6FB
5?B
5?B
33B
33B
2-B
2-B
1'B
/B
.B
-B
-B
-B
-B
-B
,B
+B
)�B
(�B
&�B
&�B
%�B
$�B
"�B
�B
�B
�B
�B
�B
�B
�B
{B
oB
hB
bB
bB
\B
VB
PB
PB
JB
JB
DB
DB

=B

=B
	7B
	7B
1B
1B
+B
+B
%B
%B
%B
%B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
  B
B
  B
  B
  B
  B
  B
  B
  B
  B	��B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BnBnBnBnBnBnBnBnBtBzBzBzBzBzBzBzBtBtBOB*B��B��B��B��B�B�B�B�B�B�B�B�B��B��B��B��B��B��B�B�B�B�B�:BB�EB��B��Bu�BG�B��B�xB�5B�B��B��BáB�|B�^B�,B�B��B��B��B��B��B��B��B�zB�hB�VB�OB�=B�+B�$B�$B�B�B�B~ B{�Bz�Bx�Bw�Bu�Bq�Bp�Bl�Bi�Bi�BhBgxBfrBdfBbZB_GBY#BP�BK�BG�BE�BC�BA�B<wB9dB8^B13B/'B.!B+B)B)B)B)B)B'�B&�B%�B!�B�B�B�BuB\B	CB=B1BBB� B��B�B�B�B�lB�ZB�TB�HB�;B�#B��B��B��BĨB��B�RB�'B�3B�'B�B�B�!B�RB�3B�'B�B��B��B�uB�\B�PB�>B�1B�%B�B�B|�Bz�Bt�Bi�BhBc`B_HB[/BTBQ�BM�BF�B>�B9eB6RB2:B-B%�B�B�B�BuBiB\BVB8B� B��B�B�B�B�B�sB�ZB�HB�/B��B��B�eB�RB�FB�.B�B��B��B�]B�PB�DB�>B�8B�2B�%B�%B�B�BB{�Bt�BgyBaUB`NBZ*BO�BA�B=~B:kB9eB9eB8_B8_B9eB7YB3@B*	B'�B#�B�B�B�B�B�B�B�B�B�B|BuBiBQB	DB2B
�B
��B
��B
�B
�B
�B
�B
�aB
�[B
�[B
�HB
�*B
�B
��B
��B
��B
��B
��B
ǻB
ĩB
B
��B
��B
�xB
�kB
�eB
�eB
�YB
�MB
�4B
�(B
�B
�	B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�vB
�WB
�>B
�B
�B
�B
�B
�B
B
~B
{�B
z�B
x�B
w�B
t�B
r�B
o�B
m�B
k�B
gzB
ftB
ftB
enB
enB
enB
dgB
caB
caB
aUB
aUB
`OB
_IB
^CB
Y$B
TB
O�B
M�B
K�B
K�B
K�B
K�B
J�B
I�B
H�B
G�B
E�B
C�B
A�B
@�B
?�B
>�B
<xB
:lB
9fB
9fB
7YB
6SB
6SB
5MB
5MB
3AB
3AB
2;B
2;B
14B
/(B
."B
-B
-B
-B
-B
-B
,B
+B
*
B
)B
&�B
&�B
%�B
$�B
"�B
�B
�B
�B
�B
�B
�B
�B
�B
|B
vB
pB
pB
jB
dB
^B
^B
XB
XB
QB
QB

KB

KB
	EB
	EB
?B
?B
9B
9B
3B
3B
3B
3B
-B
-B
-B
-B
-B
&B
-B
&B
&B
 B
 B
 B
B
B
B
 B
B
 B
 B
 B
 B
 B
 B
 B
 B	�B
 B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B
 B
 B
 B
 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�CU��CU��CU��CU��CU��CU�1CU�CU�uCV�CV#�CV�CV7�CV1�CV;�CV?�CV<�CV#CU�wCU��CU�jCU`�CU5tCT��CT��CT٫CTݷCT�1CU �CU!�CU4�CUL�CUC�CU5�CU'�CU`CT�rCT��CT�aCT)HCS� CS
�CR
�CPnKCO5�CMe�CK42CH\:CCu/C>2C<��C;�C;�C9�'C8�C8�YC8�kC8�#C8�/C9&C91[C9.�C9�C9�C9oC9f�C9��C9��C9�C: �C:�C9��C:5tC:\�C:sDC:Y:C:x�C:�VC:øC:�C;)C;:�C;h�C;��C<#dC<�PC<�C=:_C=XvC=FTC=B�C=VC=DC=VfC=)�C<��C<�cC<�C<��C<�C<�oC<��C<��C<�8C<�FC<��C<�7C=�C=�C=:C=/AC=3=C=�C=�C<��C<�pC<�C<��C<��C<�xC=2}C=}�C=��C=�1C=�"C=��C=�KC=�]C=~hC=nC=�C=~�C=o�C=ZC=w�C=�xC=��C=��C=�!C=wC=~�C=viC=MXC=;aC=@�C=�OC>�RC?��C?�jC@��CBHCDZ�CE�(CE�&CE��CE~QCE�'CEA�CECD׶CDz{CC�fCC�&CCl�CCp1CCbLCCW�CC37CB�CC�CC#�CC'�CC3�CC�CCOJCC.�CB�CB~�CBD�CB4�CA�'CA|-CAEjC@�C@�3C@�TC@�C@��C@��C@wC@,WC@ C@�C@.�C@N�C@C{C@K�C@>oC@C�C@zC?�uC?�fC?��C?�C?�KC?�wC?�kC?�~C@C@UjC@p�C@�sC@ͼC@�CC@�@C@�xC@�3C@��C@��C@�C@�oC@��C@�)C@�HCAS�CA�FCB*CB�TCD'�CEFMCE�}CFqCF:CF#.CF%�CF&�CF`�CF�'CH xCH��CI`�CJCJ�CJ��CJ2 CH�CF�CC��CC[)CB�CB�{CB�)CC�CCk�CD�CD�fCE}�CF�CG"CG�pCH5+CH?gCHt�CHpCH��CH�CH�!CHʈCH�CH�CI�CI�CI�CH�CH�TCHswCH9�CG�lCGs/CG�CF��CF�CEu�CE&CD��CDy�CC��CCa�CB�lCB�/CB-vCA��CA��CA�CAk~CAH�CA5C@�C@��C@�C@\C?��C?i�C>��C>�~C>�#C>��C>nFC>l�C>TAC>HkC>'�C>0+C>!7C>)C>�C=�)C=�UC=�*C=�_C=�C=�MC=��C=��C=~�C=
C=V�C=Y2C=)�C=ZC<��C<��C<F;C<RC;��C<3C<"C<�C<HC;�C;�AC;��C;�!C;vC;?�C;]C:�C:�C:�C:A	C:>�C:E:C:C�C:M!C:@�C:+�C:�C:�C9��C9�sC9��C9��C9s�C9/_C8�+C8V�C8)�C81�C8�C85�C86IC8�C8�C7��C7�YC7�9C7�OC7x�C7GpC7ZOC73)C7�C6��C6��C6�2C6��C6��C6�5C6�vC6}�C6a�C6>qC65fC6-�C60�C6IC5�
C5�8C5�BC5�}C5�JC5�KC5�gC5o�C5b'C5A<C5�C4�C4�uC4�#C4��C4cfC4.C4 C3ʦC3�C3�pC3O5C3
C2��C2{C2m�C2g�C2NC2=C1�C1��C1�C1��C1��C1n�C1b�C1B�C1fC1
VC0�C0��C0�zC0��C0�UC0��C0n�C0R�C0Z]C0Q�C0=�C00�C0�C/��C/�C/��C/��C/�dC/_C/3�C.�eC.��C.�}C.idC.W=C.#HC-��C-��C-��C-��C-��C-�(C-��C-hC-LC-$�C,�RC,��C,�C,��C,nUC,C,�C+ݒC+��C+�7C+w�C+=fC*�oC*�	C*`�C**UC*�C)�4C)�iC)�nC)�C)�C)�*C)��C*�1C+>�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?(ی?0A�?)�^?.�M?0A�?,V�?*)�?0��?<�v?6YK?4,=?5zx?Ic?9ԕ?P�?;"�?B�8?GRT??�W?O�$?5��?8��?0�?n/?	�>��>��?�K?�j?�K?�?~�?H�?Q�?�?<�?%ϫ?-5�?)�^?;?;? bN>�e�>ˬq>�>�x>�V�>x�5>.{=.{<��`<�C�<�0�<^҉<'�<'�<B�8<C�<^҉<^҉<C�<^҉<'�<'�<B�8;�҉<'�<B�8<C�<B�8<C�<'�<C�<^҉<'�<C�<'�<'�<C�;�҉<C�;�҉<'�;�҉<'�;��;�҉;�҉<'�<C�<C�<'�:�҉<C�<'�<'�;�҉<C�<C�;�҉;�҉;��<C�;�҉;��;�҉<'�;��<C�;��;�҉;�҉;�҉<C�<C�<C�;�҉;�҉<C�;�҉<'�<B�8;�҉;�҉<C�<C�;��<B�8;�҉;�҉<C�<C�;��;��<B�8;^҉<C�;�҉;�҉;�҉;��<'�<C�;�҉<C�;�҉<C�<C�<'�;�҉<C�<C�;�҉<C�;�҉<C�<B�8<'�;�҉;��<'�;�҉<'�;��<'�<C�<'�;�҉;�҉;�҉;��<C�<C�;�҉;�҉;��<'�<'�<B�8<C�<'�;�҉;�҉<'�<'�;^҉;�҉<C�<C�<C�<C�<'�;�҉<'�<C�<C�;�҉<'�<'�<B�8;�҉;��<'�;�҉;��<B�8;��;�҉<'�<z��;��<C�<'�;�҉<B�8<B�8<C�<C�<C�<C�<C�;��<'�<C�<C�<C�;^҉<'�;�҉;��;�҉;��;�҉<C�;�҉;��<'�;�҉;��<C�;��<C�<'�;�҉<C�<'�;�҉;�҉;��<C�;�҉<C�;��<^҉;�҉<'�<C�;��;�҉<C�;��<C�;��;^҉<C�<'�<C�<C�;��;�҉;��;�҉;�҉;��;��<C�;��;^҉;�҉;��;��;�҉;^҉;�҉;�҉<C�;�҉;�҉;��;�҉;�҉<'�;�҉;�҉<'�<C�<C�<C�;��;��<C�;��<'�;�҉;�҉;��;��;��;^҉<^҉;��;��<C�;^҉;��<C�;��<'�;��;�҉<'�<^҉<'�;��<C�;^҉<C�<'�<C�<'�<C�;�҉;^҉;�҉<'�;�҉<B�8;��<B�8<C�<'�<C�;��<B�8<C�<^҉;�҉<'�;��;��<C�;�҉;��<^҉;�҉<'�<'�<B�8<C�<C�<C�;^҉<B�8<'�<'�<C�;��;�҉;�҉<C�;^҉<'�;��<C�;�҉;�҉;�҉<C�<'�<'�<'�<C�;�҉;�҉<B�8<'�<'�;�҉<B�8;�҉<'�<C�<C�;�҉<C�;�҉<'�<'�<C�<'�<'�<B�8<'�<'�<'�<C�;�҉<C�;�҉;�҉;��<C�<C�;�҉<B�8<'�<^҉<'�<'�<'�;�҉<'�<C�<'�<C�<'�<C�<C�<'�<C�<B�8<C�<B�8<C�<^҉;�҉<C�<^҉<B�8<C�<^҉<B�8<B�8<C�<z��<'�<B�8<'�<C�<C�<'�<B�8<'�<^҉<B�8<^҉<'�<z��<B�8<z��<C�;�҉;�҉<'�<^҉<C�<B�8<'�<B�8<B�8<'�<'�<^҉<B�8<^҉<B�8<^҉<B�8<'�<^҉<B�8<B�8<C�<C�<B�8<^҉<z��<C�<'�<^҉<B�8<B�8<B�8<^҉<'�<C�0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�:1}:.(e:/Q�:/�p:/Q�:+��:1�:8�:1��:,i�:.�c:;�:>i:27�:9��:6G�:[p7::X2:4�:AM�:*�:#�R:#�:�:��:��: �:��:�H:# :!�:!Z:)z�:$��:%j�:,��:D/�:.j:(Q!:�{:%i�:�::��:��:�:f�:W9�4	9��9�Y9�Kw9���9�.9�/�9�(�9��w9�9��9�q9�>9�o�9���9��9�ц9�eJ9�r9�+z9���9��e9�Sw9��9� �9�r�9���9�q�9��9�9�);9�%�9�]�9���9�Z+9��9��U9��9��C9�49�U�9�
�9�2�9�(�9���9�Ά9�,�9�L�9��K9�j�9���9��n9�n�9�l9���9�4+9���9��9��9�α9�y9�%9�w_9�^9���9���9�T�9��w9�k�9��
9�h�9�+9�n�9�i�9�k�9�Y69�`9�9�S}9�!�9��9��9��9�/89���9�	9��"9��b9�9��9��l9��y9�l�9��X9��9�}�9�;�9��59���9���9�L�9���9�e9�i=9�be9���9��Q9�އ9�JP9�p9�׬9�c9���9�O99�V�9��9��-9�X9�^�9���9�HZ9���9��9�>�9��9�~9�39�.�9���9��Z9�/9�v�9��49��9�k09��9�;�9�:�9��U9���9�V�9�M�9��.9��\9�f!9��s9��9��9���9���9��H9�\�9��19��9�Y�9�:9�`�9��~9�t�9��9��49��e9��
9���9���9�:T9�|�9��T9���9�̄9�p9��9��9�� 9��9�!9��/9�%�9�c�9��9��49�,�9���9��<9��9��9��Z9�\9���9�[�9�#?9�9�W@9��9��9�$�9��T9���9��9�@A9��9�b9�V9��9��9�59��9��89� �9�g�9�qm9���9��a9�y9��)9��9�5f9�~9�2�9��?9���9�S�9��.9�nH9�DY9�R�9��9��9���9�k9�9�֪9�&�9�w\9�}9���9��9��9���9��9�cG9�69�q69�<9���9��,9���9�nE9�L�9���9�[9��z9�G�9��g9�g%9�ؑ9�Z?9�Y�9�.�9��9�Ǵ9�9ߝ�9���9���9Ɍ|9��9�z�9�z9�9���:��:3�:t�9�,{:��:�:0\:�.:��:�:?^9�D�:*�9࢒9���:�:(:h�:*� :H%:
`�:�:��:Sp:��:��9��|:8�:��:-�:. :Q:U��:9�:�I:��: P:>8:
�L:	�{:8�v::)�:��:V�$:�:�*9�7:�9�?f:��:]Q9���9�n�9�~/:8�: ��:�k:3_:	~�:%�h:~o:(:H:�::��:��:.��:�[:2��:c:=�`:!�:�):��:�):S��:��:��:+�:8\�:�:��:08�:�:bP:R:$n:��:�C:">Z:��:�@:��:��9���:,T:��:,�:?��:ө:a`i:JM:	�:L��:aQ:�l:lf:A�8:_�T:q-:�w:8�z:
'�:%�:�!:(R79��:wX:8�8:ǲ:<8:!�:�A:2!�9���:,Qj:�b:$�}:��: �7:X�:ϫ:I�:+!:)�-:��Y::}:"�:��:$��::$�:��:�$P:fa:�:P"�:W�M:FC�:�:I�p:�N:.�:fw:]�:F�3:��:0�:�:%v:
�:.[�:.�[:.Y�:C�S:,��:0��:H�:1�0:7�t:;:¹: ��:��:y2:"��:$S_:		�0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�:�۰:��q:��1:�2�:���:��1:�Y�:�Y�:���:���:�%�:�-3:��8:�,�:���:�զ:�W�:� �:���:�{�:�*t:��E:�֭:�+�:�R8:�&�:�Ӷ:��\:�)�:�P�:�P�:���:���:�ҧ:�|:��h:���:�{?:�O�:�ѝ:�Oi:�љ:���:��{:�*:��<:��:�N:��:��p:v��:v��:oO�:dC2:��>:g��:`|�:`s!:r��:g��:n��:c��:kD�:`F�:`>�:g�E:k%@:k:c�y:n��:c�F:`	�:gW�:\Z�:\Z�:`�:\W�:j�U:X��:\J�:\GI:\DJ:T��:T�:g-}:\-:\A:Q�:Mnn:ce:T��:Q�:_�#:Q�:I��:I�	:M<�:T�,:E�-:Eܬ:I��:I�:E�:M E:Md:M�:Ii:If�:P��:I_z:I_?:P��:M":E�K:B
:I[�:E��:TNl:IHP:P��:E��:>4E:T#m:I#h:A�/:A�y:A��:Ic:ElN:E_#:PR�:H��:A�(:A��:H��:H��:EA$:H��:P3�:A�#:E%�:Wq�:E:Ap�:H�x::3:H��:D��:H��:S��:O�L:O�s:L5~:S��:O��:L):Z¿:Z�;:SX�:�p:^O�:V�c:SP�:{��:O�:V��:SB�:O�g:Kک:Z|e:Oz�:^�:K�f:K��:K�s:V�b:w�:OJ::OB�:R�:]�:V�G:O-`:Kr�:G��:Kl.:Kg�:O:V`J:Z	\:Z�:aIW:K<y:Y܈:G�?:Nݳ:G�:R}�:G|�:]q[:N��:@�:RI�:N��:N�:R7�:J�b:Jڈ:dq&:��t:YN.:J��:NL�:U�~:NI0:J�-:Q�6:Q�:Y=$:N@�:Q�:J�&:U�b:Nu:Ud*:Ub�:U\�:UM�:B��:J1�:U'�:J+�:;� :k�:F��:Q~:F��:Fw:M�9:J}:M��:QZ;:M�I:QP�:? �:T�/:B�:j��:c��:FB�:T�~:F8�:F45:F.:B��:>Ԃ:F�:Mi�:Bbs:MUC:MU�:I��:E�:P�&:I�:I�}:E�p:I�5:I��:Iy�:M�:E�:B�:P��:P�:>`m:E�2:A�-:P�2:A�:A�#:A��:I9�:A��:E��:E�:I!]:Ep=:A��:T*:I:W��:A�w:=�q:A��:PC:6��:b��:A��:E@�::C�:A��:^�O:L��:S�U:f(�:^��:L�B:Sҷ:^ο:f �:���:f�:tÚ:bt�:|K:^�:mh�:bfl:�}:�U&:���:�m:��g:mX<:���:�j:�z�:�F�:�3:{�O:��<:�&:p�#:�TR:�M\:��s:��:���:���:���:K�:�k�:���:�
�:�O:�8�:��h:���:� �:�B�:�D�:~��:���:�t�:�DZ:���:���:���:�
�:�k9:��:��:���:�8:�(�:��:o��:��(:z�s:���:��:�EE:w=�:��B:�a�:w+?:��:��P:�%::�ʗ:��:�q�:��:��:�%�:��e:�^:��y:�XX:�,:��v:��J:��':��V:�F:��$:���:��:�W:�<=:��:�0:���:���:��:�	�:��Y:�Q�:�I:���:��.:�� :�ak:�4:��#:��p:��:��l:��*:��:���:�Z:��:��:�o�:�J�:��:��7:��:��:��:�	p:�4":�-b:�Rp:���:��V:�|�:��2:�� :��:�@�:�BZ:���:��v:ۢ�:�;�:��T:��+:��O:���:��:��:�~�:�Q:��>:�8}:��C:��M:���:���:��:��a:�
:�o�:�Ƙ:�G�:�zs:�ȫ:��!:�c�:�:��a:��x:�.':��:�):�vP:�'�:�y	:��H:��R:�7:���:��:���:�E:�p�:~�:�"k:��:��P:��:��:��0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES                                                            TEMP                                                            PSAL                                                            DOXY                                                            CHLA                                                            BBP700                                                          BBP532                                                          Pcorrected = Praw - surface offset                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              Scorrected = S(Ccorrected,Traw,Pcorrected)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      This sensor is subject to hysteresis                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            20190207034902              20190207034902                                                        