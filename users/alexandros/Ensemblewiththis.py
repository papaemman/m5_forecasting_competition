import numpy as np # linear algebra
import pandas as pd # data processing, CSV file I/O (e.g. pd.read_csv)

#import os
#for dirname, _, filenames in os.walk('/kaggle/input'):
#    for filename in filenames:
#        print(os.path.join(dirname, filename))


df0 = pd.read_csv('submission_e40-poiss-simple0.469.csv')
df1 = pd.read_csv('submission_ensembled390.46628.csv')
df2 = pd.read_csv('submission_ensembled45-0.46598.csv')
df3 = pd.read_csv('submission_e50-0.47150.csv')
df4 = pd.read_csv('submission_ensembled62-0.46286.csv')
df5 = pd.read_csv('submissionLstm.csv')
df6 = pd.read_csv('submissionKwi3-2.csv')

df1.corrwith(df2, axis = 0)


df0 = df0.sort_values(by = 'id').reset_index(drop = True)
df1 = df1.sort_values(by = 'id').reset_index(drop = True)
df2 = df2.sort_values(by = 'id').reset_index(drop = True)
df3 = df3.sort_values(by = 'id').reset_index(drop = True)
df4 = df4.sort_values(by = 'id').reset_index(drop = True)
df5 = df5.sort_values(by = 'id').reset_index(drop = True)
df6 = df6.sort_values(by = 'id').reset_index(drop = True)
sub = df1.copy()


#==============
#You may use the following types of ensemble - just change the df ids 
#====================================================================================================================================
#this is used to create a normal ensembled submission. Might as well round it up to 2 decimals. If you dont want to round it up just remove np around
##====================================================================================================================================

#for i in sub.columns :
#    if i != 'id' :
#        sub[i] = np.around(0.7022*df5[i] + 0.20878*df6[i] + 0.12 * df2[i], decimals = 2) # + 0.6224 * df3[i] + 0.08 * df4[i]
        
#sub.to_csv('submission_ensembled8.csv', index = False)

#print(sub[0:25])
#print(sub.shape)
##====================================================================================================================================

#This is used to create a submission using only certain items like predictions for HOUSEHOLD_2 or HOBBIES_2 from a submission file and replacing the predictions from another
#submission file with these. You may also use weights e.g. I want slicedweight * HOBBIES_2 predictions and (1-slicedweight) * original. You may want to remove np.around lets keep it for now
#=====================================================================================
#sliceit = sub['id'].str.contains('HOUSEHOLD_2' or 'HOBBIES_2')
#sliceit = np.array(sliceit)
#app = []
#compofapp = []
#slicedweight = 0.812
#for i in sliceit:
#    if i == True:
#        app.append(slicedweight)
#        compofapp.append(1 - slicedweight)
#    else:
#        app.append(0)
#        compofapp.append(1)
#
#app = np.array(app)
#compofapp = np.array(compofapp)
#
#print(app[58212:58321])
#
#for i in sub.columns :
#    if i != 'id' :
#        print(i)
#        sub[i] = np.around(compofapp*df10[i] + app * df3[i], decimals = 2) # + 0.6224 * df3[i] + 0.08 * df4[i]
#        
#sub.to_csv('submission_ensembled14.csv', index = False)
#
#print(sub[0:25])
#print(sub.shape)


#===================================================

#========================================================
#This replaces  values near an integer with a value slightly closer to that integer 
#print(df15[500:514])
#for i in sub.columns:
#    if i != 'id':
#        sub[i] = df15[i]
#        for j in range (0, sub.shape[0]):
#            if np.abs(sub[i][j] - np.rint(sub[i][j])) < 0.081:
#                sub[i][j] = np.around(sub[i][j] - (sub[i][j] - np.rint(sub[i][j])) / 2.7, decimals = 2) 
#
#        
#          
#
#sub.to_csv('submission_ensembled30monster.csv', index = False)
#
#print(sub[500:514])



#================================================================================
#Replace values with values closer to integer - another version more aggressive
#print(df15[500:514])
#for i in sub.columns :
#    if i != 'id' :
#        sub[i] = np.around(0.7442*df21[i] + 0.2561 * df20[i], decimals = 2) # + 0.6224 * df3[i] + 0.08 * df4[i]
#
#
#for i in sub.columns:
#    if i != 'id':
#        #sub[i] = np.around(df18[i], decimals = 2)
#        for j in range (0, sub.shape[0]):
#            if np.abs(sub[i][j] - np.rint(sub[i][j])) < 0.091:
#                sub[i][j] = np.around(sub[i][j] - (sub[i][j] - np.rint(sub[i][j])) / 1.58, decimals = 2)
#            if np.abs(sub[i][j] - np.rint(sub[i][j])) < 0.201 and np.abs(sub[i][j] - np.rint(sub[i][j])) > 0.08231:
#                sub[i][j] = np.around(sub[i][j] - (sub[i][j] - np.rint(sub[i][j])) / 6.28, decimals = 2)
#
#        
#
#
#sub.to_csv('submission_ensembled41monster.csv', index = False)
#
#print(sub[500:514])
#================================================================================
#

##=====================================================================================
##A normal ensemble. Used to ensemble long term predictions since they are considered more difficult to predict. e.g use a strong model with a simpler one for logn term preds
##
#for i in sub.columns :
#    if i != 'id' :
#        sub[i] = df21[i] # + 0.6224 * df3[i] + 0.08 * df4[i]
#        
#
#sub['F16'] = 0.9304 * df21['F16'] + 0.0686 * df9['F16']
#sub['F14'] = 0.9974 * df21['F14'] + 0.0003 * df9['F14']
#sub['F18'] = 0.9000 * df21['F18'] + 0.1000 * df9['F18']
#sub['F19'] = 0.8600 * df21['F19'] + 0.1400 * df9['F19']
#sub['F21'] = 0.8700 * df21['F21'] + 0.1300 * df9['F21']
#sub['F22'] = 0.8580 * df21['F22'] + 0.1420 * df9['F22']
#sub['F23'] = 0.8190 * df21['F23'] + 0.1810 * df9['F23']
#sub['F24'] = 0.8000 * df21['F24'] + 0.2004 * df9['F24']
#sub['F25'] = 0.7501 * df21['F25'] + 0.2500 * df9['F25']
#sub['F26'] = 0.7484 * df21['F26'] + 0.2530 * df9['F26']
#sub['F28'] = 0.7100 * df21['F28'] + 0.2900 * df9['F28']
#sub['F27'] = 0.7000 * df21['F27'] + 0.3000 * df9['F27']
#
#sub.to_csv('submission_ensembled44.csv', index = False)
#
#print(sub[0:25])
#print(sub.shape)



##==============================================================
#print(df5[200:240])
#print(df5.shape)
##this merges CA_2 predicitons from a submission file with eg 6000 rows to a normal one its suboptimal takes some time but its not that useful given that you can
#go back and fill with zeros items from other stores and use the previous ensemble method. 
#for i in range(0, df24.shape[0]):
#    if "CA_2" in df24['id'][i]:
#        for j in range(0, df25.shape[0]):
#            if df24['id'][i] == df25['id'][j]:
#                print("Matching ", i, "with ", j)
#                for q in df24.columns:
#                    if q != 'id':
#                        df24[q][i] = 0.8*df25[q][j] + 0.2*df24[q][i]
#
#            
#
#sub.to_csv('submission_ensembled70.csv', index = False)
#print(sub[0:25])
#print(sub.shape)

#Another method I used - if values predicted from 2 models are not far apart:
#                               use predictions from the strongest
#                        else:
#                               use 0.7 *pred1 + 0.3*pred2  



        
