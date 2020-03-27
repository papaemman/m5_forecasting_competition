import numpy as np
import pandas as pd
import matplotlib
matplotlib.use('TkAgg')
import matplotlib.pyplot as plt
DataArray = pd.read_csv('../../data/raw/sales_train_validation.csv')

#Plotting number of items sold per day for each store
#==========================================================================================================
#Total CA_1 sales
#==========================================================================================================

DataArray_CA1 = DataArray.loc[DataArray['store_id'] == 'CA_1']


summer_ca1 = np.zeros(shape = 1913)
for i in range (0, 3049):
    summer_ca1 = summer_ca1 + DataArray_CA1.iloc[i,6:]

print(summer_ca1)

plt.figure(figsize=(20, 6))
plt.plot(summer_ca1, label = "Total CA_1 sales")
plt.title("Total CA_1 sales")
plt.xticks(np.arange(0,1919,100))
plt.legend()
plt.show()

#==========================================================================================================

#==========================================================================================================
#Total CA_2 sales
#==========================================================================================================

DataArray_CA2 = DataArray.loc[DataArray['store_id'] == 'CA_2']


summer_ca2 = np.zeros(shape = 1913)
for i in range (0, 3049):
    summer_ca2 = summer_ca2 + DataArray_CA2.iloc[i,6:]


print(summer_ca2)

plt.figure(figsize=(20, 6))
plt.plot(summer_ca2, label = "Total CA_2 sales", c = 'red')
plt.title("Total CA_2 sales")
plt.xticks(np.arange(0,1919,100))
plt.legend()
plt.show()


#==========================================================================================================
#Total CA_1 and CA_2 sales
#==========================================================================================================

plt.figure(figsize=(20, 6))
plt.plot(summer_ca2, label = "Total CA_2 sales", c = 'red')
plt.plot(summer_ca1, label = "Total CA_1 sales", c = 'blue')
plt.title("Total CA_1 and CA_2 sales")
plt.xticks(np.arange(0,1919,100))
plt.legend()
plt.show()


#==========================================================================================================
#Total CA_3 sales
#==========================================================================================================

DataArray_CA3 = DataArray.loc[DataArray['store_id'] == 'CA_3']


summer_ca3 = np.zeros(shape = 1913)
for i in range (0, 3049):
    summer_ca3 = summer_ca3 + DataArray_CA3.iloc[i,6:]


print(summer_ca3)

plt.figure(figsize=(20, 6))
plt.plot(summer_ca3, label = "Total CA_3 sales", c = 'black')
plt.title("Total CA_3 sales")
plt.xticks(np.arange(0,1919,100))
plt.legend()
plt.show()

#==========================================================================================================
#Total CA_1 and CA_3 sales
#==========================================================================================================

plt.figure(figsize=(20, 6))
plt.plot(summer_ca3, label = "Total CA_3 sales", c = 'black')
plt.plot(summer_ca1, label = "Total CA_1 sales", c = 'blue')
plt.title("Total CA_1 and CA_3 sales")
plt.xticks(np.arange(0,1919,100))
plt.legend()
plt.show()


#==========================================================================================================
#Total CA_4 sales
#==========================================================================================================

DataArray_CA4 = DataArray.loc[DataArray['store_id'] == 'CA_4']


summer_ca4 = np.zeros(shape = 1913)
for i in range (0, 3049):
    summer_ca4 = summer_ca4 + DataArray_CA4.iloc[i,6:]


print(summer_ca4)

plt.figure(figsize=(20,6))
plt.plot(summer_ca4, label = "Total CA_4 sales", c = 'green')
plt.title("Total CA_4 sales")
plt.xticks(np.arange(0,1919,100))
plt.legend()
plt.show()


#==========================================================================================================
#Total CA1 and CA2 and CA3 and CA4 sales
#==========================================================================================================

plt.figure(figsize=(20, 6))
plt.plot(summer_ca3, label = "Total CA_3 sales", c = 'black')
plt.plot(summer_ca1, label = "Total CA_1 sales", c = 'blue')
plt.plot(summer_ca2, label = "Total CA_2 sales", c = 'red')
plt.plot(summer_ca4, label = "Total CA_4 sales", c = 'green')
plt.title("Total CA sales")
plt.xticks(np.arange(0,1919,100))
plt.legend()
plt.show()

#==========================================================================================================

#==========================================================================================================
#Total TX_1 sales
#==========================================================================================================

DataArray_TX1 = DataArray.loc[DataArray['store_id'] == 'TX_1']


summer_tx1 = np.zeros(shape = 1913)
for i in range (0, 3049):
    summer_tx1 = summer_tx1 + DataArray_TX1.iloc[i,6:]


print(summer_tx1)

plt.figure(figsize=(20, 6))
plt.plot(summer_tx1, label = "Total TX_1 sales", c = 'orange')
plt.title("Total TX_1 sales")
plt.xticks(np.arange(0,1919,100))
plt.legend()
plt.show()


#==========================================================================================================
#Total TX_2 sales
#==========================================================================================================

DataArray_TX2 = DataArray.loc[DataArray['store_id'] == 'TX_2']


summer_tx2 = np.zeros(shape = 1913)
for i in range (0, 3049):
    summer_tx2 = summer_tx2 + DataArray_TX2.iloc[i,6:]


print(summer_tx2)

plt.figure(figsize=(20, 6))
plt.plot(summer_tx2, label = "Total TX_2 sales", c = 'purple')
plt.title("Total TX_2 sales")
plt.xticks(np.arange(0,1919,100))
plt.legend()
plt.show()

#==========================================================================================================
#Total TX_1 and TX_2 sales
#==========================================================================================================

plt.figure(figsize=(20, 6))
plt.plot(summer_tx1, label = "Total TX_1 sales", c = 'orange')
plt.plot(summer_tx2, label = "Total TX_2 sales", c = 'purple')
plt.title("Total TX_1 and TX_2 sales")
plt.xticks(np.arange(0,1919,100))
plt.legend()
plt.show()

#==========================================================================================================
#Total TX_3 sales
#==========================================================================================================

DataArray_TX3 = DataArray.loc[DataArray['store_id'] == 'TX_3']


summer_tx3 = np.zeros(shape = 1913)
for i in range (0, 3049):
    summer_tx3 = summer_tx3 + DataArray_TX3.iloc[i,6:]


print(summer_tx3)

plt.figure(figsize=(20, 6))
plt.plot(summer_tx3, label = "Total TX_3 sales", c = 'grey')
plt.title("Total TX_3 sales")
plt.xticks(np.arange(0,1919,100))
plt.legend()
plt.show()

#==========================================================================================================
#Total TX1 and TX2 and TX3 sales
#==========================================================================================================

plt.figure(figsize=(20, 6))
plt.plot(summer_tx1, label = "Total TX_1 sales", c = 'orange')
plt.plot(summer_tx2, label = "Total TX_2 sales", c = 'purple')
plt.plot(summer_tx3, label = "Total TX_3 sales", c = 'grey')
plt.title("Total TX sales")
plt.xticks(np.arange(0,1919,100))
plt.legend()
plt.show()

#=========================================================================================================

#==========================================================================================================
#Total WI_1 sales
#==========================================================================================================

DataArray_WI1 = DataArray.loc[DataArray['store_id'] == 'WI_1']


summer_wi1 = np.zeros(shape = 1913)
for i in range (0, 3049):
    summer_wi1 = summer_wi1 + DataArray_WI1.iloc[i,6:]


print(summer_wi1)

plt.figure(figsize=(20, 6))
plt.plot(summer_wi1, label = "Total WI_1 sales", c = 'cyan')
plt.title("Total WI_1 sales")
plt.xticks(np.arange(0,1919,100))
plt.legend()
plt.show()

#==========================================================================================================
#Total WI_2 sales
#==========================================================================================================

DataArray_WI2 = DataArray.loc[DataArray['store_id'] == 'WI_2']


summer_wi2 = np.zeros(shape = 1913)
for i in range (0, 3049):
    summer_wi2 = summer_wi2 + DataArray_WI2.iloc[i,6:]


print(summer_wi2)

plt.figure(figsize=(20, 6))
plt.plot(summer_wi2, label = "Total WI_2 sales", c = 'sandybrown')
plt.title("Total WI_2 sales")
plt.xticks(np.arange(0,1919,100))
plt.legend()
plt.show()

#==========================================================================================================
#Total WI_1 and WI_2 sales
#==========================================================================================================

plt.figure(figsize=(20, 6))
plt.plot(summer_wi1, label = "Total WI_1 sales", c = 'cyan')
plt.plot(summer_wi2, label = "Total WI_2 sales", c = 'sandybrown')
plt.title("Total WI_1 and WI_2 sales")
plt.xticks(np.arange(0,1919,100))
plt.legend()
plt.show()

#==========================================================================================================
#Total WI_3 sales
#==========================================================================================================

DataArray_WI3 = DataArray.loc[DataArray['store_id'] == 'WI_3']


summer_wi3 = np.zeros(shape = 1913)
for i in range (0, 3049):
    summer_wi3 = summer_wi3 + DataArray_WI3.iloc[i,6:]


print(summer_wi3)

plt.figure(figsize=(20, 6))
plt.plot(summer_wi3, label = "Total WI_3 sales", c = 'seagreen')
plt.title("Total WI_3 sales")
plt.xticks(np.arange(0,1919,100))
plt.legend()
plt.show()

#==========================================================================================================
#Total WI1 and WI2 and WI3 sales
#==========================================================================================================

plt.figure(figsize=(20,6))
plt.plot(summer_wi1, label = "Total WI_1 sales", c = 'cyan')
plt.plot(summer_wi2, label = "Total WI_2 sales", c = 'sandybrown')
plt.plot(summer_wi3, label = "Total WI_3 sales", c = 'seagreen')
plt.title("Total WI sales")
plt.xticks(np.arange(0,1919,100))
plt.legend()
plt.show()

#=========================================================================================================

#==========================================================================================================
#Total CA{i}, i = 1,2,3,4 and TX{j},WI{j}, j=1,2,3 sales
#==========================================================================================================

plt.figure(figsize=(20,6))
plt.plot(summer_wi1, label = "Total WI_1 sales", c = 'cyan')
plt.plot(summer_wi2, label = "Total WI_2 sales", c = 'sandybrown')
plt.plot(summer_wi3, label = "Total WI_3 sales", c = 'seagreen')
plt.plot(summer_ca3, label = "Total CA_3 sales", c = 'black')
plt.plot(summer_ca1, label = "Total CA_1 sales", c = 'blue')
plt.plot(summer_ca2, label = "Total CA_2 sales", c = 'red')
plt.plot(summer_ca4, label = "Total CA_4 sales", c = 'green')
plt.plot(summer_tx1, label = "Total TX_1 sales", c = 'orange')
plt.plot(summer_tx2, label = "Total TX_2 sales", c = 'purple')
plt.plot(summer_tx3, label = "Total TX_3 sales", c = 'grey')
plt.title("Total sales - Store level")
plt.xticks(np.arange(0,1919,100))
plt.legend()
plt.show()

#=========================================================================================================