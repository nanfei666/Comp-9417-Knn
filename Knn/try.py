import numpy as np



make = {'alfa-romero':'1', 'audi':'2', 'bmw':'3', 'chevrolet':'4', 'dodge':'5', 'honda':'6','isuzu':'7', 'jaguar':'8', 'mazda':'9', 'mercedes-benz':'10', 'mercury':'11','mitsubishi':'12', 'nissan':'13', 'peugot':'14', 'plymouth':'15', 'porsche':'16','renault':'17', 'saab':'18', 'subaru':'19', 'toyota':'20', 'volkswagen':'21', 'volvo':'22'}
fuel_type ={'diesel':'1', 'gas':'2'}
aspiration={'std':'1', 'turbo':'2'}
num_of_doors={'four':'1', 'two':'2'}
body_style ={'hardtop':'1', 'wagon':'2', 'sedan':'3', 'hatchback':'4', 'convertible':'5'}
drive_wheels = {'4wd':'1','fwd':'2','rwd':'3'}
engine_location ={'front':'1', 'rear':'2'}
engine_type ={'dohc':'1', 'dohcv':'2', 'l':'3', 'ohc':'4', 'ohcf':'5', 'ohcv':'6', 'rotor':'7'}
num_of_cylinders ={'eight':'1', 'five':'2', 'four':'3', 'six':'4', 'three':'5', 'twelve':'6', 'two':'7'}
fuel_system = {'1bbl':'1', '2bbl':'2', '4bbl':'3', 'idi':'4', 'mfi':'5', 'mpfi':'6', 'spdi':'7', 'spfi':'8'}


def File_to_Matrix(filname):
    with open(filname,'r') as car:
        line = car.readlines()
        price = []
        modefied=[]
        for a in line:
            lines = a.strip().split(',')
            if '?' not in lines:
                lines[1] = make[lines[1]]
                lines[2] = fuel_type[lines[2]]
                lines[3] = aspiration[lines[3]]
                lines[4] = num_of_doors[lines[4]]
                lines[5] = body_style[lines[5]]
                lines[6] = drive_wheels[lines[6]]
                lines[7] = engine_location[lines[7]]
                lines[13] = engine_type[lines[13]]
                lines[14] = num_of_cylinders[lines[14]]
                lines[16] = fuel_system[lines[16]]
                modefied.append(lines)
                
        Matrix = np.zeros((len(modefied),len(modefied[0])-2))
        Index = 0
        for subList in modefied:
            listFormLine = subList.copy()
            del subList[-2]
            del subList[7]
            Matrix[Index,:] = subList[0:len(subList)]
            price.append(listFormLine[-2])
            Index+=1
        return Matrix, price
def Knn_classify(test_set, train_set, price,k):
    train_set_size = train_set.shape[0]
    diff_Mat = np.tile(test_set,(train_set_size,1)) - train_set
    diff_Mat_sq = diff_Mat **2
    sqDistance = diff_Mat_sq.sum(axis=1)
    Distance = sqDistance ** 0.5
    sortedDistIndiex = Distance.argsort()
    price_count=[]
    for i in range(k):
        price_count.append(price[sortedDistIndiex[i]])
    sorted_Price_count = sorted(price_count,reverse=True)
    
    SUM = 0.0
    for j in range(len(sorted_Price_count)):
        SUM = SUM + float(sorted_Price_count[j])
    avg_prive = SUM/len(sorted_Price_count)
    
    return avg_prive

def Nomorlize(dataset):
    MinValue = dataset.min(0)
    MaxValue = dataset.max(0)
    ranges = MaxValue- MinValue
    Nomorlized_data = np.zeros(np.shape(dataset))
    m = dataset.shape[0]
    Nomorlized_data1 = dataset - np.tile(MinValue,(m,1))
    Nomorlized_data = Nomorlized_data1/ np.tile(ranges,(m,1))
    return Nomorlized_data 

if __name__ == '__main__':
    filename = 'car.txt'
    DataMat, price = File_to_Matrix(filename)
    DataMat = Nomorlize(DataMat)
    predicted_price = Knn_classify(np.array(DataMat[0]), DataMat, np.array(price),1)
    print(predicted_price)