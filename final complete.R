#function that takes in two kenyan number plates and returns car boughts in between

no_of_cars<-function(n_1,n_2){
a<-c(rep("K",17558424))#Generates first element (first character, that is K) of a kenyan number plate

b_1<-c(rep(675324,26))
b<-c(rep(c(LETTERS),b_1))#Generates second element(second character) of a kenyan number plate

c_1<-c(rep(675324,26))
c<-rep(c(LETTERS),c_1)#Generates third element (third character) of a kenyan number plate

d_1<-rep(c(0,1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9),c(99,1,99,1,99,1,99,1,99,1,99,1,99,1,99,1,99,1,99))
d<-c(rep(d_1,17576))#Generates fourth element (first numeric) of a kenyan number plate

e_0<-rep(c(0,1,2,3,4,5,6,7,8,9),c(9,10,10,10,10,10,10,10,10,10))
e_1<-rep(c(0,1,2,3,4,5,6,7,8,9),c(10,10,10,10,10,10,10,10,10,10))
e_2<-rep(e_1,8)
e_3<-c(e_0,e_1,e_2)
e<-c(rep(e_3,17576))#Generates fifth element (second numeric) of a kenyan number plate

f_1<-rep(c(1,2,3,4,5,6,7,8,9,0),c(1,1,1,1,1,1,1,1,1,1))
f_2<-rep(f_1,99)
f_3<-c(1:9)
f_4<-c(f_2,f_3)
f<-c(rep(f_4,17576))#Generates sixth element (third numeric) of a kenyan number plate

g_1<-c(rep(999,26))
g_2<-rep(c(LETTERS),g_1)
g<-c(rep(g_2,676))#Generates seventh element (last character) of a kenyan number plate

#Generating a matrix of kenyan number plates
car_plates<-matrix(c(a,b,c,d,e,f,g),nrow=17558424,byrow=F)

#Locating the row in number plates matrix that corresponds to the first input of the function
for(i in 1:17558424){
if((car_plates[i,1]==n_1[1])&&(car_plates[i,2]==n_1[2])&&(car_plates[i,3]==n_1[3])&&(car_plates[i,4]==n_1[4])&&(car_plates[i,5]==n_1[5])&&(car_plates[i,6]==n_1[6])&&(car_plates[i,7]==n_1[7])){
n_1_row<-i
}
}

#Locating the row in number plates matrix that corresponds to the second input of the function
for(j in 1:17558424){
if((car_plates[j,1]==n_2[1])&&(car_plates[j,2]==n_2[2])&&(car_plates[j,3]==n_2[3])&&(car_plates[j,4]==n_2[4])&&(car_plates[j,5]==n_2[5])&&(car_plates[j,6]==n_2[6])&&(car_plates[j,7]==n_2[7])){
n_2_row<-j
}
}

#Computing cars bought in between the two kenyan number plates
if(n_1_row<n_2_row){
cars_bought<-n_2_row-n_1_row+1
}else{
cars_bought<-n_1_row-n_2_row+1
}
return(cars_bought)
}

##Testing cars bought between KAA001A and KAA010A
no_of_cars(c("K","A","A","0","0","1","A"),c("K","A","A","0","1","0","A"))
