
#Complementary Ensemble Empirical Mode Decomposition Based Support Vector Regression Model
CEEMDSVR=function(data,k,ensem.size, ker.funct="",svm.type=""){

  data_org=as.matrix(data)
  xt=as.matrix(data_org)
  xt=as.vector(data_org)

  #code for display no.of imf and residual

  try=Rlibeemd::ceemdan(xt,ensemble_size =ensem.size)

  imf_extr=try[,-ncol(try)]
  total_IMF=ncol(imf_extr)
  emd_residual=try[,ncol(try)]
  no_of_imf=ncol(imf_extr)
  len_extr_imf=length(imf_extr[,1])
  length_split=len_extr_imf-1
  test_data_l=ceiling(k*length_split)
  test_data_original=data_org[(test_data_l+2):length(data_org),]
  length_test_data=length(test_data_original)
  # dataset creation
  extr_imf=0
  model_svm=0
  predicted_out=matrix(nrow =length_test_data,ncol = no_of_imf)
  MSE_out=0
  RMSE_out=0
  MAPE_out=0
  MAD_out=0
  final_predict_imf=0
  for (i in 1:no_of_imf)
  {
    extr_imf=imf_extr[,i]

    yt=extr_imf[1:(len_extr_imf-1)]
    xt=extr_imf[2:len_extr_imf]
    data=data.frame(yt,xt)
    len_data=length(data[,1])
    split_train=k*len_data
    r_train=ceiling(split_train)
    traindata=data[1:r_train,]
    testdata=data[(r_train+1):len_data,]

    model_svm<-e1071::svm(yt ~ ., data=traindata,kernel=ker.funct,type=svm.type)
    print(model_svm)
    predicted_out[,i]<- stats::predict(model_svm,testdata)
    final_predict_imf=final_predict_imf+predicted_out[,i]
  }
  emd_residual
  lenght_of_residual=length(emd_residual)

  #differencing
  dif_resid=base::diff(emd_residual)
  len_dresid=length(dif_resid)
  #spliting of data set
  ytr=dif_resid[1:(len_dresid-1)]
  xtr=extr_imf[2:len_dresid]
  datar=data.frame(ytr,xtr)
  len_datar=length(datar[,1])
  split_trainr=k*len_datar
  r_trainr=round((split_trainr),1)
  traindatar=datar[1:r_trainr,]
  testdatar=datar[(r_trainr+1):len_datar,]

  model_svmr <-e1071::svm(ytr ~ ., data=traindatar,kernel=ker.funct,type=svm.type)
  summary(model_svmr)

  #out sample
  predicted_outr <- stats::predict(model_svmr,testdatar)
  length_residual_predict=length(testdatar[,1])
  adding_residual_length=lenght_of_residual-length_residual_predict
  final_prediction=final_predict_imf+predicted_outr+emd_residual[-(1:adding_residual_length)]

  # summarize accuracy
  MSE_out <- mean((test_data_original - final_prediction)^2)
  RMSE_out<- sqrt(MSE_out)


  #mean absolute deviation (MAD)
  MAD_out=mean(abs(test_data_original - final_prediction))


  #Mean absolute percent error (MAPE)
  MAPE_out=mean(abs((test_data_original-final_prediction)/test_data_original))


  #Maximum Error
  ME_out=max(abs(test_data_original-final_prediction))
  #accuracy
  prediction_accuracy=cbind(RMSE_out,MAD_out,MAPE_out,ME_out)
  #ploting IMF
  Plot_IMFs <- try
  AllIMF_plots <- graphics::plot(Plot_IMFs)

  TotalIMF =  no_of_imf
  output_f=list(Total_No_IMF=TotalIMF, Prediction_Accuracy_CEEMDSVR =prediction_accuracy, Final_Prediction_CEEMDSVR =final_prediction)
  return(output_f)
}
