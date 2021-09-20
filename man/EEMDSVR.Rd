\name{EEMDSVR}
\alias{EEMDSVR}
\title{
Ensemble Empirical Mode Decomposition Based Support Vector Regression Model
}
\description{
The EEMDSVR function helps to fit the Ensemble Empirical Mode Decomposition with Adaptive Noise Based Support Vector Regression Model. It will also provide you with accuracy measures along with an option to select the proportion of training and testing data sets. Users can choose among the available choices of kernel and types of regresion model for fitting the Support Vector Regression model. In this package we have modelled the dependency of the study variable assuming first order autocorrelation. This package will help the researchers working in the area of hybrid machine learning models.
}
\usage{
EEMDSVR(data,k,ensem.size, ker.funct="",svm.type="")
}
\arguments{
  \item{data}{
Input univariate time series data.
}
  \item{k}{
Partition value for spliting the data set into training and testing.
}
  \item{ensem.size}{
Number of copies of the input signal to use as the ensemble.
}
  \item{ker.funct}{
The available choices of kernel functions like radial basis, linear, polynomial and sigmoid for fitting Support Vector Regression. By default radial basis function works.
}
  \item{svm.type}{
SVM can be used as a regression machine. User can apply eps-regression or nu-regression. By default the CEEMDSVR uses eps-regression.
}
}
\details{
Ensemble Empirical Mode Decomposition (EEMD) method was developed by Wu and Huang (2009).EEMD significantly reduces the chance of mode mixing and represents a substantial improvement over the original EMD. This algorithm generates a Fewer IMFs on the premise of successfully separating different components of a series. Further Support Vector Regression (SVR) model applied to each decomposed components to forecast them. Finally all forecasted values are aggregated to produce final forecast value (Das et al, 2020).
}
\value{
  \item{Total_No_IMF }{Total number of IMFs after decomposition by EEMD method.}
  \item{Prediction_Accuracy_CEEMDSVR }{List of performance measures of the fitted CEEMDSVR model.}
  \item{Final_Prediction_CEEMDSVR }{Final forecasted value of the CEEMDAN based SVR model. It is obtained by combining the forecasted value of all individual IMF and fresidue.}
}
\author{
Pankaj Das, Kapil Choudhary, Girish Kumar Jha, Achal Lama
}
\references{
Das, P. (2019). Study On Machine Learning Techniques Based Hybrid Model for Forecasting in Agriculture. Published Ph.D. Thesis. (http://krishikosh.egranth.ac.in/handle/1/5810147805).

Das,P., Jha,G. K.,Lama,A., Parsad, R. and Mishra, D. (2020). Empirical Mode Decomposition based Support Vector Regression for Agricultural Price Forecasting. Indian Journal of Extension Education, 56(2), 7-12. (http://krishi.icar.gov.in/jspui/handle/123456789/44138).

Wu, Z. and Huang, N.E. (2009) Ensemble empirical mode decomposition: a noise assisted data analysis method. Advances in adaptive data analysis, 1(1), 1-41.

}
\seealso{
EMDSVRhybrid, CEEMD
}
\examples{
set.seed(6)
example_data=rnorm(500,30,5)
EEMDSVR(example_data,0.9,250,"radial","nu-regression")
}
\keyword{EEMDSVR}
\keyword{EEMD}
\keyword{SVR}
