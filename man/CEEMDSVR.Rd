\name{CEEMDSVR}
\alias{CEEMDSVR}
\title{
Complementary Ensemble Empirical Mode Decomposition Based Support Vector Regression Model
}
\description{
The CEEMDSVR function helps to fit the Complementary Ensemble Empirical Mode Decomposition with Adaptive Noise Based Support Vector Regression Model. It will also provide you with accuracy measures along with an option to select the proportion of training and testing data sets. Users can choose among the available choices of kernel and types of regresion model for fitting the Support Vector Regression model. In this package we have modelled the dependency of the study variable assuming first order autocorrelation. This package will help the researchers working in the area of hybrid machine learning models.
}
\usage{
CEEMDSVR(data,k,ensem.size, ker.funct="",svm.type="")
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
The available choices of kernel functions like radial basis, linear, polynomial and sigmoidfor fitting Support Vector Regression. By default radial basis function works.
}
  \item{svm.type}{
SVM can be used as a regression machine. User can apply eps-regression or nu-regression. By default the CEEMDSVR uses eps-regression.
}
}
\details{
Torres et al.(2011) proposed Complementary Ensemble Empirical Mode Decomposition with Adaptive Noise (CEEMDAN). This algorithm generates a Fewer IMFs on the premise of successfully separating different components of a series, which can reduce the computational cost. Further Support Vector Regression (SVR) model applied to each decomposed items to forecast them. Finally all forecasted values are aggregated to produce final forecast value (Das et al, 2020).
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
Torres, M.E., Colominas, M.A., Schlotthauer, G. and Flandrin, P. (2011). A complete ensemble empirical mode decomposition with adaptive noise. In 2011 IEEE international conference on acoustics, speech and signal processing (ICASSP) (pp. 4144--4147). IEEE.

Das,P., Jha,G. K.,Lama,A., Parsad, R. and Mishra, D. (2020). Empirical Mode Decomposition based Support Vector Regression for Agricultural Price Forecasting. Indian Journal of Extension Education, 56(2): 7-12. (http://krishi.icar.gov.in/jspui/handle/123456789/44138).

Das, P. (2019). Study On Machine Learning Techniques Based Hybrid Model for Forecasting in Agriculture. Published Ph.D. Thesis. (http://krishikosh.egranth.ac.in/handle/1/5810147805).
}
\seealso{
EMDSVRhybrid, CEEMD, EEMDSVR
}
\examples{
set.seed(6)
example_data=rnorm(500,30,5)
CEEMDSVR(example_data,0.9,250,"radial","nu-regression")
}
\keyword{CEEMDSVR}
\keyword{CEEMDAN}
\keyword{SVR}
