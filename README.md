# ```Multi-Scale Event Detection (MSED)```

**Team** : Diego Silva de Salles (CEFET/RJ), Eduardo Ogasawara (CEFET/RJ), Eduardo Bezerra (CEFET/RJ), Rafaelli Coutinho (CEFET/RJ), Carlos E. Mello (UNIRIO), Cristiane Gea (CEFET/RJ). 

**Abstract**: Information published in the communication media, such as government transitions, economic crises, or corruption scandals, is an external factor associated with financial time series. These factors can be related to events of increased uncertainty in the time series. External factors can have different cycles of fluctuations, affecting a time series over months or years. In particular, these external factors can raise the perceived financial risk and manifest as two main events in the time series: anomalies and change points. Discovering these events in the financial time series is challenging but can help minimize the investment risk. This paper presents Multi-Scale Event Detect (MSED), a technique for detecting events in financial time series. It compares the events found by the detection methods in the Intrinsic Mode Function (IMF) components with the external factors labels obtained through the Economic Policy Uncertainty (EPU) index. Our results identified a correlation between the uncertainty variations present in the EPU, with events detected in a financial time series. Using the proposed approach, it is possible to determine the most predominant nature of events based on the uncertainty variations presented in the EPU series. This information allows to specify a set of time series where the influence of uncertainty generates acceptable events for a certain investment profile, thus mitigating the risk in the investment to which it is intended to be exposed.

**Acknowledgments**: The authors thank CNPq, CAPES, and FAPERJ for partially sponsoring this research.



*** Method MSED ***:

Multiscale Event Detect
'''Args:
    serie (Pandas Datafrmae): Dataframe with time series
    noise.amp (number): Amplitude of white noise to use in denoising algorithm
    frequency_date (number): Time frequency time series default 12 months
    start_date (date): Initial date of time series
    stop_rule (string): As quoted from the EMD package documentation: ”The stop rule of sifting. The
            type1 stop rule indicates that absolute values of envelope mean must be less than
            the user-specified tolerance level in the sense that the local average of upper and
            lower envelope is zero. The stopping rules type2, type3, type4 and type5 are the
            stopping rules given by equation (5.5) of Huang et al. (1998), equation (11a),
            equation (11b) and S stoppage of Huang and Wu (2008), respectively.” default: type1
    soft_window (number): size window of soft evaluate default: 15
    trials (number): Number of times to run EMD default:100
    limit_ri_sup & limit_ri_inf (number) : Upper and lower limits of ratio approach in the energy approach filtering method | default (2.14,1.50)
    rf_dataframe: Dataframe with labels obtained from the EPU series
    type_events (list) : types of events that will be obtained
Returns:
    Pandas Dataframe: Metrics F1, Precision and Recall'''



*** Run method ***

The files brasil-stocks.R, chine-stocks.R, currencies.R, and usa-stocks.R must be executed to obtain the results presented in the article.