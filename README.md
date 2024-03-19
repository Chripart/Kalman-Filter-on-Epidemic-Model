# Kalman-Filter-on-Epidemic-Model

In this project, I created 2 new epidemic models called HCRD and HCRD-R both of which track the admissions and discharges of patients in hospitals and ICU with COVID-19.

The second model differs from the first as it adds a new parameter in the equation of the recovered state, the cases that have recovered outside of the hospital. Also in this model we smoothe with a moving average of order 8 a vital time-series in our analysis, yielding betters results   

I then compare both HCRD and HCRD-R models and their stochastic counterparts which I created using the Kalman Filter, and concluding that Kalman filter has improved greatly both models and the second stochastic model is overall the best at predicting the states.

