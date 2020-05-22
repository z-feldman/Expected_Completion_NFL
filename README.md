# Expected_Completion_NFL
Modeling expected completion in the NFL using Generalized Additive Models and XGBoost with publicly available data.
2nd Place in 2020 Ohio State Sports and Society Undergraduate Research Fair for Outstanding Sports Performance Analytics.



TODO:
Comment code a little more - lol.

Upload data as CSVs (currently only in RDS).

Re-upload plots, looks like I uploaded wrong file type.

Install nflfastR, get data back to whenever it has (2000 maybe?), train model(s) for all of it - okay looks like '06 is where air-yards starts.

Check on Mac vs. Windows XGBoost training - not same results between OS. 
See if saving the model and loading it in other OS works, rather than training from scratch - should work but need to check.


Want TODOs: 
Add in weather data. Temperature, wind (speed and direction), humidity, precipitation 
(live would be awesome, start of game may still be good also).

Test different kinds of explanatory, predictive, and stability measures for different position groups. CPOE was originally created to evaluate QBs but if we approach it agnostic of who drives the stat we can more accurately tease out how each position group affects things.
Update: QBs expected completion and cpoe are about same stability year over year (r^2 ~ .12 iirc). 
        WRs expected completion year over year r^2 ~.25 iirc, cpoe only ~.02. WRs play big role in air yards, air yards play        big role in expected completion, WRs don't drive performance over expected.
        Head Coach had like r^2 year over year ~.16 iirc for expected completion. Could be scheme based, could be correlated to QB (successful head coach will keep job, more likely to be successful if qb is successful and therefore has no change at qb, allowing the year over year stability to be tied to that).

Test if expected completion itself is a good measure to use for evaluation rather than only the CPOE (i.e. is a CPOE of 0% w/ an expected completion of 67% better/worse/same than a CPOE of 3% with an expected completion of 64%). 
