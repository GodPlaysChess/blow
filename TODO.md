Amplify articles classifier:

v.0
- store classifictaion model in some file 

2 functions:
 updateModel :: IO ()

- read bunch of files
- update the previous model, stored in files -> move files to processed

classify :: File -> Class

based on the model stored somewhere in the file - classifies the article


v.1
somehow make it as a webapp, so one can send the article over web ( look how the files are transfered and read ) , and return Class back as the API.

as an additioanl challenge to run it on Altus.. etc..

v.2 
Incorporate it to the aplify page. Here is the impact comes, mb dialogues with people whoe maintains the page etc..




--------------------
TODO:

+ db connection & crud
+ simple api
+ purescript frontend


======================= Arch

FE page - guest: just calendar view, that's it.
          cowoker: can modify calendar.
BE - authentication + looks up in db to project all data to the calendar view.
DB - postgre if I am not too lazy to do that. Or just a file in the beginning.


============================ (Data)
people (id, dates taken => days taken)
config (vacation allowed, calendar days)





features:

calendar view:
hover -> all info about the day. (yellow - normal, blue - someone is on vacation on this day)
all remaining vacations are visible.




steps:
as a beginning can just return a json with `yr -> all days` -> 