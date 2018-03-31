Amplify articles classifier:

v.0
- store classifictaion model in some file 

2 functions:
 updateModel :: IO ()

- read bunch of files
- update the previous model, stored in files -> move files to processed

classify :: File -> Class

based on the model stored somewhere in the file - classifies the article
