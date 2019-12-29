report_r=read.csv("D://sem3 materials//labs//R programming//lab_project//water_data.csv")
report_r=report_r[,c(4:11)]
str(report_r)
report_r=na.omit(report_r)#to remove null values nan

Report_model=step(lm(DO~.,data=report_r),direction="both")
Report_model
a=data.frame(Temp=29.2,PH=6.3,CONDUCTIVITY=100,BOD=1.5,NITRATE=0.1,F_COLIFORM=7942,T_COLIFORM=13575)
result=predict(Report_model,a)
result

Report_model_for=step(lm(DO~1,data=report_r),direction="forward",scope=~ Temp+PH+CONDUCTIVITY+BOD+NITRATE+F_COLIFORM+T_COLIFORM)
Report_model_for
a_for=data.frame(Temp=29.2,PH=6.3,CONDUCTIVITY=100,BOD=1.5,NITRATE=0.1,F_COLIFORM=7942,T_COLIFORM=13575)
result_for=predict(Report_model_for,a)
result

Report_model_back=step(lm(DO~.,data=report_r),direction="backward")
Report_model_back
a_back=data.frame(Temp=29.2,PH=6.3,CONDUCTIVITY=100,BOD=1.5,NITRATE=0.1,F_COLIFORM=7942,T_COLIFORM=13575)
result_back=predict(Report_model_back,a)
result_back

