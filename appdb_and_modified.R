###
#Html側で例外処理をしている場合は処理が抜けるので最後に目視で確認すること
#URLの生成で例外処理が発生した場合は35行目付近のURLを直打ちで代入すること（応急処置）

#対象サイト
#iphone topsales http://appdb.lab.applica.jp/jp/?date=2014-05-31&genre=game
#iphone paid_ranking http://appdb.lab.applica.jp/jp/?type=paid&date=2014-05-31&genre=game
#iphone http://appdb.lab.applica.jp/jp/?type=paid&date=2014-05-31&genre=game
#android topsales http://androdb.lab.applica.jp/jp/?date=2014-05-31&genre=game
#android paid_ranking http://androdb.lab.applica.jp/jp/?type=paid&date=2014-05-31&genre=game
#android paid_ranking http://androdb.lab.applica.jp/jp/?type=paid&date=2014-05-31&genre=game

#メモリ解放
rm(list=ls(all=TRUE))

#必要ライブラリ#初回だけinstall.packagesを実行（一度インストールすれば次回からは必要無し）
#install.packages("XML")
library("XML")

#書き出しディレクトリ指定
##例)"setwd("~/000_working_directory/R_workspace")"
#
getwd()#現在のワーキングディレクトリを確認

#★ここは書き換える事
#もしくはRstudioを使用している場合はctrl+shift+Kを押してGUIで設定可能
#setwd("~/000_working_directory/R_workspace"))#要書換　ここにエクスポートされるので


##作業スタート
#日付取得
today <- as.character(Sys.Date())

#HTML Parseを取得
and.top <- htmlParse(paste("http://androdb.lab.applica.jp/jp/?date=",today,"&genre=game",sep=""))
and.free <- htmlParse(paste("http://androdb.lab.applica.jp/jp/?type=paid&date=",today,"&genre=game",sep=""))#ここ直しました
and.paid <- htmlParse(paste("http://androdb.lab.applica.jp/jp/?type=paid&date=",today,"&genre=game",sep=""))

#and.topsalesから
#アプリタイトル抜き出し
and.top.title.df <- data.frame()#データの受け皿作成
for (i in 1:25){#25位まで回す
  and.top.title.xml <- xpathSApply(and.top, '//*[@class="app_title"]')#htmlをxmlに変換
  and.top.title <- xmlValue(and.top.title.xml[[i]])#xmlを値に変換
  entry.frame.title <-(data.frame(and.top.title=and.top.title))#i番目のデータフレーム作成
  and.top.title.df <- rbind(and.top.title.df,entry.frame.title)#データフレームをくっつける作業
}#一旦おわり

#前日順位抜き出し
and.top.l1rank.df <- data.frame()#以下アプリタイトルと同様
for(i in 1:25){
  and.top.date.xml <- xpathSApply(and.top, '//*[@class="date"]',function(x) gsub('\t','',xmlValue(x)))
  and.top.date.trm <- gsub('\n','',and.top.date.xml)
  and.top.date.trm.sub <- and.top.date.xml[[i]]
  entry.frame.l1rank <-(data.frame(l1_rank=and.top.date.trm.sub))
  and.top.l1rank.df <- rbind(and.top.l1rank.df,entry.frame.l1rank)
}
and.top.l1rank.mtrx <- as.matrix(and.top.l1rank.df)#データが汚かったので行列化して掃除
and.top.l1rank.mtrx.trim1 <- gsub('\n',' ',and.top.l1rank.mtrx)#改行を削除
and.top.l1rank.df2<- as.data.frame(and.top.l1rank.mtrx.trim1)#課題　半角スペースがなぜか抜けずリリース日がくっついてくる
#リリース日の削除は今後の課題orおまけ

#開発会社抜き出し　ok
and.top.infobox.xml <- xpathSApply(and.top, '//*[@class="info_box"]/a')#アプリタイトル同様
and.top.cp.df <- data.frame()
for(i in 0:24){
  i <- 2+(i*3)#htmlで開発会社名にclassが指定されていなかったため一階層上のinfoboxから取得。そのせいで初項2の階差数列扱い
  cp.name.xml <- and.top.infobox.xml[[i]]
  cp.name <- xmlValue(cp.name.xml)
  entry.frame <-(data.frame(cp.name=cp.name))
  and.top.cp.df <- rbind(and.top.cp.df,entry.frame)
}

#merge ゲームタイトル名、前日順位、開発会社のデータフレームをマージ
and.top.name <- paste("and.top.dm_",today,".csv",sep="")
and.top.dm <- cbind(and.top.title.df,and.top.l1rank.df2,and.top.cp.df)
write.csv(and.top.dm,and.top.name)

##1ジャンル終了



############################################################


#and.freeから
#アプリタイトル抜き出し
and.free.title.df <- data.frame()#データの受け皿作成
for (i in 1:25){#25位まで回す
  and.free.title.xml <- xpathSApply(and.free, '//*[@class="app_title"]')#htmlをxmlに変換
  and.free.title <- xmlValue(and.free.title.xml[[i]])#xmlを値に変換
  entry.frame.title <-(data.frame(and.free.title=and.free.title))#i番目のデータフレーム作成
  and.free.title.df <- rbind(and.free.title.df,entry.frame.title)#データフレームをくっつける作業
}#一旦おわり

#前日順位抜き出し
and.free.l1rank.df <- data.frame()#以下アプリタイトルと同様
for(i in 1:25){
  and.free.date.xml <- xpathSApply(and.free, '//*[@class="date"]',function(x) gsub('\t','',xmlValue(x)))
  and.free.date.trm <- gsub('\n','',and.free.date.xml)
  and.free.date.trm.sub <- and.free.date.xml[[i]]
  entry.frame.l1rank <-(data.frame(l1_rank=and.free.date.trm.sub))
  and.free.l1rank.df <- rbind(and.free.l1rank.df,entry.frame.l1rank)
}
and.free.l1rank.mtrx <- as.matrix(and.free.l1rank.df)#データが汚かったので行列化して掃除
and.free.l1rank.mtrx.trim1 <- gsub('\n',' ',and.free.l1rank.mtrx)#改行を削除
and.free.l1rank.df2<- as.data.frame(and.free.l1rank.mtrx.trim1)#課題　半角スペースがなぜか抜けずリリース日がくっついてくる
#リリース日の削除は今後の課題orおまけ

#開発会社抜き出し　ok
and.free.infobox.xml <- xpathSApply(and.free, '//*[@class="info_box"]/a')#アプリタイトル同様
and.free.cp.df <- data.frame()
for(i in 0:24){
  i <- 2+(i*3)#htmlで開発会社名にclassが指定されていなかったため一階層上のinfoboxから取得。そのせいで初項2の階差数列扱い
  cp.name.xml <- and.free.infobox.xml[[i]]
  cp.name <- xmlValue(cp.name.xml)
  entry.frame <-(data.frame(cp.name=cp.name))
  and.free.cp.df <- rbind(and.free.cp.df,entry.frame)
}

#merge ゲームタイトル名、前日順位、開発会社のデータフレームをマージ
and.free.name <- paste("and.free.dm_",today,".csv",sep="")
and.free.dm <- cbind(and.free.title.df,and.free.l1rank.df2,and.free.cp.df)
write.csv(and.free.dm,and.free.name)

#2ジャンル終了

############################################################


#and.paidから
#アプリタイトル抜き出し
and.paid.title.df <- data.frame()#データの受け皿作成
for (i in 1:25){#25位まで回す
  and.paid.title.xml <- xpathSApply(and.paid, '//*[@class="app_title"]')#htmlをxmlに変換
  and.paid.title <- xmlValue(and.paid.title.xml[[i]])#xmlを値に変換
  entry.frame.title <-(data.frame(and.paid.title=and.paid.title))#i番目のデータフレーム作成
  and.paid.title.df <- rbind(and.paid.title.df,entry.frame.title)#データフレームをくっつける作業
}#一旦おわり

#前日順位抜き出し
and.paid.l1rank.df <- data.frame()#以下アプリタイトルと同様
for(i in 1:25){
  and.paid.date.xml <- xpathSApply(and.paid, '//*[@class="date"]',function(x) gsub('\t','',xmlValue(x)))
  and.paid.date.trm <- gsub('\n','',and.paid.date.xml)
  and.paid.date.trm.sub <- and.paid.date.xml[[i]]
  entry.frame.l1rank <-(data.frame(l1_rank=and.paid.date.trm.sub))
  and.paid.l1rank.df <- rbind(and.paid.l1rank.df,entry.frame.l1rank)
}
and.paid.l1rank.mtrx <- as.matrix(and.paid.l1rank.df)#データが汚かったので行列化して掃除
and.paid.l1rank.mtrx.trim1 <- gsub('\n',' ',and.paid.l1rank.mtrx)#改行を削除
and.paid.l1rank.df2<- as.data.frame(and.paid.l1rank.mtrx.trim1)#課題　半角スペースがなぜか抜けずリリース日がくっついてくる
#リリース日の削除は今後の課題orおまけ

#開発会社抜き出し　ok
and.paid.infobox.xml <- xpathSApply(and.paid, '//*[@class="info_box"]/a')#アプリタイトル同様
and.paid.cp.df <- data.frame()
for(i in 0:24){
  i <- 2+(i*3)#htmlで開発会社名にclassが指定されていなかったため一階層上のinfoboxから取得。そのせいで初項2の階差数列扱い
  cp.name.xml <- and.paid.infobox.xml[[i]]
  cp.name <- xmlValue(cp.name.xml)
  entry.frame <-(data.frame(cp.name=cp.name))
  and.paid.cp.df <- rbind(and.paid.cp.df,entry.frame)
}

#merge ゲームタイトル名、前日順位、開発会社のデータフレームをマージ
and.paid.name <- paste("and.paid.dm_",today,".csv",sep="")
and.paid.dm <- cbind(and.paid.title.df,and.paid.l1rank.df2,and.paid.cp.df)
write.csv(and.paid.dm,and.paid.name)

##終了
