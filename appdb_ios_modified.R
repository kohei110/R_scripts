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
ios.top <- htmlParse(paste("http://appdb.lab.applica.jp/jp/?date=",today,"&genre=game",sep=""))
ios.free <- htmlParse(paste("http://appdb.lab.applica.jp/jp/?type=paid&date=",today,"&genre=game",sep=""))
ios.paid <- htmlParse(paste("http://appdb.lab.applica.jp/jp/?type=paid&date=",today,"&genre=game",sep=""))

#ios.topsalesから
#アプリタイトル抜き出し
ios.top.title.df <- data.frame()#データの受け皿作成
for (i in 1:25){#25位まで回す
ios.top.title.xml <- xpathSApply(ios.top, '//*[@class="app_title"]')#htmlをxmlに変換
ios.top.title <- xmlValue(ios.top.title.xml[[i]])#xmlを値に変換
entry.frame.title <-(data.frame(ios.top.title=ios.top.title))#i番目のデータフレーム作成
ios.top.title.df <- rbind(ios.top.title.df,entry.frame.title)#データフレームをくっつける作業
}#一旦おわり

#前日順位抜き出し
ios.top.l1rank.df <- data.frame()#以下アプリタイトルと同様
for(i in 1:25){
ios.top.date.xml <- xpathSApply(ios.top, '//*[@class="date"]',function(x) gsub('\t','',xmlValue(x)))
ios.top.date.trm <- gsub('\n','',ios.top.date.xml)
ios.top.date.trm.sub <- ios.top.date.xml[[i]]
entry.frame.l1rank <-(data.frame(l1_rank=ios.top.date.trm.sub))
ios.top.l1rank.df <- rbind(ios.top.l1rank.df,entry.frame.l1rank)
}
ios.top.l1rank.mtrx <- as.matrix(ios.top.l1rank.df)#データが汚かったので行列化して掃除
ios.top.l1rank.mtrx.trim1 <- gsub('\n',' ',ios.top.l1rank.mtrx)#改行を削除
ios.top.l1rank.df2<- as.data.frame(ios.top.l1rank.mtrx.trim1)#課題　半角スペースがなぜか抜けずリリース日がくっついてくる
#リリース日の削除は今後の課題orおまけ

#開発会社抜き出し　ok
ios.top.infobox.xml <- xpathSApply(ios.top, '//*[@class="info_box"]/a')#アプリタイトル同様
ios.top.cp.df <- data.frame()
for(i in 0:24){
  i <- 2+(i*3)#htmlで開発会社名にclassが指定されていなかったため一階層上のinfoboxから取得。そのせいで初項2の階差数列扱い
  cp.name.xml <- ios.top.infobox.xml[[i]]
  cp.name <- xmlValue(cp.name.xml)
  entry.frame <-(data.frame(cp.name=cp.name))
  ios.top.cp.df <- rbind(ios.top.cp.df,entry.frame)
}

#merge ゲームタイトル名、前日順位、開発会社のデータフレームをマージ
ios.top.name <- paste("ios.top.dm_",today,".csv",sep="")
ios.top.dm <- cbind(ios.top.title.df,ios.top.l1rank.df2,ios.top.cp.df)
write.csv(ios.top.dm,ios.top.name)

##1ジャンル終了



############################################################


#ios.freeから
#アプリタイトル抜き出し
ios.free.title.df <- data.frame()#データの受け皿作成
for (i in 1:25){#25位まで回す
  ios.free.title.xml <- xpathSApply(ios.free, '//*[@class="app_title"]')#htmlをxmlに変換
  ios.free.title <- xmlValue(ios.free.title.xml[[i]])#xmlを値に変換
  entry.frame.title <-(data.frame(ios.free.title=ios.free.title))#i番目のデータフレーム作成
  ios.free.title.df <- rbind(ios.free.title.df,entry.frame.title)#データフレームをくっつける作業
}#一旦おわり

#前日順位抜き出し
ios.free.l1rank.df <- data.frame()#以下アプリタイトルと同様
for(i in 1:25){
  ios.free.date.xml <- xpathSApply(ios.free, '//*[@class="date"]',function(x) gsub('\t','',xmlValue(x)))
  ios.free.date.trm <- gsub('\n','',ios.free.date.xml)
  ios.free.date.trm.sub <- ios.free.date.xml[[i]]
  entry.frame.l1rank <-(data.frame(l1_rank=ios.free.date.trm.sub))
  ios.free.l1rank.df <- rbind(ios.free.l1rank.df,entry.frame.l1rank)
}
ios.free.l1rank.mtrx <- as.matrix(ios.free.l1rank.df)#データが汚かったので行列化して掃除
ios.free.l1rank.mtrx.trim1 <- gsub('\n',' ',ios.free.l1rank.mtrx)#改行を削除
ios.free.l1rank.df2<- as.data.frame(ios.free.l1rank.mtrx.trim1)#課題　半角スペースがなぜか抜けずリリース日がくっついてくる
#リリース日の削除は今後の課題orおまけ

#開発会社抜き出し　ok
ios.free.infobox.xml <- xpathSApply(ios.free, '//*[@class="info_box"]/a')#アプリタイトル同様
ios.free.cp.df <- data.frame()
for(i in 0:24){
  i <- 2+(i*3)#htmlで開発会社名にclassが指定されていなかったため一階層上のinfoboxから取得。そのせいで初項2の階差数列扱い
  cp.name.xml <- ios.free.infobox.xml[[i]]
  cp.name <- xmlValue(cp.name.xml)
  entry.frame <-(data.frame(cp.name=cp.name))
  ios.free.cp.df <- rbind(ios.free.cp.df,entry.frame)
}

#merge ゲームタイトル名、前日順位、開発会社のデータフレームをマージ
ios.free.name <- paste("ios.free.dm_",today,".csv",sep="")
ios.free.dm <- cbind(ios.free.title.df,ios.free.l1rank.df2,ios.free.cp.df)
write.csv(ios.free.dm,ios.free.name)

#2ジャンル終了

############################################################


#ios.paidから
#アプリタイトル抜き出し
ios.paid.title.df <- data.frame()#データの受け皿作成
for (i in 1:25){#25位まで回す
  ios.paid.title.xml <- xpathSApply(ios.paid, '//*[@class="app_title"]')#htmlをxmlに変換
  ios.paid.title <- xmlValue(ios.paid.title.xml[[i]])#xmlを値に変換
  entry.frame.title <-(data.frame(ios.paid.title=ios.paid.title))#i番目のデータフレーム作成
  ios.paid.title.df <- rbind(ios.paid.title.df,entry.frame.title)#データフレームをくっつける作業
}#一旦おわり

#前日順位抜き出し
ios.paid.l1rank.df <- data.frame()#以下アプリタイトルと同様
for(i in 1:25){
  ios.paid.date.xml <- xpathSApply(ios.paid, '//*[@class="date"]',function(x) gsub('\t','',xmlValue(x)))
  ios.paid.date.trm <- gsub('\n','',ios.paid.date.xml)
  ios.paid.date.trm.sub <- ios.paid.date.xml[[i]]
  entry.frame.l1rank <-(data.frame(l1_rank=ios.paid.date.trm.sub))
  ios.paid.l1rank.df <- rbind(ios.paid.l1rank.df,entry.frame.l1rank)
}
ios.paid.l1rank.mtrx <- as.matrix(ios.paid.l1rank.df)#データが汚かったので行列化して掃除
ios.paid.l1rank.mtrx.trim1 <- gsub('\n',' ',ios.paid.l1rank.mtrx)#改行を削除
ios.paid.l1rank.df2<- as.data.frame(ios.paid.l1rank.mtrx.trim1)#課題　半角スペースがなぜか抜けずリリース日がくっついてくる
#リリース日の削除は今後の課題orおまけ

#開発会社抜き出し　ok
ios.paid.infobox.xml <- xpathSApply(ios.paid, '//*[@class="info_box"]/a')#アプリタイトル同様
ios.paid.cp.df <- data.frame()
for(i in 0:24){
  i <- 2+(i*3)#htmlで開発会社名にclassが指定されていなかったため一階層上のinfoboxから取得。そのせいで初項2の階差数列扱い
  cp.name.xml <- ios.paid.infobox.xml[[i]]
  cp.name <- xmlValue(cp.name.xml)
  entry.frame <-(data.frame(cp.name=cp.name))
  ios.paid.cp.df <- rbind(ios.paid.cp.df,entry.frame)
}

#merge ゲームタイトル名、前日順位、開発会社のデータフレームをマージ
ios.paid.name <- paste("ios.paid.dm_",today,".csv",sep="")
ios.paid.dm <- cbind(ios.paid.title.df,ios.paid.l1rank.df2,ios.paid.cp.df)
write.csv(ios.paid.dm,ios.paid.name)

##終了

