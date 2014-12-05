
#新規購入者をセット
Sales <- ts(c(717,911,1127,750,940,999,857,856,845,1056))
plot(Sales,type="l",lty=2,col="red")
Y= cumsum(Sales)#累積視聴者を作成
Y=ts(Y)
Y = c(0,Y[1:(length(Y)-1)])
Ysq = Y**2
out = lm(Sales~Y+Ysq)#切片と回帰係数を取得
summary(out)

a=out$coef[1];a
b=out$coef[2];b
c=out$coef[3];c

#飽和水準
#解なしの場合があるため2次方程の解がない場合にはスキップ

#if(b**2-4*a*c <0) next
#実際には全体をForループで複数製品のモデルを作成

#飽和水準を求めるmが正の場合を選択。今回はmminusが正であった
mplus = (-b+sqrt(b**2-4*a*c))/(2*c);mplus
mminus=(-b-sqrt(b**2-4*a*c))/(2*c);mminus
m = mminus

#pとqを求める
p=a/m
q=b+p

#モデル作成
bassModel=function(p,q,m,T=50)#Tの数字はモデルに合わせて変更
{
  S=double(T)
  Y=double(T+1)
  Y[1]=0
  for(t in 1:T)
  {
    S[t]=p*m+(q-p)*Y[t]-(q/m)*Y[t]**2
    Y[t+1]=Y[t]+S[t]
  }
  return(list(sales=S,cumSales=cumsum(S)))
}

#モデルの適用
Spred = bassModel(p,q,m)$sales#オプションでTも指定可能。何も記載しないとデフォルトで関数定義時の値が入る
Spred = ts(Spred)
ts.plot(Sales,Spred,type="l",col=c("blue","red"))
Spred=bassModel(p,q,m)$sales
CumSpred = ts(cumsum(Spred))
CumSales = ts(cumsum(Sales))
ts.plot(CumSales,CumSpred,col=c("blue","red"),ylim=c(0,m+1000),main=paste("FooFoo\n","m=",substr(m,1,5),"p=",substr(p,1,5),"q=",substr(q,1,5),sep=""))
abline(h=m,col="green")
