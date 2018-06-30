---
title: 三次元空間における行列演算の復習
date: "2017-05-30T06:31:44Z"
lang: "ja"
path: "matrix-operation-in-3d"
category:
  - "数学"
description: "射影行列・回転行列についてまとめました．"
cover: "math"
---
ぱっと思いつかなかったのでメモ．

### 任意の単位ベクトルへの射影行列
任意の単位ベクトルを$\vec{n}$とすると，$\vec{n}$への射影行列$P_{p}$は以下の様に表されます．
$$
P_{p} = \vec{n}\vec{n}^{T}
$$

### 法線ベクトルに直交する平面への射影行列
法線ベクトルへの射影を元のベクトルから引くことで，法線ベクトルと直交する平面への射影が可能となります．  
そのため，法線ベクトルと直交する平面への射影行列$P_{o}$は以下のように表されます．
$$
\begin{aligned}
P_{o} &= I - P_{p} \\
      &= I - \vec{n}\vec{n}^{T}
\end{aligned}
$$

### 任意の単位ベクトルを回転軸とした回転行列
任意の単位ベクトル$\vec{n}$を以下のように定義します．
$$
\vec{n} = \left(n_x, n_y, n_z \right) 
$$

ここで，Z軸方向の単位ベクトル$\vec{e_{z}}$を$\vec{n}$に変換する行列$P$を考えます．$P$はY軸回転$R_{y}$とZ軸回転$R_{z}$の積で以下のように表現できます．
$$
\begin{aligned}
c_{\theta} &= \frac{n_{x}}{\sqrt{n_{x}^{2} + n_{y}^{2}}} \\
s_{\theta} &= \frac{n_{y}}{\sqrt{n_{x}^{2} + n_{y}^{2}}} \\
c_{\phi} &= \frac{n_{z}}{\sqrt{n_{x}^{2} + n_{y}^{2} + n_{z}^{2}}} \\
s_{\phi} &= \frac{\sqrt{n_{x}^{2} + n_{y}^{2}}}{\sqrt{n_{x}^{2} + n_{y}^{2} + n_{z}^{2}}} \\
R_{xz} &= \left(\begin{array}{rrr}
c_{\phi} & 0 & s_{\phi} \\
0 & 1 & 0 \\
-s_{\phi}& 0 & c_{\phi}
\end{array}\right)\\
R_{xy} &= \left(\begin{array}{rrr}
c_{\theta} & -s_{\theta} & 0 \\
s_{\theta} & c_{\theta} & 0 \\
0 & 0 & 1
\end{array}\right)\\
P &= R_{xy} * R_{xz}
\end{aligned}
$$

$P$によって変換されたZ軸を$\vec{e_{z}'}(=\vec{n})$とすると，所望の任意の単位ベクトルを回転軸とする回転行列は以下の３つの変換の合成で表すことができます．

1. $\vec{e_{z}}$を$\vec{e_{z}}'$に変換する$P$
2. $\vec{e_{z}}$を回転軸とする回転$R_{a}$
3. $\vec{e_{z}}'$を$\vec{e_{z}}$に変換する$P^{T}$

したがって，最終的な回転行列$R$は以下のように表されます．

$$
\begin{aligned}
R_{a} &= \left(\begin{array}{rrr}
\cos\left(a\right) & -\sin\left(a\right) & 0 \\
\sin\left(a\right) & \cos\left(a\right) & 0 \\
0 & 0 & 1
\end{array}\right) \\
R &= P * R_{a} * P^{T} \\
&= \left(\begin{array}{rrr}
\mathit{n_{x}}^{2} + {\left(\mathit{n_{y}}^{2} + \mathit{n_{z}}^{2}\right)} \cos\left(a\right) & -\mathit{n_{x}} \mathit{n_{y}} \cos\left(a\right) + \mathit{n_{x}} \mathit{n_{y}} - {\left(\mathit{n_{z}}^{3} + {\left(\mathit{n_{x}}^{2} + \mathit{n_{y}}^{2}\right)} \mathit{n_{z}}\right)} \sin\left(a\right) & -\mathit{n_{x}} \mathit{n_{z}} \cos\left(a\right) + \mathit{n_{x}} \mathit{n_{z}} + \mathit{n_{y}} \sin\left(a\right) \\
-\mathit{n_{x}} \mathit{n_{y}} \cos\left(a\right) + \mathit{n_{x}} \mathit{n_{y}} + {\left(\mathit{n_{z}}^{3} + {\left(\mathit{n_{x}}^{2} + \mathit{n_{y}}^{2}\right)} \mathit{n_{z}}\right)} \sin\left(a\right) & \mathit{n_{y}}^{2} + {\left(\mathit{n_{x}}^{2} + \mathit{n_{z}}^{2}\right)} \cos\left(a\right) & -\mathit{n_{y}} \mathit{n_{z}} \cos\left(a\right) + \mathit{n_{y}} \mathit{n_{z}} - \mathit{n_{x}} \sin\left(a\right) \\
-\mathit{n_{x}} \mathit{n_{z}} \cos\left(a\right) + \mathit{n_{x}} \mathit{n_{z}} - {\left(\mathit{n_{x}}^{2} \mathit{n_{y}} + \mathit{n_{y}}^{3} + \mathit{n_{y}} \mathit{n_{z}}^{2}\right)} \sin\left(a\right) & -\mathit{n_{y}} \mathit{n_{z}} \cos\left(a\right) + \mathit{n_{y}} \mathit{n_{z}} + {\left(\mathit{n_{x}}^{3} + \mathit{n_{x}} \mathit{n_{y}}^{2} + \mathit{n_{x}} \mathit{n_{z}}^{2}\right)} \sin\left(a\right) & \mathit{n_{z}}^{2} + {\left(\mathit{n_{x}}^{2} + \mathit{n_{y}}^{2}\right)} \cos\left(a\right)
\end{array}\right) \\
&= \left(\begin{array}{rrr}
 \cos\left(a\right) + \mathit{n_{x}}^{2} \left(1-\cos\left(a\right)\right) & \mathit{n_{x}} \mathit{n_{y}} \left(1 - \cos\left(a\right) \right) - \mathit{n_{z}}\sin\left(a\right) & \mathit{n_{x}} \mathit{n_{z}} \left( 1 -  \cos\left(a\right) \right) + \mathit{n_{y}} \sin\left(a\right) \\
\mathit{n_{x}} \mathit{n_{y}}\left( 1 -  \cos\left(a\right) \right) + \mathit{n_{z}} \sin\left(a\right) & \cos\left(a\right) + \mathit{n_{y}}^{2}\left( 1-  \cos\left(a\right)\right) & \mathit{n_{y}} \mathit{n_{z}}
\left( 1- \cos\left(a\right) \right) - \mathit{n_{x}} \sin\left(a\right) \\
\mathit{n_{x}} \mathit{n_{z}}\left(1- \cos\left(a\right) \right) - \mathit{n_{y}}\sin\left(a\right) & \mathit{n_{y}} \mathit{n_{z}} \left(1- \cos\left(a\right) \right) + \mathit{n_{x}}\sin\left(a\right) & \cos\left(a\right) + \mathit{n_{z}}^{2} \left( 1-\cos\left(a\right) \right) 
\end{array}\right)
\end{aligned}
$$

今回の計算をSageでやると以下のような感じになりました．
```sage
var('nx ny nz')
assume((nx,'real') ,(ny,'real') ,(nz,'real'))
n = vector([nx,ny,nz])
cos_theta = n[0] / n[0:2].norm()
sin_theta = n[1] / n[0:2].norm()
cos_phi = n[2] / n.norm()
sin_phi = n[0:2].norm() / n.norm()
Rxz = matrix([[cos_phi, 0, sin_phi],[0,1,0],[-sin_phi,0,cos_phi]])
Rxy = matrix([[cos_theta, -sin_theta, 0], [sin_theta, cos_theta,0],[0,0,1]])
P = Rxy * Rxz
var('theta')
Rtheta = matrix([[cos(theta), -sin(theta), 0], [sin(theta), cos(theta), 0], [0,0,1]])
R = (P * Rtheta * P.T).simplify_full().subs(nx*nx+ny*ny+nz*nz==1)
```
