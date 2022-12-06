import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

dados1 = pd.read_csv('https://raw.githubusercontent.com/SilvaEder/ET43K/main/Motor1_dados1.csv',encoding='utf-8')
func = pd.read_json('https://raw.githubusercontent.com/SilvaEder/ET43K/main/motor1.json',encoding='utf-8')
resi = pd.read_json('https://raw.githubusercontent.com/SilvaEder/ET43K/main/resi.json',encoding='utf-8')
moda = pd.read_json('https://raw.githubusercontent.com/SilvaEder/ET43K/main/moda.json',encoding='utf-8')

#REMOVENDO A ÚLTIMA LINHA
dados1 = dados1.drop(len(dados1)-1)
dados2 = dados2.drop(len(dados2)-1)

#PLOTAGEM DOS DADOS
plt.figure(figsize=(9, 3))
plt.plot(dados1[2],dados1[1])
plt.title("Dados Aquisitados")
plt.ylabel("Força (N)")
plt.xlabel("Tesão (V)")
plt.grid(True)
plt.savefig('Pyplot01.png')
#plt.show()

#PLOTAGEM DOS DADOS E MODA
moda=np.asarray(moda)
moda[0]=sorted(moda[0], reverse=True)
x=np.arange(0,10.5,.5)
y=moda[0]

plt.figure(figsize=(9, 4))
plt.plot(dados1[2],dados1[1], label='Dados')
plt.plot(x,y,'ro', label='Moda')
plt.title("Ajustes dos Dados Aquisitados")
plt.ylabel("Força (N)")
plt.xlabel("Tesão (V)")
plt.grid(True)
plt.legend()
plt.savefig('Pyplot02.png')
plt.show()

#PLOTAGEM DOS DADOS E MODELO
plt.figure(figsize=(9, 3))
x1=np.asarray(dados1[2].tolist())
y1=(func[0][1])+(func[1][1])*x1+(func[2][1])*x1**2+(func[3][1])*x1**3
plt.plot(dados1[2],dados1[1], label='Dados')
plt.plot(x1,y1,'r', label='Modelo ')
plt.title("Modelo dos Dados Aquisitados")
plt.ylabel("Força (N)")
plt.xlabel("Tesão (V)")
plt.grid(True)
plt.legend()
plt.savefig('Pyplot03.png')
#plt.show()

#PLOTAGEM DOS RESÍDUOS
resi=np.asarray(resi)
x=np.arange(0,len(resi[0]),1)
y=resi[0]
plt.figure(figsize=(9, 3))
plt.plot(x,y,'bo')
plt.axhline(y = 0, color = 'r', linestyle = '--')
plt.title("Distribuição dos Resíduos")
plt.ylabel("Erro")
plt.xlabel("Amostras")
plt.savefig('Pyplot04.png')
#plt.show()


