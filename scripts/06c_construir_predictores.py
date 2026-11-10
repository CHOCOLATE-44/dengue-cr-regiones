"""Predictores por región y año (2015-2026): inmunidad, arrastre y oni, todos medidos en el año a-1."""
import json,math,os,re,subprocess
import pandas as pd,xlrd
from comun import fuente_boletin
S=os.path.dirname(os.path.dirname(os.path.abspath(__file__))); BASE=S+"/"
SRC=BASE+"datos/fuentes/dengue_semanal_CR_2015-2026.xlsx"
REG=['Brunca','C.Este','Central Norte','Central Sur','Chorotega','H. Caribe','H.Norte','Occidente','Pacífico Central']
ESTACIONES=['JAS','ASO','SON','OND','NDJ']
N2020={'Central Norte':'Central Norte','Huetar Caribe':'H. Caribe','Pacífico Central':'Pacífico Central','Chorotega':'Chorotega','Central Este':'C.Este','Central Sur':'Central Sur','Huetar Norte':'H.Norte','Brunca':'Brunca','Occidente':'Occidente'}
LOG=[]
def log(tipo,anio,boletin,detalle): LOG.append(dict(tipo=tipo,anio=str(anio),tipo_boletin='',boletin=boletin,se_corte='',pagina_pdf='',detalle=detalle,url=''))
s=pd.read_excel(SRC,sheet_name='Serie semanal').iloc[:,:3]; s.columns=['a','w','o']
OPS_ANIO=s.groupby('a').o.sum().to_dict(); OPS40=s[s.w<=40].groupby('a').o.sum().to_dict()
g=pd.read_excel(SRC,sheet_name='Regiones MinSalud'); g.columns=['a','w','region','casos','archivo']
K=pd.read_csv(BASE+"datos/procesados/bloques_region_anio.csv",encoding='utf-8-sig')
crudo40=K.groupby(['anio','region']).casos_crudos.sum().to_dict()
esc40=K.groupby(['anio','region']).casos_escalados.sum().to_dict()
pob={k:(None if pd.isna(v) else float(v)) for k,v in K.groupby(['anio','region']).poblacion.first().items()}
D=pd.read_pickle(S+"/datos/intermedios/D.pkl"); R=D[(D.nivel=='region')&D.usar_en_analisis&D.se_corte.notna()]
def cierre_boletin(a):
    """Último boletín con las 9 regiones y corte en SE 52 o 53 del año a."""
    x=R[(R.anio_datos==a)&(R.se_corte>=52)]
    c=[(q.fecha_publicacion.iloc[0],int(q.numero.iloc[0]),p) for p,q in x.groupby('pdf') if sorted(q.unidad)==REG]
    q=x[x.pdf==max(c)[2]]; r=q.iloc[0]
    return dict(zip(q.unidad,q.casos_acum)),fuente_boletin(r.numero,r.tipo_boletin,r.anio,r.anio_datos)+f' (SE {int(r.se_corte)})'
def cierre_2019():
    """Cuadro 1 del boletín N°1 de 2020: cierre regional de 2019 a la SE 52."""
    t=subprocess.run(['pdftotext','-layout',BASE+'datos/fuentes/extras/2020_semanal_1.pdf','-'],capture_output=True,text=True,check=True).stdout
    t=t[t.index('Cuadro 1'):t.index('TOTAL')+60]; c={}
    for n,u in N2020.items(): c[u]=int(re.search(re.escape(n)+r'\s+(\d+)\s+(\d+)\s',t).group(2))
    assert sum(c.values())==int(re.search(r'TOTAL\s+\d+\s+(\d+)',t).group(1))==OPS_ANIO[2019]
    return c,'Boletín N°1 de 2020, Cuadro 1 (SE 52)'
# Excel ASIS 2014: hoja Region, SE 1-53
sh=xlrd.open_workbook(BASE+"datos/fuentes/asis/dengue_2014.xls").sheet_by_name('Region')
h=[str(v).strip() for v in sh.row_values(4)]; w14=[sh.row_values(i) for i in range(5,sh.nrows) if isinstance(sh.cell_value(i,0),float)]
tot14={u:sum(r[h.index(u)] for r in w14) for u in REG}; a40_14={u:sum(r[h.index(u)] for r in w14 if r[0]<=40) for u in REG}
# tramo SE 41-52/53 del año a-1, escalado a OPS salvo 2014
tramo={};crudo_tramo={};f_tramo={}
for a in range(2015,2026):
    if a<=2018:
        x=g[g.a==a].groupby('region').casos.sum().to_dict(); f=f'Excel ASIS {a} (cierre) − Excel ASIS {a} (SE 40)'
    elif a==2019: x,f=cierre_2019(); f=f+' − Excel ASIS 2019 (SE 40)'
    else:
        x,f=cierre_boletin(a); f=f+(' − Excel ASIS 2020 (SE 40)' if a==2020 else ' − '+K[(K.anio==a)&(K.bloque=='lluvioso')].fuente_corte_fin.iloc[0])
    d={u:x[u]-crudo40[(a,u)] for u in REG}; dp={u:max(v,0) for u,v in d.items()}; n=sum(dp.values()); o=OPS_ANIO[a]-OPS40[a]
    for u,v in d.items():
        if v<0: log('predictores_tramo_negativo',a,u,f'Cierre menos SE 40 en casos crudos da {v}: se fija en 0 antes de calcular las proporciones (negativo_a_cero).')
    tramo.update({(a,u):dp[u]/n*o for u in REG}); assert abs(sum(tramo[(a,u)] for u in REG)-o)<1e-6; crudo_tramo.update({(a,u):d[u] for u in REG}); f_tramo[a]=f
    log('predictores_tramo',a,'SE 41 al cierre',f'Suma de la fuente {n}, OPS del tramo {o}. {f}.')
# RONI: fuente oficial de CPC desde NWS PNS 26-05
oni=pd.read_csv(BASE+"datos/fuentes/extras/RONI.ascii.txt",sep=r'\s+'); oni=oni[oni.SEAS.isin(ESTACIONES)]
assert (oni.groupby('YR').size().loc[2014:2025]==5).all()
ONI=oni.groupby('YR').ANOM.mean().to_dict()
def lt(c,p):
    """log(tasa por 100 000 + 1). Sin casos la tasa es 0 aunque falte la población."""
    return 0.0 if c==0 else math.log(c/p*100000+1)
filas=[]
for a in range(2015,2027):
    b=a-1
    for u in REG:
        notas=[]; af='escalado'; pf='anio_anterior'
        if b==2014:
            p14=pob[(2015,u)]; inm=lt(a40_14[u],p14); arr=lt(tot14[u]-a40_14[u],p14); af='crudo_asis_2014'; pf='2015_aprox'
            fi=fa='Excel ASIS 2014, hoja Region, casos crudos sin escalar; población de 2015'
            notas.append('Casos 2014 sin escalar y población 2015 como aproximación de 2014.')
        else:
            inm=lt(esc40[(b,u)],pob[(b,u)]); fi=f'Bloques {b}, casos_escalados SE 1–40'; fa=f_tramo[b]+f'; escalado a OPS {b}'
            if crudo_tramo[(b,u)]<0:
                arr=lt(0,pob[(b,u)]); af='negativo_a_cero'
                notas.append(f'Arrastre con 0 casos: cierre {b} menor que el acumulado a SE 40 ({crudo_tramo[(b,u)]} crudos).')
            else: arr=lt(tramo[(b,u)],pob[(b,u)])
            if pob[(b,u)] is None: pf='sin_poblacion_0_casos'; notas.append(f'Sin población {b}: se usa tasa 0 porque no hubo casos.')
        filas.append(dict(anio=a,region=u,inmunidad=inm,arrastre=arr,oni=round(ONI[b],4),arrastre_fuente=af,poblacion_fuente=pf,fuente_inmunidad=fi,fuente_arrastre=fa,fuente_oni=f'NOAA CPC, RONI (ERSSTv6), promedio JAS–NDJ {b}',notas=' '.join(notas)))
P=pd.DataFrame(filas); P[['inmunidad','arrastre']]=P[['inmunidad','arrastre']].round(6)
P.to_csv(BASE+"datos/procesados/predictores_region_anio.csv",index=False,encoding='utf-8-sig')
log('predictores_faltante',2014,'Excel ASIS 2014','Sin población regional 2014 ni OPS 2014: inmunidad y arrastre de 2015 usan casos crudos y población 2015 (poblacion_fuente = 2015_aprox).')
log('predictores_fuente',2020,'Boletín N°2 de 2021','Cierre regional de 2020 tomado del boletín, porque el Excel ASIS 2020 llega a SE 44.')
log('predictores_fuente','2015–2026','RONI','oni usa RONI (ERSSTv6), índice oficial de CPC según NWS PNS 26-05. Copia en extras/RONI.ascii.txt.')
json.dump(LOG,open(S+"/datos/intermedios/log_predictores.json",'w'),ensure_ascii=False,indent=0,default=str)
# panel para el modelo: respuesta SE 1-40 con sus predictores
M=K.groupby(['anio','region']).agg(casos=('casos_escalados','sum'),poblacion=('poblacion','first')).reset_index().merge(P[['anio','region','inmunidad','arrastre','oni','arrastre_fuente','poblacion_fuente']],on=['anio','region'])
M.to_csv(S+"/datos/intermedios/panel_modelo.csv",index=False,encoding='utf-8')
print(len(P),P[['inmunidad','arrastre','oni']].isna().sum().to_dict(),P.arrastre_fuente.value_counts().to_dict(),P.poblacion_fuente.value_counts().to_dict())
