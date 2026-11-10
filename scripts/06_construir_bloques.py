"""Base de análisis por bloques: seco (SE 1-18) y lluvioso (SE 19-40), por región y año 2015-2026."""
import json,os
import pandas as pd,xlrd
from comun import fuente_boletin
S=os.path.dirname(os.path.dirname(os.path.abspath(__file__))); BASE=S+"/"
SRC=BASE+"datos/fuentes/dengue_semanal_CR_2015-2026.xlsx"
OBJ=(18,40)
REG=['Brunca','C.Este','Central Norte','Central Sur','Chorotega','H. Caribe','H.Norte','Occidente','Pacífico Central']
ASIS={'central este':'C.Este','huetar norte':'H.Norte','huetar caribe':'H. Caribe','occidente':'Occidente','central sur':'Central Sur','central norte':'Central Norte','brunca':'Brunca','chorotega':'Chorotega','pacífico central':'Pacífico Central','pacifico central':'Pacífico Central'}
LOG=[]
def log(tipo,anio,boletin,detalle,se=''): LOG.append(dict(tipo=tipo,anio=str(anio),tipo_boletin='',boletin=boletin,se_corte=se,pagina_pdf='',detalle=detalle,url=''))
s=pd.read_excel(SRC,sheet_name='Serie semanal').iloc[:,:3]; s.columns=['a','w','o']
s['cum']=s.groupby('a').o.cumsum(); OPS={(int(a),int(w)):int(c) for a,w,c in zip(s.a,s.w,s.cum)}
g=pd.read_excel(SRC,sheet_name='Regiones MinSalud'); g.columns=['a','w','region','casos','archivo']
g=g.sort_values(['a','region','w']); g['cum']=g.groupby(['a','region']).casos.cumsum()
D=pd.read_pickle(S+"/datos/intermedios/D.pkl")
R=D[(D.nivel=='region')&D.usar_en_analisis&D.se_corte.notna()]
def corte(a,t):
    """Devuelve se real, acumulados por región y etiqueta de fuente del corte objetivo t."""
    if a<=2020:
        x=g[(g.a==a)&(g.w==t)]
        return t,dict(zip(x.region,x.cum)),f'Excel ASIS {a} (SE {t})'
    c=[]
    for p,x in R[R.anio_datos==a].groupby('pdf'):
        if sorted(x.unidad)!=REG: continue
        c.append((abs(int(x.se_corte.iloc[0])-t),-int(x.se_corte.iloc[0]),x.fecha_publicacion.iloc[0],int(x.numero.iloc[0]) if x.numero.iloc[0].isdigit() else 0,p))
    # más cercano; en empate la semana posterior, y con igual semana el boletín posterior
    c.sort(key=lambda v:(v[0],v[1],tuple(-ord(ch) for ch in v[2]),-v[3]))
    x=R[R.pdf==c[0][4]]; r=x.iloc[0]
    return int(r.se_corte),dict(zip(x.unidad,x.casos_acum)),fuente_boletin(r.numero,r.tipo_boletin,r.anio,r.anio_datos)+f' (SE {int(r.se_corte)})'
# población 2015-2020: casos/tasa de la hoja "Regiones" de cada Excel ASIS
POB={}
for a in range(2015,2021):
    sh=xlrd.open_workbook(f"{BASE}datos/fuentes/asis/dengue_{a}.xls").sheet_by_name('Regiones')
    for i in range(sh.nrows):
        k=str(sh.cell_value(i,0)).strip().lower()
        if k in ASIS:
            c,t=sh.cell_value(i,1),sh.cell_value(i,2)
            POB[(a,ASIS[k])]=round(c/t*100000) if t else None
Q=D[(D.nivel=='region')&D.poblacion_implicita.notna()&~D.poblacion_imputada&(D.estado_corte!='fila_invalida')]
for (a,u),x in Q[Q.anio_datos>=2021].groupby(['anio_datos','unidad']):
    if u in REG: POB[(int(a),u)]=int(round(x.poblacion_implicita.median()))
filas=[];cortes=[]
for a in range(2015,2027):
    w1,c1,f1=corte(a,18); w2,c2,f2=corte(a,40)
    n1,n2=sum(c1.values()),sum(c2.values()); o1,o2=OPS[(a,w1)],OPS[(a,w2)]
    d1,d2=n1/o1-1,n2/o2-1
    cortes.append(dict(anio=a,se_seco=w1,dif_seco=round(d1,4),fuente_seco=f1,se_lluvioso=w2,dif_lluvioso=round(d2,4),fuente_lluvioso=f2,ops1=o1,ops2=o2,suma1=n1,suma2=n2))
    if a>=2021:
        log('bloques_corte',a,f1,f'Objetivo SE 18, usado SE {w1}: {n1} vs OPS {o1} ({d1:+.1%}).',w1); log('bloques_corte',a,f2,f'Objetivo SE 40, usado SE {w2}: {n2} vs OPS {o2} ({d2:+.1%}).',w2)
    for u in REG:
        e1=c1[u]/n1*o1; e2=c2[u]/n2*o2
        base=dict(anio=a,region=u,poblacion=POB.get((a,u)))
        filas.append({**base,'bloque':'seco','se_inicio':1,'se_fin':w1,'casos_crudos':int(c1[u]),'casos_escalados':int(round(e1)),'fuente_corte_inicio':'Inicio de año','fuente_corte_fin':f1,'dif_ops_rel_corte_fin':round(d1,4)})
        filas.append({**base,'bloque':'lluvioso','se_inicio':w1+1,'se_fin':w2,'casos_crudos':int(c2[u]-c1[u]),'casos_escalados':int(round(e2-e1)),'fuente_corte_inicio':f1,'fuente_corte_fin':f2,'dif_ops_rel_corte_fin':round(d2,4)})
K=pd.DataFrame(filas); K['semanas_bloque']=K.se_fin-K.se_inicio+1
K=K[['anio','region','bloque','se_inicio','se_fin','semanas_bloque','casos_crudos','casos_escalados','poblacion','fuente_corte_inicio','fuente_corte_fin','dif_ops_rel_corte_fin']]
K['poblacion']=K.poblacion.astype('Int64')
K['_b']=(K.bloque=='lluvioso').astype(int); K=K.sort_values(['anio','_b','region']).drop(columns='_b')
K.to_csv(BASE+"datos/procesados/bloques_region_anio.csv",index=False,encoding='utf-8-sig')
# chequeos
neg=K[(K.casos_crudos<0)|(K.casos_escalados<0)]
for x in neg.itertuples(): log('bloques_negativo',x.anio,x.region,f'{x.bloque}: casos_crudos {x.casos_crudos}, casos_escalados {x.casos_escalados} (SE {x.se_inicio}–{x.se_fin}). No se corrige.',x.se_fin)
T=[]
for c in cortes:
    a=c['anio']
    for b,lo,hi,sc in (('seco',0,c['ops1'],c['suma1']),('lluvioso',c['ops1'],c['ops2'],c['suma2']-c['suma1'])):
        x=K[(K.anio==a)&(K.bloque==b)]; ops=hi-lo; e=int(x.casos_escalados.sum()); cr=int(x.casos_crudos.sum())
        T.append(dict(anio=a,bloque=b,crudo=cr,escalado=e,ops=ops,dif_esc_ops=e-ops))
        if abs(e-ops)>4: log('bloques_suma',a,b,f'Suma escalada {e} vs OPS del bloque {ops}.')
for x in K[K.poblacion.isna()].itertuples(): log('bloques_poblacion',x.anio,x.region,f'Sin población ({x.bloque}): la fuente no permite calcularla.')
log('bloques_poblacion','2015–2020','Excel ASIS','Población = casos / tasa de la hoja "Regiones" de cada Excel ASIS (asis/dengue_AAAA.xls).')
json.dump(LOG,open(S+"/datos/intermedios/log_bloques.json",'w'),ensure_ascii=False,indent=0,default=str)
json.dump(dict(cortes=cortes,totales=T),open(S+"/datos/intermedios/bloques_stats.json",'w'),ensure_ascii=False,indent=0)
print(len(K),'negativos',len(neg),'pob vacía',int(K.poblacion.isna().sum()),'max dif suma',max(abs(t['dif_esc_ops']) for t in T))
