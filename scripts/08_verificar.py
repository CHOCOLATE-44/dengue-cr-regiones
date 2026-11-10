"""Compara el Excel nuevo con el original y recoteja las cifras de texto con pdfplumber."""
import zipfile,re,os,json,sys
import pandas as pd,openpyxl,pdfplumber
from concurrent.futures import ProcessPoolExecutor
R=os.path.dirname(os.path.dirname(os.path.abspath(__file__)))+"/"
ORI=R+"datos/fuentes/dengue_semanal_CR_2015-2026.xlsx"; NUE=R+"datos/procesados/dengue_CR_2015-2026_con_boletines.xlsx"
def chk(a):
    pdf,pg,items=a
    with pdfplumber.open(R+"datos/fuentes/pdfs/"+pdf) as d: t=d.pages[pg-1].extract_text() or ''
    s=set(re.findall(r'\d[\d.,]*',t)); bad=[]
    for c,ta in items:
        if str(c) not in s and f'{c:,}' not in s: bad.append(('casos',c))
        if ta==ta and ta is not None and not ({f'{ta:.1f}'.replace('.',','),f'{ta:.1f}',f'{ta:.2f}'.replace('.',','),f'{ta:.2f}',str(int(ta))}&s): bad.append(('tasa',ta))
    return pdf,pg,bad
if __name__=='__main__':
    a=zipfile.ZipFile(ORI); b=zipfile.ZipFile(NUE)
    print('partes distintas del original:',[n for n in a.namelist() if a.read(n)!=b.read(n)])
    w=openpyxl.load_workbook(NUE,data_only=True); print(w.sheetnames)
    for s in w.sheetnames[:5]:
        print(s,pd.read_excel(ORI,sheet_name=s,header=None).equals(pd.read_excel(NUE,sheet_name=s,header=None)))
    D=pd.read_pickle(R+"datos/intermedios/D.pkl"); U=pd.read_excel(NUE,sheet_name='Regiones acumulado unificado')
    O=pd.read_csv(R+"datos/procesados/boletines_regiones_2021-2026.csv",dtype=str,encoding='utf-8-sig',keep_default_na=False); X=pd.read_excel(NUE,sheet_name='Regiones boletines',dtype=str)
    print('filas CSV/hoja',len(O),len(X),'| log CSV',len(pd.read_csv(R+'datos/procesados/log_boletines.csv')),'| unificada',len(U))
    print('unificada sin SE:',U.se_corte.isna().sum(),'| filas por año:',U.groupby('anio').size().to_dict())
    fu=U.fuente.astype(str)
    print('infografías en unificada:',fu.str.contains('mensual|Segundo|Tercero|Séptimo',regex=True).sum())
    print('dudosos en unificada:',len(D[(D.nivel=='region')&(D.estado_corte=='dudoso')&D.usar_en_analisis]),'| N°43 H.Norte en unificada:',((U.fuente=='Boletín N°43')&(U.anio==2024)&(U.region=='H.Norte')).sum())
    K=pd.read_excel(NUE,sheet_name='Bloques'); print('Bloques filas',len(K),'negativos',(K.casos_crudos<0).sum(),(K.casos_escalados<0).sum(),'poblacion vacía',K.poblacion.isna().sum())
    Q=pd.read_excel(NUE,sheet_name='Predictores'); print('Predictores filas',len(Q),'NA',Q[['inmunidad','arrastre','oni']].isna().sum().to_dict())
    T=D[D.metodo=='texto']
    J=[(p,int(g),[(int(c),t) for c,t in zip(x.casos_acum,x.tasa_100k)]) for (p,g),x in T.groupby(['pdf','pagina_pdf'])]
    with ProcessPoolExecutor(8) as ex: res=list(ex.map(chk,J))
    print('recotejo pdfplumber: páginas',len(res),'celdas',sum(len(j[2])*2 for j in J),'con diferencias',[r for r in res if r[2]])
