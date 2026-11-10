import json,re,os,zipfile,math
import pandas as pd
from comun import fuente_boletin
from xml.sax.saxutils import escape
S=os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
BASE=S+"/"
SRC=BASE+"datos/fuentes/dengue_semanal_CR_2015-2026.xlsx"; OUT=BASE+"datos/procesados/dengue_CR_2015-2026_con_boletines.xlsx"
O=pd.read_csv(BASE+"datos/procesados/boletines_regiones_2021-2026.csv",dtype={'numero':str,'usar_en_analisis':str,'poblacion_imputada':str},encoding='utf-8-sig')
D=pd.read_pickle(S+"/datos/intermedios/D.pkl")
L=pd.DataFrame(json.load(open(S+"/datos/intermedios/log.json"))+json.load(open(S+"/datos/intermedios/log_bloques.json"))+json.load(open(S+"/datos/intermedios/log_predictores.json")))
BL=pd.read_csv(BASE+"datos/procesados/bloques_region_anio.csv",encoding='utf-8-sig')
PR=pd.read_csv(BASE+"datos/procesados/predictores_region_anio.csv",encoding='utf-8-sig')
# ---- unificado
g=pd.read_excel(SRC,sheet_name='Regiones MinSalud'); g.columns=['anio','se_corte','region','casos','archivo']
g=g.sort_values(['anio','region','se_corte']); g['casos_acum']=g.groupby(['anio','region']).casos.cumsum()
g['poblacion_implicita']=None; g['fuente']='Excel ASIS'; g['poblacion_imputada']='FALSE'
U1=g[['anio','se_corte','region','casos_acum','poblacion_implicita','fuente','poblacion_imputada']].sort_values(['anio','se_corte','region'],kind='stable')
b=D[(D.nivel=='region')&D.usar_en_analisis].copy(); b['fuente']=[fuente_boletin(*v) for v in zip(b.numero,b.tipo_boletin,b.anio,b.anio_datos)]
U2=pd.DataFrame(dict(anio=b.anio_datos,se_corte=b.se_corte,region=b.unidad,casos_acum=b.casos_acum,poblacion_implicita=b.poblacion_implicita,fuente=b.fuente,poblacion_imputada=b.poblacion_imputada.map({True:'TRUE',False:'FALSE'})))
U2=U2.sort_values(['anio','se_corte'],kind='stable',na_position='last')
U=pd.concat([U1,U2],ignore_index=True)
# ---- log ordenado
ordn=['descarga','fila_invalida','bloques_negativo','bloques_suma','bloques_poblacion','bloques_corte','predictores_faltante','predictores_tramo_negativo','predictores_fuente','predictores_tramo','corte_corregido','corte_no_acumulado','corte_dudoso','corte_confirmado','usar_en_analisis','poblacion_imputada','cv_poblacion','sin_tabla','sin_tabla_provincia','infografia','tasa_vacia_en_pdf','imagen_baja_calidad','nombre_no_reconocido','nombre_variante','chequeo_unidades','chequeo_suma','chequeo_region_provincia','tabla_obsoleta','total_parrafo_distinto','se_conflicto','se_provincia','anio_titulo_erroneo','anio_datos_distinto','duplicado','duplicado_cifras_distinta_SE','misma_SE_cifras_distintas','chequeo_baja']
L['_o']=L.tipo.map({k:i for i,k in enumerate(ordn)}); L=L.sort_values('_o',kind='stable').drop(columns='_o')
L[['tipo','anio','tipo_boletin','boletin','se_corte','pagina_pdf','detalle','url']].to_csv(BASE+'datos/procesados/log_boletines.csv',index=False,encoding='utf-8-sig')
def col(i):
    s=''
    i+=1
    while i: i,r=divmod(i-1,26); s=chr(65+r)+s
    return s
z=zipfile.ZipFile(SRC)
s2=z.read('xl/worksheets/sheet2.xml').decode()
hs=re.search(r'<c r="A1"[^>]*\bs="(\d+)"',s2).group(1)
def sheet(df,widths):
    c=list(df.columns); n=len(df)+1; out=[]
    out.append('<?xml version="1.0" encoding="UTF-8" standalone="yes"?>\n<worksheet xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main">')
    out.append(f'<dimension ref="A1:{col(len(c)-1)}{n}"/><sheetViews><sheetView workbookViewId="0"><pane ySplit="1" topLeftCell="A2" activePane="bottomLeft" state="frozen"/><selection pane="bottomLeft" activeCell="A2" sqref="A2"/></sheetView></sheetViews><sheetFormatPr defaultRowHeight="15"/>')
    out.append('<cols>'+''.join(f'<col min="{i+1}" max="{i+1}" width="{w}" customWidth="1"/>' for i,w in enumerate(widths))+'</cols><sheetData>')
    def cell(ref,v,st=''):
        if v is None or (isinstance(v,float) and math.isnan(v)) or v is pd.NA or v=='': return ''
        if isinstance(v,bool): v=str(v)
        if isinstance(v,(int,float)) or hasattr(v,'dtype'):
            f=float(v); return f'<c r="{ref}"{st}><v>{int(f) if f==int(f) else repr(round(f,6))}</v></c>'
        return f'<c r="{ref}"{st} t="inlineStr"><is><t xml:space="preserve">{escape(str(v))}</t></is></c>'
    out.append('<row r="1">'+''.join(cell(f'{col(i)}1',h,f' s="{hs}"') for i,h in enumerate(c))+'</row>')
    for k,row in enumerate(df.itertuples(index=False),2):
        out.append(f'<row r="{k}">'+''.join(cell(f'{col(i)}{k}',v) for i,v in enumerate(row))+'</row>')
    out.append(f'</sheetData><autoFilter ref="A1:{col(len(c)-1)}{n}"/></worksheet>')
    return ''.join(out).encode('utf-8')
new=[('Regiones boletines',O,[7,12,10,14,9,10,18,11,10,13,11,10,30,9,50,11,10,11,11,10,11,13,60,12,12]),
     ('Regiones acumulado unificado',U,[7,9,18,11,14,28,12]),
     ('Bloques',BL,[7,18,10,10,8,14,12,15,12,28,28,20]),
     ('Predictores',PR,[7,18,11,11,8,18,20,34,60,44,80]),
     ('Log',L[['tipo','anio','tipo_boletin','boletin','se_corte','pagina_pdf','detalle','url']],[26,7,12,16,9,10,110,30])]
wb=z.read('xl/workbook.xml').decode(); rel=z.read('xl/_rels/workbook.xml.rels').decode(); ct=z.read('[Content_Types].xml').decode()
sh='';rl='';cts='';dn=''
for i,(nm,df,w) in enumerate(new):
    n=6+i; rid=f'rId{100+i}'
    sh+=f'<sheet name="{escape(nm)}" sheetId="{n}" state="visible" r:id="{rid}"/>'
    rl+=f'<Relationship Id="{rid}" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/worksheet" Target="worksheets/sheet{n}.xml"/>'
    cts+=f'<Override PartName="/xl/worksheets/sheet{n}.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml"/>'
    dn+=f'<definedName function="false" hidden="true" localSheetId="{n-1}" name="_xlnm._FilterDatabase" vbProcedure="false">&apos;{escape(nm)}&apos;!$A$1:${col(len(df.columns)-1)}${len(df)+1}</definedName>'
wb=wb.replace('</sheets>',sh+'</sheets>').replace('</definedNames>',dn+'</definedNames>')
rel=rel.replace('</Relationships>',rl+'</Relationships>'); ct=ct.replace('</Types>',cts+'</Types>')
with zipfile.ZipFile(OUT,'w',zipfile.ZIP_DEFLATED) as o:
    for it in z.infolist():
        d=z.read(it.filename)
        if it.filename=='xl/workbook.xml': d=wb.encode()
        elif it.filename=='xl/_rels/workbook.xml.rels': d=rel.encode()
        elif it.filename=='[Content_Types].xml': d=ct.encode()
        o.writestr(it,d)
    for i,(nm,df,w) in enumerate(new): o.writestr(f'xl/worksheets/sheet{6+i}.xml',sheet(df,w))
print(len(O),len(U),len(L),os.path.getsize(OUT))
