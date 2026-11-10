"""Selecciona, de los bloques detectados, las tablas de dengue por región y provincia."""
import json,re,os,collections
S=os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
m=json.load(open(S+"/datos/intermedios/manifest.json")); B=json.load(open(S+"/datos/intermedios/blocks.json"))
def kind(b):
    t=b['title']; n=len(b['rows'])
    if re.search(r'PCR|serotipo|chik|zika|malaria|cant[oó]n',t,re.I): return None
    if re.search(r'dengue',t,re.I) and re.search(r'regi[oó]n',t,re.I) and n>=4: return 'region'
    if re.search(r'dengue',t,re.I) and re.search(r'provincia',t,re.I) and n>=4: return 'provincia'
    return None
by=collections.defaultdict(lambda:{'region':[],'provincia':[]})
for b in B:
    k=kind(b)
    if k: by[b['pdf']][k].append(b)
def fuente_boletin(numero,tipo,anio,anio_datos):
    """Etiqueta de un boletín para las columnas de fuente."""
    if str(numero).isdigit(): s=('Boletín N°' if tipo=='semanal' else 'Boletín mensual N°')+str(numero)
    else: s=('Boletín mensual ' if tipo=='mensual' else 'Boletín ')+str(numero)
    return s+(f' de {anio}' if anio!=anio_datos else '')
