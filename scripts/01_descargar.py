import csv,subprocess,os,sys,json,time
from concurrent.futures import ThreadPoolExecutor
base=os.path.dirname(os.path.dirname(os.path.abspath(__file__)))+"/"
S=os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
os.makedirs(S+"/datos/intermedios",exist_ok=True)
rows=list(csv.DictReader(open(base+"datos/fuentes/links_boletines_dengue_2021-2026.csv",encoding="utf-8-sig")))
seen={}
for i,r in enumerate(rows,1):
    r['idx']=i
    key=r['numero'].strip()
    if r['anio']=='2023' and r['tipo_boletin']=='semanal' and not key and r['fecha_publicacion']=='2023-02-03':
        key='4'   # titulo sin numero; identificado por fecha y nombre de archivo
    if not key: key=f"idx{i:03d}"
    name=f"{r['anio']}_{r['tipo_boletin']}_{key}"
    if name in seen:
        seen[name]+=1; name=f"{name}_dup{seen[name]}"
    else: seen[name]=1
    r['pdf']=name+".pdf"
def get(r):
    p=base+"datos/fuentes/pdfs/"+r['pdf']; res=dict(idx=r['idx'],pdf=r['pdf'],intentos=0,ok=False,error='')
    if os.path.exists(p) and open(p,'rb').read(5)==b'%PDF-':
        res.update(ok=True,bytes=os.path.getsize(p)); return res
    for a in (1,2):
        res['intentos']=a
        t=p+".part"
        c=subprocess.run(['curl','-sSL','--fail','--max-time','240','--connect-timeout','30','-A','Mozilla/5.0','-o',t,r['url']],capture_output=True,text=True)
        if c.returncode==0 and open(t,'rb').read(5)==b'%PDF-':
            os.rename(t,p); res.update(ok=True,bytes=os.path.getsize(p),error=''); return res
        res['error']=f"curl rc={c.returncode} {c.stderr.strip()[:150]}"
        if c.returncode==0: res['error']='respuesta no es PDF'
        if os.path.exists(t): os.remove(t)
        time.sleep(3)
    return res
with ThreadPoolExecutor(4) as ex: out=list(ex.map(get,rows))
m={o['idx']:o for o in out}
for r in rows: r.update({k:m[r['idx']].get(k) for k in('intentos','ok','error','bytes')})
json.dump(rows,open(S+"/datos/intermedios/manifest.json",'w'),ensure_ascii=False,indent=0)
print('ok',sum(o['ok'] for o in out),'fail',sum(not o['ok'] for o in out))
for o in out:
    if not o['ok'] or o['intentos']>1: print(o)
# Excel ASIS (población 2015-2020 por región, hoja "Regiones")
A="https://www.ministeriodesalud.go.cr/index.php/biblioteca-de-archivos-left/documentos-ministerio-de-salud/vigilancia-de-la-salud/analisis-de-situacion-salud/"
os.makedirs(base+"datos/fuentes/asis",exist_ok=True)
for y,sl in {2015:'1637-dengue-2015',2016:'1641-situacion-del-dengue-2016',2017:'1645-dengue-2017',2018:'1651-dengue-2018',2019:'1658-dengue-2019',2020:'3597-dengue-2020'}.items():
    p=f"{base}datos/fuentes/asis/dengue_{y}.xls"
    if not os.path.exists(p): subprocess.run(['curl','-sSL','--fail','--max-time','120','-A','Mozilla/5.0','-o',p,A+sl+'/file'],check=True)
print('asis',len(os.listdir(base+"datos/fuentes/asis")))
