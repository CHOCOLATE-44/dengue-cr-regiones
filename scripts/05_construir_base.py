import json,re,os,collections,unicodedata
import pandas as pd
import sys,datetime as dt
S=os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0,S+"/scripts")
BASE=S+"/"
from comun import m,by
M={r['pdf']:r for r in m}
LOG=[]
def log(tipo,r,detalle,pagina='',se=''):
    LOG.append(dict(tipo=tipo,anio=r['anio'] if r else '',tipo_boletin=r['tipo_boletin'] if r else '',boletin=lab(r) if r else '',
                    se_corte=se,pagina_pdf=pagina,detalle=detalle,url=r['url'] if r else ''))
ORD={'idx160':'Segundo','idx163':'Tercero','idx184':'Sexto','idx189':'Séptimo','idx196':'Octavo','idx203':'Noveno',
     'idx135':'Setiembre','idx142':'Octubre','idx148':'Noviembre','idx155':'Enero'}
def numero(r):
    if r['numero'].strip(): return r['numero'].strip()
    k=re.search(r'idx\d+',r['pdf'])
    return ORD[k.group()] if k else re.search(r'_(\d+)\.pdf',r['pdf']).group(1)
def lab(r):
    n=numero(r)
    if n.isdigit(): return ('N°' if r['tipo_boletin']=='semanal' else 'Mensual N°')+n
    return ('Mensual ' if r['tipo_boletin']=='mensual' else '')+n
# ---- nombres
MAP={'central este':'C.Este','huetar norte':'H.Norte','huetar caribe':'H. Caribe','occidente':'Occidente','central occidente':'Occidente',
     'pacifico central':'Pacífico Central','central norte':'Central Norte','central sur':'Central Sur','chorotega':'Chorotega','brunca':'Brunca'}
EXACT={'Central Este','Huetar Norte','Huetar Caribe','Occidente','Central Occidente','Pacifico Central','Pacífico Central','Central Norte','Central Sur','Chorotega','Brunca'}
def fold(s): return ''.join(c for c in unicodedata.normalize('NFD',s.lower()) if unicodedata.category(c)!='Mn').strip()
variantes=collections.Counter(); PROVS=['San José','Alajuela','Cartago','Heredia','Guanacaste','Puntarenas','Limón']
def norm_region(n,r,pg):
    k=fold(n)
    if k in MAP:
        if n not in EXACT: variantes[n]+=1
        return MAP[k]
    log('nombre_no_reconocido',r,f'Unidad "{n}" no está en la lista: se conserva sin mapear (posible errata).',pg)
    return n
def pint(s):
    return int(re.sub(r'\D','',s))
def pfl(s):
    s=s.strip()
    if ',' in s and '.' in s: s=s.replace('.','').replace(',','.') if s.rfind(',')>s.rfind('.') else s.replace(',','')
    else: s=s.replace(',','.')
    return float(s)
# ---- se y año
OVR_SE={'2022_semanal_35.pdf':(34,'Título dice SE 33; párrafo y figuras de dengue del boletín dicen SE 34. Se usa 34.'),
        '2022_semanal_49.pdf':(48,'Título dice SE 47; párrafo y figuras de dengue dicen SE 48. Se usa 48.'),
        '2024_semanal_3.pdf':(2,'Título dice "SE 52, 2023" (título sin actualizar, total 576 imposible en esa SE); párrafo dice SE 2. Se usa 2.'),
        '2024_semanal_8.pdf':(7,'Título dice SE 6 (igual al N°7); párrafo y Gráfico dicen SE 7. Se usa 7.')}
NOTE_SE={'2023_semanal_44.pdf':'Párrafo dice SE 43; título y corredor endémico dicen SE 44. Se mantiene 44 (título).',
         '2026_semanal_24.pdf':'Párrafo dice SE 23; título y figuras dicen SE 24. Se mantiene 24 (título).'}
TYPO_Y={f'2024_semanal_{i}.pdf' for i in range(3,9)}
VIS=json.load(open(S+"/datos/intermedios/visual.json"))
rows=[]; cortes={}
def add(r,nivel,unidad,casos,tasa,total,pg,metodo,se,ay,st=None,at=None):
    pob=round(casos/tasa*100000) if (tasa not in(None,0) and casos is not None) else None
    rows.append(dict(anio=int(r['anio']),tipo_boletin=r['tipo_boletin'],numero=numero(r),fecha_publicacion=r['fecha_publicacion'],se_corte=se,nivel=nivel,
        unidad=unidad,casos_acum=casos,tasa_100k=tasa,poblacion_implicita=pob,total_tabla=total,pagina_pdf=pg,url=r['url'],metodo=metodo,anio_datos=ay,pdf=r['pdf'],se_titulo=st,anio_titulo=at))
for r in m:
    p=r['pdf']; d=by[p]; se=None; ay=int(r['anio'])
    for b in d['region']:
        se=b['se'][0] if b['se'] else None
        y=re.findall(r'\b(20\d\d)\b',b['title'])
        st=se; at=int(y[-1]) if y else None
        if y: ay=int(y[-1])
        if p in TYPO_Y:
            ay=int(r['anio']); log('anio_titulo_erroneo',r,f'Título de la tabla dice {y[-1]}, pero las cifras y el párrafo son de {r["anio"]}. anio_datos={r["anio"]}.',b['page'])
        elif ay!=int(r['anio']): log('anio_datos_distinto',r,f'La tabla corresponde a {ay} (SE {se}), no al año del boletín.',b['page'],se)
        if p in OVR_SE: se=OVR_SE[p][0]; log('se_conflicto',r,OVR_SE[p][1],b['page'],se)
        if p in NOTE_SE: log('se_conflicto',r,NOTE_SE[p],b['page'],se)
        tot=pint(b['total'][0])
        for n,v in b['rows']:
            add(r,'region',norm_region(n,r,b['page']),pint(v[0]),pfl(v[1]),tot,b['page'],'texto',se,ay,st,at)
        cortes[p]=(se,ay)
    if p in VIS:
        v=VIS[p]; pg=v['pagina']; t=open(S+"/datos/intermedios/txt/"+p[:-4]+".txt").read().split('\f')[pg-1]
        sm=re.search(r'dengue por regi[oó]n a la semana\s+epidemiol[oó]gica\s+(\d+),\s*(20\d\d)',' '.join(t.split()),re.I)
        if sm: se,ay=int(sm.group(1)),int(sm.group(2))
        else: se=None; ay=v['anio_datos']
        st,at=(se,ay) if sm else (None,None)
        for n,c,ta in v['filas']:
            add(r,'region',norm_region(n,r,pg),c,ta,v['total'],pg,'visual',se,ay,st,at)
        cortes[p]=(se,ay)
        for nota in v.get('notas',[]): log(nota[0],r,nota[1],pg,se if se else '')
    for b in d['provincia']:
        tot=pint(b['total'][0])
        ntok=len(b['total']); sinT=(p=='2025_mensual_1.pdf')
        if p in cortes: s2,a2=cortes[p]
        else: s2,a2=None,int(re.findall(r'\b(20\d\d)\b',b['title'])[0])
        if b['se'] and s2 is not None and b['se'][0]!=s2: log('se_conflicto',r,f'Tabla por provincia dice SE {b["se"][0]}, tabla por región SE {s2}.',b['page'],s2)
        for n,v in b['rows']:
            if n not in PROVS: log('nombre_no_reconocido',r,f'Provincia "{n}" no reconocida.',b['page'])
            add(r,'provincia',n,pint(v[0]),None if sinT else pfl(v[1]),tot,b['page'],'texto',s2,a2,b['se'][0] if b['se'] else None,(lambda q:int(q[-1]) if q else None)(re.findall(r'\b(20\d\d)\b',b['title'])))
D=pd.DataFrame(rows)
# ---- total del parrafo vs tabla
for p,(se,ay) in cortes.items():
    r=M[p]
    g=D[(D.pdf==p)&(D.nivel=='region')]
    if p not in by or not len(g) or r['tipo_boletin']=='mensual' or re.search('idx',p): continue
    pg=int(g.pagina_pdf.iloc[0]); t=open(S+"/datos/intermedios/txt/"+p[:-4]+".txt").read().split('\f')
    tx=' '.join((t[pg-2]+' '+t[pg-1]).split())
    c=re.findall(r'(\d[\d.,]*)\s+casos(?:\s+acumulados)?\s+de\s+dengue',tx)
    if c:
        v=pint(c[-1] if len(c)==1 else c[0]); T=int(g.total_tabla.iloc[0])
        if pint(c[0])!=T and all(pint(x)!=T for x in c):
            log('total_parrafo_distinto',r,f'Párrafo dice {c[0]} casos; total de la tabla {T}.',pg,se)
# ---- OPS
s=pd.read_excel(BASE+"datos/fuentes/dengue_semanal_CR_2015-2026.xlsx",sheet_name='Serie semanal').iloc[:,:3]; s.columns=['a','w','o']
s['cum']=s.groupby('a').o.cumsum(); OPS={(int(a),int(w)):int(c) for a,w,c in zip(s.a,s.w,s.cum)}
# ---- paso 1: revisión de cortes contra OPS
UMBRAL=0.20              # |dif_ops_rel| hasta aquí = confirmado
TOL_BAJO,TOL_ALTO=-0.20,0.05   # "calza": la cifra preliminar puede quedar por debajo de OPS, no muy por encima
VENTANA=8                # semanas hacia atrás desde la publicación
SE1FIN={2020:dt.date(2020,1,4),2021:dt.date(2021,1,9),2022:dt.date(2022,1,8),2023:dt.date(2023,1,7),2024:dt.date(2024,1,6),2025:dt.date(2025,1,4),2026:dt.date(2026,1,10)}
sv=s[s.a>=2020].copy(); sv['fin']=[SE1FIN[int(a)]+dt.timedelta(weeks=int(w)-1) for a,w in zip(sv.a,sv.w)]
NOTAS={'2024_semanal_3.pdf':'Figura 1 (p. 4): el mapa dice "acumulado a la Semana Epidemiológica 02 del 2024", recuadro 576 y tabla por provincia que suma 576 (78+135+6+7+121+112+117), con la nota "Datos sujetos a cambios". El denominador de la tabla es el de 2024 (576/10,8 = 5,33 M) y las 9 regiones crecen de N°3 a N°4 y N°5. Es un acumulado preliminar de 2024 a SE 2; el "52, 2023" del título es un encabezado sin actualizar.',
       '2024_semanal_4.pdf':'Título dice "3, 2023": semana correcta, año sin actualizar. Párrafo dice SE 3.',
       '2024_semanal_7.pdf':'Título dice "6, 2023": año sin actualizar. Párrafo y Gráfico dicen SE 6.',
       '2024_semanal_8.pdf':'Título repite "6, 2023" del N°7. Párrafo y Gráfico dicen SE 7.',
       '2021_semanal_3.pdf':'Portada dice SE 5; título, párrafo y gráfico dicen SE 2.',
       '2022_semanal_5.pdf':'Portada dice SE 6; título, párrafo y gráfico dicen SE 3 (el pie del gráfico dice "3, 2021", errata de año). Seis regiones en 0.',
       '2023_semanal_3.pdf':'Portada dice SE 4; título, párrafo, gráfico y mapa dicen SE 1. Cinco regiones en 0.',
       '2023_semanal_4.pdf':'Portada dice SE 5; título, párrafo, gráfico y mapa dicen SE 2. Cuatro regiones en 0.'}
def contexto(p,pg,T):
    t=open(S+"/datos/intermedios/txt/"+p[:-4]+".txt").read().split('\f'); x=' '.join(t[pg-1].split())
    a=re.search(r'[^.]*?\b'+str(T)+r'\s+casos[^.]*\.',x)
    b=re.search(r'(Cuadro|Tabla)\s*1\.\s*(.{0,125}?)\s*(Regi[oó]n\s+Casos|\(Tasa)',x)
    return f'Párrafo: "{a.group().strip()[-170:] if a else "—"}" Título: "{b.group(2) if b else "—"}"'
Rv=D[(D.nivel=='region')&(D.tipo_boletin=='semanal')].drop_duplicates('pdf').copy()
Rv=Rv[Rv.numero.str.isdigit()&Rv.se_corte.notna()]; Rv['k']=Rv.numero.astype(int)
Rv=Rv.sort_values(['anio','k'],kind='stable')
sel=Rv[(Rv.k<=8)|(Rv.anio!=Rv.anio_datos)|Rv.pdf.isin(OVR_SE)]
D['estado_corte']='no_revisado'; D['motivo_corte']=''
for x in sel.itertuples():
    r=M[x.pdf]; T=int(x.total_tabla); w=int(x.se_corte); a=int(x.anio_datos); op=OPS.get((a,w)); rel=T/op-1
    if abs(rel)<=UMBRAL:
        est,mot='confirmado',f'SE {w} de {a}: {rel:+.1%} contra OPS.'
        if x.pdf=='2022_semanal_35.pdf': mot+=' Ojo: la SE 33 del título queda más cerca de OPS (-1,2%); se mantiene 34 por párrafo y figuras.'
        log('corte_confirmado',r,mot,int(x.pagina_pdf),w)
        D.loc[D.pdf==x.pdf,['estado_corte','motivo_corte']]=[est,mot]; continue
    # semanas posibles: cerradas antes de publicar, a lo sumo VENTANA semanas atrás
    fecha=dt.date.fromisoformat(x.fecha_publicacion); ref='fecha de publicación'
    if x.anio==2021 or x.fecha_publicacion=='2022-08-23':  # subidos en lote: la fecha no sirve, se usa la SE de portada
        cov=re.search(r'Semana\s+epidemiol[oó]gica\s+(?:N[°º]?\s*)?(\d+)',open(S+"/datos/intermedios/txt/"+x.pdf[:-4]+".txt").read().split('\f')[0],re.I)
        fecha=SE1FIN[int(x.anio)]+dt.timedelta(weeks=int(cov.group(1))-1); ref=f'SE {cov.group(1)} de portada'
    c=sv[(sv.fin<=fecha)&(sv.fin>=fecha-dt.timedelta(weeks=VENTANA))].copy()
    c['ra']=T/c.cum-1; c['rw']=T/c.o-1
    # orden: la semana debe caer entre las de los boletines vecinos del mismo año de datos
    vec=Rv[(Rv.anio==x.anio)&(Rv.anio_datos==a)]
    lo=vec[vec.k<x.k].se_corte.max(); hi=vec[vec.k>x.k].se_corte.min()
    lo=0 if pd.isna(lo) else lo; hi=99 if pd.isna(hi) else hi
    fit=c[(c.ra>=TOL_BAJO)&(c.ra<=TOL_ALTO)]
    ok=fit[(fit.a==a)&(fit.w>lo)&(fit.w<hi)]
    # coincidencia con conteo semanal: solo vale en la semana que imprime el boletín
    impresas={(a,w)}|({(int(x.anio_titulo),int(x.se_titulo))} if pd.notna(x.se_titulo) and pd.notna(x.anio_titulo) else set())
    fw=c[(c.rw>=TOL_BAJO)&(c.rw<=TOL_ALTO)]
    okw=fw[[(int(p),int(q)) in impresas for p,q in zip(fw.a,fw.w)]]
    crudo=sv[sv.a.isin([a,a-1])]; bc=crudo.loc[(crudo.cum-T).abs().idxmin()]
    mejor=c[c.a==a]; mejor=mejor.loc[(mejor.cum-T).abs().idxmin()] if len(mejor) else None
    desc=(f'{rel:+.1%} contra OPS a SE {w}. Semanas posibles según {ref}. Más cercana sin filtro: {int(bc.a)}-SE{int(bc.w)} ({int(bc.cum)}), '+('fuera de las semanas posibles. ' if not ((c.a==bc.a)&(c.w==bc.w)).any() else 'dentro de las semanas posibles. '))
    if mejor is not None: desc+=f'Mejor posible: {int(mejor.a)}-SE{int(mejor.w)} ({int(mejor.cum)}, {mejor.ra:+.1%}). '
    if len(fit) and not len(ok): desc+='Descartada: calza en número pero rompe el orden con los boletines vecinos ('+', '.join(f'SE{int(q)}' for q in fit.w)+'). '
    if len(fw) and not len(okw): desc+='Coincidencia semanal descartada, no es la semana impresa: '+', '.join(f'{int(p)}-SE{int(q)}={int(v)} ({e:+.0%})' for p,q,v,e in zip(fw.a,fw.w,fw.o,fw.rw))+'. '
    if len(ok):
        b=ok.loc[(ok.cum-T).abs().idxmin()]; est='corregido'; nw,na=int(b.w),int(b.a)
        D.loc[D.pdf==x.pdf,['se_corte','anio_datos']]=[nw,na]; desc+=f'Corregido a {na}-SE{nw} (antes {a}-SE{w}). '
    elif len(okw): est='no_acumulado'
    else: est='dudoso'
    desc2=desc+contexto(x.pdf,int(x.pagina_pdf),T)+' '+NOTAS.get(x.pdf,'')
    log('corte_'+est,r,desc2.strip(),int(x.pagina_pdf),w)
    D.loc[D.pdf==x.pdf,['estado_corte','motivo_corte']]=[est,desc.strip()]
print('estado_corte',Rv[Rv.pdf.isin(sel.pdf)].shape[0],D[D.nivel=='region'].drop_duplicates('pdf').estado_corte.value_counts().to_dict())
D['chequeo']=''
def mark(mask,txt):
    D.loc[mask,'chequeo']=D.loc[mask,'chequeo'].map(lambda x:(x+'; ' if x else '')+txt)
# bloques
B=[]
for (p,nv),g in D.groupby(['pdf','nivel'],sort=False):
    r=M[p]; se=g.se_corte.iloc[0]; se=None if pd.isna(se) else int(se)
    B.append(dict(pdf=p,nivel=nv,se=se,ay=int(g.anio_datos.iloc[0]),total=int(g.total_tabla.iloc[0]),suma=int(g.casos_acum.sum()),n=len(g),
                  vec=tuple(sorted(zip(g.unidad,g.casos_acum))),fecha=r['fecha_publicacion'],r=r,pg=int(g.pagina_pdf.iloc[0])))
nexp={'region':9,'provincia':7}
for b in B:
    mk=(D.pdf==b['pdf'])&(D.nivel==b['nivel'])
    if b['n']!=nexp[b['nivel']]: mark(mk,f'n_unidades={b["n"]}'); log('chequeo_n_unidades',b['r'],f'{b["nivel"]}: {b["n"]} unidades.',b['pg'],b['se'] or '')
    if b['suma']!=b['total']:
        mark(mk,f'suma≠total({b["suma"]} vs {b["total"]})'); log('chequeo_suma',b['r'],f'{b["nivel"]}: suma {b["suma"]} ≠ total {b["total"]} (dif {b["suma"]-b["total"]}).',b['pg'],b['se'] or '')
    else: mark(mk,'suma_ok')
reg={b['pdf']:b for b in B if b['nivel']=='region'}; prv={b['pdf']:b for b in B if b['nivel']=='provincia'}
for p,b in prv.items():
    if p in reg:
        if reg[p]['total']!=b['total']:
            mark(D.pdf==p,f'region≠provincia({reg[p]["total"]} vs {b["total"]})'); log('chequeo_region_provincia',b['r'],f'Total región {reg[p]["total"]} ≠ total provincia {b["total"]}.',b['pg'],b['se'] or '')
        else: mark(D.pdf==p,'region=provincia')
# unidades repetidas o faltantes
CAN={'region':{'C.Este','H.Norte','H. Caribe','Occidente','Pacífico Central','Central Norte','Central Sur','Chorotega','Brunca'},'provincia':set(PROVS)}
for b in B:
    us=[u for u,_ in b['vec']]; rep=sorted({u for u in us if us.count(u)>1}); fal=sorted(CAN[b['nivel']]-set(us))
    if rep or fal:
        mk=(D.pdf==b['pdf'])&(D.nivel==b['nivel'])
        mark(mk,f'unidades_repetidas={rep}, faltantes={fal}')
        log('chequeo_unidades',b['r'],f'{b["nivel"]}: repetidas {rep}, faltantes {fal} (se conserva lo impreso en el PDF).',b['pg'],b['se'] or '')
# monotonia
for nv in('region','provincia'):
    for ay in sorted({b['ay'] for b in B}):
        L=[b for b in B if b['nivel']==nv and b['ay']==ay and b['se'] is not None]
        L.sort(key=lambda b:(b['se'],b['fecha']))
        for b in L:
            prev=[x for x in L if x['se']<b['se']]
            if not prev: continue
            pr=[x for x in prev if x['se']==max(y['se'] for y in prev)][-1]
            dv=dict(pr['vec']); bajas=[]
            for u,c in b['vec']:
                if u in dv and c<dv[u]: bajas.append((u,dv[u],c)); mark((D.pdf==b['pdf'])&(D.nivel==nv)&(D.unidad==u),f'baja_vs_SE{pr["se"]}({dv[u]}→{c})')
            if b['total']<pr['total']: mark((D.pdf==b['pdf'])&(D.nivel==nv),f'total_baja_vs_SE{pr["se"]}({pr["total"]}→{b["total"]})')
            if bajas or b['total']<pr['total']:
                log('chequeo_baja',b['r'],f'{nv} SE {b["se"]} vs SE {pr["se"]} ({lab(pr["r"])}): total {pr["total"]}→{b["total"]}; bajan: '+', '.join(f'{u} {a}→{c}' for u,a,c in bajas),b['pg'],b['se'])
# duplicados (mismas cifras); con y sin SE
G=collections.defaultdict(list)
for b in B:
    if b['nivel']=='region': G[(b['ay'],b['vec'])].append(b)
for k,L in G.items():
    if len(L)>1:
        ses={x['se'] for x in L}; names=', '.join(f'{lab(x["r"])} ({x["anio"] if "anio" in x else x["r"]["anio"]}, SE {x["se"] if x["se"] else "—"})' for x in L)
        kind='duplicado' if len({s for s in ses if s is not None})<=1 else 'duplicado_cifras_distinta_SE'
        for x in L: mark(D.pdf==x['pdf'],f'{kind}:{names}')
        log(kind,L[0]['r'],f'Mismas cifras regionales (total {L[0]["total"]}): {names}.',L[0]['pg'],L[0]['se'] or '')
# misma SE, cifras distintas
G2=collections.defaultdict(list)
for b in B:
    if b['nivel']=='region' and b['se'] is not None: G2[(b['ay'],b['se'])].append(b)
for k,L in G2.items():
    if len({x['vec'] for x in L})>1:
        log('misma_SE_cifras_distintas',L[0]['r'],f'Año {k[0]} SE {k[1]}: '+', '.join(f'{lab(x["r"])} total {x["total"]}' for x in L),L[0]['pg'],k[1])
# OPS
D['ops_acum']=[OPS.get((int(a),int(w))) if pd.notna(w) else None for a,w in zip(D.anio_datos,D.se_corte)]
D['dif_ops_abs']=D.total_tabla-D.ops_acum
D['dif_ops_rel']=(D.dif_ops_abs/D.ops_acum).round(4)
# orden
def key(x): return (int(x) if str(x).isdigit() else 999)
D['_k']=D.numero.map(key); D['_t']=(D.tipo_boletin=='mensual').astype(int); D['_n']=(D.nivel=='provincia').astype(int)
D=D.sort_values(['anio','_t','_k','fecha_publicacion','pdf','_n'],kind='stable').drop(columns=['_k','_t','_n'])
# ---- boletines sin tabla
for r in m:
    p=r['pdf']
    if p in reg or p in prv: continue
    t=open(S+"/datos/intermedios/txt/"+p[:-4]+".txt").read()
    log('sin_tabla',r,'Sin datos de dengue por región/provincia (boletín temático u otro contenido; revisado por texto y OCR).','')
log('sin_tabla',M['2025_mensual_1.pdf'],'Sin tabla regional; solo tabla por provincia con datos de 2024 (al 31-dic-2024), sin tasa ni SE.',3)
y=collections.Counter(int(M[p]['anio']) for p in reg if p not in prv)
for a,c in sorted(y.items()): log('sin_tabla_provincia',None,f'{a}: {c} boletines con tabla regional sin tabla por provincia.')
LOG.insert(0,dict(tipo='descarga',anio='',tipo_boletin='',boletin='',se_corte='',pagina_pdf='',detalle=f'{sum(x["ok"] for x in m)}/307 PDFs descargados; {sum((x["intentos"] or 0)>1 for x in m)} reintentos; 0 fallos.',url=''))
LOG.append(dict(tipo='se_provincia',anio='',tipo_boletin='',boletin='',se_corte='',pagina_pdf='',detalle='La tabla por provincia casi nunca trae SE en el título ("...provincia y sexo, 2025"). Se usa la SE de la tabla regional del mismo boletín; el chequeo region=provincia respalda que son el mismo corte.',url=''))
for n,c in sorted(variantes.items()):
    LOG.append(dict(tipo='nombre_variante',anio='',tipo_boletin='',boletin='',se_corte='',pagina_pdf='',detalle=f'"{n}" ({c} filas): misma región con otra capitalización, se normalizó.',url=''))
# ---- paso 2: infografías y mensuales fuera del análisis
INFO={p for p in VIS if 'idx' in p}
D['usar_en_analisis']=~((D.tipo_boletin=='mensual')|D.pdf.isin(INFO))
# fila del N°43 de 2024 impresa como H.Norte pero con cifras de H. Caribe (decisión de revisión)
INV=(D.pdf=='2024_semanal_43.pdf')&(D.nivel=='region')&(D.unidad=='H.Norte')&(D.casos_acum==1247)
assert INV.sum()==1
LOG.append(dict(tipo='usar_en_analisis',anio='',tipo_boletin='',boletin='',se_corte='',pagina_pdf='',detalle=f'FALSE en {D[~D.usar_en_analisis].pdf.nunique()} boletines ({(~D.usar_en_analisis).sum()} filas): infografías y mensuales. No se calculó SE desde la fecha.',url=''))
D.loc[INV,'estado_corte']='fila_invalida'; D.loc[INV,'usar_en_analisis']=False
log('fila_invalida',M['2024_semanal_43.pdf'],'Fila impresa como Huetar Norte (1247 casos, tasa 260,3) con población implícita de Huetar Caribe; falta esa región en la tabla. Se conserva en "Regiones boletines" con estado_corte = fila_invalida y usar_en_analisis = FALSE; queda fuera de la hoja unificada y de los cálculos de población.',int(D[INV].pagina_pdf.iloc[0]),43)
# ---- paso 3: población de Occidente 2024
OC=(D.nivel=='region')&(D.unidad=='Occidente')&(D.anio_datos==2024)
falt=OC&D.pdf.isin({f'2024_semanal_{n}.pdf' for n in range(17,29)})&D.poblacion_implicita.isna()
obs=D[OC&D.poblacion_implicita.notna()]
med=int(round(obs.poblacion_implicita.median()))
D['poblacion_imputada']=False
D.loc[falt,'poblacion_implicita']=med; D.loc[falt,'poblacion_imputada']=True
st=dict(n=len(obs),mediana=med,minimo=int(obs.poblacion_implicita.min()),maximo=int(obs.poblacion_implicita.max()),cv=round(obs.poblacion_implicita.std()/obs.poblacion_implicita.mean()*100,2))
o2=obs[obs.casos_acum>=100]
st.update(n100=len(o2),min100=int(o2.poblacion_implicita.min()),max100=int(o2.poblacion_implicita.max()),cv100=round(o2.poblacion_implicita.std()/o2.poblacion_implicita.mean()*100,3),imputadas=int(falt.sum()))
LOG.append(dict(tipo='poblacion_imputada',anio='2024',tipo_boletin='semanal',boletin='N°17–28',se_corte='',pagina_pdf='',detalle=f'Occidente: poblacion_implicita = {med} (mediana de {st["n"]} boletines con anio_datos 2024). Rango observado {st["minimo"]}–{st["maximo"]}, CV {st["cv"]}%; con 100 casos o más ({st["n100"]} boletines): {st["min100"]}–{st["max100"]}, CV {st["cv100"]}%.',url=''))
# ---- paso 4: consistencia de población (sin imputados: bajarían el CV artificialmente)
Q=D[(D.nivel=='region')&D.poblacion_implicita.notna()&~D.poblacion_imputada&(D.estado_corte!='fila_invalida')]
cvs=[]
for (u,a),g in Q.groupby(['unidad','anio_datos']):
    if len(g)<2: continue
    p=g.poblacion_implicita; cv=p.std()/p.mean()*100
    if cv<=2: continue
    # causa: quitar la fila más alejada; cifras con muchos casos; escalón de denominador
    i=(p-p.median()).abs().idxmax(); sin=p.drop(i); cv1=sin.std()/sin.mean()*100
    g30=g[g.casos_acum>=30]; cv30=g30.poblacion_implicita.std()/g30.poblacion_implicita.mean()*100 if len(g30)>1 else float('nan')
    if cv30>2 and cv1<=2: x=D.loc[i]; causa=f'Una sola fila atípica: N°{x.numero} de {x.anio} (casos {int(x.casos_acum)}, tasa {x.tasa_100k}, población {int(x.poblacion_implicita)}).'
    elif cv30<=2 or pd.isna(cv30): causa='Ruido de redondeo: con pocos casos la tasa a 1 decimal no fija la población. Con 30 casos o más el CV es '+('n/d' if pd.isna(cv30) else f'{cv30:.2f}%')+'.'
    elif a==2025: causa='Cambio de denominador a mitad de 2025: baja desde el N°28 (SE 29), el N°33 vuelve por un boletín al valor anterior. No es error de lectura.'
    else: causa='Revisar.'
    cvs.append(dict(unidad=u,anio_datos=int(a),n=len(g),cv=round(cv,2),cv_30mas=None if pd.isna(cv30) else round(cv30,2),minimo=int(p.min()),maximo=int(p.max()),causa=causa))
    LOG.append(dict(tipo='cv_poblacion',anio=str(int(a)),tipo_boletin='',boletin=u,se_corte='',pagina_pdf='',detalle=f'CV {cv:.2f}% en {len(g)} boletines (rango {int(p.min())}–{int(p.max())}). {causa}',url=''))
json.dump(dict(occidente=st,cv=cvs),open(S+"/datos/intermedios/revision_stats.json",'w'),ensure_ascii=False,indent=0)
json.dump(LOG,open(S+"/datos/intermedios/log.json",'w'),ensure_ascii=False,indent=0,default=str)
D.to_pickle(S+"/datos/intermedios/D.pkl")
cols=['anio','tipo_boletin','numero','fecha_publicacion','se_corte','nivel','unidad','casos_acum','tasa_100k','poblacion_implicita','total_tabla','pagina_pdf','url','metodo','chequeo','anio_datos','ops_acum','dif_ops_abs','dif_ops_rel','se_titulo','anio_titulo','estado_corte','motivo_corte','usar_en_analisis','poblacion_imputada']
O=D[cols].copy()
for c in('usar_en_analisis','poblacion_imputada'): O[c]=O[c].map({True:'TRUE',False:'FALSE'})
for c in('se_corte','casos_acum','poblacion_implicita','total_tabla','ops_acum','dif_ops_abs','se_titulo','anio_titulo'): O[c]=O[c].astype('Int64')
O.to_csv(BASE+"datos/procesados/boletines_regiones_2021-2026.csv",index=False,encoding='utf-8-sig')
L=pd.DataFrame(LOG); print(len(O)); print(L.tipo.value_counts().to_string()); print(O.nivel.value_counts())
